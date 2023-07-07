;;; peek.el --- Peek anything at your fingertip  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ziqi Yang <mr.meowking@anche.no>

;; Version: 0.1.0
;; Author: Ziqi Yang <mr.meowking@anche.no>
;; Keywords: typst languages tree-sitter
;; URL: https://sr.ht/~meow_king/peek
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "27")) ;; NOTE haven't been tested

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defgroup peek nil
  "Peek mode."
  :group 'convenient)

(defcustom peek-method 'overlay
  "Preferred method to display peek window.
NOTE: currently only support 'overlay'"
  :type '(choice (const :tag "use overlay" overlay)
                 (const :tag "use child frame" frame))
  :group 'peek)

(defcustom peek-overlay-position 'above
  "Specify whether the overlay should be laid above the point or below the point"
  :type '(choice (const :tag "above the point" above)
                 (const :tag "below the point" below))
  :group 'peek)

(defcustom peek-overlay-distance 4
  "Number of the lines between the peek overlay window and the point. 0 means the current line."
  :type 'natnum
  :group 'peek)

(defcustom peek-overlay-border-symbol ?-
  "Specify symbol for peek overlay window border"
  :type 'character
  :group 'peek)

(defcustom peek-clean-dead-overlays-secs 3600
  "Every the given seconds to perform `peek-clean-dead-overlays' function."
  :type 'natnum
  :group 'peek)

(defcustom peek-xref-surrounding-above-lines 1
  "Number of lines above the definition found by xref to be shown in the peek window."
  :type 'natnum
  :group 'peek)

(defcustom peek-xref-surrounding-below-lines 10
  "Number of lines below the definition found by xref to be shown in the peek window."
  :type 'natnum
  :group 'peek)

(defface peek-overlay-border-face
  ;; '((((background light))
  ;;    :inherit font-lock-doc-face :foreground "#95a5a6")
  ;;   (t
  ;;    :inherit font-lock-doc-face :foreground "#ecf0f1"))
  '((t (:inherit font-lock-doc-face)))
  "Face for borders of peek overlay window."
  :group 'peek)

(defface peek-overlay-content-face
  '((((background light))
     :background "#ecf0f1" :extend t)
    (t
     :background "#95a5a6" :extend t))
  "Additional face for content text of peek overlay window."
  :group 'peek)

(defvar-local peek-window-overlay-map
    (make-hash-table :test 'equal)
  "This variable shouldn't be customized by user. Variable structure: { window: overlay }")

(defun peek-clean-dead-overlays (&rest _args)
  "This function clean those overlays existed in dead windows in all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (let ((windows (hash-table-keys peek-window-overlay-map)))
        (dolist (window windows)
          (unless (window-live-p window)
            (peek-delete-window-overlay window)))))))

;;;###autoload
(defun peek-clean-all-overlays ()
  "Clean all overlays in all buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (let ((windows (hash-table-keys peek-window-overlay-map)))
        (dolist (window windows)
          (peek-delete-window-overlay window))))))

(defun peek-get-window-overlay (&optional window)
  "Get the overlay inside WINDOW.
If WINDOW is nil, then get the overlay inside the current window.
Return nil if there is no overlay in the window"
  (let ((w (if (windowp window) ;; no matter live or not
               window
             (get-buffer-window))))
    (gethash w peek-window-overlay-map)))

(defun peek-delete-window-overlay (&optional window)
  "Delete the overlay inside WINDOW.
If WINDOW is nil, then delete the overlay inside the current window."
  (let ((w (if (windowp window) ;; no matter live or not
               window
             (get-buffer-window))))
    (delete-overlay (gethash w peek-window-overlay-map))
    (remhash w peek-window-overlay-map)))

(defun peek-create-overlay (pos &optional active)
  "Create overlay for currently window.
By default, the custom 'active' property of the overlay is nil.
Return the newly created overlay."
  (when-let (((not (minibufferp))) ;; not in a mini-buffer
             (ol (make-overlay pos pos)))
    (overlay-put ol 'window (get-buffer-window))
    (overlay-put ol 'active nil)
    (puthash (get-buffer-window) ol peek-window-overlay-map)))

(defun peek--get-active-region-text ()
  "Get text with properties in region.
Return nil if region is not active."
  (when (use-region-p)
    (let ((text (buffer-substring (region-beginning) (region-end))))
      (setq mark-active nil) ;; deactivate region mark
      text)))

(defun peek-overlay--format-make-border (&optional borderlen)
  "Return the border string which is supposed to be used in overlay."
  ;; note that `display-line-numbers-mode' takes 2 + `line-number-display-width' columns
  (let* ((window-body-width (if borderlen
                                borderlen
                              (window-body-width)))
         (total-column-number (1- window-body-width))) ;; terminal Emacs will pad '\' at the line end
    (when display-line-numbers-mode
      (setq total-column-number
            (- total-column-number (+ 2 (line-number-display-width)))))
    (propertize
     (concat (make-string total-column-number peek-overlay-border-symbol) "\n")
     'face 'peek-overlay-border-face)))

(defun peek-overlay--format-content (str &optional borderlen)
  "Format peek overlay content and return the formatted string.
STR: the content string.
Return: formatted string which is supposed to be inserted into overlay."
  (let ((border (peek-overlay--format-make-border borderlen))
        (strlen (length str)))
    ;; `default' face is appended to make sure the display in overlay
    ;; is not affected by its surroundings.
    (add-face-text-property 0 strlen 'peek-overlay-content-face 'append str)
    (add-face-text-property 0 strlen 'default 'append str)
    (concat
     "\n" border
     str
     "\n" border "\n")))

(defun peek-overlay--set-content (ol str &optional borderlen)
  "Set the content for OL.
OL: overlay.
STR: original content string. It will be formatted using `peek-overlay--format-content' method before
being inserted into OL."
  ;; set `after-string' property according to current `active' property
  (let ((display (if (overlay-get ol 'active) 
                     nil
                   ""))
        (content (peek-overlay--format-content str borderlen)))
    (overlay-put ol 'after-string
                 (propertize content 'display display))))

(defun peek-overlay--set-active (ol active)
  "Set active/visibility of the given overlay.
OL: overlay object
ACTIVE: boolean type: t stands for visible, nil stands for invisible
Please ensure `after-string' property of OL isn't nil, otherwise this function does nothing."
  (when-let (((booleanp active)) ;; ensure `active' is nil or t
             (after-str (overlay-get ol 'after-string))) ;; ensure after-str isn't nil
    (if active
        (progn
          (overlay-put ol 'active t)
          (overlay-put ol 'after-string (propertize after-str 'display nil)))
      (progn
        (overlay-put ol 'active nil)
        (overlay-put ol 'after-string (propertize after-str 'display ""))))))

(defun peek-overlay--toggle-active (ol)
  "Set active/visibility of the given overlay.
OL: overlay object
Please ensure `after-string' property of OL isn't nil, otherwise this function does nothing."
  (if (overlay-get ol 'active)
      (peek-overlay--set-active ol nil)
    (peek-overlay--set-active ol t)))

;;;###autoload
(defun peek-overlay-show (&optional window)
  "Provide API to show peek overlay.Only toggle overlay when it has content.
If WINDOW is nil, then show overlay in the current window."
  (interactive)
  (let ((ol (peek-get-window-overlay window)))
    (peek-overlay--set-active ol t)))

;;;###autoload
(defun peek-overlay-hide (&optional window)
  "Provide API to show peek overlay. Only toggle overlay when it has content.
If WINDOW is nil, then show overlay in the current window."
  (interactive)
  (let ((ol (peek-get-window-overlay window)))
    (peek-overlay--set-active ol nil)))

;;;###autoload
(defun peek-overlay-toggle (&optional window)
  "Provide API to toggle peek overlay. Only toggle overlay when it has content.
If WINDOW is nil, then show overlay in the current window."
  (interactive)
  (let ((ol (peek-get-window-overlay window)))
    (peek-overlay--toggle-active ol)))

(defun peek-display--overlay-update ()
  "Update the overlay position in the current window if overlay is active."
  (when-let ((ol (peek-get-window-overlay))
             ((overlay-get ol 'active)) ;; only update when overlay is active/visible
             (pos (peek-overlay--get-supposed-position)))
    (move-overlay ol pos pos)))

(defun peek-overlay--get-supposed-position ()
  "Get the supposed position of the overlay in current window based `peek-overlay-position' and `peek-overlay-distance'.
Return position."
  (save-excursion
    (cl-case peek-overlay-position
      (above (forward-line (- peek-overlay-distance)))
      (below (forward-line (1+ peek-overlay-distance))))
    (point)))

;;;###autoload
(defun peek-overlay-dwim ()
  "Peek overlay do what I mean.
If there is an active region, then store the region into the overlay in the current window;
Else toggle the display of the overlay."
  (interactive)
  (let ((ol (peek-get-window-overlay)))
    (unless ol
      (setq ol (peek-create-overlay (peek-overlay--get-supposed-position))))
    (if (use-region-p)
        (progn
          (peek-overlay--set-content ol (peek--get-active-region-text))
          (message "region stored"))
      (peek-overlay--toggle-active ol))
    (peek-display--overlay-update)))

(defun peek--xref-get-surrounding-text (above below)
  "Get surrounding content around point from ABOVE lines above point to BELOW lines below point.
Both ABOVE and BELOW need to be non-negative"
  (save-excursion
    (let (p1 p2)
      (forward-line (- above))
      (setq p1 (point))
      (forward-line (+ above below))
      (setq p2 (line-end-position))
      (buffer-substring p1 p2))))

(defun peek--xref-get-definition-content ()
  "Get content for xref definition."
  (save-excursion
    (let ((xref-prompt-for-identifier nil)) ;; don't prompt, just use identifier at point
      (call-interactively 'xref-find-definitions))
    (pop (car (xref--get-history))) ;; clear xref history
    (peek--xref-get-surrounding-text
     peek-xref-surrounding-above-lines peek-xref-surrounding-below-lines)))

;;;###autoload
(defun peek-xref-definition-dwim ()
  "Peek xref definition (the same behavior as you call `xref-find-definitions').
If the peek window is deactivated/invisible, then show peek window for xref definition, else hide the peek window."
  (interactive)
  (let ((ol (peek-get-window-overlay)))
    (unless ol
      (setq ol (peek-create-overlay (peek-overlay--get-supposed-position))))
    (unless (overlay-get ol 'active) ;; set content before shown
      ;; it seems like during the following operation, olivetti doesn't instantly work, do we
      ;; need to lock window-body-width before
      (let ((window-body-width (window-body-width)))
        (peek-overlay--set-content ol (peek--xref-get-definition-content) window-body-width)))
    (peek-overlay--toggle-active ol)
    (peek-display--overlay-update)))

;;;###autoload
(define-minor-mode global-peek-mode
  "Gloabl peek mode."
  :global t
  :lighter "peek"
  (cond
   (global-peek-mode
    (peek-clean-all-overlays)
    (run-with-timer peek-clean-dead-overlays-secs t 'peek-clean-dead-overlays)
    ;; (add-to-list 'window-state-change-functions 'peek-clean-dead-overlays) ;; may cause performance error
    (add-hook 'post-command-hook 'peek-display--overlay-update))
   (t
    (peek-clean-all-overlays)
    ;; (setq window-state-change-functions (remove 'peek-clean-dead-overlays window-state-change-functions))
    (remove-hook 'post-command-hook 'peek-display--overlay-update))))

(provide 'peek)
