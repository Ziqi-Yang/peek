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

;; buffer local variable peek-window-overlay-map stores one overlay for every window

;; custom properties for overlays:
;;   1. active: nil or t, control whether the overlay is visible/invisible
;;       you should never directly set the value for this property, use corresponding method instead
;;   2. peek-type: indicate the current type of overlay
;;       - 'string : marked region or eldoc message
;;       - 'xref   : xref-find-definition
;;   3. peek-lines: list of string, only stores strings for 'string type overlay
;;   4. peek-last-xref: string, last searched identifier for 'xref type overlay
;;   5. peek-offset: used for scrolling content inside peek window

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

(defcustom peek-overlay-window-size 11
  "Height of the peek overlay window. A value of 0 may cause undefined behavior."
  :type 'natnum
  :group 'peek)

(defcustom peek-xref-surrounding-above-lines 1
  "Number of lines above the definition found by xref to be shown in the peek window. This value should be less than the `peek-overlay-window-size', otherwise undefined behavior."
  :type 'natnum
  :group 'peek)

(defcustom peek-mode-keymap
  (let ((map (make-sparse-keymap)))
    ;; Browse file
    (define-key map (kbd "M-n") 'peek-next-line)
    (define-key map (kbd "M-p") 'peek-prev-line)
    map)
  "Keymap used for peek mode."
  :type 'keymap
  :group 'typst)

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
    (overlay-put ol 'peek-type   'string)
    (overlay-put ol 'peek-lines  '())
    (overlay-put ol 'peek-offset 0)
    (overlay-put ol 'peek-last-xref "")
    (puthash (get-buffer-window) ol peek-window-overlay-map)))

(defun peek--get-active-region-text ()
  "Get text with properties in region.
Return nil if region is not active."
  (when (use-region-p)
    (let ((text (buffer-substring (region-beginning) (region-end))))
      (setq mark-active nil) ;; deactivate region mark
      text)))

(defun peek-overlay--format-make-border (&optional wdw)
  "Return the border string which is supposed to be used in overlay.
WDW: window body width"
  ;; note that `display-line-numbers-mode' takes 2 + `line-number-display-width' columns
  (let* ((window-body-width (if wdw
                                wdw
                              (window-body-width)))
         (total-column-number (1- window-body-width))) ;; terminal Emacs will pad '\' at the line end
    (when display-line-numbers-mode
      (setq total-column-number
            (- total-column-number (+ 2 (line-number-display-width)))))
    (propertize
     (concat (make-string total-column-number peek-overlay-border-symbol) "\n")
     'face 'peek-overlay-border-face)))

(defun peek-overlay--format-content (str &optional wdw)
  "Format peek overlay content and return the formatted string.
STR: the content string.
WDW: window body width.
Return: formatted string which is supposed to be inserted into overlay."
  (let ((border (peek-overlay--format-make-border wdw))
        (strlen (length str)))
    ;; `default' face is appended to make sure the display in overlay
    ;; is not affected by its surroundings.
    (add-face-text-property 0 strlen 'peek-overlay-content-face 'append str)
    (add-face-text-property 0 strlen 'default 'append str)
    (concat
     "\n" border
     str
     (if (string-match "\n" (substring str (1- strlen) strlen))
         ""
       "\n")
     border "\n")))

(defun peek-overlay--set-content (ol str &optional wdw)
  "Set the content for OL.
OL: overlay.
STR: original content string. It will be formatted using `peek-overlay--format-content' method before
being inserted into OL.
WDW: window body width."
  ;; set `after-string' property according to current `active' property
  (let ((display (if (overlay-get ol 'active) 
                     nil
                   ""))
        (content (peek-overlay--format-content str wdw)))
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

(defun peek-get-or-create-window-overlay ()
  "Get the current window's peek overlay. If there isn't one, the create it."
  (let ((ol (peek-get-window-overlay)))
    (unless ol
      (setq ol (peek-create-overlay (peek-overlay--get-supposed-position))))
    ol))


;;;###autoload
(defun peek-overlay-marked-region-dwim ()
  "Peek overlay do what I mean.
If there is an active region, then store the region into the overlay in the current window;
Else toggle the display of the overlay."
  (interactive)
  (unless global-peek-mode
    (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay)))
    (overlay-put ol 'peek-type 'string)
    (if (use-region-p)
        (progn
          (overlay-put ol 'peek-lines
                       (split-string (peek--get-active-region-text) "\n"))
          (peek-overlay-auto-set-content ol)
          (message "region stored"))
      (peek-overlay--toggle-active ol))
    (peek-display--overlay-update)))

(defun peek--xref-get-surrounding-text (above)
  "Get surrounding content around point from ABOVE lines above point with `peek-overlay-window-size' height.
Both ABOVE and BELOW need to be non-negative"
  (save-excursion
    (let (p1 p2)
      (forward-line (- above))
      (setq p1 (point))
      (forward-line (+ above peek-overlay-window-size))
      (setq p2 (line-end-position))
      (buffer-substring p1 p2))))

(defun peek-overlay-get-content--xref (ol &optional xuli)
  "Get content for xref definition.
OL: overlay.
XULI: xref use last identifier, boolean type"
  (save-excursion
    (unless xuli
      (overlay-put ol 'peek-last-xref (thing-at-point 'symbol)))
    (xref-find-definitions (overlay-get ol 'peek-last-xref))
    (pop (car (xref--get-history))) ;; clear xref history
    (forward-line (overlay-get ol 'peek-offset))
    (peek--xref-get-surrounding-text peek-xref-surrounding-above-lines)))

(defun peek-overlay-get-content--string (ol)
  (let* ((lines (overlay-get ol 'peek-lines))
         (lines-len (length lines))
         (offset (min (1- lines-len) (overlay-get ol 'peek-offset)))
         (bound-max (min (+ offset peek-overlay-window-size) lines-len)))
    (string-join (cl-subseq lines offset bound-max) "\n")))

(defun peek-overlay-auto-set-content (ol &optional xuli)
  "Automatically set content for OL.
OL: overlay.
WDW: window body width.
XULI: xref use last identifier."
  (let ((peek-type (overlay-get ol 'peek-type)))
    (cond
     ((eq peek-type 'string)
      (peek-overlay--set-content ol (peek-overlay-get-content--string ol)))
     ((eq peek-type 'xref)
      ;; it seems like during the following operation, olivetti doesn't work immediately, do we
      ;; need to lock window-body-width before
      (let ((window-body-width (window-body-width)))
        (peek-overlay--set-content ol (peek-overlay-get-content--xref ol xuli) window-body-width)))
     (t
      (error "Invalid peek-type!")))))

;;;###autoload
(defun peek-next-line ()
  "Scroll down current peek window 1 line. Only works when overlay is active/visible."
  (interactive)
  (when-let ((ol (peek-get-or-create-window-overlay))
             ((overlay-get ol 'active))
             (peek-type (overlay-get ol 'peek-type))
             (offset (overlay-get ol 'peek-offset))
             (bound-max (cond
                         ((eq peek-type 'string)
                          (length (overlay-get ol 'peek-lines)))
                         ((eq peek-type 'xref)
                          1.0e+INF) ;; infinity
                         (e
                          (error "Invalid peek-type!")))))
    (overlay-put ol 'peek-offset (min (1+ offset) bound-max))
    (peek-overlay-auto-set-content ol (eq peek-type 'xref))))

;;;###autoload
(defun peek-prev-line ()
  "Scroll up current peek window 1 line. Only works when overlay is active/visible."
  (interactive)
  (when-let ((ol (peek-get-or-create-window-overlay))
             ((overlay-get ol 'active))
             (peek-type (overlay-get ol 'peek-type))
             (offset (overlay-get ol 'peek-offset))
             (bound-min (cond
                         ((or (eq peek-type 'string) (eq peek-type 'xref))
                          0)
                         (e
                          (error "Invalid peek-type!")))))
    (overlay-put ol 'peek-offset (max (1- offset) bound-min))
    (peek-overlay-auto-set-content ol (eq peek-type 'xref))))

;;;###autoload
(defun peek-xref-definition-dwim ()
  "Peek xref definition (the same behavior as you call `xref-find-definitions').
If the peek window is deactivated/invisible, then show peek window for xref definition, else hide the peek window."
  (interactive)
  (unless global-peek-mode
    (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay)))
    (overlay-put ol 'peek-type 'xref)
    (unless (overlay-get ol 'active) ;; set content before shown
      (peek-overlay-auto-set-content ol))
    (peek-overlay--toggle-active ol)
    (peek-display--overlay-update)))

;;;###autoload
(define-minor-mode global-peek-mode
  "Gloabl peek mode."
  :global t
  :lighter "peek"
  :keymap peek-mode-keymap
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
