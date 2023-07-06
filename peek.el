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

(defcustom peek-overlay-position 'below
  "Specify whether the overlay should be laid above the point or below the point"
  :type '(choice (const :tag "above the point" above)
                 (const :tag "below the point" below))
  :group 'peek)

(defcustom peek-overlay-distance 1
  "Number of the lines between the peek overlay window and the point. 0 means the current line."
  :type 'natnum
  :group 'peek)

(defface peek-overlay-border-face
  ;; '((((background light))
  ;;    :inherit font-lock-doc-face :foreground "#95a5a6")
  ;;   (t
  ;;    :inherit font-lock-doc-face :foreground "#ecf0f1"))
  '((t (:inherit font-lock-doc-face)))
  "Face used for borders of peek overlay window."
  :group 'peek)

(defvar peek-window-overlay-map
  (make-hash-table :test 'equal)
  "Variable structure: { window: overlay }")

(defun peek-clean-dead-overlays ()
  "This function clean those overlays existed in dead windows.
It should be hooked at `window-state-change-hook'."
  (while-let ((windows (hash-table-keys peek-window-overlay-map)))
    (when-let ((window (pop windows))
               ((window-live-p window)))
      (peek-delete-window-overlay window))))

(defun peek-get-window-overlay (&optional window)
  "Get the overlay inside WINDOW.
If WINDOW is nil, then get the overlay inside the current window.
Return nil if there is no overlay in the window"
  (let ((w (if (windowp window) ;; no matter live or not
               window
             (get-buffer-window))))
    (gethash w peek-window-overlay-map)))

(defun peek-delete-window-overlay (&optional WINDOW)
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
    (buffer-substring (region-beginning) (region-end))))

(defun peek-overlay--format-make-border ()
  "Return the border string which is supposed to be used in overlay."
  ;; note that `display-line-numbers-mode' takes 2 + `line-number-display-width' columns
  (let ((total-column-number (1- (window-body-width)))) ;; terminal Emacs will pad '\' at the line end
    (when display-line-numbers-mode
      (setq total-column-number
            (- total-column-number (+ 2 (line-number-display-width)))))
    (propertize
     (concat (make-string total-column-number ?-) "\n")
     'face 'peek-overlay-border-face)))

(defun peek-overlay--format-content (str)
  "Format peek overlay content and return the formatted string.
STR: the content string.
Return: formatted string which is supposed to be inserted into overlay."
  (let ((border (peek-overlay--format-make-border)))
    (concat
     "\n" border
     str 
     "\n" border "\n")))

(defun peek-overlay--set-content (ol str)
  "Set the content for OL.
OL: overlay.
STR: original content string. It will be formatted using `peek-overlay--format-content' method before
being inserted into OL."
  ;; set `after-string' property according to current `active' property
  (let ((display (if (overlay-get ol 'active) 
                     nil
                   ""))
        (content (peek-overlay--format-content str)))
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

(defun peek-overlay-dwim ()
  "Peek overlay do what I mean.
If there is an active region, then store the region into the overlay in the current window;
Else toggle the display of the overlay."
  (interactive)
  (let ((ol (peek-get-window-overlay)))
    (unless ol
      (setq ol (peek-create-overlay (peek-overlay--get-supposed-position))))
    (if (use-region-p)
        (peek-overlay--set-content ol (peek--get-active-region-text))
      (peek-overlay--toggle-active ol))
    (peek-display--overlay-update)))

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

(define-minor-mode peek-mode
  "Gloabl peek mode."
  :global t
  :lighter "peek"
  (cond
   (peek-mode
    (add-hook 'window-state-change-hook peek-clean-dead-overlays))
   (t
    ;; TODO
    (remove-hook 'window-state-change-hook peek-clean-dead-overlays)
    )))

(provide 'peek)
