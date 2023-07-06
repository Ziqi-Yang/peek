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

(defcustom peek-overlay-distance 1
  "Number of the lines between the peek overlay window and the point. 0 means the current line."
  :type 'natnum
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
Active parameter specify that whether the overlay should be visible. (can be manually changed later)
By default, the custom 'active' property of the overlay is nil.
Return the newly created overlay."
  (when-let (((not (minibufferp))) ;; not in a mini-buffer
             (ol (make-overlay pos pos)))
    (overlay-put ol 'window (get-buffer-window))
    (overlay-put ol 'active active)
    (puthash (get-buffer-window) ol peek-window-overlay-map)))

(defun peek--get-active-region-text ()
  "Get text with properties in region.
Return nil if region is not active."
  (when (use-region-p)
    (buffer-substring (region-beginning) (region-end))))

(defun peek-overlay--toggle-active (ol)
  "Toggle the visibility of the given overlay"
  (when (overlayp ol)
    (if (overlay-get ol 'active)
        (overlay-put ol 'active nil)
      (overlay-put ol 'active t))))

(defun peek-overlay--toggle-active-dwim (ol)
  "Toggle the visibility of the given overlay according to what I mean.
If the overlay's `after-string' property is nil, then the visibility of the given overlay won't toggle."
  (when (overlay-get ol 'after-string)
    (peek-overlay--toggle-active ol)))

(defun peek-overlay-dwim ()
  "Peek overlay do what I mean.
If there is a active region, then store the region into the overlay in the current window;
Else display the overlay in the current window."
  (interactive)
  (let ((ol (peek-get-window-overlay)))
    (unless ol
      (setq ol (peek-create-overlay (peek-overlay--get-supposed-position))))
    (if (use-region-p)
        (overlay-put ol 'after-string (peek--get-active-region-text)) ;; TODO add decoration
      (peek-overlay--toggle-active-dwim ol))
    (when (overlay-get ol 'active)
      (peek-display--overlay-update))))

(defun peek-display--overlay-update ()
  "Update the overlay position in the current window if overlay is active."
  (when-let ((ol (peek-get-window-overlay))
             ((overlay-get ol 'active))
             (pos (peek-overlay--get-supposed-position))) ;; when overlay is active/visible
    (move-overlay ol pos pos)))

(defun peek-overlay--get-supposed-position ()
  "Get the supposed position of the overlay in current window based `peek-overlay-position' and `peek-overlay-distance'.
Return position."
  (save-excursion
    (cond
     ((eq peek-overlay-position 'above)
      (forward-line (- peek-overlay-distance)))
     ((eq peek-overlay-distance 'below)
      (forward-line (1+ peek-overlay-distance))))
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
