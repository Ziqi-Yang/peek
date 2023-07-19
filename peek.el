;;; peek.el --- Peek anything at your fingertip  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Ziqi Yang <mr.meowking@anche.no>

;; Version: 0.1.0
;; Author: Ziqi Yang <mr.meowking@anche.no>
;; Keywords: convenience, tools
;; URL: https://sr.ht/~meow_king/peek
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "26.1"))

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

;; buffer local variable peek-window-overlay-map stores
;; one overlay for every window

;; custom properties for overlays:
;;   1. active: nil or t, control whether the overlay is visible/invisible
;;       you should never directly set the value for this property, use
;;       corresponding method instead
;;   2. peek-type: indicate the current type of overlay
;;       - 'string : marked region or eldoc message
;;       - 'definition
;;   3. peek-lines: list of string, only stores strings for 'string type
;;       overlay
;;   5. peek-offset: used for scrolling content inside peek view
;;   6. peek-markers:
;;       - 'string: (begin-marker, end-marker).  Used to mark the beginning
;;         and the end of the region.  Used to live update content.
;;       - 'definition: (marker).  Stores the marker of a definition.

;;; Code:

(require 'display-line-numbers)
(require 'cl-lib)
(require 'xref)
(require 'subr-x)			;for `hash-table-keys'

(defgroup peek nil
  "Peek mode."
  :group 'convenient)

(defcustom peek-method 'overlay
  "Preferred method to display peek view."
  :type '(choice (const :tag "use overlay" overlay)
                 (const :tag "use child frame" frame)))

(defcustom peek-overlay-position 'above
  "Specify whether the overlay should be laid above the point or below the point."
  :type '(choice (const :tag "above the point" above)
                 (const :tag "below the point" below)))

(defcustom peek-overlay-distance 4
  "Number of the lines between the peek overlay window and the point.
0 means directly above/below the current line."
  :type 'natnum)

(defcustom peek-overlay-border-character ?\N{BOX DRAWINGS LIGHT HORIZONTAL}
  "Specify symbol for peek overlay window border.
Nil to use the similar approach as `make-separator-line'."
  :type 'character)

(defcustom peek-clean-dead-overlays-secs 3600
  "Every the given seconds to perform `peek-clean-dead-overlays' function."
  :type 'natnum)

(defcustom peek-overlay-window-size 11
  "Height of the peek overlay window.  A value of 0 may cause undefined behavior."
  :type 'natnum)

(defcustom peek-definition-surrounding-above-lines 1
  "Number of Lines above the xref definition to be shown in peek view.
This value should be less than the
`peek-overlay-window-size', otherwise undefined behavior."
  :type 'natnum)

(defcustom peek-live-update t
  "Whether to automatically update content when text in marked region changes."
  :type 'boolean)

(defcustom peek-mode-keymap
  (let ((map (make-sparse-keymap)))
    ;; Browse file
    (define-key map (kbd "M-n") 'peek-next-line)
    (define-key map (kbd "M-p") 'peek-prev-line)
    map)
  "Keymap used for peek mode."
  :type 'keymap)

(defface peek-overlay-ascii-border-face
  '((t (:inherit font-lock-doc-face)))
  "Face for borders of peek overlay window.")

(defface peek-overlay-visual-border-face
  `((t (:inherit separator-line :extend t)))
  "Face for borders of peek overlay window.")

(defface peek-overlay-content-face
  '((((background light))
     :background "#ecf0f1" :extend t)
    (t
     :background "#95a5a6" :extend t))
  "Additional face for content text of peek overlay window.")

(defcustom peek-enable-eldoc-message-integration nil
  "Show eldoc message on a peek view.
Related function: `eldoc-message-function'."
  :type 'boolean)

(defcustom peek-enable-eldoc-display-integration nil
  "Show eldoc docs inside a peek view.
Note you need Emacs version >= 28.1.
Related function: `eldoc-display-functions'."
  :type 'boolean)

(defcustom peek-eldoc-message-overlay-position 2
  "Number of the lines between the peek eldoc message overlay window and the point.
0 means directly above the current line.
< 0 means above the current line;
> 0 means below the current line."
  :type 'integer)

(defvar-local peek--eldoc-message-overlay nil
  "Special overlay for handling eldoc message.
Customize `peek-enable-eldoc-message-integration' to enable/disable this feature")

(defvar peek--eldoc-message-status nil
  "The value of this variable shouldn't manually edited.
Whether peek eldoc message is in enabled status.")

(defvar peek--eldoc-previous-message-function nil
  "Previous `eldoc-message-function'.
Previous `eldoc-message-function' before enabling
`peek-enable-eldoc-message-integration'.  Note you are supposed not to
manually change `eldoc-message-function' between enabling and disabling
the `global-peek-mode'.")

(defvar-local peek--window-overlay-map nil
  ;; we need to manually set hash table for each buffer, otherwise we will
  ;; always change its global value
  "Buffer-local window-overlay map.")

(defvar-local peek--live-update-associated-overlays nil
  "Associated overlays for this buffer.
This variable is used to update associated overlays when certain part of this
buffer change.
Related hook: `after-change-functions'.")

(defvar peek--marked-region-markers nil
  "Store the last stored marked region markers (beginning marker . end marker).")

(defvar peek--marked-region-unused nil
  "Indicate that `peek--marked-region-markers' hasn't been used.
This variable is used to make sure that the right peek overlay show after
storing a marked region can cross buffers.
So later peek view toggles are still buffer-local.")

(defvar peek--definition-func nil
  "This variable stores a function which is used to go to definition.")

(defvar peek--definition-func-args nil
  "This variable stores the arguments passed to `peek--definition-func'.")

;;; =============================================================================
;;; Base Functions
;;; =============================================================================

(defun peek--ensure-window-overlay-map ()
  "If the hash table for current buffer hasn't been initialized, then create it."
  (unless peek--window-overlay-map
    (setq-local peek--window-overlay-map (make-hash-table :test 'equal))))

(defun peek-clean-dead-overlays (&rest _args)
  "This function clean those overlays existed in dead windows in all buffers."
  (peek--ensure-window-overlay-map)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let (((hash-table-p peek--window-overlay-map))
                 (windows (hash-table-keys peek--window-overlay-map)))
        (dolist (window windows)
          (unless (window-live-p window)
            (peek-delete-window-overlay window)))))))

;;;###autoload
(defun peek-clean-all-overlays ()
  "Clean all overlays in all buffers."
  (interactive)
  (peek--ensure-window-overlay-map)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let (((hash-table-p peek--window-overlay-map))
                 (windows (hash-table-keys peek--window-overlay-map)))
        (dolist (window windows)
          (peek-delete-window-overlay window))))))

(defun peek-get-window-overlay (&optional window)
  "Get the overlay inside WINDOW.
If WINDOW is nil, then get the overlay inside the current window.
Return nil if there is no overlay in the window"
  (peek--ensure-window-overlay-map)
  (let ((w (if (windowp window)  ; no matter live or not
               window
             (get-buffer-window))))
    (gethash w peek--window-overlay-map)))

(defun peek-delete-window-overlay (&optional window)
  "Delete the overlay inside WINDOW.
If WINDOW is nil, then delete the overlay inside the current window."
  (peek--ensure-window-overlay-map)
  (let ((w (if (windowp window)  ; no matter live or not
               window
             (get-buffer-window))))
    (delete-overlay (gethash w peek--window-overlay-map))
    (remhash w peek--window-overlay-map)))

(defun peek-create-overlay (pos)
  "Create overlay for currently window.
POS: position of the overlay.
By default, the custom _active_ property of the overlay is nil.
Return the newly created overlay."
  (peek--ensure-window-overlay-map)
  (when-let (((not (minibufferp)))  ; not in a mini-buffer
             (ol (make-overlay pos pos)))
    (overlay-put ol 'window (get-buffer-window))
    (overlay-put ol 'active nil)
    (overlay-put ol 'peek-type   'string)
    (overlay-put ol 'peek-lines  '())
    (overlay-put ol 'peek-offset 0)
    (overlay-put ol 'peek-last-xref "")
    (puthash (get-buffer-window) ol peek--window-overlay-map)))

(defun peek-get-or-create-window-overlay (&optional window)
  "Get or create a overlay for WINDOW.
If window is nil, the use current WINDOW.
Get the the given(or current if not given) window's peek overlay.
If there isn't one, the create it."
  (let ((ol (peek-get-window-overlay window)))
    (unless ol
      (setq ol (peek-create-overlay (peek-overlay--get-supposed-position))))
    ol))

(defun peek-overlay--set-active (ol active)
  "Set active/visibility of the given overlay.
OL: overlay object
ACTIVE: boolean type: t stands for visible, nil stands for invisible
Please ensure `after-string' property of OL isn't nil,
otherwise this function does nothing."
  (when-let (((booleanp active)) ;; ensure `active' is nil or t
             (after-str (overlay-get ol 'after-string)))  ; ensure after-str isn't nil
    (if active
        (progn
          (overlay-put ol 'active t)
          (overlay-put ol 'after-string (propertize after-str 'display nil)))
      (progn
        (overlay-put ol 'active nil)
        (overlay-put ol 'after-string (propertize after-str 'display ""))))))

(defun peek-overlay--format-make-border (&optional wdw)
  "Return the border string which is supposed to be used in overlay.
WDW: window body width"
  ;; note that `display-line-numbers-mode' takes 2 + `line-number-display-width' columns
  (if peek-overlay-border-character
      (let* ((window-body-width (if wdw
                                    wdw
                                  (window-body-width)))
             (total-column-number (if (display-graphic-p)
                                      window-body-width
                                    ;; terminal Emacs will pad '\' at the line end
                                    (1- window-body-width)))
             ;; NOTE temporary solution for randomly exceeding 1 border character when
             ;; use `peek-definition'
             (total-column-number (1- total-column-number)))
        (when display-line-numbers-mode
          (setq total-column-number
                (- total-column-number (+ 2 (line-number-display-width)))))
        (propertize
         (concat (make-string total-column-number peek-overlay-border-character) "\n")
         'face 'peek-overlay-ascii-border-face))
    (propertize "\n" 'face 'peek-overlay-visual-border-face)))

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
     border
     str
     (if (string-match "\n" (substring str (1- strlen) strlen))
         ""
       "\n")
     border)))

(defun peek-overlay--set-content (ol str &optional wdw)
  "Set the content for OL.
OL: overlay.
STR: original content string.  It will be formatted using
`peek-overlay--format-content' method before being inserted into OL.
WDW: window body width."
  ;; set `after-string' property according to current `active' property
  (let ((display (if (overlay-get ol 'active)
                     nil
                   ""))
        (content (peek-overlay--format-content str wdw)))
    (overlay-put ol 'after-string
                 (propertize content 'display display))))

(defun peek-overlay--toggle-active (ol)
  "Set active/visibility of the given overlay.
OL: overlay object
Please ensure `after-string' property of OL isn't nil,
otherwise this function does nothing."
  (if (overlay-get ol 'active)
      (peek-overlay--set-active ol nil)
    (peek-overlay--set-active ol t)))

;;;###autoload
(defun peek-overlay-show (&optional window)
  "Provide an API to show peek overlay.Only toggle overlay when it has content.
If WINDOW is nil, then show overlay in the current window."
  (interactive)
  (let ((ol (peek-get-window-overlay window)))
    (peek-overlay--set-active ol t)))

;;;###autoload
(defun peek-overlay-hide (&optional window)
  "Provide an API to show peek overlay.  Only toggle overlay when it has content.
If WINDOW is nil, then show overlay in the current window."
  (interactive)
  (let ((ol (peek-get-window-overlay window)))
    (peek-overlay--set-active ol nil)))

;;;###autoload
(defun peek-overlay-toggle (&optional window)
  "Provide an API to toggle peek overlay.  Only toggle overlay when it has content.
If WINDOW is nil, then show overlay in the current window."
  (interactive)
  (let ((ol (peek-get-window-overlay window)))
    (peek-overlay--toggle-active ol)))

(defun peek--regions-overlap (r1s r1e r2s r2e)
  "Detect whether the two region are overlapped.
The openness and closeness of the given regions should be the same as marker
region.
region1: R1S, R1E; region2: R2S, R2E
Return boolean."
  (if (or (< r1e r1s) (< r2e r2s))
      (error "R1S should >= R1E, R2S should >= R2E")
    (not (or (<= r2e r1s) (<= r1e r2s)))))

;;; =============================================================================
;;; Main Functions
;;; =============================================================================

(defun peek-display--overlay-update (&optional ol)
  "Update the overlay position if overlay is active.
OL: overlay.  Get current overlay if OL is nil."
  (when-let ((ol (if (overlayp ol)
                     ol
                   (peek-get-window-overlay)))
             ((overlay-get ol 'active))  ; only update when overlay is active/visible
             (pos (peek-overlay--get-supposed-position)))
    (move-overlay ol pos pos)))

(defun peek-after-change-function (rb re _plen)
  "This function is used for `after-change-functions' to live update peek view.
RB, RE, _PLEN: see `after-change-functions'."
  (dolist (ol peek--live-update-associated-overlays)
    (message "%s" (overlay-get ol 'peek-markers))
    (if (and (eq (overlay-get ol 'peek-type) 'string)
             (consp (overlay-get ol 'peek-markers))  ; string from region
             ;; the region of ol is still in this buffer
             (eq (current-buffer)
                 (marker-buffer (car (overlay-get ol 'peek-markers)))))
        (progn
          (when-let ((markers (overlay-get ol 'peek-markers))
                     (srb (marker-position (car markers)))  ; source region beginning
                     (sre (marker-position (cdr markers)))  ; source region end
                     ;; when region overlapped (change occurs in source region)
                     ((peek--regions-overlap srb sre rb re))
                     (text (buffer-substring srb sre)))
            (overlay-put ol 'peek-lines
                         (split-string text "\n"))
            (peek-overlay-auto-set-content ol)))
      ;; remove ol when ol's source buffer isn't the current buffer
      (setq peek--live-update-associated-overlays
            (delete ol peek--live-update-associated-overlays))))
  ;; remove hook when there is no associated overlays
  (when (= (length peek--live-update-associated-overlays) 0)
    (remove-hook 'after-change-functions #'peek-after-change-function t)))

(defun peek--mark-region ()
  "Get text with properties in region.
OL: overlay
Return two markers (mb . me) represent the beginning and the end of the region.
Return nil if there is no region."
  (when (use-region-p)
    (let* ((rb (region-beginning))
           (re (region-end))
           (mb (make-marker))
           (me (make-marker)))
      (set-marker mb rb (current-buffer))
      (set-marker me re (current-buffer))
      (deactivate-mark)
      (cons mb me))))

(defun peek-overlay--get-supposed-position ()
  "Get the supposed position of a overlay.
The calculation is based on `peek-overlay-position' and `peek-overlay-distance'.
Return position."
  (save-excursion
    (cl-case peek-overlay-position
      (above (forward-line (- peek-overlay-distance)))
      (below (forward-line (1+ peek-overlay-distance)))
      (t (error "Unrecognized value for `peek-overlay-position`")))
    (point)))

;;;###autoload
(defun peek-overlay-eldoc-message-hide ()
  "Hide peek eldoc message overlay."
  (when peek--eldoc-message-overlay
    (peek-overlay--set-active peek--eldoc-message-overlay nil)))

;;;###autoload
(defun peek-overlay-eldoc-message-enable ()
  "Show peek eldoc message overlay."
  (interactive)
  (add-hook 'post-command-hook #'peek-overlay-eldoc-message-hide)
  (setq peek--eldoc-message-status t)
  ;; avoid covering `peek--eldoc-previous-message-function'
  (unless (eq eldoc-message-function 'peek-overlay-eldoc-message-function)
    (setq peek--eldoc-previous-message-function eldoc-message-function
          eldoc-message-function 'peek-overlay-eldoc-message-function)))

;;;###autoload
(defun peek-overlay-eldoc-message-disable ()
  "Disable peek eldoc message overlay."
  (interactive)
  (peek-overlay-eldoc-message-hide)
  (remove-hook 'post-command-hook #'peek-overlay-eldoc-message-hide)
  (setq peek--eldoc-message-status nil
        eldoc-message-function peek--eldoc-previous-message-function))

;;;###autoload
(defun peek-overlay-eldoc-message-toggle-stauts ()
  "Toggle (enable/disable) peek eldoc message overlay."
  (interactive)
  (if peek--eldoc-message-status
      (peek-overlay-eldoc-message-disable)
    (peek-overlay-eldoc-message-enable)))

;;;###autoload
(defun peek-overlay-eldoc-message-function (format-string &rest args)
  "Display peek overlay window FORMAT-STRING under point with extra ARGS."
  (when-let ((format-string)
             (ol (if peek--eldoc-message-overlay
                     peek--eldoc-message-overlay
                   (let ((ol (make-overlay 1 1)))
                     (overlay-put ol 'window (get-buffer-window))
                     (overlay-put ol 'active nil)
                     (overlay-put ol 'peek-type   'string)
                     (overlay-put ol 'peek-lines  '())
                     (overlay-put ol 'peek-offset 0)
                     (overlay-put ol 'peek-last-xref "")
                     (setq peek--eldoc-message-overlay ol))))
             (pos (peek-overlay-eldoc-message--get-supposed-position)))
    (move-overlay ol pos pos)
    (peek-overlay--set-active ol t)
    (overlay-put ol 'peek-lines (split-string (apply #'format-message format-string args) "\n"))
    (peek-overlay-auto-set-content ol)))

(defun peek-overlay-eldoc-message--get-supposed-position ()
  "Get supposed eldoc message overlay position.
The calculation is based on `peek-eldoc-message-overlay-position' and
the current point.
Return position."
  (save-excursion
    (forward-line peek-eldoc-message-overlay-position)
    (point)))

;;;###autoload
(defun peek-display-eldoc (docs interactive)
  "Related function: `eldoc-display-functions'.
DOCS: docs passed by eldoc.
Only works when INTERACTIVE is t."
  (when (>= emacs-major-version 28)
    (when-let ((interactive)
               (docs-content (with-current-buffer (eldoc--format-doc-buffer docs)
                               (buffer-string)))
               (ol (peek-get-or-create-window-overlay)))
      (overlay-put ol 'peek-type 'string)
      (overlay-put ol 'peek-lines
                   (split-string docs-content "\n"))
      (peek-overlay-auto-set-content ol)
      (peek-overlay--set-active ol t)
      (peek-display--overlay-update ol))))

(defun peek-definition--get-surrounding-text ()
  "Get surrounding content for xref definition.
Get surrounding content around point from
`peek-definition-surrounding-above-lines'
lines above the point with `peek-overlay-window-size' height."
  (let ((above peek-definition-surrounding-above-lines)
        p1 p2)
    (forward-line (- above))
    (setq p1 (point))
    (forward-line (+ above peek-overlay-window-size))
    (setq p2 (line-end-position))
    (font-lock-ensure p1 p2)
    (buffer-substring p1 p2)))

(defun peek-definition--set-marker (ol func &optional args)
  "Call get definition function, set marker of that function and get the content.
OL: overlay
FUNC, ARGS see `peek-definition'."
  (let ((buffer (current-buffer))
        (pos (point))
        (marker (make-marker))
        content)
    ;; go to definition
    (apply func args)
    ;; set marker for the definition
    (set-marker marker (point) (current-buffer))
    (overlay-put ol 'peek-markers (list marker))
    ;; set peek-offset to 0
    (overlay-put ol 'peek-offset 0)
    ;; get content
    (setq content (peek-definition--get-surrounding-text))
    ;; go back to original place
    (switch-to-buffer buffer)
    (goto-char pos)
    content))

(defun peek-definition--get-content (ol)
  "Get the content of the definition.
This function should be called only after once called
`peek-definition--set-marker'.
OL: overlay."
  (let ((marker (car (overlay-get ol 'peek-markers))))
    (message "%s" (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char (marker-position marker))
        (let* ((offset (overlay-get ol 'peek-offset))
               (left-line-count (forward-line offset)))
          (overlay-put ol 'peek-offset (- offset left-line-count)))
        (peek-definition--get-surrounding-text)))))

(defun peek-overlay-get-content--string (ol)
  "Get content for overlay.  Peek-type: string.
OL: overlay."
  (let* ((lines (overlay-get ol 'peek-lines))
         (lines-len (length lines))
         (offset (min (1- lines-len) (overlay-get ol 'peek-offset)))
         (bound-max (min (+ offset peek-overlay-window-size) lines-len)))
    (string-join (cl-subseq lines offset bound-max) "\n")))

(defun peek-overlay-auto-set-content (ol &optional uld)
  "Automatically set content for OL.
OL: overlay.
ULD: use last definition.
When ULD is nil, and peek view is _definition_ type, please also set
`peek--definition-func' and `peek--definition-func-args'."
  (let ((peek-type (overlay-get ol 'peek-type)))
    (cond
     ((eq peek-type 'string)
      (peek-overlay--set-content ol (peek-overlay-get-content--string ol)))
     ((eq peek-type 'definition)
      ;; it seems like during the following operation, olivetti doesn't work immediately, do we
      ;; need to lock window-body-width before
      (let ((window-body-width (window-body-width)))
        (peek-overlay--set-content
         ol
         (if uld
             (peek-definition--get-content ol)
           (peek-definition--set-marker
            ol
            peek--definition-func
            peek--definition-func-args))
         window-body-width)))
     (t
      (error "Invalid peek-type!")))))

;;;###autoload
(defun peek-next-line ()
  "Scroll down current peek view 1 line.
Only works when overlay is active/visible."
  (interactive)
  (when-let ((ol (peek-get-or-create-window-overlay))
             ((overlay-get ol 'active))
             (peek-type (overlay-get ol 'peek-type))
             (offset (overlay-get ol 'peek-offset))
             (bound-max (cl-case peek-type
                          (string
                           (1- (length (overlay-get ol 'peek-lines))))
                          (definition
                           1.0e+INF)  ; infinity (buffer end)
                          (t
                           (error "Invalid peek-type!")))))
    (overlay-put ol 'peek-offset (min (1+ offset) bound-max))
    (peek-overlay-auto-set-content ol t)))

;;;###autoload
(defun peek-prev-line ()
  "Scroll up current peek view 1 line.
Only works when overlay is active/visible."
  (interactive)
  (when-let ((ol (peek-get-or-create-window-overlay))
             ((overlay-get ol 'active))
             (peek-type (overlay-get ol 'peek-type))
             (offset (overlay-get ol 'peek-offset))
             (bound-min (cl-case peek-type
                          (string
                           0)
                          (definition
                           '-inf)
                          (t
                           (error "Invalid peek-type!"))))
             (next-offset (if (eq bound-min '-inf)
                              (1- offset)
                            (max (1- offset) bound-min))))
    (overlay-put ol 'peek-offset next-offset)
    (peek-overlay-auto-set-content ol t)))

;;;###autoload
(define-minor-mode global-peek-mode
  "Gloabl peek mode."
  :global t
  :lighter " peek"
  :keymap peek-mode-keymap
  (cond
   (global-peek-mode
    (when peek-enable-eldoc-message-integration  ; eldoc-message-function
      (peek-overlay-eldoc-message-enable))
    (when (and (>= emacs-major-version 28)
               peek-enable-eldoc-display-integration)  ; eldoc-display-functions
      (add-hook 'eldoc-display-functions #'peek-display-eldoc))
    (peek-clean-all-overlays)
    (run-with-timer peek-clean-dead-overlays-secs t #'peek-clean-dead-overlays)
    ;; (add-to-list 'window-state-change-functions 'peek-clean-dead-overlays)  ; may cause performance error
    (add-hook 'post-command-hook #'peek-display--overlay-update))
   (t
    (when peek-enable-eldoc-message-integration
      (peek-overlay-eldoc-message-disable))
    (when (and (>= emacs-major-version 28)
               peek-enable-eldoc-display-integration)
      (remove-hook 'eldoc-display-functions #'peek-display-eldoc))
    (peek-clean-all-overlays)
    ;; (setq window-state-change-functions (remove 'peek-clean-dead-overlays window-state-change-functions))
    (remove-hook 'post-command-hook #'peek-display--overlay-update))))

;;;###autoload
(defun peek-overlay-dwim ()
  "Peek overlay do what I mean.
If there is an active region, then store the region into the overlay
in the current window;
Else toggle the display of the overlay.
Related features:
  - store marked region
  - hide/show peek view."
  (interactive)
  (unless global-peek-mode
    (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay)))
    (if (use-region-p)
        (progn
          (unless (eq (overlay-get ol 'peek-type) 'string)
            (overlay-put ol 'peek-offset 0)
            (overlay-put ol 'peek-type 'string))
          (setq peek--marked-region-markers (peek--mark-region)
                peek--marked-region-unused t)
          (message "region stored"))
      (progn
        (when (and (eq (overlay-get ol 'active) nil)  ; after toggle, overlay show
                   (eq peek--marked-region-unused t))
          (let* ((mb (car peek--marked-region-markers))
                 (me (cdr peek--marked-region-markers))
                 (source-buffer (marker-buffer mb))
                 (text (with-current-buffer source-buffer
                         (buffer-substring
                          (marker-position mb) (marker-position me)))))
            (when peek-live-update
              (with-current-buffer source-buffer
                ;; add this overlay to source buffer associated overlay list
                (add-to-list 'peek--live-update-associated-overlays ol)
                ;; add local hook
                (add-hook 'after-change-functions #'peek-after-change-function nil t)))
            (overlay-put ol 'peek-markers peek--marked-region-markers)
            (overlay-put ol 'peek-lines
                         (split-string text "\n")))
          (peek-overlay-auto-set-content ol)
          (setq peek--marked-region-unused nil))
        (peek-overlay--toggle-active ol)))))

;;;###autoload
(defun peek-view-refresh ()
  "Refresh content in the current peek view.
Peek view will only be refreshed when it is used to display a marked region."
  (interactive)
  (when-let ((ol (peek-get-window-overlay))
             ((eq (overlay-get ol 'peek-type) 'string)) ; NOTE
             (markers (overlay-get ol 'peek-markers))
             (mb (car markers))
             (me (cdr markers))
             (source-buffer (marker-buffer mb))
             (text (with-current-buffer source-buffer
                     (buffer-substring
                      (marker-position mb) (marker-position me)))))
    (overlay-put ol 'peek-lines
                 (split-string text "\n"))
    (peek-overlay-auto-set-content ol)))

;;;###autoload
(defun peek-overlay-set-custom-content (str &optional window)
  "Set custom content for the peek overlay window in current buffer.
STR: content string.
WINDOW: the attached window object in current buffer.  If nil, the use
current window.
You can pass STR with properties(like face) to show change
the display of the content.  The STR will be splitted into
lines so the peek view can be scrolled."
  (unless global-peek-mode
    (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay window)))
    (overlay-put ol 'peek-type 'string)
    (overlay-put ol 'peek-markers nil)
    (overlay-put ol 'peek-lines (split-string str "\n"))
    (peek-overlay-auto-set-content ol)))

;;;###autoload
(defun peek-definition (func &optional args)
  "Peek the definition using given FUNC.
FUNC: function.  This function should act like going to the definition.
ARGS: a list of parameters passed to the function call when INTERACTIVE is nil.
Example: `peek-xref-definition'."
  (unless global-peek-mode
    (global-peek-mode 1))
  (let ((ol (peek-get-or-create-window-overlay))
        (peek--definition-func func)
        (peek--definition-func-args args))
    
    (unless (eq (overlay-get ol 'peek-type) 'defintion)
      (overlay-put ol 'peek-offset 0)
      (overlay-put ol 'peek-type 'definition))

    (peek-overlay-auto-set-content ol)
    (peek-overlay--set-active ol t)))

(defun peek-goto-xref-defintion-func (identifier)
  "Go to definition of IDENTIFIER."
  (xref-find-definitions identifier)
  ;; clear xref history
  (pop (car (xref--get-history))))

;;;###autoload
(defun peek-xref-definition ()
  "Peek xref definition."
  (interactive)
  (peek-definition
   'peek-goto-xref-defintion-func
   (list (thing-at-point 'symbol))))

(provide 'peek)

;;; peek.el ends here
