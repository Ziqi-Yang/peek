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
  "Peek mode"
  :group 'convenient)

(defvar peek-window-overlay-map
  (make-hash-table :test 'equal)
  "This variable stores overlay for each window")

(defun peek-clean-dead-overlays()
  "This function clean those overlays existed in dead windows.
It should be hooked at `window-state-change-hook'"
  (while-let ((windows (hash-table-keys peek-window-overlay-map)))
    (when-let ((window (pop windows))
               ((window-live-p window)))
      (delete-overlay (gethash window peek-window-overlay-map))
      (remhash window peek-window-overlay-map))))

(defun peek-get-current-overlay()
  "Get overlay at current window."
  (gethash (get-buffer-window) peek-window-overlay-map))

(defun peek-create-overlay(pos)
  "Create overlay for currently window"
  (let ((peek--ol (make-overlay pos pos)))
    (overlay-put peek--ol 'window (get-buffer-window))
    (puthash (get-buffer-window) peek--ol peek-window-overlay-map)))

(define-minor-mode peek-mode
  "Gloabl peek mode."
  :global t
  :lighter "peek"
  (cond
   (peek-mode
    ;; TODO
    )
   (t
    ;; TODO
    )))

(provide 'peek)
