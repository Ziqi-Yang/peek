(push (expand-file-name "~/projects/emacs/peek/") load-path)

(require 'peek)

(setq
 debug-on-error t
 ;; peek-live-update nil
 ;; peek-overlay-border-symbol nil
 peek-enable-eldoc-message-integration t
 peek-enable-eldoc-display-integration t)

(remove-hook 'eldoc-display-functions 'eldoc-display-in-buffer)

(global-peek-mode)
