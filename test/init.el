(push (expand-file-name "~/projects/emacs/peek/") load-path)

(require 'peek)

(setq
 ;; peek-live-update nil
 peek-enable-eldoc-message-integration t
 peek-enable-eldoc-display-integration t)

(remove-hook 'eldoc-display-functions 'eldoc-display-in-buffer)

(global-peek-mode)
