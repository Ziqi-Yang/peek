# Peek

![Static Badge](https://img.shields.io/badge/made_with-emacs-purple)

[project](https://sr.ht/~meow_king/peek/)/[mailing lists](https://sr.ht/~meow_king/peek/lists)/[tickets](https://sr.ht/~meow_king/peek/trackers)  

## Features
1. Peek window follows your cursor.
2. Buffer and window local peek windows. Also capable for content sharing between different buffer and windows.
3. Store text of marked region, and then display it on the peek window.
4. Peek the destination of `xref-find-definitions`.
5. `eldoc-message-function` and `eldoc-display-functions` integration.
6. Scroll up or down inside peek window. 

TODO `eldoc-display-functions` may be a `hook-type`
TODO cannot display eldoc message?

## Demo

## Usage

- Store marked region and peek it later:
   1. Mark a region
   2. Use `peek-overlay-dwim` to store the region
   3. Use `peek-overlay-dwim` again to show a peek window of the marked content. You can use this command in other buffer/window to show the marked content. 
   4. Use `peek-overlay-dwim` to hidden the peek window.
   
- Find definition of a symbol.
   1. Use `peek-xref-definition-dwim` to show the definition at the cursor point in peek window.
   2. Use `peek-xref-definition-dwim` again to hide the peek window. You can also use `peek-overlay-dwim` to do this job.
   
- Display eldoc for the symbol under cursor.
  1. Please refer to **Configuration** -> **Example** Section to enable the eldoc display function integration.
  2. Use `eldoc` to diplay eldoc for the symbol under cursor.
  3. Use `peek-overlay-dwim` to hide the peek window.
  
- Display eldoc message
  Customize `peek-enable-eldoc-message-integration` to `t` to enable the eldoc message integration. You may also want to customize `peek-eldoc-message-overlay-position` too.   
  Note: `peek-overlay-eldoc-message-toggle-stauts` function can be used to toggle whether the peek window for eldoc message will be shown.
  
- Scroll up/down in the peek window
  - `M-n`: peek-next-line 
  - `M-p`: peek-prev-line 

## Configuration

### Example

``` emacs-lisp
(use-package peek
  :straight (:type git :host sourcehut :repo "meow_king/peek")
  ;; it seems like making keybindings for `peek-mode-keymap' here will
  ;; make `global-peek-mode' not able to be automatically enabled in `config' section
  ;; :bind
  ;; ;; default bindings
  ;; (:map peek-mode-keymap
  ;;   ("M-n" . peek-next-line)
  ;;   ("M-p" . peek-prev-line))

  :custom
  ;; only list some mostly-want-changed settings 
  (peek-overlay-window-size 11) ;; lines
  ;; one line before the place found by `xref-find-definitions' will also appear in peek window 
  (peek-xref-surrounding-above-lines 1)
  (peek-overlay-position 'above) ;; or below

  (peek-enable-eldoc-message-integration t) ;; enable `eldoc-message-function' integration
  ;; eldoc message overlay at two lines below the point
  ;; It's recommended to set the eldoc message overlay below the point since the pop up of
  ;; the peek overlay may cause visual shaking
  (peek-eldoc-message-overlay-position 2)

  (peek-enable-eldoc-display-integration t) ;; enable `eldoc-display-functons'  integration

  :config
  (global-peek-mode 1)

  ;; Keybindings 
  ;; `keymap-global-set' was introduced in emacs 29
  (keymap-global-set "C-x P p" #'peek-overlay-dwim)
  (keymap-global-set "C-x P d" #'peek-xref-definition-dwim)
  (keymap-global-set "C-x P m" #'peek-overlay-eldoc-message-toggle-stauts)
  (keymap-global-set "C-c c d" #'eldoc)

  ;; ;; Eldoc display setting
  ;; ;; Besides making `peek-enable-eldoc-display-integration' to t, you may want to remove
  ;; ;;   other eldoc display functions.
  ;; (setq eldoc-display-functions
  ;;   (remove 'eldoc-display-in-buffer 'eldoc-display-functions))
  ;; ;; Or simply set peek-display-eldoc as the only display function of eldoc-display-functions
  (setq eldoc-display-functions '(peek-display-eldoc)))
```

### All Customization Variables

Go to `customize` -> `peek`

### Additional API
These API may be useful for advanced customization:

- `eldoc-message-function` related API: `peek-overlay-eldoc-message-toggle-stauts`, `peek-overlay-eldoc-message-disable`, `peek-overlay-eldoc-message-enable`. Possible customization direction: for model editing mode like `evil`, you can use these function to only enable displaying eldoc message overlay(peek window) when in _insert_ mode.

- `peek-overlay-set-custom-content`, `peek-overlay-toggle`, `peek-overlay-hide`, `peek-overlay-show`


## Future Plan
1. Support `Child frame`. (Currently `Peek` only support `overlay`.)
2. Pseudo `overlay` that behaves like floating on the upper layer of the text (like child frame, so we have better terminal support). Maybe I should take a look at the source code of `corfu` or `company`.

## Related Projects

### Reference

#### Overlay
1. [citre](https://github.com/universal-ctags/citre/blob/master/citre-ui-peek.el)
2. [inline-docs](https://repo.or.cz/inline-docs.git/blob/HEAD:/inline-docs.el)

### Other

#### Overlay
1. [quick-peek](https://github.com/cpitclaudel/quick-peek)
2. [lsp-ui](https://github.com/emacs-lsp/lsp-ui/blob/master/lsp-ui-peek.el)

#### Child Frame
1. http://tuhdo.github.io/emacs-frame-peek.html
2. [vertico-posframe](https://github.com/tumashu/vertico-posframe/blob/main/vertico-posframe.el)
