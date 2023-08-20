# Peek

![Static Badge](https://img.shields.io/badge/made_with-emacs-purple)

[project](https://sr.ht/~meow_king/peek/)/[mailing lists](https://sr.ht/~meow_king/peek/lists)/[tickets](https://sr.ht/~meow_king/peek/trackers)  

This package allow you to create a peek view below/above cursor point to show things.

Note: this package is still in frequent updating, with function name changing possibly. 

## Features
1. Peek view follows your cursor.
2. Buffer and window local peek views. Also capable for content sharing between different buffer and windows.
3. Store text of marked region, and then display it on the peek view.
4. Peek the destination of `xref-find-definitions`.
5. `eldoc-message-function` and `eldoc-display-functions` integration.
6. Scroll up or down inside peek view. 
7. Live update
8. Image Preview
   - Support
     - image presentation using a string which has no `\n` inside and uses `display` text property
     to show image, like `insert-image` with default `STRING` parameter or `insert-image-file` function.
     - image mode
   - Doesn't Support:
     - image presentation using a overlay which uses `display` text property on its `before-string`
     or `after-string`, like `put-image`


## Demo

[Demo](demo.md)

## Usage

- Store marked region and peek it later:
   1. Mark a region
   2. Use `peek-overlay-dwim` to store the region
   3. Use `peek-overlay-dwim` again to show a peek view of the marked content. You can use this command in other buffer/window to show the marked content. 
   4. Use `peek-overlay-dwim` to hidden the peek view.  
     
   Tip: You can make the peek view of the marked region automatically updated by 
   customizing `peek-live-update` to `t`. Or you want to manually update content, you
   can use `peek-view-refresh` command. It should be noted that live updating/refreshing
   peek view can only be done when the source buffer(owns marked region) is alive.
   
- Find definition of a symbol.
   1. Use `peek-xref-definition` to show the definition at the cursor point in peek view.
   2. Use `peek-overlay-dwim` to hide the peek view.
   
- Display eldoc for the symbol under cursor.  
  note: you need Emacs version >= 28.1  
  1. Customize `peek-enable-eldoc-display-integration' to t.
  2. You may also want to remove other eldoc display functions
  
  ```emacs-lisp
  (remove-hook 'eldoc-display-functions 'eldoc-display-in-buffer)
  ```
  
  3. Use `eldoc` to diplay eldoc for the symbol under cursor.
  4. Use `peek-overlay-dwim` to hide the peek view.
  
- Display eldoc message  
  Customize `peek-enable-eldoc-message-integration` to `t` to enable the eldoc message integration. You may also want to customize `peek-eldoc-message-overlay-position` too.   
  Note: `peek-overlay-eldoc-message-toggle-stauts` function can be used to toggle whether the peek view for eldoc message will be shown.
  
- Scroll up/down in the peek view
  - `M-n`: peek-next-line 
  - `M-p`: peek-prev-line 

## Configuration

### Example

``` emacs-lisp
(use-package peek
  :straight (:type git :host sourcehut :repo "meow_king/peek")

  :custom
  ;; only list some settings that are wanted to be chaned by most people
  (peek-overlay-window-size 11)  ; lines
  ;; you can also set `peek-overlay-border-character' to nil to achieve a similar
  ;; looking as `make-separator-line', which is useful when you find there is a wrong
  ;; number of border characters when using default settings. However, in this case,
  ;; please consider report a bug.
  (peek-overlay-border-character ?\N{BOX DRAWINGS LIGHT HORIZONTAL})
  
  (peek-overlay-position 'above)  ; or below
  (peek-overlay-distance 4)  ; the distance between peek view and the cursor point
  
  ;; one line before the place found by `peek-definition' will also appear
  ;; in peek window. Note `peek-definition' is the underlying function of
  ;; `peek-xref-definition'
  (peek-definition-surrounding-above-lines 1)
  
  (peek-live-update t)  ; live update peek view of a marked region

  (peek-enable-eldoc-message-integration t)  ; enable `eldoc-message-function' integration
  ;; eldoc message overlay at two lines below the point
  ;; It's recommended to set the eldoc message overlay below the point since the pop up of
  ;; the peek overlay may cause visual shaking
  (peek-eldoc-message-overlay-position 2)

  ;; enable `eldoc-display-functons'  integration
  ;; note: you need Emacs version >= 28.1
  (peek-enable-eldoc-display-integration t)

  :config
  (global-peek-mode 1)

  ;; Keybindings 
  ;; default keybindings in peek-mode-keymap
  (define-key peek-mode-keymap (kbd "M-n") 'peek-next-line)
  (define-key peek-mode-keymap (kbd "M-p") 'peek-prev-line)
  
  ;; or you can use `keymap-global-set', which is introduced in emacs 29
  (global-set-key (kbd "C-x P p") #'peek-overlay-dwim)
  (global-set-key (kbd "C-x P d") #'peek-xref-definition)
  (global-set-key (kbd "C-x P m") #'peek-overlay-eldoc-message-toggle-stauts)
  (global-set-key (kbd "C-c c d") #'eldoc)
  
  ;; Eldoc display setting
  ;; Besides making `peek-enable-eldoc-display-integration' to t, you may want to remove
  ;;   other eldoc display functions.
  (remove-hook 'eldoc-display-functions 'eldoc-display-in-buffer)
  
  ;; you may also want to set scroll margin (see its docs)
  (setq-default scroll-margin 5))
```

### All Customization Variables

Go to `customize` -> `peek`

### Additional API

These API may be useful for advanced customization:

- `eldoc-message-function` related API: `peek-overlay-eldoc-message-toggle-stauts`, `peek-overlay-eldoc-message-disable`, `peek-overlay-eldoc-message-enable`. Possible customization direction: for model editing mode like `evil`, you can use these function to only enable displaying eldoc message overlay(peek view) when in _insert_ mode. Personally I use [meow](https://github.com/meow-edit/meow), and this is my settings:

``` emacs-lisp
(add-hook 'meow-insert-enter-hook 'peek-overlay-eldoc-message-enable)
(add-hook 'meow-insert-exit-hook 'peek-overlay-eldoc-message-disable)
```

- `peek-overlay-set-custom-content`, `peek-overlay-toggle`, `peek-overlay-hide`, `peek-overlay-show`

- `peek-definition`. This function can be used to create custom peek definition
command like `peek-xref-definition`.

``` emacs-lisp
;; goto-definition function: any number of parameters, no requirement for returned
;; value. The only requirement is that it should act to go the the point of definition.
(defun peek-goto-xref-defintion-func (identifier)
  "Go to the definition of IDENTIFIER."
  (xref-find-definitions identifier)
  ;; clear xref history
  (pop (car (xref--get-history))))

;; integration
(defun peek-xref-definition ()
  "Peek xref definition."
  (interactive)
  (peek-definition
   'peek-goto-xref-defintion-func
   (list (thing-at-point 'symbol))))
```

## Derived Projects

- [peek-collection](https://git.sr.ht/~meow_king/peek-collection): A collection 
of convenient integration for Emacs package Peek. 


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
