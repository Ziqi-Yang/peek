# Peek

![Static Badge](https://img.shields.io/badge/made_with-emacs-purple)

[project](https://sr.ht/~meow_king/peek/)/[mailing lists](https://sr.ht/~meow_king/peek/lists)/[tickets](https://sr.ht/~meow_king/peek/trackers)  

## Features
1. Peek window follows your cursor.
2. Different peek window for different buffer and window.
3. Store text of marked region, and then display it on the peek window.
4. Peek the destination of `xref-find-definitions`.
5. `eldoc-message-function` and `eldoc-display-functions` integration.
6. Scroll up or down inside peek window. 

## Demo
All features enabled version:

## Configuration

### Example

``` emacs-lisp

```

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
