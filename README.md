# gpt.el
A Plugin that brings the power of ChatGPT to Emacs

## Table of contents
* [Installation](#installation)
* [Commands](#commands)
* [On a side note](#on-a-side-note)
* [TODO](#todo)

## Installation
**Using Doom**
In your `packages.el`
```lisp
(package! gpt
  :recipe (:host github :repo "NewDawn0/gpt.el"))
```
In your `config.el`
```lisp
(use-package! gpt
  :defer t
  :bind
  ("C-c h" . gpt-history)
  ("C-c q" . gpt-query)
  ("C-c e" . gpt-explain-region)
  ("C-c r" . gpt-refactor-region)
  ("C-c R" . gpt-rewrite-region)
  ("C-c c" . gpt-correct-region)
  ("C-c Q" . gpt-query-region)
  ("C-c f" . gpt-find-bugs-in-region))
```

**Using Straight (Untested)**
```lisp
(use-package gpt
  :straight (:host github :repo "NewDawn0/gpt.el")
  :bind
  ("C-c h" . gpt-history)
  ("C-c q" . gpt-query)
  ("C-c e" . gpt-explain-region)
  ("C-c r" . gpt-refactor-region)
  ("C-c R" . gpt-rewrite-region)
  ("C-c c" . gpt-correct-region)
  ("C-c Q" . gpt-query-region)
  ("C-c f" . gpt-find-bugs-in-region))
```

**Using Quelpa (Untested)**
```lisp
(require 'quelpa-use-package)
(use-package gpt
  :quelpa ((gpt :fetcher git :url "https://github.com/NewDawn0/gpt.el.git") :upgrade t)
  :bind
  ("C-c h" . gpt-history)
  ("C-c q" . gpt-query)
  ("C-c e" . gpt-explain-region)
  ("C-c r" . gpt-refactor-region)
  ("C-c R" . gpt-rewrite-region)
  ("C-c c" . gpt-correct-region)
  ("C-c Q" . gpt-query-region)
  ("C-c f" . gpt-find-bugs-in-region))
```

## Commands
**Minibuf commands**
+ `gpt-query` `C-c q` Queries ChatGPT from a query using the minibuf
+ `gpt-query-region` `C-c Q` Queries ChatGPT with the selected region as context

**Region commands**
+ `gpt-explain-region` `C-c e` Explains the selected region
+ `gpt-refactor-region` `C-c r` Refactors a selected region
+ `gpt-rewrite-region` `C-c R` Rewrites a selected region
+ `gpt-correct-region` `C-c c` Corrects a selected region
+ `gpt-find-bugs-in-region` `C-c f` Tries to find possible bugs and errors in a selected region

**History commands**
+ `gpt-history` `C-c h` Shows the prompt and response history
+ `gpt-clear-history` Clears the prompt and response history

## On a side note
- This package should be crossplatform but has only been tested on macOs and Arch Linux
- If anyone has tested the untested, please let me know
- Found a bug or request a feature, use the GitHub issue tracker

## TODO
- [x] Add Installation guide
- [ ] Decode the cp1252 encoded respose to Utf-8
- [ ] Add syntax highlighting to code snippets (if possible)
