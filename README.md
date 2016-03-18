magit-gitflow
=============

[GitFlow][gitflow] plugin for [magit.el][magit]


Setup
-----

Install [gitflow] and put the following in your `.emacs`:

```lisp
;;; C-f in the magit status buffer invokes the magit-gitflow popup. If you
;;; would like to use a different key, set the magit-gitflow-popup-key variable
;;; before loading magit-gitflow
;; (setq magit-gitflow-popup-key "C-n")

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
```

Press <kbd>C-f</kbd> in magit status buffer and you will be presented with
the gitflow popup menu.

All gitflow commands are also accessible through the Magit/Extensions/GitFlow
pop-down menu.


[gitflow]: https://github.com/petervanderdoes/gitflow
[magit]: https://github.com/magit/magit/tree/master
