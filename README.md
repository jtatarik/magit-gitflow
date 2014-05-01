magit-gitflow
=============

[GitFlow][gitflow] plugin for magit.el

Early stage of development.

This version only works with [magit/next][magit] branch.


Setup
-----

```lisp
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
```

Gitflow commands are invoked by pressing <kbd>C-f</kbd> in magit status
buffer. Or use the Magit/Extensions/GitFlow menu. 


[gitflow]: https://github.com/petervanderdoes/gitflow
[magit]: https://github.com/magit/magit/tree/next
