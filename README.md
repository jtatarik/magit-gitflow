magit-gitflow
=============

[GitFlow][gitflow] plugin for magit.el

Early stage of development, but should be already usable. It currently
requires a patched version of [magit] to work.


Setup
-----

```lisp
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
```

Gitflow commands are invoked by pressing <kbd>C-f</kbd> in magit status
buffer. Or use the Magit/Extensions/GitFlow menu. 


[gitflow]: https://github.com/petervanderdoes/gitflow
[magit]: https://github.com/jtatarik/magit/tree/fix_add_group_arguments
