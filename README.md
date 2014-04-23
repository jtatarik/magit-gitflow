magit-gitflow
=============

[GitFlow][gitflow] plugin for magit.el

Early stage of development, but should be already usable. It currently
requires a patched version of [magit] to work.


```lisp
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
```


[gitflow]: https://github.com/petervanderdoes/gitflow
[magit]: https://github.com/jtatarik/magit/tree/fix_add_group_arguments
