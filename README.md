magit-gitflow
=============

[GitFlow][gitflow] plugin for magit.el

Requires [patched] version of [magit] to work.


Setup
-----

```lisp
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
```

Gitflow commands are invoked by pressing <kbd>C-f</kbd> in magit status
buffer. Or use the Magit/Extensions/GitFlow menu. 


[gitflow]: https://github.com/petervanderdoes/gitflow
[patched]: https://github.com/jtatarik/magit/tree/fix_add_group_arguments
[magit]: https://github.com/magit/magit/tree/master
