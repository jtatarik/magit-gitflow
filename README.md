magit-gitflow
=============

[GitFlow][gitflow] plugin for magit.el


Setup
-----

Put the following in your `.emacs`:

```lisp
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
```

Press <kbd>C-f</kbd> in magit status buffer and you will be presented with
gitflow popup menu.

All gitflow commands are also accessible through the Magit/Extensions/GitFlow
pop-down menu.


[gitflow]: https://github.com/petervanderdoes/gitflow
[magit]: https://github.com/magit/magit/tree/master
