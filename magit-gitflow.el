;;; magit-gitflow.el --- gitflow plugin for magit           -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Jan Tatarik

;; Author: Jan Tatarik <Jan.Tatarik@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; (require 'magit-gitflow)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;;; Code:

(require 'magit)

(defun magit-run-gitflow (&rest args)
  (apply #'magit-run-git "flow" args))


(define-minor-mode magit-gitflow-mode
  "Magit GitFlow plugin"
  :lighter " Gitflow"
  (or (derived-mode-p 'magit-mode)
      (user-error "This mode only makes sense with magit")))

(defun turn-on-magit-gitflow ()
  "Unconditionally turn on `magit-gitflow-mode'."
  (magit-gitflow-mode 1))


(defun magit-gitflow-init ()
  (interactive)
  (magit-run-gitflow "init" "-d" magit-custom-options))

(defun magit-gitflow-init-prefix (key prompt)
  (let* ((config-key (format "gitflow.prefix.%s" key))
         (default-prefix (or (magit-get config-key) "")))
    (magit-set
     (read-string prompt default-prefix) config-key)))

(defun magit-gitflow-init-feature ()
  (interactive)
  (magit-gitflow-init-prefix "feature" "Feature branch prefix: "))

(defun magit-gitflow-init-release ()
  (interactive)
  (magit-gitflow-init-prefix "release" "Release branch prefix: "))

(defun magit-gitflow-init-hotfix ()
  (interactive)
  (magit-gitflow-init-prefix "hotfix" "Hotfix branch prefix: "))

(defun magit-gitflow-init-support ()
  (interactive)
  (magit-gitflow-init-prefix "support" "Support branch prefix: "))

(defun magit-gitflow-init-versiontag ()
  (interactive)
  (magit-gitflow-init-prefix "versiontag" "Version tag prefix: "))


(defun magit-gitflow-feature-start (name)
  (interactive "sFeature name: ")
  (magit-run-gitflow "feature" "start"  magit-custom-options name))

(defun magit-gitflow-feature (cmd)
  (let* ((prefix (magit-get "gitflow.prefix.feature"))
         (current-branch (magit-get-current-branch))
         (current-feature (if (string-prefix-p prefix current-branch)
                              (substring current-branch (length prefix))
                            "")))

    (magit-run-gitflow "feature" cmd
                       magit-custom-options
                       (read-string "Feature name: " current-feature))))

(defun magit-gitflow-feature-finish ()
  (interactive)
  (magit-gitflow-feature "finish"))

(defun magit-gitflow-feature-publish ()
  (interactive)
  (magit-gitflow-feature "publish"))

(defun magit-gitflow-feature-delete ()
  (interactive)
  (magit-gitflow-feature "delete"))


(defun magit-gitflow-release-start (version)
  (interactive "sVersion: ")
  (magit-run-gitflow "release" "start" version))

(defun magit-gitflow-release (cmd)
  (let* ((prefix (magit-get "gitflow.prefix.release"))
         (current-branch (magit-get-current-branch))
         (current-release (if (string-prefix-p prefix current-branch)
                              (substring current-branch (length prefix))
                            "")))

    (magit-run-gitflow "release" cmd
                       (read-string "Version: " current-release))))

(defun magit-gitflow-release-finish ()
  (interactive)
  (magit-gitflow-release "finish"))

(defun magit-gitflow-release-publish ()
  (interactive)
  (magit-gitflow-release "publish"))

(defun magit-gitflow-release-delete ()
  (interactive)
  (magit-gitflow-release "delete"))

(easy-menu-define magit-gitflow-extension-menu nil
  "Gitflow extension menu"
  '("GitFlow" :visible magit-gitflow-mode

    ("Initialization/setup" :visible magit-gitflow-mode
      ["Initialize defaults" magit-gitflow-init
       :help "Initialize GitFlow in the current repository"]
      ["Set feature prefix" magit-gitflow-init-feature]
      ["Set release prefix" magit-gitflow-init-release]
      ["Set hotfix prefix" magit-gitflow-init-hotfix]
      ["Set support prefix" magit-gitflow-init-support]
      ["Set versiontag prefix" magit-gitflow-init-versiontag])

    ("Feature"
     ["Start" magit-gitflow-feature-start
      :help "Start new feature"]
     ["Finish" magit-gitflow-feature-finish
      :help "Finish new feature"]
     ["Publish" magit-gitflow-feature-publish]
     ["Delete" magit-gitflow-feature-delete])

    ("Release"
     ["Start" magit-gitflow-release-start]
     ["Finish" magit-gitflow-release-finish]
     ["Publish" magit-gitflow-release-publish]
     ["Delete" magit-gitflow-release-delete])))

(easy-menu-add-item 'magit-mode-menu '("Extensions")
                    magit-gitflow-extension-menu)


(progn
  (magit-key-mode-add-group 'gitflow-init)
  (magit-key-mode-insert-action 'gitflow-init "i" "Initialize defaults" 'magit-gitflow-init)
  (magit-key-mode-insert-action 'gitflow-init "f" "Feature prefix" 'magit-gitflow-init-feature)
  (magit-key-mode-insert-action 'gitflow-init "r" "Release prefix" 'magit-gitflow-init-release)
  (magit-key-mode-insert-action 'gitflow-init "h" "Hotfix prefix" 'magit-gitflow-init-hotfix)
  (magit-key-mode-insert-action 'gitflow-init "s" "Support prefix" 'magit-gitflow-init-support)
  (magit-key-mode-insert-action 'gitflow-init "v" "Version tag prefix" 'magit-gitflow-init-versiontag)
  (magit-key-mode-insert-switch 'gitflow-init "-f" "Force reinitialization" "--force")
  (magit-key-mode-generate 'gitflow-init)


  (magit-key-mode-add-group 'gitflow-feature-start)
  (magit-key-mode-insert-action 'gitflow-feature-start "s" "Start" 'magit-gitflow-feature-start)
  (magit-key-mode-insert-switch 'gitflow-feature-start "-F" "Fetch" "--fetch")
  (magit-key-mode-generate 'gitflow-feature-start)


  (magit-key-mode-add-group 'gitflow-feature-finish)
  (magit-key-mode-insert-action 'gitflow-feature-finish "f" "Finish" 'magit-gitflow-feature-finish)
  (magit-key-mode-insert-switch 'gitflow-feature-finish "-F" "Fetch" "--fetch")
  (magit-key-mode-insert-switch 'gitflow-feature-finish "-r" "Rebase" "--rebase")
  (magit-key-mode-insert-switch 'gitflow-feature-finish "-p" "Preserve merges" "--preserve-merges")
  (magit-key-mode-insert-switch 'gitflow-feature-finish "-k" "Keep branch" "--keep")
  (magit-key-mode-insert-switch 'gitflow-feature-finish "--kr" "Keep remote branch" "--keep-remote")
  (magit-key-mode-insert-switch 'gitflow-feature-finish "--kl" "Keep local branch" "--keep-local")
  (magit-key-mode-insert-switch 'gitflow-feature-finish "-D" "Force delete branch" "--force_delete")
  (magit-key-mode-insert-switch 'gitflow-feature-finish "-s" "Squash" "--squash")
  (magit-key-mode-insert-switch 'gitflow-feature-finish "-n" "No fast-forward" "--no-ff")
  (magit-key-mode-generate 'gitflow-feature-finish)


  (magit-key-mode-add-group 'gitflow-feature-publish)
  (magit-key-mode-insert-action 'gitflow-feature-publish "p" "Publish" 'magit-gitflow-feature-publish)
  (magit-key-mode-generate 'gitflow-feature-publish)

  (magit-key-mode-add-group 'gitflow-feature-delete)
  (magit-key-mode-insert-action 'gitflow-feature-delete "d" "Delete" 'magit-gitflow-feature-delete)
  (magit-key-mode-insert-switch 'gitflow-feature-delete "-f" "Force" "--force")
  (magit-key-mode-insert-switch 'gitflow-feature-delete "-r" "Delete remote" "--remote")
  (magit-key-mode-generate 'gitflow-feature-delete)

  
  )



(defvar magit-gitflow-feature-keymap (make-sparse-keymap))
(define-key magit-gitflow-feature-keymap (kbd "s") 'magit-key-mode-popup-gitflow-feature-start)
(define-key magit-gitflow-feature-keymap (kbd "f") 'magit-key-mode-popup-gitflow-feature-finish)
(define-key magit-gitflow-feature-keymap (kbd "p") 'magit-key-mode-popup-gitflow-feature-publish)
(define-key magit-gitflow-feature-keymap (kbd "d") 'magit-key-mode-popup-gitflow-feature-delete)

(defvar magit-gitflow-release-keymap (make-sparse-keymap))
(define-key magit-gitflow-release-keymap (kbd "s") 'magit-gitflow-release-start)
(define-key magit-gitflow-release-keymap (kbd "f") 'magit-gitflow-release-finish)
(define-key magit-gitflow-release-keymap (kbd "p") 'magit-gitflow-release-publish)
(define-key magit-gitflow-release-keymap (kbd "d") 'magit-gitflow-release-delete)

(defvar magit-gitflow-keymap (make-sparse-keymap))
(define-key magit-gitflow-keymap (kbd "i") 'magit-key-mode-popup-gitflow-init)
(define-key magit-gitflow-keymap (kbd "f") magit-gitflow-feature-keymap)
(define-key magit-gitflow-keymap (kbd "r") magit-gitflow-release-keymap)

(define-key magit-mode-map (kbd "C-f") magit-gitflow-keymap)

(provide 'magit-gitflow)
;;; magit-gitflow.el ends here
