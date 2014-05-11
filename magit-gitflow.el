;;; magit-gitflow.el --- gitflow extension for magit           -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Jan Tatarik

;; Author: Jan Tatarik <Jan.Tatarik@gmail.com>

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

;; Keywords: vc tools
;; Package: magit-gitflow
;; Package-Requires: ((magit "1.3.0"))


;;; Commentary:

;; Gitflow plugin for Magit.

;; (require 'magit-gitflow)
;; (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
;;
;; C-f in magit status buffer will invoke gitflow action selector.
;;

;;; Code:

(require 'magit)
(require 'cl-macs)

(eval-and-compile
  ;; Added in Emacs 24
  (unless (fboundp 'cl-flet)
    (defalias 'cl-flet 'flet)))

(defvar magit-gitflow-mode-lighter " GitFlow")

(defvar magit-gitflow-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-f") 'magit-key-mode-popup-gitflow)
    map))

(define-minor-mode magit-gitflow-mode
  "GitFlow support for Magit."
  :lighter magit-gitflow-mode-lighter
  :keymap  magit-gitflow-mode-map
  (or (derived-mode-p 'magit-mode)
      (user-error "This mode only makes sense with magit")))

(defun turn-on-magit-gitflow ()
  "Unconditionally turn on `magit-gitflow-mode'."
  (magit-gitflow-mode 1))

(easy-menu-define magit-gitflow-extension-menu nil
  "GitFlow extension menu"
  '("GitFlow" :visible magit-gitflow-mode

    ("Initialization/setup"
     ["Initialize defaults" magit-gitflow-init
      :help "Initialize GitFlow in the current repository"]
     ["Set feature prefix" magit-gitflow-init-feature]
     ["Set release prefix" magit-gitflow-init-release]
     ["Set hotfix prefix" magit-gitflow-init-hotfix]
     ["Set support prefix" magit-gitflow-init-support]
     ["Set versiontag prefix" magit-gitflow-init-versiontag])

    ("Feature"
     ["Start" magit-key-mode-popup-gitflow-feature-start]
     ["Finish" magit-key-mode-popup-gitflow-feature-finish]
     ["Publish" magit-gitflow-feature-publish]
     ["Delete" magit-key-mode-popup-gitflow-feature-delete]
     ["Track" magit-gitflow-feature-track]
     ["Diff" magit-gitflow-feature-diff]
     ["Pull" magit-gitflow-feature-pull]
     ["Rebase" magit-key-mode-popup-gitflow-feature-rebase])

    ("Release"
     ["Start" magit-key-mode-popup-gitflow-release-start]
     ["Finish" magit-key-mode-popup-gitflow-release-finish]
     ["Publish" magit-gitflow-release-publish]
     ["Delete" magit-key-mode-popup-gitflow-release-delete]
     ["Track" magit-gitflow-release-track])

    ("Hotfix"
     ["Start" magit-key-mode-popup-gitflow-hotfix-start]
     ["Finish" magit-key-mode-popup-gitflow-hotfix-finish]
     ["Publish" magit-gitflow-hotfix-publish]
     ["Delete" magit-key-mode-popup-gitflow-hotfix-delete])

    ["Support" magit-key-mode-popup-gitflow-support-start]))

(easy-menu-add-item 'magit-mode-menu '("Extensions")
                    magit-gitflow-extension-menu)


;;;
;;; Utilities
;;;

(defun magit-run-gitflow (&rest args)
  "Execute 'git flow' with given ARGS."
  (apply #'magit-run-git "flow" args))


(defmacro define-magit-gitflow-cmd (cmd)
  "Create defun to execute 'git flow CMD' commands.

The new function will be called magit-run-gitflow-CMD."
  (let ((defun-name (intern (format "magit-run-gitflow-%s" cmd)))
        (version-prompt (format "%s name: " (upcase-initials cmd)))
        (config-key (format "gitflow.prefix.%s" cmd)))

    `(defun ,defun-name (args)
       (let* ((prefix (magit-get ,config-key))
              (current-branch (magit-get-current-branch))
              (current-feature (if (string-prefix-p prefix current-branch)
                                   (substring current-branch (length prefix))
                                 "")))

         (magit-run-gitflow ,cmd args
                            magit-custom-options
                            (read-string ,version-prompt current-feature))))))

(define-magit-gitflow-cmd "feature")
(define-magit-gitflow-cmd "release")
(define-magit-gitflow-cmd "hotfix")


(defmacro define-magit-gitflow-branch-cmd (branch cmd)
  "Create defun to execute 'git flow BRANCH CMD' commands.

The new function will be called magit-gitflow-BRANCH-CMD."

  (let ((branch-execute (intern (format "magit-run-gitflow-%s" branch)))
        (defun-name (intern (format "magit-gitflow-%s-%s" branch cmd))))

    `(defun ,defun-name ()
       (interactive)
       (,branch-execute ,cmd))))


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

(define-magit-gitflow-branch-cmd "feature" "finish")
(define-magit-gitflow-branch-cmd "feature" "publish")
(define-magit-gitflow-branch-cmd "feature" "delete")
(define-magit-gitflow-branch-cmd "feature" "rebase")
(define-magit-gitflow-branch-cmd "feature" "track")

(defun magit-gitflow-feature-diff ()
  (interactive)
  (let* ((prefix (magit-get "gitflow.prefix.feature"))
         (current-branch (magit-get-current-branch))
         (base (magit-get (format "gitflow.branch.%s.base" current-branch))))

    (when (and (string-prefix-p prefix current-branch) base)
      (magit-diff base current-branch))))

(defun magit-gitflow-feature-pull ()
  (interactive)
  (let ((remote (magit-read-remote "Remote" nil t)))
    (magit-run-gitflow "feature" "pull"
                       remote
                       (magit-read-remote-branch "Feature" remote))))




(defun magit-gitflow-release-start (version)
  (interactive "sRelease name: ")
  (magit-run-gitflow "release" "start" magit-custom-options version))

(defun magit-gitflow-release-finish ()
  (interactive)
  (let* ((prefix (magit-get "gitflow.prefix.release"))
         (current-branch (magit-get-current-branch))
         (current-release (if (string-prefix-p prefix current-branch)
                              (substring current-branch (length prefix))
                            ""))
         (args (append '("release" "finish") magit-custom-options (list (read-string "Release name: " current-release)))))
    (magit-commit-internal "flow" args)))

(define-magit-gitflow-branch-cmd "release" "publish")
(define-magit-gitflow-branch-cmd "release" "delete")
(define-magit-gitflow-branch-cmd "release" "track")

(defun magit-gitflow-hotfix-start (version)
  (interactive "sHotfix name: ")
  (magit-run-gitflow "hotfix" "start" magit-custom-options version))

(defun magit-gitflow-hotfix-finish ()
  (interactive)
  (let* ((prefix (magit-get "gitflow.prefix.hotfix"))
         (current-branch (magit-get-current-branch))
         (current-hotfix (if (string-prefix-p prefix current-branch)
                             (substring current-branch (length prefix))
                           ""))
         (args (append '("hotfix" "finish") magit-custom-options (list (read-string "Hotfix name: " current-hotfix)))))
    (magit-commit-internal "flow" args)))

(define-magit-gitflow-branch-cmd "hotfix" "publish")
(define-magit-gitflow-branch-cmd "hotfix" "delete")


(defun magit-gitflow-support-start ()
  (interactive)
  (magit-run-gitflow "support" "start" magit-custom-options
                     (read-string "Support branch name: ")
                     (magit-read-rev "Base")))

(defmacro with-key-mode-group (group &rest body)
  (let ((groupname (make-symbol "groupname")))
    `(let ((,groupname ,group))
       (cl-flet ((insert-action (&rest args) (apply #'magit-key-mode-insert-action ,groupname args))
                 (insert-switch (&rest args) (apply #'magit-key-mode-insert-switch ,groupname args))
                 (insert-argument (&rest args) (apply #'magit-key-mode-insert-argument ,groupname args)))

         (magit-key-mode-add-group ,groupname)
         ,@body
         (magit-key-mode-generate ,groupname)))))


;;;
;;; Commands
;;;

(progn
  ;;
  ;; git flow INIT
  ;;
  (with-key-mode-group 'gitflow-init
   (insert-action "i" "Initialize defaults" 'magit-gitflow-init)
   (insert-action "f" "Feature prefix" 'magit-gitflow-init-feature)
   (insert-action "r" "Release prefix" 'magit-gitflow-init-release)
   (insert-action "h" "Hotfix prefix" 'magit-gitflow-init-hotfix)
   (insert-action "s" "Support prefix" 'magit-gitflow-init-support)
   (insert-action "v" "Version tag prefix" 'magit-gitflow-init-versiontag)
   (insert-switch "-f" "Force reinitialization" "--force"))

  ;;
  ;; git flow FEATURE
  ;;
  (with-key-mode-group 'gitflow-feature-start
    (insert-action "s" "Start" 'magit-gitflow-feature-start)
    (insert-switch "-F" "Fetch" "--fetch"))

  (with-key-mode-group 'gitflow-feature-finish
    (insert-action "f" "Finish" 'magit-gitflow-feature-finish)
    (insert-switch "-F" "Fetch" "--fetch")
    (insert-switch "-r" "Rebase" "--rebase")
    (insert-switch "-p" "Preserve merges" "--preserve-merges")
    (insert-switch "-k" "Keep branch" "--keep")
    (insert-switch "-Kr" "Keep remote branch" "--keepremote")
    (insert-switch "-Kl" "Keep local branch" "--keeplocal")
    (insert-switch "-D" "Force delete branch" "--force_delete")
    (insert-switch "-s" "Squash" "--squash")
    (insert-switch "-n" "No fast-forward" "--no-ff"))

  (with-key-mode-group 'gitflow-feature-delete
    (insert-action "d" "Delete" 'magit-gitflow-feature-delete)
    (insert-switch "-f" "Force" "--force")
    (insert-switch "-r" "Delete remote" "--remote"))

  (with-key-mode-group 'gitflow-feature-rebase
    (insert-action "r" "Rebase" 'magit-gitflow-feature-rebase)
    (insert-switch "-i" "Interactive" "--interactive")
    (insert-switch "-p" "Preserve merges" "--preserve-merges"))

  (with-key-mode-group 'gitflow-feature
    (insert-action "s" "Start" 'magit-key-mode-popup-gitflow-feature-start)
    (insert-action "f" "Finish" 'magit-key-mode-popup-gitflow-feature-finish)
    (insert-action "p" "Publish" 'magit-gitflow-feature-publish)
    (insert-action "d" "Delete" 'magit-key-mode-popup-gitflow-feature-delete)
    (insert-action "t" "Track" 'magit-gitflow-feature-track)
    (insert-action "D" "Diff" 'magit-gitflow-feature-diff)
    (insert-action "P" "Pull" 'magit-gitflow-feature-pull)
    (insert-action "r" "Rebase" 'magit-key-mode-popup-gitflow-feature-rebase))

  ;;
  ;; git flow RELEASE
  ;;
  (with-key-mode-group 'gitflow-release-start
    (insert-action "s" "Start" 'magit-gitflow-release-start)
    (insert-switch "-F" "Fetch" "--fetch"))

  (with-key-mode-group 'gitflow-release-finish
    (insert-action "f" "Finish" 'magit-gitflow-release-finish)
    (insert-switch "-F" "Fetch before finish" "--fetch")
    (insert-switch "-s" "Sign" "--sign")
    (insert-argument "=u" "Signing key" "--signingkey=" 'read-file-name)
    (insert-argument "=m" "Tag message" "--message=" 'read-string)
    (insert-argument "=f" "Tag message file" "--messagefile=" 'read-file-name)
    (insert-switch "-p" "Push after finish" "--push")
    (insert-switch "-k" "Keep branch" "--keep")
    (insert-switch "-Kr" "Keep remote branch" "--keepremote")
    (insert-switch "-Kl" "Keep local branch" "--keeplocal")
    (insert-switch "-D" "Force delete branch" "--force_delete")
    (insert-switch "-n" "Don't tag" "--tag")
    (insert-switch "-b" "Don't back-merge master" "--nobackmerge")
    (insert-switch "-S" "Squash" "--squash"))

  (with-key-mode-group 'gitflow-release-delete
     (insert-action  "d" "Delete" 'magit-gitflow-release-delete)
     (insert-switch  "-f" "Force" "--force")
     (insert-switch  "-r" "Delete remote branch" "--remote"))

  (with-key-mode-group 'gitflow-release
    (insert-action "s" "Start" 'magit-key-mode-popup-gitflow-release-start)
    (insert-action "f" "Finish" 'magit-key-mode-popup-gitflow-release-finish)
    (insert-action "p" "Publish" 'magit-gitflow-release-publish)
    (insert-action "d" "Delete" 'magit-key-mode-popup-gitflow-release-delete)
    (insert-action "t" "Track" 'magit-gitflow-release-track))


  ;;
  ;; git flow HOTFIX
  ;;
  (with-key-mode-group 'gitflow-hotfix-start
                       (insert-action "s" "Start" 'magit-gitflow-hotfix-start)
                       (insert-switch "-F" "Fetch" "--fetch"))

  (with-key-mode-group 'gitflow-hotfix-finish
                       (insert-action "f" "Finish" 'magit-gitflow-hotfix-finish)
                       (insert-switch "-F" "Fetch before finish" "--fetch")
                       (insert-switch "-s" "Sign" "--sign")
                       (insert-argument "=u" "Signing key" "--signingkey=" 'read-file-name)
                       (insert-argument "=m" "Tag message" "--message=" 'read-string)
                       (insert-argument "=f" "Tag message file" "--messagefile=" 'read-file-name)
                       (insert-switch "-p" "Push after finish" "--push")
                       (insert-switch "-k" "Keep branch" "--keep")
                       (insert-switch "-Kr" "Keep remote branch" "--keepremote")
                       (insert-switch "-Kl" "Keep local branch" "--keeplocal")
                       (insert-switch "-D" "Force delete branch" "--force_delete")
                       (insert-switch "-n" "Don't tag" "--tag")
                       (insert-switch "-b" "Don't back-merge master" "--nobackmerge"))

  (with-key-mode-group 'gitflow-hotfix-delete
                       (insert-action  "d" "Delete" 'magit-gitflow-hotfix-delete)
                       (insert-switch  "-f" "Force" "--force")
                       (insert-switch  "-r" "Delete remote branch" "--remote"))

  (with-key-mode-group 'gitflow-hotfix
                       (insert-action "s" "Start" 'magit-key-mode-popup-gitflow-hotfix-start)
                       (insert-action "f" "Finish" 'magit-key-mode-popup-gitflow-hotfix-finish)
                       (insert-action "p" "Publish" 'magit-gitflow-hotfix-publish)
                       (insert-action "d" "Delete" 'magit-key-mode-popup-gitflow-hotfix-delete))


  ;;
  ;; git flow SUPPORT
  ;;
  (with-key-mode-group 'gitflow-support-start
                       (insert-action "s" "Start" 'magit-gitflow-support-start)
                       (insert-switch "-F" "Fetch" "--fetch"))

  ;;
  ;; git flow
  ;;
  (with-key-mode-group 'gitflow
    (insert-action "i" "Init" 'magit-key-mode-popup-gitflow-init)
    (insert-action "f" "Feature" 'magit-key-mode-popup-gitflow-feature)
    (insert-action "r" "Release" 'magit-key-mode-popup-gitflow-release)
    (insert-action "h" "Hotfix" 'magit-key-mode-popup-gitflow-hotfix)
    (insert-action "s" "Support" 'magit-key-mode-popup-gitflow-support-start)))


(provide 'magit-gitflow)
;;; magit-gitflow.el ends here
