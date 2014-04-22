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
(require 'cl-macs)

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

(defun magit-gitflow-feature-rebase ()
  (interactive)
  (magit-gitflow-feature "rebase"))

(defun magit-gitflow-feature-track ()
  (interactive)
  (magit-gitflow-feature "track"))

;; FIXME: display the resulting diff
(defun magit-gitflow-feature-diff ()
  (interactive)
  (magit-gitflow-feature "diff"))

;; TODO: offer list of current features to choose from
(defun magit-gitflow-feature-checkout ()
  (interactive)
  (magit-gitflow-feature "checkout"))

(defun magit-gitflow-feature-pull ()
  (interactive)
  (let ((remote (magit-read-remote "Remote" nil t)))
    (magit-run-gitflow "feature" "pull"
                       remote
                       (magit-read-remote-branch "Feature" remote))))


(defun magit-gitflow-release-start (version)
  (interactive "sVersion: ")
  (magit-run-gitflow "release" "start" magit-custom-options version))

(defun magit-gitflow-release (cmd)
  (let* ((prefix (magit-get "gitflow.prefix.release"))
         (current-branch (magit-get-current-branch))
         (current-release (if (string-prefix-p prefix current-branch)
                              (substring current-branch (length prefix))
                            "")))

    (magit-run-gitflow "release" cmd
                       magit-custom-options
                       (read-string "Version: " current-release))))

(defun magit-gitflow-release-finish ()
  (interactive)
  (let* ((prefix (magit-get "gitflow.prefix.release"))
        (current-branch (magit-get-current-branch))
        (current-release (if (string-prefix-p prefix current-branch)
                             (substring current-branch (length prefix))
                           ""))
        (args (append '("release" "finish") magit-custom-options (list (read-string "Version: " current-release)))))
    (magit-commit-internal "flow" args)))

(defun magit-gitflow-release-publish ()
  (interactive)
  (magit-gitflow-release "publish"))

(defun magit-gitflow-release-delete ()
  (interactive)
  (magit-gitflow-release "delete"))

(defun magit-gitflow-release-track ()
  (interactive)
  (magit-gitflow-release "track"))


(defmacro with-key-mode-group (group &rest body)
  `(cl-flet ((insert-action (&rest args) (apply #'magit-key-mode-insert-action ,group args))
             (insert-switch (&rest args) (apply #'magit-key-mode-insert-switch ,group args))
             (insert-argument (&rest args) (apply #'magit-key-mode-insert-argument ,group args)))

     (magit-key-mode-add-group ,group)
     ,@body
     (magit-key-mode-generate ,group)))

(progn
  (with-key-mode-group 'gitflow-init
   (insert-action "i" "Initialize defaults" 'magit-gitflow-init)
   (insert-action "f" "Feature prefix" 'magit-gitflow-init-feature)
   (insert-action "r" "Release prefix" 'magit-gitflow-init-release)
   (insert-action "h" "Hotfix prefix" 'magit-gitflow-init-hotfix)
   (insert-action "s" "Support prefix" 'magit-gitflow-init-support)
   (insert-action "v" "Version tag prefix" 'magit-gitflow-init-versiontag)
   (insert-switch "-f" "Force reinitialization" "--force"))

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
    (insert-action "c" "Checkout" 'magit-gitflow-feature-checkout)
    (insert-action "P" "Pull" 'magit-gitflow-feature-pull)
    (insert-action "r" "Rebase" 'magit-key-mode-popup-gitflow-feature-rebase))

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

  (with-key-mode-group 'gitflow
    (insert-action "i" "Init" 'magit-key-mode-popup-gitflow-init)
    (insert-action "f" "Feature" 'magit-key-mode-popup-gitflow-feature)
    (insert-action "r" "Release" 'magit-key-mode-popup-gitflow-release)))

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
     ["Start" magit-key-mode-popup-gitflow-feature-start]
     ["Finish" magit-key-mode-popup-gitflow-feature-finish]
     ["Publish" magit-gitflow-feature-publish]
     ["Delete" magit-key-mode-popup-gitflow-feature-delete]
     ["Track" magit-gitflow-feature-track]
     ["Diff" magit-gitflow-feature-diff]
     ["Checkout" magit-gitflow-feature-checkout]
     ["Pull" magit-gitflow-feature-pull]
     ["Rebase" magit-key-mode-popup-gitflow-feature-rebase])

    ("Release"
     ["Start" magit-key-mode-popup-gitflow-release-start]
     ["Finish" magit-key-mode-popup-gitflow-release-finish]
     ["Publish" magit-gitflow-release-publish]
     ["Delete" magit-key-mode-popup-gitflow-release-delete]
     ["Track" magit-gitflow-release-track])))

(easy-menu-add-item 'magit-mode-menu '("Extensions")
                    magit-gitflow-extension-menu)

(define-key magit-mode-map (kbd "C-f") 'magit-key-mode-popup-gitflow)

(provide 'magit-gitflow)
;;; magit-gitflow.el ends here
