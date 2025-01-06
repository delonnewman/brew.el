;;; brew.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Delon Newman
;;
;; Author: Delon Newman <contact@delonnewman.name>
;; Maintainer: Delon Newman <contact@delonnewman.name>
;; Created: October 03, 2024
;; Modified: October 03, 2024
;; Version: 0.0.1
;; Keywords: tools unix
;; Homepage: https://github.com/delonnewman/brew.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'transient)


(defvar brew-command-path "/opt/homebrew/bin/brew")


(let ((buffer nil))
  (defun brew--get-buffer ()
    (if (and buffer (buffer-live-p buffer))
        buffer
      (setq buffer (generate-new-buffer "*homebrew output*")))))


(defun brew--list-all-command ()
  (format "%s list -1" brew-command-path))


(transient-define-suffix brew-list-all ()
  :transient t
  :description "List all installed formulae and casks"
  (interactive)
  (message "%s" (brew--list-all-command))
  (async-shell-command
   (brew--list-all-command)
   (brew--get-buffer)))


(defun brew--list-files-command (formula-or-cask)
  (format "%s list %s" brew-command-path formula-or-cask))


(transient-define-suffix brew-list-files (formula-or-cask)
  :transient t
  :description "List files associated with the formula or cask"
  (interactive "sEnter formula or cask name: ")
  (message "%s" (brew--list-files-command formula-or-cask))
  (async-shell-command
   (brew--list-files-command formula-or-cask)
   (brew--get-buffer)))


(defun brew--list-outdated-command ()
  (format "%s outdated" brew-command-path))


(transient-define-suffix brew-list-outdated ()
  :transient t
  :description "List outdated formulae and casks"
  (interactive)
  (message "%s" (brew--list-outdated-command))
  (async-shell-command
   (brew--list-outdated-command)
   (brew--get-buffer)))


(transient-define-prefix brew-list ()
  "Homebrew list command"
  ["Homebrew List"
   ("f" "Files associated with a formula" brew-list-files)
   ("o" "Outdated" brew-list-outdated)
   ("a" "All" brew-list-all)])


(transient-define-suffix brew-config ()
  :transient t
  :key "c"
  :description "Show Homebrew and system configuration info"
  (interactive)
  (async-shell-command
   (format "%s config" brew-command-path)
   (brew--get-buffer)))


(transient-define-suffix brew-dependencies (formula-or-cask)
  :transient t
  :key "i"
  :description "Show dependencies for formula"
  (interactive "sEnter formula or cask name: ")
  (async-shell-command
   (format "%s deps --tree %s" brew-command-path formula-or-cask)
   (brew--get-buffer)))


(transient-define-suffix brew-info-formula (formula-or-cask)
  :transient t
  :key "i"
  :description "Show summary of information about a formula or cask"
  (interactive "sEnter formula or cask name: ")
  (async-shell-command
   (format "%s info %s" brew-command-path formula-or-cask)
   (brew--get-buffer)))


(transient-define-suffix brew-info-stats ()
  :transient t
  :key "i"
  :description "Show stats for Hombrew installation"
  (interactive)
  (async-shell-command
   (format "%s info" brew-command-path)
   (brew--get-buffer)))


(transient-define-prefix brew-info ()
  "Homebrew info command"
  ["Homebrew Info"
   ("f" "Formula or cask info" brew-info-formula)
   ("s" "System stats" brew-info-stats)])


(transient-define-suffix brew-upgrade-formula (formula-or-cask)
  :transient t
  :description "Upgrade a formula or cask"
  (interactive "sEnter formula or cask name: ")
  (async-shell-command
   (format "%s upgrade %s" brew-command-path formula-or-cask)
   (brew--get-buffer)))


(transient-define-suffix brew-upgrade-all ()
  :transient t
  :description "Upgrade any outdated casks and formulae"
  (interactive)
  (async-shell-command
   (format "%s upgrade" brew-command-path)
   (brew--get-buffer)))


(transient-define-prefix brew-upgrade ()
  "Homebrew upgrade command"
  ["Homebrew Upgrade"
   ("u" "Formula or Cask" brew-upgrade-formula)
   ("a" "All" brew-upgrade-all)])


;; TODO: add prefix for setting flags
(transient-define-suffix brew-install (formula-or-cask)
  :transient t
  :key "I"
  :description "Install a formula or cask"
  (interactive "sEnter formula or cask name: ")
  (async-shell-command
   (format "%s install %s" brew-command-path formula-or-cask)
   (brew--get-buffer)))


(transient-define-suffix brew-uninstall (formula-or-cask)
  :transient t
  :key "r"
  :description "Uninstall a formula or cask"
  (interactive "sEnter formula or cask name: ")
  (async-shell-command
   (format "%s install %s" brew-command-path formula-or-cask)
   (brew--get-buffer)))


(transient-define-suffix brew-reinstall (formula-or-cask)
  :transient t
  :key "R"
  :description "Reinstall a formula or cask"
  (interactive "sEnter formula or cask name: ")
  (async-shell-command
   (format "%s reinstall %s" brew-command-path formula-or-cask)
   (brew--get-buffer)))


(transient-define-suffix brew-update ()
  :transient t
  :key "u"
  :description "Fetch the newest version of Hombrew and all fomulae"
  (interactive)
  (async-shell-command
   (format "%s update" brew-command-path)
   (brew--get-buffer)))


(transient-define-suffix brew-describe-command (command)
  :transient t
  :key "H"
  :description "Display help information for command"
  (interactive "sEnter command name: ")
  (async-shell-command
   (format "%s help %s" brew-command-path command)
   (brew--get-buffer)))


(transient-define-suffix brew-search (text-or-regex)
  :transient t
  :key "s"
  :description "Perform a substring search of cask tokens and formula names"
  (interactive "sEnter search: ")
  (async-shell-command
   (format "%s search %s" brew-command-path text-or-regex)
   (brew--get-buffer)))


(transient-define-prefix brew ()
  "Interact with Homebrew"
  ["Information"
   ("s" "Search" brew-search)
   ("l" "List" brew-list)
   ("i" "Info" brew-info)
   ("d" "Dependencies" brew-dependencies)
   ("c" "Configuration" brew-config)]
  ["Actions"
   ("I" "Install" brew-install)
   ("r" "Uninstall" brew-uninstall)
   ("R" "Reinstall" brew-reinstall)
   ("u" "Update" brew-update)
   ("U" "Upgrade" brew-upgrade)]
  [(brew-describe-command)])


(provide 'brew)
;;; brew.el ends here
