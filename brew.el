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


(transient-define-suffix brew-list ()
  :transient t
  :key "l"
  :description "List all installed formulae and casks"
  (interactive)
  (async-shell-command
   (format "%s list -1" brew-command-path)
   (brew--get-buffer)))


(transient-define-suffix brew-config ()
  :transient t
  :key "c"
  :description "Show Homebrew and system configuration info"
  (interactive)
  (async-shell-command
   (format "%s config" brew-command-path)
   (brew--get-buffer)))


(transient-define-suffix brew-list-files (formula-or-cask)
  :transient t
  :key "L"
  :description "List files associated with the formula or cask"
  (interactive "sEnter formula or cask name: ")
  (async-shell-command
   (format "%s list %s" brew-command-path formula-or-cask)
   (brew--get-buffer)))


(transient-define-suffix brew-info (formula-or-cask)
  :transient t
  :key "i"
  :description "Show summary of information about a formula or cask"
  (interactive "sEnter formula or cask name: ")
  (async-shell-command
   (format "%s info %s" brew-command-path formula-or-cask)
   (brew--get-buffer)))


(transient-define-suffix brew-upgrade-one (formula-or-cask)
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
   ("u" "Formula or Cask" brew-upgrade-one)
   ("a" "All" brew-upgrade-all)])


;; TODO: add suffix for setting flags
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
   ("l" "List all" brew-list)
   ("L" "List files" brew-list-files)
   ("i" "Formula info" brew-info)
   ("c" "Configuration" brew-config)]
  ["Actions"
   ("I" "Install" brew-install)
   ("r" "Uninstall" brew-uninstall)
   ("u" "Update" brew-update)
   ("U" "Upgrade" brew-upgrade)
   ]
  [(brew-describe-command)])

(provide 'brew)
;;; brew.el ends here
