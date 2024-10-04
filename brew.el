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
;; Homepage: https://github.com/delonnewman/brew
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar brew-command-path "/opt/homebrew/bin/brew")

(let ((buffer nil))
  (defun brew--get-buffer ()
    (if buffer
        buffer
      (setq buffer (generate-new-buffer "*Homebrew*")))))

(defun brew-list ()
  (interactive)
  (async-shell-command (format "%s list -1" brew-command-path) (brew--get-buffer)))

(defun brew-info (formula)
  (interactive "sEnter formula or cask name: ")
  (async-shell-command
   (format "%s info %s" brew-command-path formula)
   (brew--get-buffer)))

(defun brew-describe-command (command)
  (interactive "sEnter command name: ")
  (async-shell-command
   (format "%s help %s" brew-command-path command)
   (brew--get-buffer)))

(provide 'brew)
;;; brew.el ends here
