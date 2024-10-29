;;; brew-summary.el --- Open a buffer with a summary of a Homebrew formula  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Delon Newman

;; Author: Delon Newman <contact@delonnewman.name>
;; Keywords: unix, tools
;;

(require 'brew)


(defun brew--formula-file-listing (formula-or-cask)
  "Return a list of files that belong to a FORMULA-OR-CASK."
  (process-lines brew-command-path "list" formula-or-cask))


(defun brew-summary-mode (formula-or-cask)
  "Open a summary of a FORMULA-OR-CASK."
  (interactive "sEnter formula or cask name: ")
  )


(provide 'brew-summary)
;;; brew.el ends here
