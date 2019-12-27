;;; flow-mode.el --- Flow support for Emacs          -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Randall Leeds

;; Author: Randall Leeds <randall@bleeds.info>
;; Keywords: javascript, flow, languages
;; Package-Requires: (typescript-mode)

;;; Commentary:

;; This package derives a new mode from typescript-mode for editing JavaScript
;; with Flow types.

;;; Code:

(require 'typescript-mode)

(define-derived-mode flow-mode typescript-mode "Flow"
  "JavaScript with Flow type checking")

(provide 'flow-mode)
;;; flow-mode.el ends here
