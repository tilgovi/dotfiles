;;; early-init.el -- Early customizations -*- lexical-binding: t -*-

;;; Commentary:

;;; Inhibit package.el in favor of straight.el.
;;; Perform any other early setup.

;;; Code:

(setq package-enable-at-startup nil)

(when (eq system-type 'darwin)
  (setenv "PATH"
          (concat "/opt/homebrew/bin" path-separator
                  "/opt/homebrew/sbin" path-separator
                  (getenv "PATH")))
  (setq exec-path (split-string (getenv "PATH") path-separator)))

;;; early-init.el ends here
