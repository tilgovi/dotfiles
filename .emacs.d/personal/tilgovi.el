;;; tilgovi.el -- Emacs customizations for @tilgovi

;;; Commentary:

;;; Code:
(eval-when-compile (require 'use-package))
(eval-when-compile (setq use-package-expand-minimally byte-compile-current-file))

(when (eq system-type 'darwin)
  (prelude-swap-meta-and-super))

(when window-system
  (toggle-frame-fullscreen))

;; This definition and the hook below auto-balance and -center windows.
(defun center-window (window)
  "Tile and center buffers in WINDOW at 80 columns."
  (let* ((margins (window-margins window))
         (edges (window-edges window))
         (left (car edges))
         (right (nth 2 edges))
         (width (- right left))
         (excess (- width 100)))
    (if (> excess 0)
        (progn
          (setq truncate-lines t)
          ;(set-window-fringes window 2 nil)
          (set-window-margins window (floor (/ (float excess) 2.0))))
      (set-window-margins window 0))
    (unless (equal margins (window-margins window))
      (balance-windows (window-frame window)))))

(add-hook
 'window-configuration-change-hook
 (lambda ()
   (walk-windows 'center-window 'f)))


(use-package add-node-modules-path
  :hook ((flow-mode . add-node-modules-path) (js2-mode . add-node-modules-path)))

(use-package chruby
  :hook (ruby-mode . chruby-use-corresponding))

(use-package company)

(use-package company-flow
  :after (company)
  :init
  (add-to-list 'company-backends 'company-flow)
  :config
  (add-to-list 'company-flow-modes 'flow-mode))

(use-package company-terraform
  :after (company)
  :init
  (add-to-list 'company-backends 'company-terraform))

(use-package company-tern
  :requires company
  :init
  (add-to-list 'company-backends 'company-tern t))

(use-package elpy
  :config
  (elpy-enable))

(use-package editorconfig
  :hook (editor-config-custom-hooks . (lambda (props) (whitespace-mode))))

(use-package import-js)

(use-package flow-minor-mode
  :hook (flow-mode . flow-minor-enable-automatically))

(use-package flow-mode
  :mode "\\.m?jsx?\\'"
  :interpreter "node")

(use-package flycheck
  :config
  (flycheck-add-mode 'javascript-eslint 'flow-mode)
  (defun flycheck-maybe-select-python-mypy ()
    (when (flycheck-may-enable-checker 'python-mypy)
      (flycheck-select-checker 'python-mypy)))
  (add-hook 'python-mode-hook 'flycheck-maybe-select-python-mypy)
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (flycheck-add-next-checker 'python-mypy 'python-pylint))

(use-package flycheck-flow
  :after (flycheck)
  :config
  (flycheck-add-mode 'javascript-flow 'flow-mode)
  (flycheck-add-mode 'javascript-flow-coverage 'flow-mode))

(use-package google-c-style
  :hook (c-mode-common . google-set-c-style))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package py-yapf
  :config
  (add-hook 'python-mode-hook 'py-yapf-enable-on-save))

(use-package pipenv
  :hook (python-mode . pipenv-mode))

(use-package pretty-fonts
  :config
  (pretty-fonts-set-kwds
   '((pretty-fonts-fira-font prog-mode-hook org-mode-hook))))

(use-package pyvenv
  :config
  (defun pyvenv-auto ()
    "Automatically activate any virtualenv found in a project root directory."
    (let* ((buffer (current-buffer))
           (directory (buffer-local-value 'default-directory buffer))
           (root (locate-dominating-file directory "venv")))
      (if root (pyvenv-activate (concat root "venv")) (pyvenv-deactivate))))
  (add-hook 'python-mode-hook 'pyvenv-auto))

(use-package robe
  :after (chruby company)
  :hook (ruby-mode . robe-mode)
  :config
  (defadvice inf-ruby-console-auto
      (before activate-rvm-for-robe activate) (chruby-use-corresponding))
  (add-to-list 'company-backends 'company-robe t))

(use-package rust-mode
  :init
  (setq-default rust-format-on-save t))

(use-package racer
  :after (rust-mode)
  :hook (rust-mode . racer-mode))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package tern
  :hook ((flow-mode . tern-mode) (js2-mode . tern-mode))
  :init
  (setq tern-command (append tern-command '("--no-port-file"))))

(use-package theme-changer
  :init
  ;; Day / Night themes
  (setq calendar-location-name "Oakland, CA")
  (setq calendar-latitude 37.80)
  (setq calendar-longitude -122.27)
  :config (change-theme 'base16-solarized-light 'base16-solarized-dark))

(use-package toml-mode)

(use-package yasnippet)

(provide 'tilgovi)
;;; tilgovi.el ends here
