;;; tilgovi.el -- Emacs customizations for @tilgovi

;;; Commentary:

;;; Code:
(eval-when-compile (require 'use-package))

(setq mac-command-modifier 'meta)

(toggle-frame-fullscreen)
;;(setq frame-title-format (list "%b - " invocation-name "@" system-name))

;; Use JSX always for JavaScript files
(add-to-list 'auto-mode-alist '("\\.m?jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

;; https://emacs.stackexchange.com/a/27609
(defun my/use-eslint-from-node-modules ()
  (let ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               (lambda (dir)
                 (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))
                   (and eslint (file-executable-p eslint)))))))
    (when root
      (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
        (setq-local flycheck-javascript-eslint-executable eslint)))))

;; This definition and the hook below auto-balance and -center windows.
(defun center-window (window)
  "Tile and center buffers in WINDOW at 80 columns."
  (let* ((margins (window-margins window))
         (edges (window-edges window))
         (left (car edges))
         (right (nth 2 edges))
         (width (- right left))
         (excess (- width 80)))
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
  :config
  (eval-after-load 'js2-mode
    '(add-hook 'js2-jsx-mode-hook #'add-node-modules-path)))

(use-package auto-virtualenv
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv))

(use-package chruby
  :config
  (add-hook 'ruby-mode-hook 'chruby-use-corresponding))

(use-package company)

(use-package company-flow
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-flow)))

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

(use-package company-terraform
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-terraform)))

(use-package company-tern
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-tern t)))

(use-package elpy
  :config
  (elpy-enable))

(use-package flycheck
  :config
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (global-flycheck-mode))

(use-package editorconfig
  :config
  (add-hook
   'editorconfig-custom-hooks
   (lambda (props) (whitespace-mode))))

(use-package import-js)

(use-package flow-js2-mode)

(use-package flow-minor-mode
  :config
  (add-hook 'js2-jsx-mode-hook 'flow-minor-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package py-yapf
  :config
  (add-hook 'python-mode-hook 'py-yapf-enable-on-save))

(use-package robe
  :config
  (eval-after-load 'chruby
    '(defadvice inf-ruby-console-auto
         (before activate-rvm-for-robe activate)
       (chruby-use-corresponding)))
  (eval-after-load 'company
    '(add-to-list 'company-backends 'robe-mode t))
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package rust-mode
  :config
  (setq-default rust-format-on-save t)
  (use-package racer
    :config
    (add-hook 'rust-mode-hook 'racer-mode)))

(use-package smartparens
  :config
  (smartparens-global-mode))

(use-package tern
  :config
  (setq tern-command (append tern-command '("--no-port-file")))
  (add-hook 'js2-jsx-mode-hook (lambda () (tern-mode t))))

(use-package theme-changer
  :config
  ;; Day / Night themes
  (setq calendar-location-name "Oakland, CA")
  (setq calendar-latitude 37.80)
  (setq calendar-longitude -122.27)
  (change-theme 'solarized-light 'solarized-dark))

(use-package toml-mode)

(use-package yasnippet)

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode))

(provide 'tilgovi)
;;; tilgovi.el ends here
