;;; init.el -- Emacs initialization file for @tilgovi

;;; Commentary:

;;; Load packages using straight.el with use-package integration.
;;; Customizations are kept separate (see custom.el).

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(auto-save-visited-mode t)
 '(backup-directory-alist `((".*" \, temporary-file-directory)))
 '(base16-distinct-fringe-background nil)
 '(base16-highlight-mode-line 'contrast)
 '(base16-theme-256-color-source 'colors)
 '(beacon-mode t)
 '(blink-cursor-mode nil)
 '(clang-format-style "Chromium")
 '(column-number-mode t)
 '(company-idle-delay 0)
 '(company-insertion-on-trigger t)
 '(company-insertion-triggers nil)
 '(company-minimum-prefix-length 1)
 '(company-quickhelp-delay 0.2)
 '(company-quickhelp-mode t)
 '(company-show-numbers ''t)
 '(company-show-quick-access ''t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-flip-when-above t)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(cursor-type 'bar)
 '(custom-safe-themes
   '("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))
 '(default-frame-alist
    '((font . "Iosevka Term-13")
      (fullscreen . maximized)
      (line-spacing . 0.3)))
 '(editorconfig-mode t)
 '(eldoc-idle-delay 0)
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults))
 '(elpy-rpc-python-command "python3")
 '(exec-path-from-shell-arguments nil)
 '(fill-column 80)
 '(flycheck-checker-error-threshold 1000)
 '(flycheck-clang-language-standard "c++2a")
 '(flycheck-disabled-checkers '(javascript-jshint))
 '(flycheck-indication-mode nil)
 '(global-auto-revert-mode t)
 '(global-company-mode t)
 '(global-diff-hl-mode t)
 '(global-flycheck-mode t)
 '(global-hl-line-mode nil)
 '(global-prettier-mode t)
 '(global-subword-mode t)
 '(global-whitespace-cleanup-mode t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(kill-whole-line t)
 '(lsp-auto-guess-root t)
 '(lsp-enable-file-watchers nil)
 '(lsp-eslint-auto-fix-on-save t)
 '(lsp-eslint-package-manager "yarn")
 '(lsp-headerline-breadcrumb-enable nil)
 '(lsp-signature-function 'lsp-signature-posframe)
 '(lsp-terraform-server `(,"terraform-ls" "serve"))
 '(lsp-ui-doc-position 'at-point)
 '(menu-bar-mode nil)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ns-right-alternate-modifier 'none)
 '(prescient-persist-mode t)
 '(projectile-completion-system 'default)
 '(projectile-mode t nil (projectile))
 '(projectile-project-search-path '(("~/src" . 2)))
 '(python-indent-def-block-scale 1)
 '(python-shell-extra-pythonpaths '("~/.local/venvs"))
 '(pyvenv-virtualenvwrapper-python "~/.local/share/virtualenvs/virtualenvwrapper/bin/python")
 '(read-process-output-max (* 1024 1024) t)
 '(recentf-max-saved-items 500)
 '(recentf-mode t)
 '(replace-char-fold t)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((eval let
           ((project-directory
             (car
              (dir-locals-find-file default-directory))))
           (setq lsp-clients-typescript-server-args
                 `("--tsserver-path" ,(concat project-directory ".yarn/sdks/typescript/bin/tsserver")
                   "--stdio"))
           (setq lsp-eslint-node-path ".yarn/sdks"))))
 '(save-place-mode t nil (saveplace))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position t)
 '(search-default-mode 'char-fold-to-regexp)
 '(selectrum-mode t)
 '(selectrum-prescient-mode t)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(smartparens-global-mode t)
 '(sp-autoskip-closing-pair 'always)
 '(sp-base-key-bindings 'paredit)
 '(straight-use-package-by-default t)
 '(tab-always-indent 'complete)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(typescript-auto-indent-flag nil)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-ignore-buffers-re "\"^\\\\*\"")
 '(uniquify-separator "\"/\"")
 '(volatile-highlights-mode t)
 '(vterm-kill-buffer-on-exit t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-optional-tags t)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 2)
 '(web-mode-style-padding 2)
 '(which-key-mode t)
 '(winner-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eldoc-box-body ((t (:family "sans-serif"))))
 '(terraform--resource-name-face ((t (:inherit font-lock-variable-name-face)))))

(add-to-list 'auto-mode-alist '("\\.[cm]js\\'" . js-mode))
(setq-default auto-fill-function 'do-auto-fill)

;; install use-package
(when (functionp 'straight-use-package) (straight-use-package 'use-package))
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

;; Set the emoji font
(when window-system
  (set-fontset-font t 'symbol (font-spec :family "Symbols Nerd Font"))
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji"))
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append))

;; Use real fullscreen on macOS
(when (and (eq system-type 'darwin) window-system)
  (add-hook 'window-setup-hook 'toggle-frame-fullscreen))

(defun balance-windows-margins ()
  "Balance the margins of windows on the selected frame.
Balance the margins such that the first column and the fill column are the same
distance from the left and right edge, respectively."
  (interactive)
  (walk-windows
   (lambda (window)
     (let* ((buffer (window-buffer window))
            (fill-column (cond ((window-minibuffer-p window) 120)
                               (t (buffer-local-value 'fill-column buffer))))
            (font-width (window-font-width window))
            (body-width (* (+ fill-column 0) font-width))
            (total-width (window-pixel-width window))
            (fringe-width (apply '+ (seq-take (window-fringes window) 2)))
            (scroll-bar-width (window-scroll-bar-width window))
            (divider-width (window-right-divider-width window))
            (extras-width (+ fringe-width scroll-bar-width divider-width))
            (excess (max (- total-width body-width extras-width) 0))
            (excess-columns (/ excess font-width))
            (margin (floor (/ (float excess-columns) 2))))
       (set-window-margins window margin)))))

(defun set-window-default-parameters ()
  "Set default window parameters."
  (interactive)
  (walk-windows
   (lambda (window)
     (when (not (window-parameter window 'min-margins))
           (set-window-parameter window 'min-margins '(0 . 0))))))

(add-hook 'window-buffer-change-functions
          (lambda (frame)
            (with-selected-frame frame
              (set-window-default-parameters)
              (balance-windows)
              (balance-windows-margins))))

(use-package add-node-modules-path
  :hook ((js-mode . add-node-modules-path)
         (typescript-mode . add-node-modules-path)))

(use-package base16-theme)

(use-package beacon)

(use-package browse-kill-ring
  :functions browse-kill-ring-default-keybindings
  :config
  (browse-kill-ring-default-keybindings))

(use-package chruby
  :hook (ruby-mode . chruby-use-corresponding))

(use-package cider)

(use-package clojure-mode)

(use-package company
  :defines company-backends)

(use-package company-emoji
  :requires company
  :config (add-to-list 'company-backends 'company-emoji))

(use-package company-tabnine
  :disabled
  :requires company
  :config (add-to-list 'company-backends '(company-capf :with company-tabnine :separate)))

(use-package crux
  :config
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key [(shift return)] 'crux-smart-open-line)
  (global-set-key (kbd "M-o") 'crux-smart-open-line)
  (global-set-key [(control shift return)] 'crux-smart-open-line-above)
  (global-set-key [(control shift up)] 'move-text-up)
  (global-set-key [(control shift down)] 'move-text-down)
  (global-set-key [(meta shift up)] 'move-text-up)
  (global-set-key [(meta shift down)] 'move-text-down)
  (global-set-key (kbd "C-c n") 'crux-cleanup-buffer-or-region)
  (global-set-key (kbd "C-c f") 'crux-recentf-find-file)
  (global-set-key (kbd "C-M-z") 'crux-indent-defun)
  (global-set-key (kbd "C-c u") 'crux-view-url)
  (global-set-key (kbd "C-c e") 'crux-eval-and-replace)
  (global-set-key (kbd "C-c s") 'crux-swap-windows)
  (global-set-key (kbd "C-c D") 'crux-delete-file-and-buffer)
  (global-set-key (kbd "C-c d") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
  (global-set-key (kbd "C-c r") 'crux-rename-buffer-and-file)
  (global-set-key (kbd "C-c t") 'crux-visit-term-buffer)
  (global-set-key (kbd "C-c k") 'crux-kill-other-buffers)
  (global-set-key (kbd "C-c I") 'crux-find-user-init-file)
  (global-set-key (kbd "C-c S") 'crux-find-shell-init-file))

(use-package diff-hl)

(use-package diminish
  :diminish (auto-fill-function beacon-mode subword-mode))

(use-package elpy
  :functions elpy-enable
  :config
  (elpy-enable))

(use-package editorconfig
  :diminish
  :hook (editor-config-custom-hooks . (lambda (props) (whitespace-mode))))

(use-package exec-path-from-shell
  :functions exec-path-from-shell-initialize
  :config
  (when window-system
    (exec-path-from-shell-initialize)))

(use-package flycheck
  :diminish
  :functions flycheck-add-next-checker flycheck-may-enable-checker flycheck-select-checker
  :config
  (defun flycheck-maybe-select-python-mypy ()
    (when (flycheck-may-enable-checker 'python-mypy)
      (flycheck-select-checker 'python-mypy)))
  (add-hook 'python-mode-hook 'flycheck-maybe-select-python-mypy)
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (flycheck-add-next-checker 'python-mypy 'python-pylint))

(use-package go-mode)

(use-package google-c-style
  :hook (c-mode-common . google-set-c-style))

(use-package jest)

(use-package json-mode)

(use-package jsonnet-mode)

(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :config
  (ligature-set-ligatures 'prog-mode
                          '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->" "->-" ">-" ">>-"
                            "=<<" "=<" "=<=" "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>" "=>=" ">=" ">>="
                            "<->" "<-->" "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "-------->"
                            "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "/=" "~=" "<>" "===" "!==" "=/=" "=!="
                            ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "<." "<.>" ".>" "+:" "-:" "=:" ":>" "__"
                            "(*" "*)" "[|" "|]" "{|" "|}" "++" "+++" "\\/" "/\\" "|-" "-|" "<!--" "<!---" "<***>"))
  (global-ligature-mode t))

(use-package lsp-mode
  :diminish
  :defines lsp-deferred lsp-eslint-auto-fix-on-save
  :functions lsp-eslint-fix-all
  :hook ((js-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (tuareg-mode . lsp-deferred)
         (typescript-mode . lsp-deferred))
  :preface
  (defun lsp--eslint-before-save (orig-fun)
    "Run lsp-eslint-apply-all-fixes and then run the original lsp--before-save."
    (when lsp-eslint-auto-fix-on-save (lsp-eslint-fix-all))
    (funcall orig-fun))
  :config
  (advice-add 'lsp--before-save :around #'lsp--eslint-before-save))

(use-package lsp-java
  :after lsp
  :hook (java-mode . lsp-deferred))

(use-package lsp-ui
  :defines lsp-ui-mode-map
  :config
  (define-key lsp-ui-mode-map [remap js-find-symbol] #'xref-find-definitions))

(use-package move-text)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package pipenv
  :hook (python-mode . pipenv-mode))

(use-package prettier
  :diminish)

(use-package projectile
  :diminish
  :config
   (global-set-key (kbd "C-c p") 'projectile-command-map))

(use-package pyvenv
  :functions pyvenv-activate pyvenv-deactivate
  :preface
  (defun pyvenv-auto ()
    "Automatically activate any virtualenv found in a project root directory."
    (let* ((buffer (current-buffer))
           (directory (buffer-local-value 'default-directory buffer))
           (root (locate-dominating-file directory "venv")))
      (if root (pyvenv-activate (concat root "venv")) (pyvenv-deactivate))))
  :config
  (add-hook 'python-mode-hook 'pyvenv-auto))

(use-package rainbow-mode
  :diminish
  :hook prog-mode)

(use-package reformatter
  :defines python-format
  :functions reformatter-define
  :hook ((python-mode . python-format-on-save))
  :config
  (reformatter-define python-format
    :program "black"
    :args '("-")
    :lighter " PY"))

(use-package rego-mode)

(use-package ripgrep)

(use-package rust-mode
  :init
  (setq-default rust-format-on-save t))

(use-package racer
  :requires rust-mode
  :hook (rust-mode . racer-mode))

(use-package rect
  :straight nil)

(use-package selectrum)

(use-package selectrum-prescient
  :requires selectrum)

(use-package smartparens
  :diminish
  :init
  (require 'smartparens-config))

(use-package super-save
  :diminish
  :functions super-save-mode
  :config
  (super-save-mode +1))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package theme-changer
  :defines calendar-location-name calendar-latitude calendar-longitude
  :functions change-theme
  :init
  ;; Day / Night themes
  (setq calendar-location-name "Oakland, CA")
  (setq calendar-latitude 37.80)
  (setq calendar-longitude -122.27)
  :config (change-theme 'base16-tomorrow 'base16-tomorrow-night))

(use-package toml-mode)

(use-package tuareg)

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode)))

(use-package uniquify
  :straight nil)

(use-package volatile-highlights
  :diminish)

(use-package yaml-mode)

(use-package yasnippet)

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package which-key
  :diminish)

(use-package whitespace-cleanup-mode
  :diminish)

;;; init.el ends here
