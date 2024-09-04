;;; init.el -- Emacs initialization file for @tilgovi

;;; Commentary:

;;; Load packages using straight.el with use-package integration.

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apheleia-global-mode t)
 '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
 '(auto-save-visited-mode t)
 '(backup-directory-alist `((".*" \, temporary-file-directory)))
 '(base16-theme-256-color-source 'colors)
 '(base16-theme-distinct-fringe-background nil)
 '(blink-cursor-mode nil)
 '(clang-format-style "Chromium")
 '(column-number-mode t)
 '(corfu-auto t)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(cursor-type 'bar)
 '(default-frame-alist
   '((font . "Iosevka Etoile-14") (fullscreen . fullboth) (line-spacing . 0.25)))
 '(editorconfig-mode t)
 '(eglot-events-buffer-size 0)
 '(exec-path-from-shell-arguments nil)
 '(fill-column 80)
 '(fixed-pitch-use-extended-default t)
 '(fixed-pitch-whitelist-hooks '(yaml-ts-mode-hook))
 '(flycheck-checker-error-threshold 1000)
 '(flycheck-clang-language-standard "c++2a")
 '(flycheck-disabled-checkers '(javascript-jshint))
 '(flycheck-indication-mode nil)
 '(gc-cons-threshold 100000000)
 '(global-auto-revert-mode t)
 '(global-corfu-mode t)
 '(global-diff-hl-mode t)
 '(global-flycheck-mode t)
 '(global-hl-line-mode nil)
 '(global-mise-mode t)
 '(global-prettier-mode t)
 '(global-subword-mode t)
 '(global-whitespace-cleanup-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(kill-whole-line t)
 '(menu-bar-mode nil)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(ns-right-alternate-modifier 'none)
 '(pixel-scroll-precision-mode t)
 '(prescient-persist-mode t)
 '(projectile-completion-system 'default)
 '(projectile-mode t nil (projectile))
 '(projectile-project-search-path '(("~/src" . 2)))
 '(python-indent-def-block-scale 1)
 '(read-process-output-max (* 1024 1024) t)
 '(recentf-max-saved-items 500)
 '(recentf-mode t)
 '(replace-char-fold t)
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
 '(rust-format-on-save t)
 '(save-place-mode t nil (saveplace))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position t)
 '(search-default-mode 'char-fold-to-regexp)
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(smartparens-global-mode t)
 '(sp-autoskip-closing-pair 'always)
 '(sp-base-key-bindings 'paredit)
 '(straight-use-package-by-default t)
 '(tab-always-indent 'complete)
 '(tool-bar-mode nil)
 '(treesit-auto-install 'prompt)
 '(truncate-lines t)
 '(typescript-auto-indent-flag nil)
 '(typescript-indent-level 2)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-ignore-buffers-re "\"^\\\\*\"")
 '(uniquify-separator "\"/\"")
 '(vterm-kill-buffer-on-exit t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-enable-optional-tags t)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 2)
 '(web-mode-style-padding 2)
 '(winner-mode t)
 '(yas-global-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:width expanded :family "Iosevka Term"))))
 '(terraform--resource-name-face ((t (:inherit font-lock-variable-name-face))))
 '(variable-pitch ((t (:inherit default)))))

(setq-default auto-fill-function 'do-auto-fill)

;; Install use-package
(when (functionp 'straight-use-package) (straight-use-package 'use-package))
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

;; Set the fonts to use for symbols
(set-fontset-font t 'unicode "Symbols Nerd Font Mono" nil 'prepend)
(set-fontset-font t 'symbol "Apple Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

(defun balance-margins (&optional window-or-frame)
  "Balances the margins of WINDOW-OR-FRAME.
WINDOW-OR-FRAME is optional and defaults to the selected frame.
If WINDOW-OR-FRAME denotes a frame, balance the margins of all
windows of that frame. If WINDOW-OR-FRAME denotes a window,
recursively balance the sizes of all child windows of that window."
  (interactive)
  (let* ((window
	  (cond
	   ((or (not window-or-frame)
		(frame-live-p window-or-frame))
	    (frame-root-window window-or-frame))
	   ((or (window-live-p window-or-frame)
		(window-child window-or-frame))
	    window-or-frame)
	   (t
	    (error "Not a window or frame %s" window-or-frame)))))
    (walk-window-subtree
     (lambda (window)
       (let* ((buffer (window-buffer window))
              (fill-column (buffer-local-value 'fill-column buffer))
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
         (set-window-margins window margin)))
     window)))

(add-hook 'window-size-change-functions 'balance-margins)
(add-hook 'window-size-change-functions 'balance-windows)

(defvar my/light-theme 'tango)
(defvar my/dark-theme 'tango-dark)

(defun my/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme my/light-theme t))
    ('dark (load-theme my/dark-theme t))))

(when (boundp 'ns-system-appearance-change-functions)
  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))

(use-package add-node-modules-path
  :hook ((js-ts-mode . add-node-modules-path)
         (typescript-ts-mode . add-node-modules-path)))

(use-package apheleia
  :defines apheleia-formatters
  :config
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("google-java-format" "--skip-sorting-imports" "-")))

(use-package base16-theme
  :defer t
  :init
  (setq my/light-theme 'base16-selenized-white)
  (setq my/dark-theme 'base16-selenized-black))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :hook ((corfu-mode . corfu-popupinfo-mode)))

(use-package corfu-prescient
  :functions corfu-prescient-mode
  :config
  (corfu-prescient-mode 1))

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

(use-package editorconfig
  :hook (editor-config-custom-hooks . (lambda (props) (whitespace-mode))))

(use-package eglot
  :straight nil)

(use-package exec-path-from-shell
  :functions exec-path-from-shell-initialize
  :if window-system
  :config
  (exec-path-from-shell-initialize))

(use-package fixed-pitch
  :straight (:type git :host github :repo "cstby/fixed-pitch-mode"))

(use-package flycheck
  :functions flycheck-add-next-checker flycheck-may-enable-checker flycheck-select-checker
  :config
  (defun flycheck-maybe-select-python-mypy ()
    (when (flycheck-may-enable-checker 'python-mypy)
      (flycheck-select-checker 'python-mypy)))
  (add-hook 'python-ts-mode-hook 'flycheck-maybe-select-python-mypy)
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (flycheck-add-next-checker 'python-mypy 'python-pylint))

(use-package go-mode
  :mode (("\\.go\\'" . go-mode)
         ("go\\.work\\'" . go-dot-work-mode)
         ("go\\.mod\\'" . go-dot-mod-mode)))

(use-package google-c-style
  :hook (c-mode-common . google-set-c-style))

(use-package graphql-ts-mode
  :mode "\\.graphql\\'")

(use-package groovy-mode
  :mode ("/Jenkinsfile\\'" "\\.g\\(?:ant\\|roovy\\|radle\\)\\'"))

(use-package htmlize)

(use-package jest)

(use-package jsonnet-mode
  :mode ("\\.libsonnet\\'" "\\.jsonnet\\'"))

(use-package js-ts-mode
  :straight nil
  :mode "\\.[cm]js\\'")

(use-package marginalia
  :functions marginalia-mode
  :init
  (marginalia-mode))

(use-package markdown-mode
  :mode "\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'")

(use-package mise)

(use-package mood-line
  :functions mood-line-mode
  :config
  (mood-line-mode))

(use-package move-text)

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package ox-clip)

(use-package projectile
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map))

(use-package rainbow-mode
  :hook prog-mode)

(use-package rego-mode
  :mode "\\.rego\\'")

(use-package ripgrep)

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package racer
  :requires rust-mode
  :hook (rust-mode . racer-mode))

(use-package rect
  :straight nil)

(use-package super-save
  :functions super-save-mode
  :config
  (super-save-mode +1))

(use-package terraform-mode
  :mode ("\\.tf\\(vars\\)?\\'"))

(use-package theme-changer
  :unless (boundp 'ns-system-appearance-change-functions)
  :defines calendar-location-name calendar-latitude calendar-longitude
  :functions change-theme
  :init
  ;; Day / Night themes
  (setq calendar-location-name "Oakland")
  (setq calendar-latitude 37.80)
  (setq calendar-longitude -122.27)
  :config
  (change-theme my/light-theme my/dark-theme))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package treesit-auto
  :functions global-treesit-auto-mode
  :config
  (global-treesit-auto-mode))

(use-package typescript-ts-mode
  :straight nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package uniquify
  :straight nil)

(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :config
  (vertico-mode))

(use-package vertico-prescient
  :straight (:files (:defaults "extensions/*"))
  :config
  (vertico-prescient-mode 1))

(use-package yaml-mode
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'")

(use-package yasnippet)

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package whitespace-cleanup-mode)

;;; init.el ends here
