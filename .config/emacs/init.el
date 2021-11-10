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

;; load customizations
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load (expand-file-name "custom.el" user-emacs-directory))

;; install use-package
(when (functionp 'straight-use-package) (straight-use-package 'use-package))
(eval-when-compile
  (add-to-list 'load-path (expand-file-name "straight/repos/use-package" user-emacs-directory))
  (require 'use-package))

;; Set the emoji font
(when window-system
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji"))
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append))

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
       (set-window-margins window margin margin)))))

(defun set-window-default-parameters ()
  "Set default window parameters."
  (interactive)
  (walk-windows
   (lambda (window)
     (when (not (window-parameter window 'min-margins))
           (set-window-parameter window 'min-margins '(0 . 0))))))

(add-hook 'window-configuration-change-hook
          (lambda ()
            (set-window-default-parameters)
            (balance-windows)
            (balance-windows-margins)))

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

(use-package company
  :defines company-backends)

(use-package company-emoji
  :requires company
  :config (add-to-list 'company-backends 'company-emoji))

(use-package company-tabnine
  :disabled
  :requires company
  :config (add-to-list 'company-backends '(company-capf :with company-tabnine :separate)))

(use-package composite
  :straight nil
  :init
  (defvar composition-ligature-table (make-char-table nil))
  :hook
  (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
    . (lambda () (setq-local composition-function-table composition-ligature-table))))
  :config
  (let ((ligatures `((?!  ,(regexp-opt '("!!" "!=" "!==")))
                     (?#  ,(regexp-opt '("#!" "##" "###" "####" "#(" "#:" "#=" "#?" "#[" "#_" "#_(" "#{")))
                     (?$  ,(regexp-opt '("$>")))
                     (?&  ,(regexp-opt '("&&" "&&&" "&=")))
                     (?*  ,(regexp-opt '("***" "*/" "*>")))
                     (?+  ,(regexp-opt '("++" "+++" "+>")))
                     (?-  ,(regexp-opt '("--" "---" "-->" "-<" "-<<" "->" "->>" "-|" "-~")))
                     (?.  ,(regexp-opt '(".-" ".." "..." "..<" ".=" ".?")))
                     (?/  ,(regexp-opt '("/*" "//" "///" "//=" "/=" "/==" "/>")))
                     (?:  ,(regexp-opt '("::" ":::" "::=" ":<" ":=" ":>" ":?" ":?>")))
                     (?\; ,(regexp-opt '(";;")))
                     (?<  ,(regexp-opt '("<!--" "<#--"
                                         "<$" "<$>" "<*" "<*>" "<+" "<+>"
                                         "<-" "<-<" "<->" "<-|"
                                         "</" "</>"
                                         "<:" "<<" "<<-" "<<<" "<<=" "<=" "<=<" "<==" "<==>" "<=>" "<=|"
                                         "<>" "<|" "<|>" "<||" "<|||" "<~" "<~>" "<~~")))
                     (?=  ,(regexp-opt '("=!=" "=/=" "=:=" "=<<" "==" "===" "==>" "=>" "=>>")))
                     (?>  ,(regexp-opt '(">-" ">--" ">->" ">:" ">=" ">=>" ">>" ">>-" ">>=" ">>>" ">]")))
                     (?? ,(regexp-opt '("?." "?:" "?=" "??")))
                     (?@  ,(regexp-opt '("@_")))
                     (?\[ ,(regexp-opt '("[|" "[<" "[||]")))
                     (?\] ,(regexp-opt '("]#")))
                     (?^  ,(regexp-opt '("^=")))
                     (?_  ,(regexp-opt '("__" "_|_")))
                     (?{ ,(regexp-opt '("{|")))
                     (?|  ,(regexp-opt '("|-" "|->" "|=" "|=>" "|>" "|]" "||" "||-" "||=" "||>" "|||>" "|}")))
                     (?~  ,(regexp-opt '("~-" "~>" "~@" "~~" "~~>"))))))
    (dolist (char-regexp ligatures)
      (apply (lambda (char regexp) (set-char-table-range
                                    composition-ligature-table
                                    char `([,regexp 0 compose-gstring-for-graphic])))
             char-regexp))))

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

(use-package elpy
  :functions elpy-enable
  :config
  (elpy-enable))

(use-package editorconfig
  :hook (editor-config-custom-hooks . (lambda (props) (whitespace-mode))))

(use-package exec-path-from-shell
  :functions exec-path-from-shell-initialize
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
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

(use-package jsonnet-mode)

(use-package lsp-mode
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

(use-package projectile
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
  :init
  (require 'smartparens-config))

(use-package super-save
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

(use-package undo-tree)

(use-package uniquify
  :straight nil)

(use-package volatile-highlights)

(use-package yaml-mode)

(use-package yasnippet)

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package which-key)

(use-package whitespace-cleanup-mode)

;;; init.el ends here
