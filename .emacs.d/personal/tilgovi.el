;;; tilgovi.el -- Emacs customizations for @tilgovi

;;; Commentary:

;;; Code:
(eval-when-compile (require 'use-package))
(eval-when-compile (setq use-package-expand-minimally byte-compile-current-file))

;; Set macOS modifier key bindings
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Set the emoji font
(when window-system
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji"))
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append))

(defun balance-windows-margins ()
  "This function balances the margins of all windows on the selected
   frame such that the first column and the fill column are the same
   distance from the left and right edge, respectively."
  (interactive "P")
  (walk-windows
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
       (set-window-margins window margin margin)))))


(defun set-window-default-parameters ()
  "This function sets default window parameters."
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

(use-package chruby
  :hook (ruby-mode . chruby-use-corresponding))

(use-package company)

(use-package company-emoji
  :requires company
  :config (add-to-list 'company-backends 'company-emoji))

(use-package company-tabnine
  :disabled
  :requires company
  :config (add-to-list 'company-backends '(company-capf :with company-tabnine :separate)))

(use-package composite
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

(use-package elpy
  :config
  (elpy-enable))

(use-package editorconfig
  :hook (editor-config-custom-hooks . (lambda (props) (whitespace-mode))))

(use-package flycheck
  :config
  (defun flycheck-maybe-select-python-mypy ()
    (when (flycheck-may-enable-checker 'python-mypy)
      (flycheck-select-checker 'python-mypy)))
  (add-hook 'python-mode-hook 'flycheck-maybe-select-python-mypy)
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (flycheck-add-next-checker 'python-mypy 'python-pylint))

(use-package google-c-style
  :hook (c-mode-common . google-set-c-style))

(use-package lsp-mode
  :defines (lsp-eslint-fix-all lsp-eslint-auto-fix-on-save lsp-eslint-server-command)
  :hook ((js-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (typescript-mode . lsp-deferred))
  :preface
  (defun lsp--eslint-before-save (orig-fun)
    "Run lsp-eslint-apply-all-fixes and then run the original lsp--before-save."
    (when lsp-eslint-auto-fix-on-save (lsp-eslint-fix-all))
    (funcall orig-fun))
  :config
  (let* ((path "~/.vscode/extensions/dbaeumer.vscode-eslint-*/server/out/eslintServer.js")
         (expanded-path (expand-file-name (car (last (file-expand-wildcards path)))))) (setq
         lsp-eslint-server-command `("node" ,expanded-path "--stdio")))
  (advice-add 'lsp--before-save :around #'lsp--eslint-before-save))

(use-package lsp-java
  :after lsp
  :hook (java-mode . lsp-deferred))

(use-package lsp-ui
  :config
  (define-key lsp-ui-mode-map [remap js-find-symbol] #'xref-find-definitions))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package pipenv
  :hook (python-mode . pipenv-mode))

(use-package pyvenv
  :preface
  (defun pyvenv-auto ()
    "Automatically activate any virtualenv found in a project root directory."
    (let* ((buffer (current-buffer))
           (directory (buffer-local-value 'default-directory buffer))
           (root (locate-dominating-file directory "venv")))
      (if root (pyvenv-activate (concat root "venv")) (pyvenv-deactivate))))
  :config
  (add-hook 'python-mode-hook 'pyvenv-auto))

(use-package rust-mode
  :init
  (setq-default rust-format-on-save t))

(use-package racer
  :requires rust-mode
  :hook (rust-mode . racer-mode))

(use-package selectrum
  :config (selectrum-mode +1))

(use-package selectrum-prescient
  :requires selectrum
  :config (selectrum-prescient-mode +1))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package theme-changer
  :init
  ;; Day / Night themes
  (setq calendar-location-name "Oakland, CA")
  (setq calendar-latitude 37.80)
  (setq calendar-longitude -122.27)
  :config (change-theme 'base16-atelier-forest-light 'base16-atelier-forest))

(use-package toml-mode)

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode)))

(use-package yasnippet)

(provide 'tilgovi)
;;; tilgovi.el ends here
