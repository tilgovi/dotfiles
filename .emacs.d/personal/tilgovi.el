;;; tilgovi.el -- Emacs customizations for @tilgovi

;;; Commentary:

;;; Code:
(eval-when-compile (require 'use-package))

(prelude-fullscreen)
;;(setq frame-title-format (list "%b - " invocation-name "@" system-name))

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

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

(use-package elpy
  :config
  (elpy-enable))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package py-yapf
  :config
  (add-hook 'python-mode-hook 'py-yapf-enable-on-save))

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'(lambda ()
                                (whitespace-mode 0)
                                (setq whitespace-line-column 100)
                                (whitespace-mode 1)))
  (setq-default rust-format-on-save t)
  (use-package racer
    :config
    (add-hook 'rust-mode-hook 'racer-mode)))

(use-package smartparens
  :config
  (smartparens-global-mode))

(use-package theme-changer
  :config
  ;; Day / Night themes
  (setq calendar-location-name "Oakland, CA")
  (setq calendar-latitude 37.80)
  (setq calendar-longitude -122.27)
  (change-theme 'solarized-light 'solarized-dark))

(use-package toml-mode)

(use-package yasnippet)

(use-package virtualenvwrapper)


(provide 'tilgovi)
;;; tilgovi.el ends here
