(when window-system
  (if (eq system-type 'darwin)
      (toggle-frame-fullscreen)
    (toggle-frame-maximized)))
