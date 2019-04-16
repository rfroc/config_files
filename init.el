;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/") t)
   ;; (package-refresh-contents)
)

(package-initialize)

;; turn off splash screen
(setq inhibit-splash-screen t)

;; set initial emacs position and size on screen
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 166) ; chars
              (height . 56) ; lines
              ;; (background-color . "thistle")
              (left . 200)
              (top . 50)))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 166)
              (height . 56)
              ;; (background-color . "thistle")
              (left . 200)
              (top . 50)
             )
      )
   )
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))
  )
)

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("b47946a3a69d091a75b89819983835ac6f7fb94c9b491da38a4907b1185bebe0" default)))
 )


(custom-set-faces
 )

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; save/restore opened files and window config on start
(desktop-save-mode 1) ;; 0 to turn off

;; set theme
(load-theme 'bliss)

;; set tab to always use spaces
;; set tab width to four spaces
(progn
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
)

;; show cursor position
(column-number-mode 1)

