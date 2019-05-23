;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
)

(require 'use-package)

;; turn off splash screen
(setq inhibit-splash-screen t)
;; show cursor position
(column-number-mode 1)
;; save/restore opened files and window config on start
(desktop-save-mode 1) ;; 0 to turn off
;; set tab to always use spaces
;; set tab width to four spaces
(progn
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
)

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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b47946a3a69d091a75b89819983835ac6f7fb94c9b491da38a4907b1185bebe0" default)))
 '(package-selected-packages
   (quote
    (company-c-headers company-shell company-tern company use-package yasnippet auto-complete web-mode js2-mode bliss-theme))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; set theme
(load-theme 'bliss)
;; (enable-theme 'bliss)



;; enable autocomplete
(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))



;; map just-one-space
(global-set-key (kbd "C-c x") 'just-one-space);




(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
;; Better imenu

(use-package js2-imenu-extras-mode
  :hook js2-mode)
  
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package emmet-mode
  :hook (sgml-mode css-mode web-mode)
  :custom (emmet-move-cursor-between-quotes t))

(use-package org
  :bind (("C-c l" . 'org-store-link)
         ("C-c a" . 'org-agenda))
  :custom (org-log-done t)
  )
