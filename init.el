(package-initialize)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))

(require 'use-package)

;; turn off splash screen
(setq inhibit-splash-screen t)
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
              (width . 256) ; chars
              (height . 64) ; lines
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

;; show cursor position
(column-number-mode 1)
;; show clock
(display-time-mode 1)

(set-language-environment "UTF-8")
;; (set-default-coding-systems 'utf-8-unix)

;; font is Hack
(add-to-list 'default-frame-alist '(font . "Hack Nerd Font Mono 10"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("04589c18c2087cd6f12c01807eed0bdaa63983787025c209b89c779c61c3a4c4" default)))
 '(package-selected-packages
   (quote
    (js-react-redux-yasnippets exec-path-from-shell json-mode expand-region crux xref-js2 js2-refactor magithub magit prettier-js indium htmlize lorem-ipsum yasnippet-snippets yasnippet yasnippet-classic-snippets tide flycheck company helm-descbinds helm-projectile helm emmet-mode web-mode js2-mode cherry-blossom-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; map global keyboard shortcuts
(global-set-key (kbd "C-c x") 'just-one-space);
(global-set-key [f2] 'eshell);
(global-set-key [f12] 'browse-url-of-file);
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; lorem-ipsum
(global-set-key (kbd "C-c C-k s") 'lorem-ipsum-insert-sentences)
(global-set-key (kbd "C-c C-k p") 'lorem-ipsum-insert-paragraphs)
(global-set-key (kbd "C-c C-k l") 'lorem-ipsum-insert-list)


(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))
(global-set-key [f3] 'window-split-toggle);

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))
(global-set-key [f4] 'transpose-windows);

(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
;;  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode)))

;; ;; Better imenu
(use-package js2-imenu-extras-mode
  :hook js2-mode)

;; js2-refactor and xref-js2
(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
(define-key js-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))


(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)))  ;; auto-enable for .js/.jsx files
  
  ;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
(add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
(add-to-list 'web-mode-comment-formats '("javascript" . "//" ))

;; jsx syntax highlighting
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(use-package emmet-mode
  :hook (sgml-mode css-mode web-mode js2-mode)
  :custom (emmet-move-cursor-between-quotes t))

(use-package org
  :bind (("C-c l" . 'org-store-link)
         ("C-c a" . 'org-agenda))
  :custom (org-log-done t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm mode temporary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(helm-autoresize-mode t)
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)


;; (setq helm-display-function 'helm-display-buffer-in-own-frame
;;         helm-display-buffer-reuse-frame t
;;         helm-use-undecorated-frame-option t)`

(helm-mode 1)

(require 'helm-descbinds)
(helm-descbinds-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; end temporary helm mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package rjsx-mode
;;    :ensure t
;;    :mode (
;;          ("\\.jsx\\'" . rjsx-mode)))

(use-package company
  :defer t
  :init (global-company-mode)
  :config
  (setq company-idle-delay              0.1
        company-minimum-prefix-length   2
        company-show-numbers            t
        company-dabbrev-downcase        nil))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (flycheck-add-next-checker 'javascript-eslint  'javascript-jshint 'append)
  ;;(setq flycheck-eslintrc "~/.eslintrc.json")
  (flycheck-add-mode 'javascript-eslint 'web-mode))
  
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (company-mode 1))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

;;(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-hook 'web-mode-hook #'setup-tide-mode)


(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(add-to-list 'load-path "~/.emacs.d/es6-snippets")
(use-package es6-snippets)

;; TODO: enable tidy-HTML in flycheck

;; enable set-goal-column
;;(put 'set-goal-column 'disabled nil)

;; prettier
(use-package prettier-js
  :after js2-mode
  :init
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  :config
  (setq prettier-js-args '("--bracket-spacing" "true"
                           "--tab-width" "4")))

;; (put 'downcase-region 'disabled nil)
;; (put 'upcase-region 'disabled nil)

(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)))


(use-package expand-region
  :ensure t)
(global-set-key (kbd "C-=") 'er/expand-region)

;; causes selected text to be deleted or replaced by subsequent
;; key presses, as in (all) other editors
(delete-selection-mode t)


;; TODO js2-minor-mode for js/jsx files
