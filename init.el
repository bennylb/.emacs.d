;;; package --- Summary

;;; Code:

;;; Commentary:


;;; Custom-set configuration

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes t)
 '(helm-external-programs-associations
   (quote
    (("ods" . "localc")
     ("odt" . "lowriter")
     ("docx" . "lowriter")
     ("pdf" . "evince")
     ("wmv" . "mpv")
     ("m4v" . "mpv")
     ("mov" . "mpv")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; Misc

(defun restart-systemd-emacs ()
  "Restart emacs systemd daemon"
  (interactive)
  (save-some-buffers)
  (delete-frame)
  (if (daemonp)
      (call-process-shell-command "/usr/bin/systemctl --user restart emacs &")
    (message "%s" "No daemon running")
    ))

(defun stop-systemd-emacs ()
  "Stop emacs systemd daemon"
  (interactive)
  (save-some-buffers)
  (delete-frame)
  (if (daemonp)
      (shell-command "systemctl --user stop emacs &")
    (message "%s" "No daemon running")
    ))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq inhibit-startup-screen t)

(global-set-key [f9] 'toggle-menu-bar-mode-from-frame)

(setq make-backup-files nil)

(show-paren-mode 1)
(column-number-mode 1)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

(global-set-key (kbd "C-c i") 'imenu)

(winner-mode 1)

(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Add kernel style
	    (c-add-style
	     "linux-tabs-only"
	     '("linux" (c-offsets-alist
			(arglist-cont-nonempty
			 c-lineup-gcc-asm-reg
			 c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
	  (lambda ()
	    (let ((filename (buffer-file-name)))
	      ;; Enable kernel mode for the appropriate files
	      (when (and filename
			 (string-match (expand-file-name "~/src/linux-trees")
				       filename))
		(setq indent-tabs-mode t)
		(setq show-trailing-whitespace t)
		(c-set-style "linux-tabs-only")))))

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


;;; Formatting

(setq c-default-style "linux")
;;; Trying to insert space instead of tabs
;;; with the following settings was exhibiting
;;; strange behaviour so they're unset untill
;;; I can find a better solution.
;;(setq-default indent-tabs-mode nil)
;;(setq-default tab-width 4)
;;(setq-default indent-line-function 'insert-tab)
;;(setq tab-stop-list (number-sequence 4 200 4))


;;; Use-package management configuration

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(setq package-archives '(("gnu"		 .	"https://elpa.gnu.org/packages/")
			 ;;("marmalade"	 .	"https://marmalade-repo.org/packages/")
			 ("melpa"	 .	"https://melpa.milkbox.net/packages/")
			 ("melpa-stable" .	"https://stable.melpa.org/packages/")))

(package-initialize)

;; (if (not (package-installed-p 'use-package))
;;     (progn
;;       (package-refresh-contents)
;;       (package-install 'use-package))
;;   )
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish) ;; if you use :diminish
(require 'bind-key) ;; if you use :bind variant
(setq use-package-verbose t)

(use-package emacs-lisp
  :commands emacs-lisp-mode
  :init
  (add-to-list 'auto-mode-alist '("\\ensime.*\\'" . emacs-lisp-mode))
  )

(use-package eshell
  :commands (eshell eshell-mode)
  :config
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  )

(use-package info+
  :ensure t)

(use-package pinentry
  :disabled t)

(use-package hydra
  :commands (hydra)
  :ensure t)

(use-package swiper
  :bind
  (("C-s" . swiper))
  :init
  (use-package ivy
    :bind
    (("C-c C-r"	. ivy-resume)
     ("<f6>"	. ivy-resume)
     ;; TODO bind ‘C-M-n’ (‘ivy-next-line-and-call’)
     ;;("C-M-n"	.	ivy-next-line-and-call)
     )
    :init (ivy-mode 1)
    :config
    (progn
      (setq ivy-use-virtual-buffers t)
      (setq ivy-height 15)
      (setq ivy-count-format "(%d/%d) ")
      (setq projectile-completion-system 'ivy)
      (use-package ivy-hydra
	:ensure t))
    :ensure t)
  (use-package counsel
    :bind
    (("M-x"	. counsel-M-x)
     ("C-x C-f"	. counsel-find-file)
     ("<f1> f"	. counsel-describe-function)
     ("<f1> v"	. counsel-describe-variable)
     ("<f1> l"	. counsel-load-library)
     ("<f2> i"	. counsel-info-lookup-symbol)
     ("<f2> u"	. counsel-unicode-char)
     ("C-c g"	. counsel-git)
     ("C-c j"	. counsel-git-grep))
    :config
    (use-package counsel-projectile
      :commands counsel-projectile
      :ensure t)
    :ensure t)
  :ensure t)

(use-package helm
  :init
  (helm-mode 1)
  :diminish helm-mode
  :bind
  (("M-x"	. helm-M-x)
   ("C-x C-f"	. helm-find-files)
   ("C-x b"	. helm-mini)
   ("M-y"	. helm-show-kill-ring)
   ("C-x r b"	. helm-filtered-bookmarks))
  :config
  (progn
    (require 'helm-config)
    (helm-autoresize-mode)
    (setq helm-buffer-max-length nil)
    ;; (progn
    ;;   (defun old-ff (&optional no-op) (interactive)
    ;;          (call-interactively 'find-file))
    ;;   (helm-add-action-to-source "Fallback find-file"
    ;;			       'old-ff
    ;;			       helm-source-find-files)
    ;;   (define-key helm-map (kbd "C-q")
    ;;     (lambda () (interactive)
    ;;       (helm-quit-and-execute-action 'old-ff))))
    (setq projectile-completion-system 'helm)
    )
  (use-package helm-descbinds
    :commands (helm-descbinds helm-descbinds-mode)
    :config (helm-descbinds-mode)
    :ensure t)
  (use-package helm-firefox
    :commands helm-firefox-bookmarks
    :disabled t)
  :disabled t)

(use-package projectile
  :commands (projectile-mode)
  :config
  (use-package helm-projectile
    :config
    (helm-projectile-on)
    :ensure t)
  :disabled t)

(use-package diminish
  :init (diminish 'abbrev-mode)
  :ensure t)

(use-package dired-subtree
  :config
  (eval-after-load 'dired '(define-key dired-mode-map (kbd "i") 'dired-subtree-toggle))
  ;;(eval-after-load 'dired+ '(define-key dired-mode-map (kbd "i") 'dired-subtree-toggle))
  ;;(add-hook 'dired-mode (lambda () (local-set-key (kbd "i") 'dired-subtree-toggle)))
  ;;(add-hook 'dired+ (lambda () (local-set-key (kbd "i") 'dired-subtree-toggle)))
  :ensure t)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config (setq ibuffer-formats
		'((mark modified read-only " "
			(name 30 30 :left :elide) ; change: 30s were originally 18s
			" "
			(size 9 -1 :right)
			" "
			(mode 16 16 :left :elide)
			" " filename-and-process)
		  (mark " "
			(name 16 -1)
			" " filename))))

(use-package tramp
  :defer t
  :config
  (progn (tramp-set-completion-function "ssh" '((tramp-parse-sconfig "/etc/ssh_config")
						(tramp-parse-sconfig "~/.ssh/config")))))

(use-package term
  :commands (term ansi-term)
  :config
  (progn
    (add-hook 'term-mode-hook (lambda () (company-mode -1)))
    (defun autopair-mode-disable ()
      (autopair-mode -1))
    (add-hook 'term-mode-hook 'autopair-mode-disable)
    (add-hook 'term-mode-hook (lambda () (setq yas-dont-activate t)))))

(use-package org
  :commands (org-mode org-agenda)
  :config
  (setq org-todo-keywords '((type "TODO(t)" "PAUSED(p@)" "|" "DONE(d)")))
  (setq org-agenda-files (list
			  "~/Sync/Drop_Box/Org/Personal.org"
			  "~/Uni/Uni.org"
			  "~/Uni/Sem2-2015/Sem2-2015.org"
			  ))
  (setq org-directory "~/Sync/Drop_Box/Org")
  (setq org-mobile-directory "~/Sync/Drop_Box/MobileOrg")
  (setq org-mobile-inbox-for-pull "~/Sync/Drop_Box/Org/mobileinbox.org")
  ;; org-mobile-files overides org-agenda-files
  ;;(setq org-mobile-files '("~/Sync/Drop_Box/Org"))
  (setq org-mobile-force-id-on-agenda-items nil)
  (setq org-list-allow-alphabetical t)

  (setq org-src-fontify-natively t))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode)
  :ensure t)

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :disabled t)

(use-package switch-window
  :bind ("C-x o" . switch-window)
  :ensure t)

(use-package which-key
  :config
  (which-key-mode)
  :disabled t)

(use-package discover
  :config
  (global-discover-mode 1)
  :ensure t)

(use-package discover-my-major
  :bind (("C-h C-m" . discover-my-major)
	 ("C-h M-m" . discover-my-mode))
  :ensure t)

(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
	 ("C-c q" . vr/query-replace))
  :ensure t)

(use-package regex-tool
  :commands regex-tool
  :ensure t)

(use-package flycheck
  :commands (flycheck-mode)
  :init (add-hook 'c-mode-hook #'flycheck-mode)
  ;;:config (add-hook 'after-init-hook #'global-flycheck-mode)
  :ensure t)

(use-package paredit
  :commands #'enable-paredit-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  :ensure t)

(use-package autopair
  :diminish (autopair-mode)
  :config
  (progn
    (add-hook 'prog-mode-hook 'autopair-mode))
  :ensure t)

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :ensure t)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  :ensure t)

(use-package yasnippet
  :defer t
  :init
  :config (yas-reload-all)
  :ensure t)

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (progn
    (use-package company-quickhelp
      :config (company-quickhelp-mode 1)
      :ensure t)
    (setq company-idle-delay 0)
    (setq company-minimum-prefix-length 1)
    (setq completion-ignore-case t)
    ;; The following was taken from
    ;; https://github.com/company-mode/company-mode/issues/94#issuecomment-40884387
    (define-key company-mode-map [remap indent-for-tab-command]
      'company-indent-for-tab-command)

    (setq tab-always-indent 'complete)

    (defvar completion-at-point-functions-saved nil)

    (defun company-indent-for-tab-command (&optional arg)
      (interactive "P")
      (let ((completion-at-point-functions-saved completion-at-point-functions)
	    (completion-at-point-functions '(company-complete-common-wrapper)))
	(indent-for-tab-command arg)))

    (defun company-complete-common-wrapper ()
      (let ((completion-at-point-functions completion-at-point-functions-saved))
	(company-complete-common)))
    )
  :ensure t)

(use-package auto-complete
  :commands (auto-complete-mode global-auto-complete-mode)
  ;;:config (ac-config-default)
  :ensure t)

(defun toggle-completion-mode ()
  "Toggle/switch/swap from company to auto-complete mode"
  (interactive)
  (if (company-mode)
      (progn
	(company-mode -1)
	(auto-complete-mode 1))
    (if (auto-complete-mode)
	(progn
	  (auto-complete-mode -1)
	  (company-mode 1)))))

;; (if (company-mode)
;;     (message "company-mode is enabled."))

;; (symbol-value company-mode)
;; (symbol-value auto-complete-mode)
;;  ;;(auto-complete-mode -1)
;; (company-mode 1)


(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t)
  :ensure t)

(use-package highlight-indentation
  :commands highlight-indentation-mode
  :config
  (progn
    ;; TODO Add hooks here
    )
  :ensure t)

(use-package auto-highlight-symbol
  :bind (("<f3>"   . ahs-forward)
	 ("S-<f3>" . ahs-backward))
  :ensure t)

(use-package spaceline
  :config
  (progn
    (use-package powerline
      :ensure t)
    (require 'spaceline-config)
    (spaceline-emacs-theme)
    (add-hook 'Info-mode-hook 'spaceline-info-mode))
  :ensure t)

(use-package magit
  :bind ("C-x g" . magit-status)
  :ensure t
  :pin melpa-stable)

(use-package gitignore-mode
  :commands gitignore-mode
  :ensure t)

(use-package irony
  :commands (irony-mode)
  :init (add-hook 'c-mode-hook 'irony-mode)
  :config
  (progn
    (use-package flycheck-irony
      :config
      (eval-after-load 'flycheck
	'(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
      :ensure t)
    (use-package company-irony
      :config
      (use-package company-irony-c-headers
	:ensure t)
      (eval-after-load 'company
	'(add-to-list 'company-backends '(company-irony-c-headers company-irony)))
      (add-hook 'irony-mode-hook #'yas-minor-mode)
      :ensure t)
    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
	'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
	'irony-completion-at-point-async))
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package elpy
  :commands (elpy-mode)
  :init (add-hook 'python-mode-hook 'elpy-mode)
  :config
  (progn
    (use-package pyvenv
      :ensure t)
    (use-package pony-mode
      :disabled t)
    (use-package tern-django
      :disabled t))
  :ensure t)

(use-package ensime
  :defer t
  :init (add-hook 'scala-mode-hook #'flycheck-mode)
  :disabled t)

(use-package web-mode
  :mode (("\\.php\\'"  . web-mode)
	 ("\\.html\\'" . web-mode))
  :config
  (progn
    (defun my-web-mode-hook ()
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-code-indent-offset 4))
    (add-hook 'web-mode-hook 'my-web-mode-hook)
    (add-hook 'web-mode-hook 'toggle-completion-mode)
    )
  :ensure t)

(use-package eclimd
  ;;:load-path "~/.emacs.d/elpa/emacs-eclim-*"
  :commands start-eclimd
  :config
  (progn
    (add-hook 'java-mode-hook #'yas-minor-mode)
    (add-hook 'java-mode-hook 'eclim-mode)
    (use-package eclim
      :config
      ;; Swap the following to change
      ;; between AC and company mode.
      (require 'ac-emacs-eclim-source)
      (ac-emacs-eclim-config)
      ;;(require 'company-emacs-eclim)
      ;;(company-emacs-eclim-setup)
      )))

(use-package js2-mode
  :mode "\\.js\\'"
  :commands js2-mode
  :config
  (progn
    (add-hook 'js2-mode-hook #'yas-minor-mode)
    (use-package tern
      :config
      (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
      (use-package company-tern
	:config
	(add-to-list 'company-backends 'company-tern)
	(setq company-tooltip-align-annotations t)
	:ensure t)
      (use-package tern-auto-complete
	:config
	(defun ac-tern ()
	  (add-hook 'js-mode-hook 'toggle-completion-mode)
	  (eval-after-load 'tern
	    '(progn
	       (require 'tern-auto-complete)
	       (tern-ac-setup))))
	(ac-tern)
	:disabled t)
      :ensure t)
    (use-package js2-refactor
      :config
      (add-hook 'js2-mode-hook #'js2-refactor-mode)
      (js2r-add-keybindings-with-prefix "C-c C-m")
      (use-package discover-js2-refactor
	:ensure t)
      :ensure t)
    (use-package js2-highlight-vars
      :config
      (add-hook 'js2-mode-hook 'js2-highlight-vars-mode)
      :disabled t))
  :ensure t)

(use-package skewer-mode
  :commands (skewer-mode skewer-html-mode skewer-css-mode)
  :config
  (progn
    (use-package simple-httpd
      :commands httpd-start
      :config
      (setq httpd-root "/home/ben/scratch/skewer")
      :ensure t)
    (use-package js2-mode
      :ensure t))
  :ensure t)

(use-package pkgbuild-mode
  :mode ("\\PKGBUILD\\'" . pkgbuild-mode)
  :ensure t)

(use-package shackle
  :config
  (progn
    (setq helm-display-function #'pop-to-buffer)
    (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.46)))
    (shackle-mode))
  :disabled t)

(use-package auctex
  :commands auctex
  :init
  (progn
    (add-hook 'TeX-mode-hook #'yas-minor-mode)
    (add-hook 'TeX-mode-hook 'company-mode))
  :config (use-package company-auctex
	    :config (company-auctex-init)
	    :ensure t)
  :disabled t)

(use-package edit-server
  :defer t
  :disabled t)

(defvar my-custom-theme nil)
(setq my-custom-theme 'monokai)

(defun my-load-theme (frame)
  (select-frame frame)
  (load-theme my-custom-theme t))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'my-load-theme)
  (load-theme my-custom-theme t))

(use-package leuven-theme
  :defer t
  :ensure t)

(use-package material-theme
  :defer t
  :ensure t)

(use-package monokai-theme
  :defer t
  :ensure t)

(use-package aurora-theme
  :defer t
  :ensure t)

(use-package spacemacs-theme
  :defer t
  :ensure t)

(use-package badwolf-theme
  :defer t
  :ensure t)

(use-package soothe-theme
  :defer t
  :ensure t)

(use-package flatland-theme
  :defer t
  :ensure t)

(use-package gruvbox-theme
  :defer t
  :ensure t)

(use-package pdf-tools
  :if (eq system-type 'gnu/linux)
  :commands (pdf-view-mode)
  :init (pdf-tools-install)
  :ensure t)

(provide '.emacs)
;;; .init.el ends here
