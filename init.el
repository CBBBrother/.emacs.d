(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(xterm-mouse-mode 1)

(setq frame-title-format nil)
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq visible-bell 1)

(defalias 'list-buffers 'ibuffer)
(setq-default cursor-type 'bar)
(setq cursor-in-non-selected-windows nil)

(setq-default require-final-newline t)
(setq create-lockfiles nil)
(setq delete-trailing-lines t)
(setq org-hide-emphasis-markers t)

(column-number-mode)
(global-display-line-numbers-mode t)
(delete-selection-mode 1)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(prefer-coding-system 'utf-8)

(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark)))

(require 'package)
(setq use-package-always-ensure t)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package monokai-theme
  :ensure t)
(load-theme 'monokai t)

(add-to-list 'default-frame-alist
             '(font . "MesloLGL Nerd Font Mono-16"))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :config
    (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c i") 'counsel-imenu)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-c f") 'counsel-fzf)
    (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(use-package treemacs
    :commands (treemacs-follow-mode
               treemacs-filewatch-mode
               treemacs-fringe-indicator-mode
               treemacs-git-mode)
    :bind (([f8]        . treemacs)
           ("M-0"       . treemacs-select-window)
           ("C-x t t"   . treemacs)
           :map treemacs-mode-map
           ([mouse-1]   . treemacs-single-click-expand-action))
    :config
    (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
          treemacs-missing-project-action  'remove
          treemacs-sorting                 'alphabetic-asc
          treemacs-follow-after-init       t
          treemacs-width                   50)
    :config
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (use-package treemacs-projectile
      :after projectile
      :bind (:map projectile-command-map
             ("h" . treemacs-projectile)))

    (use-package treemacs-magit
      :after magit
      :commands treemacs-magit--schedule-update
      :hook ((magit-post-commit
              git-commit-post-finish
              magit-post-stage
              magit-post-unstage)
             . treemacs-magit--schedule-update))

    (use-package treemacs-persp
      :after persp-mode
      :demand t
      :functions treemacs-set-scope-type
      :config (treemacs-set-scope-type 'Perspectives)))

;; M-x all-the-icons-install-fonts 
(use-package all-the-icons)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(toggle-frame-maximized)

(use-package go-mode)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)
(defconst my-cc-style
  '("k&r"
    (c-offsets-alist . ((innamespace . [0])))))
(c-add-style "my-cc-style" my-cc-style)
(setq c-default-style "my-cc-style")

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (c++-mode . lsp-deferred)
	 (go-mode . lsp-deferred)
	 (python-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
(setq exec-path (append exec-path '("/opt/homebrew/bin")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#fbf1c7" "#9d0006" "#79740e" "#b57614" "#076678" "#b16286" "#427b58" "#282828"])
 '(column-number-mode t)
 '(exwm-floating-border-color "#d3c5a0")
 '(fci-rule-color "#504945")
 '(global-display-line-numbers-mode t)
 '(highlight-tail-colors ((("#eee4b4" "#f3f3c1") . 0) (("#e8e5bb" "#eff3cf") . 20)))
 '(hl-sexp-background-color "#efebe9")
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#a89984"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#79740e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#928374"))
 '(menu-bar-mode nil)
 '(objed-cursor-color "#9d0006")
 '(package-selected-packages
   '(yasnippet flycheck go-mode dashboard dracula-theme treemacs-persp treemacs-magit magit org-bullets treemacs-projectile all-the-icons counsel which-key try use-package))
 '(pdf-view-midnight-colors (cons "#282828" "#fbf1c7"))
 '(rustic-ansi-faces
   ["#fbf1c7" "#9d0006" "#79740e" "#b57614" "#076678" "#b16286" "#427b58" "#282828"])
 '(tool-bar-mode nil)
 '(vc-annotate-background "#fbf1c7")
 '(vc-annotate-color-map
   (list
    (cons 20 "#79740e")
    (cons 40 "#8d7410")
    (cons 60 "#a17512")
    (cons 80 "#b57614")
    (cons 100 "#b3620e")
    (cons 120 "#b14e08")
    (cons 140 "#af3a03")
    (cons 160 "#af472e")
    (cons 180 "#b0545a")
    (cons 200 "#b16286")
    (cons 220 "#aa415b")
    (cons 240 "#a32030")
    (cons 260 "#9d0006")
    (cons 280 "#9a2021")
    (cons 300 "#97413c")
    (cons 320 "#946258")
    (cons 340 "#504945")
    (cons 360 "#504945")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "MesloLGL Nerd Font Mono" :foundry "nil" :slant normal :weight normal :height 120 :width normal)))))
(put 'upcase-region 'disabled nil)
