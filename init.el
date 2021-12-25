(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

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
(setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))

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

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

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
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c i") 'counsel-imenu)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-c f") 'counsel-fzf)
    (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(use-package dashboard
  :ensure t
 :bind
  (("<f2>" . open-dashboard))
  :init
  (setq dashboard-center-content t
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-set-init-info t
	dashboard-set-footer t
	dashboard-set-navigator t
       	dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (registers . 5)))
   (dashboard-setup-startup-hook)
  :config
  (defun dashboard-goto-recent-files ()
      "Go to recent files."
      (interactive)
      (let ((func (local-key-binding "r")))
        (and func (funcall func))))
  
  (defun open-dashboard ()
      "Open the *dashboard* buffer and jump to the first widget."
      (interactive)
      ;; Check if need to recover layout
      (if (> (length (window-list-1))
             ;; exclude `treemacs' window
             (if (and (fboundp 'treemacs-current-visibility)
                      (eq (treemacs-current-visibility) 'visible))
                 2
               1))
          (setq dashboard-recover-layout-p t))

      (delete-other-windows)

      ;; Refresh dashboard buffer
      (when (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
      (dashboard-insert-startupify-lists)
      (switch-to-buffer dashboard-buffer-name)

      ;; Jump to the first section
      (dashboard-goto-recent-files))
  )

(setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

(use-package treemacs
    :commands (treemacs-follow-mode
               treemacs-filewatch-mode
               treemacs-fringe-indicator-mode
               treemacs-git-mode)
    :bind (([f8]        . treemacs)
           ("M-0"       . treemacs-select-window)
           ("C-x 1"     . treemacs-delete-other-windows)
           ("C-x t 1"   . treemacs-delete-other-windows)
           ("C-x t t"   . treemacs)
           ("C-x t b"   . treemacs-bookmark)
           ("C-x t C-t" . treemacs-find-file)
           ("C-x t M-t" . treemacs-find-tag)
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

(set-frame-font "Iosevka Term Medium Oblique-16" nil t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

(defconst my-cc-style
  '("k&r"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc-style" my-cc-style)
(setq c-default-style "my-cc-style")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-hide-emphasis-markers t)

(toggle-frame-maximized)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#fbf1c7" "#9d0006" "#79740e" "#b57614" "#076678" "#b16286" "#427b58" "#282828"])
 '(custom-safe-themes
   '("7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" default))
 '(exwm-floating-border-color "#d3c5a0")
 '(fci-rule-color "#504945")
 '(highlight-tail-colors ((("#eee4b4" "#f3f3c1") . 0) (("#e8e5bb" "#eff3cf") . 20)))
 '(hl-sexp-background-color "#efebe9")
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#a89984"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#79740e"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#928374"))
 '(objed-cursor-color "#9d0006")
 '(package-selected-packages
   '(treemacs-persp treemacs-magit magit org-bullets treemacs-projectile all-the-icons counsel which-key try use-package))
 '(pdf-view-midnight-colors (cons "#282828" "#fbf1c7"))
 '(rustic-ansi-faces
   ["#fbf1c7" "#9d0006" "#79740e" "#b57614" "#076678" "#b16286" "#427b58" "#282828"])
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
 )
