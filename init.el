(setq inhibit-startup-screen t)
(setq frame-title-format nil)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default require-final-newline t)
(use-package display-line-numbers
      :ensure nil
      :hook (prog-mode . display-line-numbers-mode))

(prefer-coding-system 'utf-8)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)

(require 'powerline)
(powerline-default-theme)


(require 'package)
(setq package-enable-at-startup nil)
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

(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

(use-package projectile
  :ensure t)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package counsel
  :ensure t)

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

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

;; (use-package doom-themes
;;  :ensure t)

;; (load-theme 'whiteboard t)

(use-package all-the-icons)

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
                          (projects . 5)
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

;;(set-frame-font "Liberation Mono for Powerline-18" nil t)
;; (set-frame-font "Iosevka Term Medium Oblique-20" nil t)
(set-frame-font "Courier-18" nil t)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(bound-and-true-p ns-use-native-fullscreen)
(setq ns-use-native-fullscreen nil)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq-default cursor-type 'bar)
(setq cursor-in-non-selected-windows nil)
;; (global-hl-line-mode)

(defconst my-cc-style
  '("k&r"
    (c-offsets-alist . ((innamespace . [0])))))

(c-add-style "my-cc-style" my-cc-style)
(setq c-default-style "my-cc-style")

(global-set-key (kbd "C-c w") 'whitespace-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" default))
 '(package-selected-packages
   '(magit org-bullets treemacs-projectile all-the-icons counsel which-key try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
