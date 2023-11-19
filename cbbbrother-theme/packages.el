;;; package --- Summary
;;; Commentary:
;;; Code:
(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (projectile-register-project-type 'ymake '("ya.make")
        :project-file "ya.make"
        :compile "ya make"
        :run "ya make -t"))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :init
  (setq dashboard-startup-banner "~/.emacs.d/cbbbrother-theme/witcher.png"))

(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (registers . 5)))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(use-package ivy
  :ensure t
  :init
  (ivy-mode))

(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

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
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

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
          treemacs-width                   40)
    :config
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))

(use-package all-the-icons)

(use-package powerline
  :ensure t
  :commands (powerline-default-theme
             powerline-reset))

(use-package yaml-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(use-package magit
  :ensure t)

(provide 'packages)
;;; packages.el ends here
