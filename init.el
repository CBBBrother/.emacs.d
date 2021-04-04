(setq inhibit-startup-screen t)

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


(tool-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq auto-save-default nil)

(require 'uniquify)
(require 'ido)
(ido-mode t)

(global-display-line-numbers-mode)
