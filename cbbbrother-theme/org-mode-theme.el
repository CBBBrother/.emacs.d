;;; package --- Summary
;;; Commentary:
;;; Code:

(setq org-hide-emphasis-markers t)

(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'org-mode-theme)
;;; org-mode-theme.el ends here
