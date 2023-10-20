;;; package --- Summary
;;; Commentary:
;;; Code:

(setq org-hide-emphasis-markers t)

(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "PROG(p)" "DONE(d)")))

(setq org-agenda-files '("~/Yandex.Disk.localized/orgs"))

(defun my:org-mode-hook ()
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))))

(add-hook 'org-mode-hook #'my:org-mode-hook)
(add-hook 'org-mode-hook 'org-indent-mode)

(provide 'org-mode-theme)
;;; org-mode-theme.el ends here
