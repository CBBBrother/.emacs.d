;;; package --- Summary
;;; Commentary:
;;; Code:

(use-package flycheck
  :ensure t)

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable nil)
  :commands lsp-ui-mode)

(provide 'cbbb-lsp)
;;; lsp.el ends here
