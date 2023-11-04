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

(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)


(provide 'cbbb-lsp)
;;; lsp.el ends here
