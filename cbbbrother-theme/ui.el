;;; package --- Summary
;;; Commentary:
;;; Code:
(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(setq-default fill-column 140)
(setq-default truncate-lines t)

(setq frame-title-format nil)

(column-number-mode)
(global-display-line-numbers-mode t)
(delete-selection-mode 1)
(hl-line-mode)

(setq-default cursor-type 'hollow)
(setq cursor-in-non-selected-windows nil)
(setq visible-bell 1)

(provide 'ui)
;;; ui.el ends here
