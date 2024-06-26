;; -*- lexical-binding: t; -*-

;; [[file:../gwp-scratch.note::98635e62][98635e62]]
(fset 'yes-or-no-p 'y-or-n-p)
;; 98635e62 ends here

;; [[file:../gwp-scratch.note::f794969f][f794969f]]
(setq mouse-yank-at-point t)
;; f794969f ends here

;; [[file:../gwp-scratch.note::9e84761e][9e84761e]]
;; 默认不折行显示, 这样更整齐些
(set-default 'truncate-lines t)
(setq truncate-partial-width-windows t)
;; 9e84761e ends here

;; [[file:../gwp-scratch.note::d8ca820b][d8ca820b]]
;; credit: crafted-defaults.el
;;
;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
;; Using `advice' here to make it easy to reverse in custom
;; configurations with `(advice-remove 'yes-or-no-p #'y-or-n-p)'
;;
;; N.B. Emacs 28 has a variable for using short answers, which should
;; be preferred if using that version or higher.
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (advice-add 'yes-or-no-p :override #'y-or-n-p))

;; Do not save duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

(provide 'init-defaults)
;; d8ca820b ends here
