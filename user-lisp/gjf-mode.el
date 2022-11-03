;; [[file:../gwp-scratch.note::781e14bf][781e14bf]]
(defvar gjf-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.gjf\\'" . gjf-mode))
(add-to-list 'auto-mode-alist '("\\.com\\'" . gjf-mode))

(defvar gjf-keywords
  '("opt" "scf" "scrf" "freq" "nosymm" "geom" "external" "iop" "force" "int")
  "gjf keywords.")

;; create the regex string for each class of keywords
(defvar gjf-keywords-regexp (regexp-opt gjf-keywords 'words))

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq gjf-font-lock-keywords
  `(
    ;; (,mylsl-functions-regexp . font-lock-function-name-face)
    (,gjf-keywords-regexp . font-lock-keyword-face)
    ;; note: order above matters. “mylsl-keywords-regexp” goes last because
    ;; otherwise the keyword “state” in the function “state_entry”
    ;; would be highlighted.
))

(define-derived-mode gjf-mode text-mode
  "Major mode for editing gaussian job files"
  (setq font-lock-defaults '(gjf-font-lock-keywords t t))
  (setq mode-name "gjf")
  (run-hooks 'gjf-mode-hook)
)

(provide 'gjf-mode)
;; 781e14bf ends here
