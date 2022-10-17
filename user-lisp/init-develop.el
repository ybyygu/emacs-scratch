;; [[file:../gwp-scratch.note::8970c514][8970c514]]
(general-define-key :prefix-map 'gwp::magit-map)

(use-package magit
  :demand t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  (:map gwp::magit-map
   ;; ("j" . magit-next-line)
   ;; ("k" . magit-previous-line)
   ("g" . magit-status)
   ("s" . magit-status)
   ("x" . magit-checkout)
   ("c" . magit-commit)
   ("p" . magit-push)
   ("u" . magit-pull)
   ("e" . magit-ediff-resolve)
   ("r" . magit-rebase-interactive)
   :map magit-status-mode-map
   ;; ("j" . magit-next-line)
   ;; ("k" . magit-previous-line)
   :map magit-hunk-section-map
   ;; ("j" . magit-next-line)
   ;; ("k" . magit-previous-line)
   ))


(use-package magit-popup)
;; 8970c514 ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-develop)
;; provide:1 ends here
