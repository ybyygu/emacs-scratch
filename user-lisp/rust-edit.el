;; [[file:../gwp-scratch.note::3241e30e][3241e30e]]
(require 'smartparens)
(require 'rust-utils)

(defun rust-edit-toggle-pub ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (if (string= "pub " (buffer-substring (point) (+ (point) 4)))
        (delete-region (point) (+ (point) 4))
      (insert "pub "))))

(defun rust-edit-insert-option (&optional result)
  (interactive)
  (when (region-active-p)
    (sp-wrap-with-pair "<")
    (backward-char)
    (insert (or result "Option"))))

(defun rust-edit-insert-result ()
  (interactive)
  (rust-edit-insert-option "Result"))

(defun rust-edit-unwrap-option (&optional result)
  "Remove Option type wrapper"
  (interactive)

  (when (region-active-p)
    (when (> (mark) (point))
      (exchange-point-and-mark))
    (sp-unwrap-sexp)
    (when (search-backward (or result "Option") (line-beginning-position) t)
      (delete-region (match-beginning 0) (match-end 0))
      )))

(defun rust-edit-unwrap-result ()
  (interactive)
  (rust-edit-unwrap-option "Result"))

(transient-define-prefix rust-edit-transient ()
  "rust development tools"
  ["Option"
   ("o" "Wrap in Option" rust-edit-insert-option)
   ("u" "Unwrap Option" rust-edit-unwrap-option)
   ]
  ["Result"
   ("r" "Wrap in Result" rust-edit-insert-result)
   ("k" "Unwrap Result" rust-edit-unwrap-result)
   ]
  ["Toggle"
   ("p" "toggle pub at point" rust-edit-toggle-pub)
   ("m" "toggle mut at point" rust-toggle-mutability)
   ]
  )
;; 3241e30e ends here

;; [[file:../gwp-scratch.note::524a7643][524a7643]]
(require 'rust-cargo)

;;;###autoload
(defun rust-edit-cargo-watch ()
  "Compile using `cargo watch`"
  (interactive)
  ;; 编译时随新内容自动滚动更新
  (let ((compilation-scroll-output t))
    (rust--compile "env RUSTFLAGS=-Awarnings cargo watch -x d")))

;; taken from cargo.el
;;
;; workaround cargo issue: https://github.com/rust-lang/cargo/issues/5895
;;
;; to make "jump-to-error" work, we need start compilation in workspace root dir
(defun rust-edit--cargo-compile (args)
  (when (null rust-buffer-project)
    (rust-update-buffer-project))
  (let* ((old-directory default-directory) ; save current directory
         (default-directory
           (or (and rust-buffer-project
                    (file-name-directory rust-buffer-project))
               default-directory)))
    ;; 编译时随新内容自动滚动更新
    (let ((compilation-scroll-output 'first-error))
      (compile (format "cargo.sh \"%s\" %s" old-directory args)))))

;;;###autoload
(defun rust-edit-cargo-doc-open ()
  "Execute `cargo doc --open` command"
  (interactive)
  (rust-edit--cargo-compile "doc --open --no-deps"))

;;;###autoload
(defun rust-edit-cargo-watch ()
  "Execute `cargo watch -x d` command"
  (interactive)
  (rust-edit--cargo-compile "watch -x d"))

;;;###autoload
(defun rust-edit-cargo-update ()
  "Execute `cargo update` command"
  (interactive)
  (rust-edit--cargo-compile "update"))

;; 方便编译窗口操作
(with-eval-after-load 'compile
  (define-key compilation-mode-map (kbd "o") 'compile-goto-error))

(transient-define-prefix rust-edit-cargo-transient ()
  "rust development tools"
  ["cargo"
   ("b" "cargo watch build" rust-edit-cargo-watch)
   ("d" "cargo doc" rust-edit-cargo-doc-open)
   ("u" "cargo update" rust-edit-cargo-update)
   ]
  )
;; 524a7643 ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'rust-edit)
;; provide:1 ends here
