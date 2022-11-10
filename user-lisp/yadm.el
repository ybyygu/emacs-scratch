;; [[file:../gwp-scratch.note::840c0c02][840c0c02]]
(require 'magit)

;; https://www.reddit.com/r/emacs/comments/gjukb3/yadm_magit/
(use-package tramp
  :ensure nil
  :config
  (setenv "SHELL" (executable-find "bash"))
  (unless (assoc-default "yadm" tramp-methods)
    ;; https://github.com/TheLocehiliosan/yadm/blob/master/yadm.md#commands
    (add-to-list 'tramp-methods
                 '("yadm"
                   (tramp-login-program "yadm")
                   (tramp-login-args (("enter")))
                   (tramp-login-env (("SHELL") ("/bin/sh")))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-c"))))))

(defun yadm--files ()
  (let ((default-directory "~/"))
    (cl-delete-if-not
     #'file-exists-p
     (process-lines "yadm" "ls-tree" "--full-tree" "-r" "--name-only" "HEAD"))))

;;;###autoload
(defun yadm-find-file ()
  (interactive)
  (let ((default-directory  "~/"))
    (find-file
     (completing-read "Yadm file: " (yadm--files)))))

;;;###autoload
(defun yadm-status ()
  (interactive)
  (with-current-buffer (magit-status "/yadm::")))

;; ;;;###autoload
;; (defun yadm-stage ()
;;   (interactive)
;;   (let ((file
;;          (let ((default-directory "~/"))
;;            (read-file-name "Stage file: "))))
;;     (if (equal (expand-file-name file)
;;                (expand-file-name "~/.yadm/"))
;;         (user-error "Can't stage yadm dir itself.")
;;       (magit-with-toplevel
;;         (magit-stage-1 nil (list file))))))

;;;###autoload
(defun yadm-add-file ()
  (interactive)

  ;; if in dired buffer, use the file at point
  (let* ((file-path
          (if (derived-mode-p 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name)))
    (if (equal file-path (expand-file-name "~/.yadm/"))
        (user-error "Can't stage yadm dir itself."))
    (when file-path
      (when (s-contains? "/home/" file-path)
        (message "%s" file-path)
        (shell-command (concat "yadm add " file-path))))))
;; 840c0c02 ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'yadm)
;; provide:1 ends here
