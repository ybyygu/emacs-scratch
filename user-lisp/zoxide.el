;; [[file:../gwp-scratch.note::*docs][docs:1]]
;; -*- lexical-binding: t; -*-
;; docs:1 ends here

;; [[file:../gwp-scratch.note::8597bcb8][8597bcb8]]
(require 'recentf)
(defun gwp::zoxide-recent-directories ()
  (let* ((output (shell-command-to-string "zoxide query --list"))
         (dirs (split-string output "[\r\n]+" t)))
    dirs))

(defun gwp::dired-recent-directories ()
  (let* ((recent-dirs
          (mapcar (lambda (file)
                    (if (file-directory-p file) file (file-name-directory file)))
                  recentf-list)))
    recent-dirs))

;;;###autoload
(defun gwp::zoxide-add-directory (path)
  "将 dir 加入 zoxide 数据库中"
  (when path
    (when (file-directory-p path)
      (message "zoxide: record %s" path)
      (call-process "zoxide" nil nil nil "add" path))))

;; open recent directory
;; credit: http://stackoverflow.com/questions/23328037/in-emacs-how-to-maintain-a-list-of-recent-directories
;;;###autoload
(defun gwp::recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (require 'consult)

  (let* ((recent-dirs (delete-dups
		       (append (gwp::zoxide-recent-directories) (gwp::dired-recent-directories))))
	 ;; do not sort candidates
	 (vertico-sort-function nil)
	 (default-directory (consult--read recent-dirs :prompt "Directory: " :category 'file)))
    (gwp::zoxide-add-directory default-directory)
    (find-file (expand-file-name default-directory))))
;; 8597bcb8 ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'zoxide)
;; provide:1 ends here
