;; [[file:gwp-scratch.note::158fcd0c][158fcd0c]]
;; Load path
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)
(push (expand-file-name "user-lisp" user-emacs-directory) load-path)

;; Packages
(require 'package)

(setq package-archives
      '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
        ("org" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
        ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; (use-package command-log-mode)

(require 'init-defaults)
(require 'init-core)
(require 'init-edit)
(require 'init-xxx)
(require 'init-ui)
(require 'init-dired)
(require 'init-workspace)
(require 'init-org)
(require 'init-develop)
(require 'init-note)
(require 'init-completion)
(require 'init-eaf)
(require 'init-bindings)
;; 158fcd0c ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(fd-dired goto-chg goto-last-change yasnippet which-key vertico use-package symbol-overlay smartparens simpleclip rust-mode rime org-superstar org-sidebar org-noter org-download orderless meow marginalia magit-popup magit ivy-rich ivy-hydra helpful golden-ratio general find-file-in-project embark-consult el-patch doom-themes doom-modeline crux counsel command-log-mode cargo burly ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:foreground "gray100" :background "#9c7618" :weight bold)))))
