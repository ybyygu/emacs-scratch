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
 '(custom-safe-themes
   '("f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" default))
 '(package-selected-packages
   '(keyfreq corfu-doc corfu-terminal corfu magit-todos dired-hide-dotfiles dired-hacks-utils dired-single citre yasnippet which-key vertico use-package symbol-overlay smartparens simpleclip rust-mode rime org-superstar org-sidebar org-noter org-download orderless meow marginalia magit-popup magit ivy-rich ivy-hydra helpful goto-last-change goto-chg golden-ratio general format-all find-file-in-project fd-dired embark-consult el-patch doom-themes doom-modeline crux counsel command-log-mode cargo burly ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#37474f"))))
 '(org-done ((t (:foreground "yellow" :weight bold :background "#263238"))))
 '(org-headline-done ((t (:foreground "gray" :weight normal))))
 '(org-level-1 ((t (:foreground "#e3f2fd" :height 1.1 :background nil :weight normal :box nil))))
 '(org-level-2 ((t (:foreground "#e3f2fd" :height 1.0 :background nil :weight normal :box nil))))
 '(org-table ((t (:foreground "#e3f2fd"))))
 '(org-todo ((t (:background "#263238" :foreground "yellow" :weight bold))))
 '(region ((t (:background "#555555"))))
 '(secondary-selection ((t (:foreground "green"))))
 '(show-paren-match ((t (:foreground "gray100" :background "#9c7618" :weight bold))))
 '(solaire-hl-line-face ((t (:background "#37474f")))))
