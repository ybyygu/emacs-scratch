;; [[file:gwp-scratch.note::158fcd0c][158fcd0c]]
;; Load path
(push (expand-file-name "site-lisp" user-emacs-directory) load-path)
(push (expand-file-name "user-lisp" user-emacs-directory) load-path)

;; Packages
(require 'package)
;; (setq package-archives
;;       '(("gnu"   . "http://elpa.gnu.org/packages/")
;;         ("melpa" . "http://melpa.org/packages/")))
(setq package-archives
      '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
        ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")
        ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(require 'init-ui)
(require 'init-edit)
(require 'init-org)
(require 'init-develop)
;; 158fcd0c ends here

;; [[file:gwp-scratch.note::f1b9d1b9][f1b9d1b9]]
(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))
;; (add-subdirs-to-load-path "~/.emacs.d/site-lisp/emacs-application-framework")
(add-subdirs-to-load-path "~/.emacs.d/")
;; (require 'eaf-browser)
;; (require 'eaf-pdf-viewer)

(require 'eaf)
;; f1b9d1b9 ends here
