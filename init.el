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
  :ensure t
  :init
  (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1
        which-key-sort-uppercase-first nil))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package hydra)

(use-package crux
  :config
  (bind-key [remap move-beginning-of-line] #'crux-move-beginning-of-line))

(require 'init-edit)
(require 'init-ui)
(require 'init-dired)
(require 'init-org)
(require 'init-develop)

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :after org
  :config
  ;; (require 'eaf-file-manager)
  (require 'eaf-browser)
  (require 'eaf-image-viewer)
  (require 'eaf-pdf-viewer)
  (require 'eaf-demo)
  (require 'eaf-org)
  ;; (require 'eaf-interleave)
  ;; (require 'eaf-terminal)
  ;; (require 'eaf-evil)

  ;; 按键绑定等
  (defalias 'browse-web #'eaf-open-browser)
  (eaf-bind-key close_buffer "q" eaf-pdf-viewer-keybinding)
  (eaf-bind-key add_annot_highlight "d"  eaf-pdf-viewer-keybinding)
  (eaf-bind-key add_annot_underline "a" eaf-pdf-viewer-keybinding)
  (eaf-bind-key add_annot_squiggly "s" eaf-pdf-viewer-keybinding)
  (eaf-bind-key undo_annot_action "u" eaf-pdf-viewer-keybinding)
  (eaf-bind-key nil "SPC" eaf-pdf-viewer-keybinding) ;; unbind, see more in the Wiki
  (eaf-bind-key zoom_in "=" eaf-pdf-viewer-keybinding)
  (eaf-bind-key zoom_out "-" eaf-pdf-viewer-keybinding)
  (eaf-bind-key jump_to_page "p" eaf-pdf-viewer-keybinding)
  ;; (define-key eaf-mode-map* (kbd "C-c B") #'eaf-open-bookmark)


  (defun eaf-org-open-file (file &optional link)
    "An wrapper function on `eaf-open'."
    (eaf-open file))

  ;; use `emacs-application-framework' to open PDF file: link
  (add-to-list 'org-file-apps '("\\.pdf\\'" . eaf-org-open-file))

  ;; 修复 evil 按键问题. 这里需要启用 eaf-browser 支持, 不然会报错
  ;; (require 'eaf-evil)
  ;; (define-key key-translation-map (kbd "SPC")
  ;;   (lambda (prompt)
  ;;     (if (derived-mode-p 'eaf-mode)
  ;;         (pcase eaf--buffer-app-name
  ;;           ("pdf-viewer" (kbd eaf-evil-leader-key))
  ;;           (_  (kbd "SPC")))
  ;;       (kbd "SPC"))))

  :custom
  ;; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
  ;; (eaf-browser-continue-where-left-off t)
  ;; (eaf-browser-enable-adblocker t)
  (eaf-pdf-dark-mode nil)
  (confirm-kill-processes nil)	; 退出不需要确认杀死进程
  ;; (browse-url-browser-function 'eaf-open-browser)
  )
