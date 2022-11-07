;; [[file:gwp-scratch.note::ec81fe51][ec81fe51]]
(defcustom init-no-x-flag t
  "for running in remote ssh server no X display"
  :type 'boolean)

;; 自动检测, 自动设置
(and (fboundp 'x-create-frame) (getenv "DISPLAY")
     (setq init-no-x-flag nil))
;; ec81fe51 ends here

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

(unless init-no-x-flag
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; 158fcd0c ends here

;; [[file:gwp-scratch.note::07c1e867][07c1e867]]
;; 将 custom 定义为独立的文件
(setq custom-file (locate-user-emacs-file "custom.el"))
;; Create the custom-file if it doesn't exist.
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load-file custom-file)
;; 07c1e867 ends here

;; [[file:gwp-scratch.note::0cc5b7b8][0cc5b7b8]]
(require 'init-defaults)
(require 'init-core)
(require 'init-general)
(require 'init-meow)
(require 'init-edit)
(require 'init-ui)
(require 'init-dired)
(require 'init-workspace)

(unless init-no-x-flag
  (require 'init-org)
  (require 'init-develop)
  (require 'init-note)
  (require 'init-eaf))
(require 'init-completion)
(require 'init-chemistry)
(require 'init-bindings)

;; 也可这么设置
;; (use-package init-bindings :ensure nil)
;; 0cc5b7b8 ends here
