;; -*- lexical-binding: t; -*-

;; [[file:../gwp-scratch.note::24325443][24325443]]
(use-package with-editor)

(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)
(add-hook 'vterm-mode-hook  'with-editor-export-editor)

;; 进入 insert 编辑模式
(add-hook 'with-editor-mode-hook 'meow-insert-mode)
;; 24325443 ends here

;; [[file:../gwp-scratch.note::81cb1ab5][81cb1ab5]]
(use-package find-file-in-project
  :config
  (setq ffip-use-rust-fd t))

;;;###autoload
(defun gwp::find-file-from-clipboard ()
  "打开 clipboard 中复制的文件路径"
  ;;   --> trajectory-analysis/src/part.rs:25:26
  ;; (setq xx "  --> trajectory-analysis/src/part.rs:25:26")
  ;; (string-match "\\([^/:]+:[0-9]+\\)" xx)
  ;; (match-string 1 xx)
  (interactive)
  (require 'find-file-in-project)
  (let* ((str (gui-get-primary-selection))
         (path (progn (string-match "\\([^/:]+:[0-9]+\\)" str)
                      (match-string 1 str))))
    (message "find file: %s" path)
    (if path
        (ffip-find-files path nil))))
;; 81cb1ab5 ends here

;; [[file:../gwp-scratch.note::8970c514][8970c514]]
(use-package magit
  :ensure t
  :unless init-no-x-flag
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; 隐藏untracked文件列表. 更多时候的操作是stage/commit
  (setq magit-section-initial-visibility-alist (quote ((untracked . hide))))
  ;;禁用magit中的gravatars支持, 响应能快一些.
  (setq magit-revision-show-gravatars nil)
  ;; 进入 magit-status 后, 将光标定在 unstaged 一栏
  (setq magit-status-initial-section '(2))
  (gwp::local-leader-def
    :keymaps 'dired-mode-map
    "l"      #'(magit-dired-log :which-key "git log")
    )
  (gwp::local-leader-def
    :keymaps 'magit-status-mode-map
    "D" #'magit-file-delete
    "j" #'magit-dired-jump
    "o" #'magit-diff-visit-file-other-window
    "O" #'magit-diff-visit-file-other-frame
    "r" #'magit-file-rename
    "t" #'magit-todos-list
    "f" #'magit-find-file
    )

  :bind
  (:map gwp::magit-map
        ;; ("j" . magit-next-line)
        ("g" . magit-status)
        ("s" . magit-status)
        ("x" . magit-checkout)
        ("c" . magit-commit)
        ("p" . magit-push)
        ("u" . magit-pull)
        ("e" . magit-ediff-resolve)
        ("r" . magit-rebase-interactive)
        ("f" . magit-file-dispatch)
        :map magit-status-mode-map
        ;; ("j" . magit-next-line)
        ;; ("k" . magit-previous-line)
        :map magit-hunk-section-map
        ;; ("j" . magit-next-line)
        ;; ("k" . magit-previous-line)
        ))

(use-package magit-popup)

;; 显示 src 中的 TODO FIXME 等项
(use-package magit-todos
  :ensure t
  :config
  ;; 2022-11-01: 会影响 magit 响应速度, 现禁用
  ;; (magit-todos-mode)
  (bind-key "t" #'magit-todos-list gwp::magit-map))
;; 8970c514 ends here

;; [[file:../gwp-scratch.note::275df196][275df196]]
(require 'yadm)

(bind-key "." #'yadm-status gwp::magit-map)
(bind-key "." #'yadm-find-file gwp::develop-map)

(gwp::local-leader-def
  :keymaps 'dired-mode-map
  "a" #'yadm-add-file)
;; 275df196 ends here

;; [[file:../gwp-scratch.note::a267f2ee][a267f2ee]]
(use-package rust-mode
  :requires smartparens
  :config
  (require 'smartparens-rust)
  ;; Don't pair lifetime specifiers
  (sp-local-pair 'rust-mode "'" nil :actions nil)
  ;; rust 回车后自动格式化 {|}
  ;; https://emacs.stackexchange.com/questions/2837/automatically-formatting-brackets
  (sp-local-pair 'rust-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
  ;; 解决 smartparents-rust 设置中带来的问题, 去除多余的设置
  (sp-local-pair 'rust-mode "<" nil :actions :rem)
  (sp-local-pair 'rust-mode "<" ">")
  ;; Rust closure中使用, 字符串中不成对
  (sp-local-pair 'rust-mode "|" "|" :unless '(sp-in-string-p sp-in-comment-p))
  (bind-keys :map rust-mode-map
             ("M-n" . rust-end-of-defun)
             ("M-p" . rust-beginning-of-defun)))

(use-package cargo)

(require 'rust-edit)
(gwp::local-leader-def
  :keymaps 'rust-mode-map
  "e" #'rust-edit-transient
  "b" #'rust-edit-cargo-transient)


;; via https://github.com/twlz0ne/separedit.el
(use-package separedit
  :custom
  (separedit-default-mode 'markdown-mode)
  :config
  (define-key prog-mode-map (kbd "C-c C-;") #'separedit-dwim))
;; a267f2ee ends here

;; [[file:../gwp-scratch.note::f2289888][f2289888]]
;; 2022-10-28: 不设置的话不能正常处理 elisp 代码(org src block 中)
(use-package format-all
  :demand t
  :bind ("C-c C-f" . format-all-buffer)
  :custom
  (format-all-default-formatters
   '(("Emacs Lisp" emacs-lisp)
     ("Python" black)
     ("Rust" rustfmt)
     ("Shell" shfmt)
     ("TOML" prettier)
     ("Lua" lua-fmt)
     ("Dockerfile" dockfmt)
     ("CMake" cmake-format)
     ("C" clang-format)
     ("C++" clang-format)
     ("HTML" html-tidy)
     ("JSON" prettier)
     ("YAML" prettier)))
  :config
  )
;; f2289888 ends here

;; [[file:../gwp-scratch.note::0deb729c][0deb729c]]
;; symbol-overlay
;;;  a highlight-symbol replacement.
(use-package symbol-overlay
  :requires transient
  :config
  ;; 等价设置; 备忘
  ;; (setq symbol-overlay-map (make-sparse-keymap))
  ;; (setq gwp::symbol-overlay-map (make-sparse-keymap))
  ;; (define-key gwp::symbol-overlay-map (kbd "h") 'symbol-overlay-put)
  ;; (define-key gwp::symbol-overlay-map (kbd "n") 'symbol-overlay-jump-next)
  ;; (define-key gwp::symbol-overlay-map (kbd "p") 'symbol-overlay-jump-prev)
  ;; (define-key gwp::symbol-overlay-map (kbd "w") 'symbol-overlay-save-symbol)
  ;; (define-key gwp::symbol-overlay-map (kbd "t") 'symbol-overlay-toggle-in-scope)
  ;; (define-key gwp::symbol-overlay-map (kbd "e") 'symbol-overlay-echo-mark)
  ;; (define-key gwp::symbol-overlay-map (kbd "d") 'symbol-overlay-jump-to-definition)
  ;; (define-key gwp::symbol-overlay-map (kbd "s") 'symbol-overlay-isearch-literally)
  ;; (define-key gwp::symbol-overlay-map (kbd "q") 'symbol-overlay-query-replace)
  ;; (define-key gwp::symbol-overlay-map (kbd "r") 'symbol-overlay-rename)
  ;; 以下命令仅在高亮区域外才用得上
  ;; (add-hook 'symbol-overlay-mode-hook #'org-mark-jump-unhide)
  (advice-add #'symbol-overlay-jump-next :after #'gwp::goto-line-unhide)
  (advice-add #'symbol-overlay-jump-prev :after #'gwp::goto-line-unhide)

  ;; 方便 hjkl 移动
  (unbind-key "h" symbol-overlay-map)
  (bind-key "?" #'symbol-overlay-map-help symbol-overlay-map)

  (transient-define-prefix gwp::symbol-overlay-transient ()
    "citre tags"
    ["View:"
     ("n" "next" symbol-overlay-switch-forward :transient t) ; 当在高亮的字符外时, 可快速返回.
     ("p" "previous" symbol-overlay-switch-backward :transient t)
     ("t" "toggle in scope" symbol-overlay-toggle-in-scope)
     ]
    ["Edit"
     ("h" "highlight" symbol-overlay-put) ; 原位时可用 i
     ("d" "remove all" symbol-overlay-remove-all)
     ("r" "rename" symbol-overlay-rename)
     ("R" "replace" symbol-overlay-query-replace)
     ]
    )
  :bind
  (:map gwp::develop-map
        ("h" . gwp::symbol-overlay-transient)))
;; 0deb729c ends here

;; [[file:../gwp-scratch.note::985a2495][985a2495]]
(gwp::local-leader-def
  :keymaps 'emacs-lisp-mode-map
  "e" #'eval-last-sexp
  "r" #'eval-region
  "b" #'eval-buffer
  "d" #'eval-defun
  )
;; 985a2495 ends here

;; [[file:../gwp-scratch.note::e2126f7c][e2126f7c]]
(unless init-no-x-flag (use-package ess))
;; e2126f7c ends here

;; [[file:../gwp-scratch.note::*jinja2][jinja2:1]]
(use-package jinja2-mode)
;; jinja2:1 ends here

;; [[file:../gwp-scratch.note::a9baf9f2][a9baf9f2]]
(setq python-indent-guess-indent-offset-verbose nil)

(use-package python
  :ensure nil
  :config
  (unbind-key "C-c C-f" python-mode-map)
  )
;; a9baf9f2 ends here

;; [[file:../gwp-scratch.note::f8651bde][f8651bde]]
(use-package citre
  :requires transient
  :commands (citre-jump citre-jump-back citre-peak citre-create-tags-file)
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  :config
  (transient-define-prefix gwp::citre-transient ()
    "citre tags"
    ["Jump:"
     ("j" "jump" citre-jump)
     ("b" "jump back" citre-jump-back)
     ("p" "peek" citre-peak)
     ]
    ["Edit"
     ("c" "create tags file" citre-create-tags-file)
     ("u" "update tags file" citre-update-tags-file)
     ]
    )
  :bind
  (:map gwp::develop-map
        ("j" . gwp::citre-transient)))
;; f8651bde ends here

;; [[file:../gwp-scratch.note::0ce7e90e][0ce7e90e]]
(use-package gptel
  :ensure t
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-temperature 0.6)                    ; 控制生成文本的随机性 (0.0-2.0)
  (gptel-log-level 'info)                    ; 调试日志级别
  (gptel-use-curl t)                         ; 使用 curl 而不是 url-retrieve
  :hook
  ;; 【新增】启用回复高亮模式，让 AI 回复内容左侧有视觉提示
  (gptel-post-response-functions . gptel-end-of-response)

  :bind
  (:map gwp::develop-map
        ("gr" . gptel-rewrite)
        ("g RET" . gptel-send)
        ("gm" . gptel-menu)
        ("gg" . gptel))
  :config
  ;; 【新增】全局开启高亮模式
  (gptel-highlight-mode 1)

  ;; 自定义安全读取 API Key 函数
  (defun my/gptel-read-api-key (file)
    "安全地从文件中读取 API key"
    (if (file-exists-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (string-trim (buffer-string)))
      (warn "API key 文件不存在: %s" file)
      nil))

  ;; 结构化后端配置
  ;; openai compatible models
  (setq gptel-backends
        (cl-loop for (name key-file . config) in
                 `(("GLM" "~/Install/configs/llms/glm-key.txt"  ; 建议将 API key 单独存放
                    :protocol "https" :host "open.bigmodel.cn"
                    :endpoint "/api/coding/paas/v4/chat/completions"  ; 注意完整的 endpoint
                    :models (glm-4.7))
                   ("SiliconFlow" "~/Install/configs/llms/siliconflow-key.txt"
                    :protocol "https" :host "api.siliconflow.cn"
                    :models (Pro/deepseek-ai/DeepSeek-V3 Pro/deepseek-ai/DeepSeek-R1))
                   ("Aliyun Qwen" "~/Install/configs/llms/qwen-key.txt"
                    :protocol "https" :host "dashscope.aliyuncs.com"
                    :endpoint "/compatible-mode/v1/chat/completions"
                    :models (qwen-max-latest deepseek-v3 deepseek-r1 qwq-plus))
                   ("DeepSeek" "~/Install/configs/llms/deepseek-key.txt"
                    :protocol "https" :host "api.deepseek.com"
                    :endpoint "/chat/completions"
                    :models (deepseek-chat deepseek-reasoner))
                   ("OpenRouter" "~/Install/configs/llms/openrouter-key.txt"
                    :host "openrouter.ai"
                    :endpoint "/api/v1/chat/completions"
                    :models (deepseek/deepseek-r1:free minimax/minimax-01 openai/o3-mini-high google/gemini-2.0-flash-001)))
                 when (or (null key-file) (file-exists-p key-file))
                 collect
                 (let ((key (when key-file (my/gptel-read-api-key key-file))))
                   (apply #'gptel-make-openai name
                          :stream t
                          :key key
                          (append config '(:endpoint "/v1/chat/completions"))))))

  ;; gemini models
  (let ((key (my/gptel-read-api-key "~/Install/configs/llms/google-key.txt")))
    (gptel-make-gemini "Gemini"
      :stream t
      :key key
      :models '(gemini-2.0-flash-thinking-exp-01-21 gemini-3-flash-preview)))

  ;; 设置默认后端（需在 backend 定义之后）
  (setq gptel-backend (gptel-get-backend "GLM")
        gptel-model 'glm-4.7)

  ;; 移除默认 ChatGPT
  (dolist (item gptel--known-backends)
    (if (string= (car item) "ChatGPT")
        (setq gptel--known-backends (cl-remove item gptel--known-backends))))

  (defun gwp::gptel-build-directives (promptdir)
    "从 PROMPTDIR 构建 (key . content) 形式的指令列表"
    (mapcar
     (lambda (file)
       (cons (intern (file-name-base file))  ; 文件名转为符号作为key
             (with-temp-buffer
               (insert-file-contents file)   ; 完整原始内容作为value
               (buffer-string))))
     (directory-files promptdir t "\\.md\\'"))) ; 获取所有.md扩展名文件

  (setq gptel-directives (gwp::gptel-build-directives "~/Install/configs/llms/prompts")))
;; 0ce7e90e ends here

;; [[file:../gwp-scratch.note::*gptel/markdown][gptel/markdown:1]]
(defun gwp/gptel-paste-markdown-as-org ()
  "从剪贴板(kill-ring)读取 Markdown 文本，转换后插入。
使用 `current-kill' 代替 `gui-get-selection' 以避免编码问题。"
  (interactive)
  (require 'gptel-org)
  ;; (current-kill 0) 会自动触发 interprogram-paste-function，
  ;; 将系统剪贴板的内容同步到 Emacs kill-ring 中并返回，
  ;; 这样拿到的就是解码正确的中文了。
  (let ((md-text (current-kill 0)))
    (if (and md-text (not (string-empty-p md-text)))
        (let ((org-text (gptel--convert-markdown->org md-text)))
          ;; 记录位置并插入
          (push-mark)
          (insert org-text)
          (message "已转换并粘贴"))
      (message "剪贴板为空！"))))


;; 也可以做一个针对 Region（选区）的转换版本
(defun gwp/gptel-convert-region-markdown-to-org (beg end)
  "将选区内的 Markdown 文本转换为 Org 格式。"
  (interactive "r")
  (require 'gptel-org)
  (let* ((md-text (buffer-substring-no-properties beg end))
         (org-text (gptel--convert-markdown->org md-text)))
    (delete-region beg end)
    (insert org-text)))
;; gptel/markdown:1 ends here

;; [[file:../gwp-scratch.note::*gptel-magit][gptel-magit:1]]
(use-package gptel-magit
  :ensure t
  :hook (magit-mode . gptel-magit-install))
;; gptel-magit:1 ends here

;; [[file:../gwp-scratch.note::d1b26252][d1b26252]]
(use-package claude-code
  :ensure t
  :after transient
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :config
  (setq claude-code-terminal-backend 'vterm)
  ;; (setq claude-code-term-name "xterm-256color")
  (setq claude-code-program-switches '("--verbose"))
  ;; (setq claude-code-enable-notifications t)
  ;; (setq claude-code-notification-function 'claude-code--default-notification)

  ;; 方法一: 通过 claude-code-proxy 的方式设置代理, 接入 OPENAI-compatible 模型
  ;; (progn
  ;;   (setenv "ANTHROPIC_BASE_URL" "http://localhost:8082")
  ;;   (setenv "ANTHROPIC_AUTH_TOKEN" "api-key")
  ;;   (setq claude-code-program "/usr/local/bin/claude")
  ;;   )

  ;; 方法二: 使用 kimi Anthropic-compatible API interface
  ;; kimi 提供了 Anthropic-compatible API 接口, 参考链接 https://platform.moonshot.ai/docs/guide/agent-support.en-US#install-cline
  ;; kimi 存在充值与限速问题, 参考链接 https://platform.moonshot.cn/docs/pricing/limits#%E9%99%90%E9%80%9F%E6%A6%82%E5%BF%B5%E8%A7%A3%E9%87%8A
  ;; 可以关注 LLM-Red-Team/kimi-cc 项目中的讨论, 例如 https://github.com/LLM-Red-Team/kimi-cc/issues/35
  ;; (progn
  ;; (setenv "ANTHROPIC_BASE_URL" "https://open.bigmodel.cn/api/anthropic")
  ;; (setenv "ANTHROPIC_AUTH_TOKEN" "api-key")
  ;; (setq claude-code-program "/usr/local/bin/claude")
  ;; )
  )
;; d1b26252 ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-develop)
;; provide:1 ends here
