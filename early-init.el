;;; early-init.el --- 配置树只放代码：状态与缓存落点 -*- lexical-binding: t; -*-
;;
;; 加载时机：Emacs 启动的最早阶段（早于 init.el，也早于 site-start.el）。
;; 凡是“会写盘的位置”必须在这里定死；否则它们默认算在 user-emacs-directory 里，
;; 也就是配置仓库本身 —— 而仓库随 syncthing 跨机，运行态会把冲突带过去。
;;
;; 三处落点（详见 PORT-EMACS31.md §五）：
;;   ~/.config/emacs/        代码（git 管，跨机靠它）
;;   ~/.local/state/emacs/   运行态（每台机器一份：最近文件/书签/历史/custom…）
;;   ~/.cache/emacs/         可重建的缓存与包树（elpa/ straight/ eln-cache/）
;;
;; 演化轨与测试靠环境变量改道；启动器只设这些变量，不复制这份定义：
;;   GWP_STATE_DIR  GWP_CACHE_DIR  GWP_SERVER_NAME
;;
;; 代码中的其它地方不要再写这些路径 —— 单一来源，改地址只改这里。

(defvar gwp-state-dir
  (file-name-as-directory
   (expand-file-name (or (getenv "GWP_STATE_DIR") "~/.local/state/emacs/")))
  "运行态目录：每台机器一份，不进版本控制。")

(defvar gwp-cache-dir
  (file-name-as-directory
   (expand-file-name (or (getenv "GWP_CACHE_DIR") "~/.cache/emacs/")))
  "包树与缓存目录：可重建，不进版本控制。")

(mapc (lambda (dir) (make-directory dir t))
      (list gwp-state-dir gwp-cache-dir))

;;; 包树与编译产物（可重建）
(setq package-user-dir (expand-file-name "elpa/" gwp-cache-dir))
;; straight 自己会拼上 straight/ 子目录；init-core.el 的引导路径也读这个变量
(setq straight-base-dir gwp-cache-dir)
;; 系统自带的 eln 目录（/usr/lib/emacs/*/native-lisp/）必须留着，否则连 Emacs 自己的 lisp 都要重编
(setq native-comp-eln-load-path
      (cons (expand-file-name "eln-cache/" gwp-cache-dir)
            (cdr native-comp-eln-load-path)))

;;; 运行态
(setq custom-file                 (expand-file-name "custom.el" gwp-state-dir)
      recentf-save-file           (expand-file-name "recentf" gwp-state-dir)
      savehist-file               (expand-file-name "history" gwp-state-dir)
      bookmark-default-file       (expand-file-name "bookmarks" gwp-state-dir)
      bm-repository-file          (expand-file-name "bm-repository" gwp-state-dir)
      tramp-persistency-file-name (expand-file-name "tramp" gwp-state-dir)
      org-id-locations-file       (expand-file-name ".org-id-locations" gwp-state-dir)
      project-list-file           (expand-file-name "projects" gwp-state-dir)
      transient-history-file      (expand-file-name "transient/history.el" gwp-state-dir)
      eshell-directory-name       (expand-file-name "eshell/" gwp-state-dir)
      ;; 下面两项现在没人用，但默认落点在仓库里：留着位置，免得哪天被谁写进来
      url-configuration-directory (expand-file-name "url/" gwp-state-dir)
      auto-save-list-file-prefix  (expand-file-name "auto-save-list/.saves-" gwp-state-dir))

;;; 启动期行为
;; Emacs 31 新增 User Lisp Directory 特性：配置目录下的 user-lisp/ 会被递归
;; byte-compile（.elc 直接写进源码目录）、扫 autoload、并把子目录加进 load-path。
;; 这份配置的 user-lisp/ 恰好同名，但它自己管理加载（init.el 加 load-path +
;; 按固定顺序 require）——不由 Emacs 代管。开着会有三个代价：机器本地的 .elc
;; 落进 git 树里（还会遮蔽真源，见 AGENTS.md）、启动时为了解析
;; transient-define-prefix 之类的 cookie 而去 load 模块（触发 Recursive load 告警
;; 并打乱加载顺序）、以及多出一份机器本地的 autoloads 文件。
;; 关闭后 30.2/31.1 行为一致；这个变量必须在这里设（init.el 之前生效）。
(setq user-lisp-auto-scrape nil)

;;; socket 名
;; 日用实例必须叫 gwp：~/.local/share/applications/gwp-emacsclient.desktop 按名字找它。
;; 演化轨用 GWP_SERVER_NAME=gwp-dev 另起一个，两个实例并存互不抢。
;; server-socket-dir 不动 —— emacsclient 与 Emacs 必须走同一套默认推导（/run/user/*/emacs），
;; 单方面改这里会让 emacsclient 找不到 socket。
(setq server-name (or (getenv "GWP_SERVER_NAME") "gwp"))

(provide 'early-init)
;;; early-init.el ends here
