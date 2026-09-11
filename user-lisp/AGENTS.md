# user-lisp — 模块地图

> 版本：V1.0 ｜ 更新：2026-09-10 ｜ 创建：2026-09-10
> 角色：模块导航与目录级维护知识 ｜ 给要修改本目录某个 `.el` 的 AI
> 父级：[../AGENTS.md](../AGENTS.md) — 项目边界、验证方法与提交约定以父级为准

## 职责三要素

- **做什么**：说清每个模块负责什么、谁加载谁、依赖哪些仓库外的东西，减少改错文件。
- **不做什么**：不重复父级的项目级规则；不把未确认的用途写成结论。
- **谁消费我**：准备修改 `user-lisp/` 下文件时，先读本文件定位模块，再读具体文件。

## 模块地图

| 文件 | 职责 | 主要内容 |
|---|---|---|
| `init-defaults.el` | 全局默认值 | `use-short-answers`、`truncate-lines`、`indent-tabs-mode`、kill 去重 |
| `init-core.el` | 基础设施 | crux、transient、recentf、bookmark、paren/smartparens、auto-revert；server 启动；straight bootstrap；归档刷新命令 `gwp::package-refresh-archives` |
| `init-general.el` | 按键基础设施 | general.el、leader 前缀常量与 `gwp::` 前缀 map |
| `init-meow.el` | 编辑状态机 | meow、多光标、剪贴板 |
| `init-edit.el` | 编辑增强 | avy、isearch、goto-chg、xah-replace-pairs |
| `init-ui.el` | 外观与窗口 | doom-modeline/themes、rime 输入法、hl-line/hl-todo、ace-window/burly/zoom/winner |
| `init-dired.el` | 文件管理 | dired/dired-x、dired-collapse、fd-dired |
| `init-workspace.el` | 工作区与终端 | tab-bar、bm、eshell 函数集、zoxide 懒加载 |
| `init-org.el` | Org 主干（最大） | org、org-modern/valign/org-appear/org-superstar、org-download、ob-mermaid、ox-typst/ox-odt、org-zotero、org-protocol |
| `init-note.el` | 笔记体系 | denote 系列、org-noter、denote-protocol |
| `init-develop.el` | 开发与 AI | magit 系、rust-mode/cargo、citre、gptel + gptel-magit、claude-code（`:straight`） |
| `init-completion.el` | 补全与检索 | vertico/consult/orderless、corfu/cape、embark、which-key、yasnippet、dabbrev（词典 `../english-words.txt`） |
| `init-chemistry.el` | 化学输入模式 | `(require 'cp2k-mode)`、`(require 'gjf-mode)` |
| `init-bindings.el` | Leader 键位总表 | 汇总各模块按键，**必须最后加载** |
| `init-eaf.el` | EAF（未启用） | `init.el` 中该 require 已注释，改它不会生效 |
| `rust-edit.el` | Rust 编辑支持 | 被 `init-develop.el` require |
| `zoxide.el` | 目录跳转 | `gwp::recent-dirs`，经 `use-package ... :commands` 懒加载 |
| `yadm.el` | dotfiles 访问 | 被 `init-develop.el` require，magit 集成 |
| `cp2k-mode.el` | CP2K 输入文件 major mode | **第三方代码**（Lianheng Tong, 2014），改动前先想清楚是否值得与上游分叉 |
| `gjf-mode.el` | Gaussian 输入文件 mode | 被 `init-chemistry.el` require |
| `orca.el` | ORCA 输入助手（transient） | **未被任何模块加载**，用途待确认 |
| `a.el` | gptel 试验草稿 | **未被任何模块加载**，用途待确认 |
| `zotero.so` | 二进制扩展（13M） | 与 `emacs-zotero.el` 相关，未被 `.el` 直接引用，用途待确认 |
| `.env` | `GSYNC_REMOTE`（旧远程同步目标） | 无 `.el` 读取；远程服务器已基本不用 |

外部符号链接（仓库里只有链接，没有内容）：`denote-protocol.el`、`emacs-zotero.el`、`org-attach-extra.el`、`org-note-search.el` → 指向 `~/Workspace/Notes/resources/...`；`../site-lisp/org-zotero` 同理。

## 加载关系

```
init.el
 ├─ init-develop ──(require)──► yadm.el, rust-edit.el
 ├─ init-chemistry ─(require)──► cp2k-mode.el, gjf-mode.el
 ├─ init-workspace ─(懒加载)───► zoxide.el          （触发 gwp::recent-dirs 时）
 └─ init-bindings   （最后，汇总按键）
```

其他模块彼此不 `require`；共享状态通过 `gwp::*` 前缀的变量与 keymap 传递。

## 局部隐性知识

### 四个符号链接指向仓库外

- **现象**：这几个文件在 git 里没有内容，`git status` 里是 `??`。
- **真相**：它们是指向 `~/Workspace/Notes/resources/...` 的符号链接，实际开发在那边。
- **错误后果**：整理或移动 Notes 目录会静默破坏这些功能；要改它们的实现得去源仓库。

### 文件头的 tangle 注释是历史痕迹

- **现象**：多数文件开头有 `;; [[file:../gwp-scratch.note::xxxxxxxx][xxxxxxxx]]`。
- **真相**：旧 literate 流程留下的标记，与当前真源无关。
- **错误后果**：误以为要回 note 里改。碰到时顺手删除即可，不必批量清理。

### 按键分三层，`init-bindings` 必须最后

- **现象**：改了某键不生效，或报 keymap 未定义。
- **真相**：meow 提供编辑状态；general.el 建立 `gwp::` leader 与前缀 map；各模块往 map 里挂键（`init-develop`、`init-note`、`init-org`）；`init-bindings` 统一注册 leader。
- **错误后果**：在错误的层找键位；或调整 `init.el` 顺序导致 leader 缺失。

### `:ensure nil` 是本地模块的标记

- **现象**：某个 `use-package` 声明查不到对应 MELPA 包。
- **真相**：`:ensure nil` + `:commands`/`:bind` 表示加载本目录的同名 `.el` 文件（如 `zoxide`）。
- **错误后果**：误以为是缺失依赖而去装包或删声明。

### 第三方文件不要顺手改

- **现象**：`cp2k-mode.el` 写法和本项目其他文件不同。
- **真相**：它是 2014 年的第三方 major mode，被原样收录。
- **错误后果**：改动会与上游分叉，未来升级需重新移植。
