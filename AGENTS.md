# gwp-scratch — 个人 Emacs 配置

> 版本：V1.0 ｜ 更新：2026-09-10 ｜ 创建：2026-09-10
> 角色：项目宪法与 AI 协作入口 ｜ 给接手本项目的 AI，以及未来的自己
> 关联：[user-lisp/AGENTS.md](user-lisp/AGENTS.md) — 模块地图与局部维护知识

## 职责三要素

- **做什么**：维护一份日常在用的 Emacs 配置（chemacs2 profile `gwp`），验收底线是"正在使用的功能不退化"。
- **不做什么**：不重构、不清点或删除用途未确认的文件、不统一包管理器、不引入没人要的工具链与流程。
- **谁消费我**：接手本项目的 AI（先本文件 → 再 [user-lisp/AGENTS.md](user-lisp/AGENTS.md)）；人工协作者按需查阅。

## 协作方式

ybyygu 提供需求、使用体验与方向取舍；AI 负责读代码、做最小修改、按影响面验证、维护本目录的代码与文档。日常维护成本由 AI 承担，不要求先补齐旧配置的说明。

## 真源与所有权

| 对象 | 状态 |
|---|---|
| `init.el`、`user-lisp/*.el`、`snippets/` | **唯一真源**，可直接修改（`init.el` 是装配入口，实现在 `user-lisp/`） |
| `gwp-scratch.note`、`gwp-scratch.note_archive` | **不属于本项目**。旧的 literate 源码，已停用：不读、不写、不比对、不回收 |
| `elpa/`、`straight/`、`eln-cache/`、`site-lisp/` | 第三方与构建产物：**不手改**；`elpa`/`straight` 可重建，`eln-cache` 可删 |
| `pkg/`、`a.el`、`orca.el`、`start-eaf.sh`、`run.sh`、`test.json`、`ai-20250217.note`、`data/` | 遗留物：**原地保留**，用途逐个确认前不动 |
| `custom.el` | Emacs Custom UI 自动写入的机器本地设置：不当真源，不手改，未纳管 |

## 加载拓扑

```
~/.local/bin/emacs               # 入口 wrapper，EMACS_PROFILE 默认 gwp（yadm 管理）
  └─ chemacs2 → ~/.emacs-profiles.el
       profile gwp → 本目录为 user-emacs-directory，server socket = gwp
         └─ init.el
              ├─ load-path：site-lisp/ 及其子目录、user-lisp/
              ├─ package.el（USTC 镜像）→ custom.el
              ├─ init-defaults → init-core → init-general → init-meow → init-edit
              │    → init-ui → init-dired → init-workspace
              ├─ 有图形界面时：init-org → init-note        （init-eaf 已注释）
              └─ init-develop → init-completion → init-chemistry → init-bindings（必须最后）
```

`init-no-x-flag` 为 t（无 X / 远程终端）时，跳过 `init-org`、`init-note`，且不自动安装包。

## snippets 约定

`snippets/<mode>/` 是 yasnippet 目录，正本入库；`.yas-compiled-snippets.el` 是编译缓存（内含机器绝对路径），已在 `.gitignore` 中忽略。**它不会自动重建**，且只要存在就被无条件加载、不比对 mtime——改完 snippet 必须删掉它或 `M-x yas-recompile-all`，否则改动永不生效；已运行的实例还需 `M-x yas-reload-all` 才会重读目录。**各目录当前都不留缓存**（2026-09-11 起；要加载速度再 `M-x yas-recompile-all`，但要记得改完再删）。

- `markdown-mode/` 是日常写作主力：`code` + 19 个 `prompt-*` 提示词模板；2018 年那批 markdown 语法 snippet（`+`/`h1`/`img`/`link`/`ordered-list` 等 29 个）因不用已于 2026-09-11 删除。
- `prompt-*` 全部用 `key: utf8` 触发（输入 `utf8` 后由补全列表按 `# name:` 选择），文件内容是提示词正文。
- 正文里的字面反引号要转义成 ``\` ``（美元符同理 ``\$``）：yasnippet 把 `` `…` `` 当 elisp 求值、`${…}` 当字段，未转义时展开会把散文静默换成错误串。
- 两类容易误判的情况：`$0`/`$1` 在少数提示词里是**有意**的光标落点（插入后光标停在 `<ybyygu>` 之类的位置），别当残留修掉；`yas-indent-line` 默认会按 markdown 规则重排展开文本的缩进（嵌套子项变平级），所以正文带嵌套缩进的提示词都加了 `# expand-env: ((yas-indent-line 'none))` 逐字保留（当前：`prompt-git-commit`、`prompt-grill-me`）。校验脚本对这一类应报 0 个 △，出现 △ 就是新问题。
- 新增提示词：在 `snippets/markdown-mode/` 下照现有格式加文件（`# name:` 写清用途）即可，不需要改配置。

## 维护约定

- **一次一个具体问题**：改动范围由这个问题决定，不顺手扩展。
- **包管理**：默认用 `:ensure`（package.el + USTC 镜像，**只此一份**，https）；只有需要 GitHub 直装时才用 `:straight`（当前仅 3 处）。新增依赖不要开辟第三条路。**启动不联网**：归档刷新只在 `M-x gwp::package-refresh-archives`（`user-lisp/init-core.el`）里发生。新机器、或新增 `:ensure` 包后报 “unavailable”，先跑这条命令再重启；`elpa/archives/*/archive-contents` 的 mtime 就是归档新鲜度。
- **提交**：中文 commit，写清"为什么改、对使用有什么影响"；GitHub `ybyygu/emacs-scratch` 是 private 备份仓库，不 rebase / force-push 已推送历史。
- **文档回环**：装配结构、模块职责、维护约定变化时，同步本文件与 [user-lisp/AGENTS.md](user-lisp/AGENTS.md)；普通修复不更新文档。
- **验证**：见下节。启动基线尚未采集，首次改动前先记录现状。

## 验证

| 级别 | 做法 | 覆盖 |
|---|---|---|
| 编译 | `emacs -Q --batch` 把改动文件 `batch-byte-compile` 到 `/tmp` | 语法与编译告警 |
| snippet | 在隔离副本里删掉 `.yas-compiled-snippets.el`，`emacs -Q --batch` 加载目录并 `yas-expand-snippet`，与正文逐字比对 | 展开静默损坏、反引号/字段被求值 |
| 装配 | batch 加载 profile `gwp`（注意会触发 `server-start`，见隐性知识） | require 链是否断裂 |
| 目视 | 独立 socket 起一个实例确认，通过后再重启日用 daemon | 交互行为、有 X 的分支 |

## 隐性知识

### batch 环境测不到 org / note

- **现象**：batch 下加载 `init.el` 一切正常，但改动的 org 配置毫无反应。
- **真相**：无 X 时 `init-no-x-flag` 为 t，`init-org`、`init-note` 整块被跳过。
- **错误后果**：把"没测到"当成"没问题"。

### batch 加载会启动 server

- **现象**：`emacs --batch` 加载 `init.el` 时打印 "Starting a server..."。
- **真相**：`init-core.el` 末尾有 `(unless (server-running-p) (server-start))`。
- **错误后果**：与日用 daemon 抢 socket；测试需另指定 `server-name`。

### snippet 改了或新增了却不出现

- **现象**：编辑或新增 `snippets/**` 后，`C-.` 的候选列表里还是旧内容，新文件完全不出现。
- **真相**：目录下若有 `.yas-compiled-snippets.el`，yasnippet 无条件 `load` 它、不比对 mtime；缓存只由 `M-x yas-recompile-all` 生成，不会自动重建；已运行的实例还要 `M-x yas-reload-all` 才会重读目录。
- **错误后果**：把“缓存陈旧”误判为格式写错、键位不对，或以为编辑没保存。

### 改动只在重启后生效

- **现象**：改完 `.el`，日用 Emacs 里没有变化。
- **真相**：daemon 已加载旧版本代码。
- **错误后果**：重复修改或误判失败。

### 输入法行为分三层，改之前先定位

- **现象**：改了“输入法”的设置没效果，或不知道该改哪里。
- **真相**：① RIME 数据目录 `~/.local/share/fcitx5/rime/`（**在仓库外**，fcitx5 与 emacs-rime 共用，改完要重新部署）；② emacs-rime 的 predicate 与按键（`user-lisp/init-ui.el`，决定“何时自动切英文”）；③ 跑的是哪份配置：`~/.emacs-profiles.el` 的 profile + `gwp` socket daemon（当前由 `~/.config/autostart/emacs.desktop` → systemd `app-emacs@autostart.service` 拉起）。该 RIME 方案的 `ascii_mode` 只有“中文”一个状态，所以 Emacs 里感受到的“自动中英文切换”实际由 ② 决定。
- **错误后果**：去 RIME 侧改 `switch_key` 想解决 Emacs 里的行为；或改完 `.el` 忘了重启 daemon，以为没生效。

### 不要在源码目录留 `.elc`

- **现象**：编辑 `.el` 后行为仍是旧的。
- **真相**：同名 `.elc` 会被优先加载（当前仓库内没有任何 `.elc`）。
- **错误后果**：隐式状态遮蔽真源；编译验证请输出到 `/tmp`。

### 仓库位于同步区内

- **现象**：仓库根出现 `*.sync-conflict-*` 文件（最近一次 2026-01-27）。
- **真相**：`~/Install` 由 syncthing 共享给 3 台设备，`elpa/`、`eln-cache/`、`history`、`recentf` 等运行态也在其中。
- **错误后果**：把动态产物写进仓库会引发跨机冲突；清理冲突文件前先确认差异。

### 密钥与外部数据在仓库外

- **现象**：gptel 等配置可用，但仓库里搜不到 key。
- **真相**：密钥统一放在 `~/Install/configs/llms/*.txt`；仓库根的 `english-words.txt`（370,105 词，已入库）被 `init-completion.el` 当作 ispell 备用词典引用。
- **错误后果**：误删根目录数据文件会破坏补全；新增密钥不要写进 `.el`。

## 索引

| 文档 | 职责 |
|---|---|
| `AGENTS.md`（本文件） | 项目宪法：边界、加载拓扑、约定、验证、隐性知识 |
| [user-lisp/AGENTS.md](user-lisp/AGENTS.md) | 模块地图、加载依赖、目录级隐性知识 |
