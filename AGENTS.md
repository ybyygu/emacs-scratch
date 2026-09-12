# 个人 Emacs 配置 — 仓库宪法

> 版本：V2.2 ｜ 更新：2026-09-12 ｜ 创建：2026-09-10
> 角色：本仓库的宪法与 AI 协作入口——管的是活动那份配置代码（开发轨产生改动、日用轨接收已接受的快照）｜ 给接手这份配置的 AI，以及未来的自己
> 关联：**区域级**框架（动机、目标、原则、目标架构）在 [../docs/framework.md](../docs/framework.md)；区域入口、纪律与导航在 [../AGENTS.md](../AGENTS.md)——pi 会自动加载父目录那份，本文件不重复 ｜ [../docs/registry.md](../docs/registry.md)（现状：版本、二进制、socket）｜ [user-lisp/AGENTS.md](user-lisp/AGENTS.md) ｜ [docs/learnings.md](docs/learnings.md) ｜ [PORT-EMACS31.md](PORT-EMACS31.md)

## 职责三要素

- **做什么**：维护这份日常在用的 Emacs 配置（两个载体：`emacs-daily` 默认轨、`emacs-dev` 开发轨），验收底线是"正在使用的功能不退化"。
- **不做什么**：不动冻结的保底轨、不在 `emacs-daily` 上做开发、不重构、不清点或删除用途未确认的文件、不统一包管理器、不引入没人要的工具链与流程。
- **谁消费我**：接手这份配置的 AI（先 [../AGENTS.md](../AGENTS.md) → 本文件 → [user-lisp/AGENTS.md](user-lisp/AGENTS.md)）；人工协作者按需查阅。

## 协作方式

ybyygu 提供需求、使用体验与方向取舍；AI 负责读代码、做最小修改、按影响面验证、维护本目录的代码与文档。日常维护成本由 AI 承担，不要求先补齐旧配置的说明。

## 真源与所有权

| 对象 | 状态 |
|---|---|
| `early-init.el`、`init.el`、`user-lisp/*.el`、`snippets/` | **唯一真源**，可直接修改（`early-init.el` 定死状态与缓存落点，`init.el` 是装配入口，实现在 `user-lisp/`） |
| 本仓库的两个 worktree | `emacs-dev/`（`dev`，**改动只在这里**）、`emacs-daily/`（`daily`，日用快照，只接受 fast-forward） |
| 保底轨目录 `gwp-scratch/` | **独立仓库**（自带 `.git`，`master` 冻结）：不属于本仓库，不接收本仓库的 merge |
| `~/.config/emacs` | 指向 `emacs-daily` 的**符号链接**：门牌，不是真源；在里面手改等于改 daily |
| `elpa/`、`straight/`、`eln-cache/`、运行态 | **已不在仓库里**：包树与缓存落 `~/.cache/emacs[-dev]/`，运行态落 `~/.local/state/emacs[-dev]/`（由 `early-init.el` 定死，路径表见 [../AGENTS.md](../AGENTS.md)） |
| `gwp-scratch.note`、`gwp-scratch.note_archive` | **不属于本项目**。旧的 literate 源码，已停用：不读、不写、不比对、不回收 |
| 保底轨目录里的 `pkg/`、`a.el`、`orca.el`、`start-eaf.sh`、`run.sh`、`test.json`、`ai-20250217.note`、`data/`、`ltximg/` | 保底轨的遗留物：原地保留，本仓库不处理 |

## 载体与加载拓扑

区域级地图（三条轨、socket、发布与回退）在 [../AGENTS.md](../AGENTS.md)。本文件只记与代码有关的部分；**版本号（系统 Emacs、解包树、socket 名）是会浮动的现状，登记在 [../docs/registry.md](../docs/registry.md)，本文件不写**。

- 本仓库的两处载体是**同一分支的不同提交**：`emacs-dev/`（`dev`，演化）与 `emacs-daily/`（`daily`，发布快照）；`~/.config/emacs` 只是后者的符号链接。**施工窗口未关闭**：`emacs-daily/` 与那道门牌是目标形态，现状见 [../docs/registry.md](../docs/registry.md)，进度见 [PORT-EMACS31.md](PORT-EMACS31.md)。
- 保底轨跑的是旁边那个**独立仓库**（`gwp-scratch/`，自带 `.git`）里的**冻结老配置**（没有 `early-init.el`），不在本文件范围内；要动它先读 [../gwp-scratch/AGENTS.md](../gwp-scratch/AGENTS.md)。
- 加载链（两处载体共用这一份代码）：

```
init.el
  ├─ load-path：site-lisp/ 及其子目录、user-lisp/
  ├─ package.el（USTC 镜像）→ custom.el（落在各轨的 state 目录）
  ├─ init-defaults → init-core → init-general → init-meow → init-edit
  │    → init-ui → init-dired → init-workspace
  ├─ 有图形界面时：init-org → init-note        （init-eaf 已注释）
  └─ init-develop → init-completion → init-chemistry → init-bindings（必须最后）
```

`early-init.el` 比 `init.el` 更早：定死 `package-user-dir`／`straight-base-dir`／eln 目录表／各运行态文件／`server-name`，并关掉 `user-lisp-auto-scrape`（新版 Emacs 新增的 user-lisp 自动处理：否则 `.elc` 与 autoloads 会写进源码目录、还会打乱加载顺序，见 [PORT-EMACS31.md](PORT-EMACS31.md) §3.7）。`init-no-x-flag` 为 t（无 X / 远程终端）时，跳过 `init-org`、`init-note`，且不自动安装包。

## snippets 约定

`snippets/<mode>/` 是 yasnippet 目录，正本入库；`.yas-compiled-snippets.el` 是编译缓存（内含机器绝对路径），已在 `.gitignore` 中忽略。**它不会自动重建**，且只要存在就被无条件加载、不比对 mtime——改完 snippet 必须删掉它或 `M-x yas-recompile-all`，否则改动永不生效；已运行的实例还需 `M-x yas-reload-all` 才会重读目录。**各目录当前都不留缓存**（2026-09-11 起；要加载速度再 `M-x yas-recompile-all`，但要记得改完再删）。

- `markdown-mode/` 是日常写作主力：`code` + 19 个 `prompt-*` 提示词模板；2018 年那批 markdown 语法 snippet（`+`/`h1`/`img`/`link`/`ordered-list` 等 29 个）因不用已于 2026-09-11 删除。
- `prompt-*` 全部用 `key: utf8` 触发（输入 `utf8` 后由补全列表按 `# name:` 选择），文件内容是提示词正文。
- 正文里的字面反引号要转义成 ``\` ``（美元符同理 ``\$``）：yasnippet 把 `` `…` `` 当 elisp 求值、`${…}` 当字段，未转义时展开会把散文静默换成错误串。
- 两类容易误判的情况：`$0`/`$1` 在少数提示词里是**有意**的光标落点（插入后光标停在 `<ybyygu>` 之类的位置），别当残留修掉；`yas-indent-line` 默认会按 markdown 规则重排展开文本的缩进（嵌套子项变平级），所以正文带嵌套缩进的提示词都加了 `# expand-env: ((yas-indent-line 'none))` 逐字保留（当前：`prompt-git-commit`、`prompt-grill-me`）。校验脚本对这一类应报 0 个 △，出现 △ 就是新问题。
- 新增提示词：在 `snippets/markdown-mode/` 下照现有格式加文件（`# name:` 写清用途）即可，不需要改配置。

## 维护约定

- **发布纪律**：改动只在 `emacs-dev/`；发布 = `git -C ../emacs-daily merge --ff-only <已验证提交>` + 重启 `gwp` + 验收；紧急 hotfix 可以落在 daily，但必须尽快 `cherry-pick` 回 dev。区域级细则见 [../AGENTS.md](../AGENTS.md)。
- **一次一个具体问题**：改动范围由这个问题决定，不顺手扩展。
- **包管理**：默认用 `:ensure`（package.el + USTC 镜像，**只此一份**，https）；只有需要 GitHub 直装时才用 `:straight`（当前仅 3 处）。新增依赖不要开辟第三条路。**启动不联网**：归档刷新只在 `M-x gwp::package-refresh-archives`（`user-lisp/init-core.el`）里发生。新机器、或新增 `:ensure` 包后报 “unavailable”，先跑这条命令再重启；`~/.cache/emacs/elpa/archives/*/archive-contents` 的 mtime 就是归档新鲜度。
- **提交**：中文 commit，写清"为什么改、对使用有什么影响"；GitHub `ybyygu/emacs-scratch` 是 private 备份仓库，不 rebase / force-push 已推送历史。
- **文档回环**：装配结构、模块职责、维护约定变化时，同步本文件与 [user-lisp/AGENTS.md](user-lisp/AGENTS.md)；**区域级**目标或原则变化先改 [../docs/framework.md](../docs/framework.md)，载体、路径、socket 等现状值改 [../docs/registry.md](../docs/registry.md)，纪律与导航改 [../AGENTS.md](../AGENTS.md)；普通修复不更新文档。
- **经验落点**：先判范围——**代码级**（Emacs 行为、snippet、包）写 [docs/learnings.md](docs/learnings.md)，**区域级**（载体、所有权、发布、同步区）写 [../docs/learnings.md](../docs/learnings.md)；两份正本各自独立，本文件的「隐性知识」节只同步代码级摘要（条目编号与代码级正本一致）；`user-lisp/` 的目录级陷阱留在它自己的蓝图里。
- **验证**：见下节。

## 验证

| 级别 | 做法 | 覆盖 |
|---|---|---|
| 编译 | `emacs -Q --batch` 把改动文件 `batch-byte-compile` 到 `/tmp` | 语法与编译告警 |
| snippet | 隔离副本删掉 `.yas-compiled-snippets.el` → batch 展开 → 与正文逐字比对（清单 F 段已自动化） | 展开静默损坏、反引号/字段被求值 |
| 体验清单 | `~/Incoming/checklist.sh`（隔离实例 + socket `gwp-check`，54 条二值 + 5 观察，顺带列启动日志错误） | 装配、落点、snippet、rime 谓词、gptel、magit、vterm 模块 |
| 运行实例 | `~/Incoming/accept.sh gwp gwp`（15 项落点 + socket 名 + 版本 + 日志零错误 + 配置树零新增） | 日用实况 |
| 手点 | 中文输入、`M-x vterm`、org 附件左窗、snippet 补全、界面一眼 | batch 测不到的交互 |

## 隐性知识

> 速查索引：**正本在 [docs/learnings.md](docs/learnings.md)**（条目编号与正本一致）；新经验先改正本，此处只同步摘要。

- **batch 环境测不到 org / note**：无 X 时 `init-org`/`init-note` 整块被跳过，"没反应"不等于"没问题" —— learnings #1
- **batch 加载会启动 server**：会与日用 daemon 抢 socket，测试要另指定 `server-name` —— learnings #2
- **snippet 改了或新增了却不出现**：`.yas-compiled-snippets.el` 缓存被无条件加载、不比对 mtime —— learnings #3
- **改动只在重启后生效**：daemon 里跑的是旧版本代码 —— learnings #4
- **输入法行为分三层，改之前先定位**：RIME 数据目录 / emacs-rime 的 predicate / 跑的是哪份配置 —— learnings #5
- **升级包之后 vterm / rime 打不开**：原生模块不在包内容里，升级后要在**新包目录**里重编 —— learnings #6
- **不要在源码目录留 `.elc`**：同名 `.elc` 会被优先加载，遮蔽真源（`user-lisp-auto-scrape` 开着时会主动往里写，本配置已关） —— learnings #7
- **仓库位于同步区内**：`~/Install` 由 syncthing 共享，运行态入库会引发跨机冲突（现已由 `early-init.el` 移出仓库） —— learnings #8
- **密钥与外部数据在仓库外**：密钥在 `~/Install/configs/llms/*.txt`，别写进 `.el` —— learnings #9
- **state 目录里的 custom.el 可能缺 cookie**：它在按机器的 state 目录里，每台机器各犯一次 —— learnings #10
- **配置路径一改，native 缓存就失效**：`.eln` 文件名含源文件路径哈希，搬迁后要受控预热、等编译池排空再退 —— learnings #11

## 索引

| 文档 | 职责 | AI 何时读 |
|---|---|---|
| `AGENTS.md`（本文件） | 仓库宪法：边界、载体与加载链、约定、验证、隐性知识速查 | 进入仓库即读 |
| [../docs/framework.md](../docs/framework.md) | **区域框架**：动机、目标、设计原则、目标架构、成功标准、边界 | 做取舍、判定"这事能不能做"时 |
| [../AGENTS.md](../AGENTS.md) | **区域入口**：载体角色、可执行纪律、导航、遗留物声明 | 涉及发布、回退、搬迁、其它轨时 |
| [../docs/registry.md](../docs/registry.md) | **现状登记处**：版本、二进制、socket、启动器与桌面项 | 换版本、改入口、核对"现在跑的是哪个" |
| [user-lisp/AGENTS.md](user-lisp/AGENTS.md) | 模块地图、加载依赖、目录级局部陷阱 | 改 `user-lisp/` 下模块前 |
| [docs/learnings.md](docs/learnings.md) | **代码级经验库正本**：Emacs 行为、batch 盲区、snippet 缓存、原生模块… | 排查同类问题前、新经验写入时 |
| [../docs/learnings.md](../docs/learnings.md) | **区域级经验库正本**：所有权、门牌、发布方向、同步区 | 做发布/回退/搬迁前后 |
| [PORT-EMACS31.md](PORT-EMACS31.md) | **过程档案**：本区域的版本迁移与架构变更（决议、证据、施工、回退） | 动结构前；回查"当时为什么这么定" |
