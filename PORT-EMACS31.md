# Emacs 31 移植 + 标准路径迁移 · 交接文档

> 版本：V1.2 ｜ 更新：2026-09-12 ｜ 创建：2026-09-11 ｜ 状态：**待办 1、2 完成并验收；环境按轨隔离已落地；下一步是体验清单（第 3 项）**
> 角色：本轮迁移的唯一入口。新会话先读本文件与 `AGENTS.md`，**不要重推已有结论**。
> 上游：`AGENTS.md`（项目宪法）、`user-lisp/AGENTS.md`（模块地图）

## 一、目标（一句话）

让这份配置在 **Emacs 31** 下与今天**体验一致或更好**，并把配置搬到标准路径 **`~/.config/emacs`**，退役 chemacs。

## 二、已定决议（不再讨论）

| # | 决议 | 依据 |
|---|---|---|
| 1 | 目标是"31 下可用"，不是"把包都升了" | 用户原话：`emacs 31 下我的配置可正常用` |
| 2 | 落点 `~/.config/emacs`，实体目录，就是 git 仓库本身 | XDG 标准；`/usr/bin/emacs` 自动认；用户确认"看着挺舒服" |
| 3 | chemacs 退役：删 `~/.emacs.d`（stub）、`~/.emacs-profiles.el`、`~/.local/bin/emacs` wrapper | doom 已弃用，chemacs 当初只为它存在；它只切 `user-emacs-directory`，管不了包树/二进制/状态 |
| 4 | socket 名保持 `gwp`；**不改 `server-socket-dir`** | `~/.local/share/applications/gwp-emacsclient.desktop` 按名字找 Emacs；改 socket 目录会让 `emacsclient` 失联 |
| 5 | 状态按机器（XDG），配置按 git | 见第五节映射表；配置审计第 1 条（状态出同步区）的落实 |
| 6 | 双轨：`~/Incoming/emacs-dev` 施工 → 验证 → 提升；用**独立克隆**而非 worktree | worktree 会把 dev 书签写进同步区 `.git/worktrees/`，跨机变坏引用 |
| 7 | 31 先用**解包的 31.1** 跑施工面，系统包留到最后一步 | 零系统风险；回退弹药在 pacman 缓存 |
| 8 | 保持 30.2 兼容 | 其他机器 + 回退面都需要 |
| 9 | 演化轨用 Emacs 自带的 `--init-directory` 起 | 标准机制；`-Q` 会关掉 `init-file-user`，反而测不到真实启动路径（31 的 user-lisp 自动处理只在真实启动下发生，见 3.7） |
| 10 | **环境按轨隔离**：演化轨用 `~/.cache/emacs-dev/` + `~/.local/state/emacs-dev/`；`early-init.el` 的默认值仍是生产那套 XDG 路径 | 待办 5 要给 31 重编 rime/vterm 的 `.so`，那些文件就写在包目录里——两轨共用一棵树，等于"给 31 重编"顺手弄坏日用 30.2。隔离后两轨只在"提升"那一刻交割快照 |
| 11 | 文档提交走稳定库 `master`；实现提交走 `dev`；`master` 一前进就 `dev rebase`；**`dev` 在提升前不推远端** | 这样 `merge --ff-only` 永远成立（实测：cherry-pick 文档进 dev 不能让 master 成为祖先，见 3.10） |

## 三、已取得的证据（不要重做）

### 3.1 2026-08-28 那次"升级事故"的真实经过

`/var/log/pacman.log`：

```
13:11  upgraded    emacs-wayland (30.2-3 -> 31.1-1)
15:31  pacman -U   https://archive.archlinux.org/.../emacs-30.2-3...   ← 包名错，无用
15:34  downgraded  emacs-wayland (31.1-1 -> 30.2-3)
```

跑了 **2 小时 20 分**就退回；`eln-cache/31.1-8806c27d`（160 个 `.eln`）就是那次留下的，**不是别的机器**。

### 3.2 31.1 沙箱实测（2026-09-11，结论：源码层已兼容）

同一份配置、同一个隔离包树，30.2 与 31.1 对照：**14 个模块全部加载成功**，12 项能力探针逐项一致；31 只多一条 `Missing 'lexical-binding' cookie` 警告（已在待办 1 修掉）；`point-at-bol`/`point-at-eol` 在 31.1 里仍存在。

### 3.3 包树现状（自洽但有化石）

- `elpa/` 顶层 231 个条目 = **205 个包目录** + 26 个 `.signed` 签名文件；53 个包有 2–3 个版本；
- **0 个**包要求 `emacs > 30.2` → 当前状态自洽，可安全作为基线；
- 化石来自两代：2025-04 与 2026-01/02。

### 3.4 动态模块在 31.1 下的实测（2026-09-12，比 V1.1 的说法更确切）

| 模块 | 证据 | 结论 |
|---|---|---|
| `rime-20251105.1505/librime-emacs.so`（2026-01 编译） | 30.2 与 31.1 下均 `(require 'rime)` → `(load-file rime--module-path)` → `(rime-lib-version)` 返回 `1.0.5`，与 elisp 侧 `rime-version` **一致** | 模块可加载、可调用；且 `rime-activate` 里的版本一致性检查会通过，**不会**触发它自己的 `make lib` 重编 |
| `vterm-20251119.1653/vterm-module.so` | 31.1 下 `(require 'vterm)` 成功、`vterm-mode` 可用 | 加载层可用 |

**仍未验**：真实交互（在 31.1 里真打字输入中文、真开一个 vterm 终端）。这两项属于待办 3 的手点清单。换版本仍要留一道"模块可用性"检查，但"模块必然失效"这个假设已被证据推翻。

### 3.5 系统与回退弹药

- Arch Linux；`extra/emacs-wayland` 已是 **31.1-2**（当前装的是 `emacs-wayland 30.2-3`）；升级只牵动它自己，依赖全满足；
- 回退包在缓存：`/var/cache/pacman/pkg/emacs-wayland-30.2-3-x86_64.pkg.tar.zst`，`pacman -U <该文件>`（用户实测 3 分钟）；31.1-1 的包也在。

### 3.6 日用实例的入口实况

`~/.config/autostart/emacs.desktop` → systemd `app-emacs@autostart.service`，`ExecStart=:/usr/bin/emacs`（**GUI 实例**，不是 `--daemon`；`init-no-x-flag` 因此为 nil，org/note 正常加载）→ 靠 `~/.emacs.d/init.el`（chemacs stub）读 `~/.emacs-profiles.el` 的 `default` profile。配置一旦落到标准路径，**systemd 那行一个字都不用改**。

### 3.7 Emacs 31 的 user-lisp 自动处理（新发现，代价很大）

31 新增 User Lisp Directory 特性（`startup.el`，`user-lisp-auto-scrape` 默认 t）：配置目录下若有 `user-lisp/`，它会被**递归 byte-compile（.elc 直接写进源码目录）、扫 autoload、子目录进 load-path**。这份配置的目录恰叫这个名字，于是：

- 第一次真实启动 31.1（非 `-Q`）就在仓库里生成 25 个 `.elc` + `user-lisp/.user-lisp-autoloads.el`；
- 它还会为了解析 `transient-define-prefix` 之类的 cookie 去 `load` 模块，**打乱加载顺序**，引发一串看起来像"31 不兼容"的假故障：`meow/:config: Invalid function: gwp::text-edit-def`、`Cannot load org-zotero`、`org-note-search-transient` / `rust-edit-transient` 变量 void、`use-package: Unrecognized keyword: :straight`、`jka-compr Recursive load`；
- **`user-lisp-auto-scrape` 设为 nil 后，31.1 的启动日志零错误行**（30.2 也是）。该变量必须在 `early-init.el` 里设（startup.el 文档明说 init.el 太晚）；
- 残留风险：`prepare-user-lisp` 在禁用状态下仍会 **load** 已存在的 `user-lisp/.user-lisp-autoloads.el`，所以切换必须用**干净 checkout**（不含 `.elc` 与该文件）。

### 3.8 解包的 31.1 必须显式给 `--dump-file`

解包树不在编译期假定的 `/usr/lib/emacs/31.1/x86_64-pc-linux-gnu/` 下，不显式指 dump 文件时 Emacs 退回 `loadup` 现场重编：又慢（V1.1 记的"31 加载 8.1s"就是这么来的）又会在 `--init-directory`/daemon 下直接崩（`Symbol's function definition is void: file-name-sans-extension`）。启动器已自动取值，并在"0 个或多个 pdmp"时明确报错。

### 3.9 其它已实测的小事实

- `custom-file` **不会**被 Emacs 自动加载（daemon 实测 custom.el 的变量没生效）；
- **`setq` 早于 `defcustom` 的值会保留**（8 项路径变量实测）；
- `.eln` 文件名含**源文件路径**的哈希（同内容不同路径 → 不同 `.eln`）：包树一搬，`eln-cache` 必然失效重编；缓存目录名 `<ver>-<hash>` 只含版本与编译器指纹，与路径无关；
- **eshell 不自建目录**：实测空 state 目录下 `eshell-write-history` 静默不写（`file-writable-p` 为 nil，只 message 一句）→ 显式建 `eshell/`；`transient`（`transient--pp-to-file` 里 `make-directory`）与 `auto-save-list`（`files.el` 里 `make-directory`）都自建，不用管；
- 启动的联网边界（核实过，别把"不联网"说大）：`package-installed-p 'use-package` 为 t（内置，那行 `package-install` 不会跑）；straight 的引导只在 `straight/repos/straight.el/bootstrap.el` 缺失时联网；`:ensure` 只在**缺包**时联网。也就是说：**归档刷新确实只在显式命令里发生，但"缺件"仍会联网**——新机器或换包时的联网是 bootstrap 行为，不是日常行为。

### 3.10 提升路径的分支拓扑（实测纠正）

稳定库 `master` 上有只动文档的提交，`dev` 从更早的点分出。实测结论：**把文档提交 cherry-pick 进 `dev` 并不能让 `master` 成为 `dev` 的祖先**（哈希不同），`merge --ff-only` 仍不成立。可行做法（已落地）：文档提交留在 `master`，实现提交留在 `dev`，`master` 一前进就把 `dev` rebase 上去（`dev` 在提升前不推远端，所以 rebase 无代价）。2026-09-12 已按此法修好，`git merge-base --is-ancestor stable/master dev` 通过。

## 四、施工面（我的工作面，日用零接触）

| 路径 | 内容 |
|---|---|
| `~/Incoming/emacs-dev/` | 配置的**独立克隆**，branch `dev`；remote `stable`=旧库、`github`=私有库（**提升前不推**）。树里只有代码 |
| `~/Incoming/dev-emacs` | 演化轨启动器（**在克隆外**，不进仓库）。`dev-emacs` = 系统 30.2；`dev-emacs --31` = 解包版 31.1（自动带 `--dump-file`）。它导出隔离环境：`GWP_CACHE_DIR=~/.cache/emacs-dev/`、`GWP_STATE_DIR=~/.local/state/emacs-dev/`、`GWP_SERVER_NAME=gwp-dev`；已有 dev 实例在跑时用 `--socket gwp-dev2` 另起 |
| `~/Incoming/accept.sh` | 验收脚本两用：`accept.sh --defaults` 验 `early-init.el` 的**默认落点**（生产 XDG 路径，不带覆盖）；`accept.sh gwp-dev gwp-dev [--expect-31] [--log 启动日志]` 验运行中的实例（15 项落点 + socket 名 + 版本 + **启动日志错误全列** + 仓库零新增文件） |
| `~/Incoming/emacs-31.1/` | 从 pacman 缓存解包的 `emacs-wayland-31.1-1`（282MB，未安装） |
| `~/Incoming/emacs-pkg-snapshots/2026-09-11/` | `elpa-30.2-baseline.tgz` + `manifest-30.2.txt`（205 包 + 3 个 straight 仓库） |

### 4.1 2026-09-12 的验收记录

- 30.2 与 31.1 各起 daemon（隔离落点）→ `accept.sh` 全绿，**两版启动日志均零错误行**；
- `accept.sh --defaults` 全绿（生产 XDG 默认值没被演化轨带偏；eln 表保留系统 native-lisp、剔除配置目录项）；
- 干净克隆能起（符号链接随克隆复原、克隆内零新增文件）；
- 旧账 1（`highlight` 缺 autoloads）已在演化轨修好（`package-generate-autoloads` 补文件），启动日志因此干净；
- 稳定轨日用实例（socket `gwp`、`~/.emacs.d` + chemacs 入口）全程未触碰。

## 五、状态映射表（已落进 `early-init.el`）

| 现在（散在仓库里，随 syncthing 跨机） | 新家（按机器） |
|---|---|
| `elpa/`、`straight/`、`eln-cache/` | `~/.cache/emacs/{elpa,straight,eln-cache}` |
| `custom.el`、`recentf`、`history`、`bookmarks`、`bm-repository`、`tramp`、`projects`、`.org-id-locations`、`transient/`、`eshell/` | `~/.local/state/emacs/` |
| 仓库只留代码 | `init.el`、`early-init.el`、`user-lisp/`、`snippets/`、`site-lisp/` |

`early-init.el` 一次设定 `package-user-dir`、`straight-base-dir`、`native-comp-eln-load-path`（保留系统目录、剔除配置目录项）、`custom-file`、`recentf-save-file`、`savehist-file`、`bookmark-default-file`、`bm-repository-file`、`tramp-persistency-file-name`、`org-id-locations-file`、`project-list-file`、`transient-history-file`、`eshell-directory-name`、`server-name "gwp"`、`user-lisp-auto-scrape nil`，外加两项防御性落点（`url-configuration-directory`、`auto-save-list-file-prefix`）与 `eshell/` 目录创建。接受 `GWP_STATE_DIR`/`GWP_CACHE_DIR`/`GWP_SERVER_NAME` 覆盖（演化轨与测试用；**默认值就是生产落点**，所以切换时路径不用改）。

路径只有一个来源：`init-core.el` / `init-workspace.el` 里原先各自覆盖 `bookmarks`、`bm-repository` 的两行已删（bm 那行曾把 `bm-repository` 写进仓库根）。

## 六、待办（按序，判据已冻结，事前不改）
| # | 动作 | 验收判据 | 状态 |
|---|---|---|---|
| 1 | 施工面写 `early-init.el`；`init.el` 补 `lexical-binding` cookie、去掉它自己的 `(setq custom-file …)`；启动器与 `early-init.el` 共用同一份定义 | 30.2 与 31.1 各跑一次：state/cache 真落到新家；`git status` 显示施工面**零新增文件**；`server-name` = `gwp` | ✅ 2026-09-12（两版 daemon + 两版有 X 全量 init；`accept.sh` 全绿；`accept.sh --defaults` 验默认值） |
| 2 | 把"新克隆跑不起来"的根补进 git：4 个 `user-lisp/*.el` 符号链接、`site-lisp/org-zotero` | 干净克隆后能起，无 `site-lisp` 目录错误 | ✅ 2026-09-12（`/tmp/fresh-clone` 实测能起；`site-lisp/treesit-jump/` 故意不并入：上游仓库、无人引用） |
| 3 | 体验清单：batch 能验的全部跑通（socket 名、`emacsclient -s gwp`、rime 谓词、附件目录左窗、denote、agenda 路径、snippet 展开、gptel 后端）；交互项列成短清单交用户点 | 清单全绿；交互项由用户确认 | ⏳ **下一步**（注意：符号链接指向的绝对路径在三台机器上是否都存在，只有在别的机器上才能真验——先在文档里记成"待其他机器确认"） |
| 4 | 切换准备：备份 `~/.emacs.d`、`~/.emacs-profiles.el`、`~/.local/bin/emacs` → 快照目录；**从批准的 git 提交做干净 checkout 到 `~/.config/emacs`**（不是 `mv` 整个工作树——旧树里有 elpa/straight/eln-cache/history/recentf/legacy 目录，搬过去等于把要清理的东西原样带进新家）；把当前仓库内的运行态拷进 `~/.local/state/emacs/`；旧路径留 README 指向新家（**其他机器还没迁移完之前，不要删同步区的旧目录**） | 备份可解包复原；`emacs` 裸命令直接起新配置；新家目录里只有代码；旧路径只剩 README | ⏳ |
| 5 | 31 落地：复跑清单（含 rime/vterm **真实交互**）→ 需要时重编模块 → 全量 native 编译留日志 → 出报告 → 用户决定切系统包（`pacman -Syu emacs-wayland`） | 模块可用；清单全绿；回退命令已验证 | ⏳ |
| 6 | 提升路径：`dev` → push `github` → 用户点头 → 稳定库 `git merge --ff-only` → 重启 → 跑清单 | 提升前后 `git log` 线性；不满意可 `git reset --hard <tag>` | ⏳ 拓扑已修好（见 3.10 与决议 11），可按原计划做 |

## 七、尚未裁决的开放决策（不阻塞待办 3，但挡在待办 5 之前）

| 决策 | 现状 | 为何要定 |
|---|---|---|
| **包策略**：冻结已验证集合，还是继续跟 MELPA 最新 | 未裁决；`~/Incoming/emacs-pkg-snapshots/2026-09-11/manifest-30.2.txt` 只是施工快照，不是正式输入 | 换机器/重建包树时，"跟新"会悄悄漂移；"冻结"要求一份写进仓库的清单（package.el 包版本 + straight 仓库 commit）。当前启动的联网边界是：归档刷新确实只在显式命令里，但**缺件会联网 bootstrap**（见 3.9） |
| **化石包清理**：如 `highlight`（无人 `require`）、`names`、以及 53 个多版本包 | 未动（`AGENTS.md` 不许擅自清点） | 待办 5 要全量 native 编译，化石会一起被编译、浪费启动与排错精力 |
| **其他机器**：各自跑什么版本、路径是否一致、绝对路径的符号链接是否都解得开 | 未知（缓存目录名不能作证据，见 3.1） | 每台机器都要重编模块、重建包树；协调成本还没算过 |

## 八、回退面

- 配置：`git reset --hard <tag>`（提升前打 tag）；
- 系统 Emacs：`pacman -U /var/cache/pacman/pkg/emacs-wayland-30.2-3-x86_64.pkg.tar.zst`；
- 包树：`tar -xzf ~/Incoming/emacs-pkg-snapshots/2026-09-11/elpa-30.2-baseline.tgz`；
- 整套回今天：解包 `~/.emacs.d` 那份备份 + 恢复 `~/.emacs-profiles.el` 与 wrapper；
- 演化轨环境（`~/.cache/emacs-dev`、`~/.local/state/emacs-dev`）删掉即干净，日用从未依赖。

## 九、已知旧账

1. ~~`highlight-20210318.2248` 缺 `*-autoloads.el`~~ → **2026-09-12 已在演化轨修好**（`package-generate-autoloads` 补出文件）；切换时随包树一起带过去。该包无人 `require`，若确认不用可删（待用户裁决，按 `AGENTS.md` 不擅自清点）；
2. `names-20221227.1825` 的 `defadvice` 自 30.1 起废弃告警（无害，两版都有）；
3. `init-core.el` 用了废弃的 `point-at-bol`/`point-at-eol`（31 仍在，32 可能删）；
4. `custom.el` 与 `init-ui.el` 的 faces 有重复（Custom UI 写的盖住了配置）；
5. `AGENTS.md` 里"入口 wrapper 由 yadm 管理"与实测不符——yadm 三个都没纳管；
6. `~/.emacs-profiles.el` 里 `default` 与 `gwp` 指向同一目录，纯冗余（随 chemacs 一起退役）；
7. `user-lisp/zotero.so` 是机器本地编译产物（`emacs-zotero.el` 用 `condition-case` 包着 `require`，缺了不致命）——将来该进 `~/.cache`，不该进 git。

## 十、不要做的事

- 不动 `server-socket-dir`；不改 systemd/autostart；
- 不在稳定轨直接改**配置**（一切在施工面）；只动文档可以，但会让 `master` 前进 → **必须顺手 `dev rebase`**；
- 不在清单通过前升级系统 Emacs；
- 不用"整目录 copy/mv 工作树"做提升或搬家（丢 diff、丢历史、把 legacy 与运行态一起带过去）；
- 不重推"chemacs 是否值得"（已裁决：退役）；
- 不让 31 的 `user-lisp-auto-scrape` 重新打开（会把 `.elc` 写进仓库并打乱加载顺序，见 3.7）；
- 不让演化轨与稳定轨共用同一棵包树/同一个 state（见决议 10）。

## 十一、新会话怎么接着干

1. 读本文件 + `AGENTS.md` + `user-lisp/AGENTS.md`；
2. 确认施工面还在（`~/Incoming/emacs-dev`、`~/Incoming/dev-emacs`、`~/Incoming/accept.sh`、`~/Incoming/emacs-31.1`、快照目录）；顺手 `git -C ~/Incoming/emacs-dev fetch stable && git merge-base --is-ancestor stable/master dev`，确认提升路径仍然可 ff；
3. 从"六、待办"里**第一个未完成项**继续，判据照表；做一步就更新本文件的"状态"列；
4. 需要用户决策的只有：切换时机（第 4/5 步）、体验清单的交互项、第 5 步切系统包。

## 更新记录

- **2026-09-11 建立**：目标、决议、证据、施工面、状态映射、待办与判据、回退面、旧账、禁做项。
- **2026-09-12 V1.1**：待办 1、2 完成并验收；新增决议 9、10；新增证据 3.7（31 的 user-lisp 自动处理）、3.8（`--dump-file`）、3.9（custom-file 不自动加载等）；施工面表更新；映射表补两项防御性落点。
- **2026-09-12 V1.2**：按一轮整体复核订正——①决议 10 改为"环境按轨隔离"（原 V1.1 让两轨共用包树，会在待办 5 重编模块时弄坏日用），②新增决议 11 与证据 3.10（提升路径的分支拓扑，cherry-pick 方案被证伪，已按"文档走 master + dev rebase"修好并验证可 ff），③3.4 用实测替换"模块必然失效"的推断（rime 模块两版都能加载调用、版本也一致；只差真实交互），④3.9 补 eshell 不自建目录、启动联网的真实边界、`.eln` 名字含路径哈希，⑤待办 4 明确"干净 checkout 而非 mv 工作树"并加上"其他机器迁移前不删旧目录"，⑥旧账 1 标记已修，⑦`accept.sh` 增加 `--defaults` 模式与启动日志错误全列，⑧新增"尚未裁决的开放决策"一节（包策略、化石包、其他机器）。
