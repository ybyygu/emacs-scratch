# Emacs 31 移植 + 标准路径迁移 · 交接文档

> 版本：V1.1 ｜ 更新：2026-09-12 ｜ 创建：2026-09-11 ｜ 状态：**待办 1、2 已完成并验收；下一步是体验清单（第 3 项）**
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
| 5 | 状态按机器（XDG），配置按 git | 见第五节映射表；这是配置审计第 1 条（状态出同步区）的落实 |
| 6 | 双轨：`~/Incoming/emacs-dev` 施工 → 验证 → 提升到稳定轨；用**独立克隆**而非 worktree | worktree 会把 dev 书签写进同步区 `.git/worktrees/`，跨机变坏引用 |
| 7 | 31 先用**解包的 31.1** 跑施工面，系统包留到最后一步 | 零系统风险；回退弹药在 pacman 缓存 |
| 8 | 保持 30.2 兼容 | 其他机器 + 回退面都需要 |
| 9 | 演化轨用 Emacs 自带的 `--init-directory` 起（不再 `-Q` + 启动器里写一份环境定义） | 标准机制；`-Q` 会关掉 `init-file-user`，反而测不到真实启动路径（31 的 user-lisp 自动处理只在真实启动下发生，见 3.7） |
| 10 | 演化轨的状态/缓存**就用真实 XDG 路径**（`~/.local/state/emacs`、`~/.cache/emacs`），dev/稳定只靠 socket 名与配置目录区分 | 验证的就是将来要用的那套路径；包树与编译缓存提前在正确位置热好，切换当天没有环境搬运 |

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

同一份配置、同一个隔离包树，30.2 与 31.1 对照：

- **14 个模块全部加载成功**，12 项能力探针（org/attach/denote/vertico/corfu/meow/magit/vterm…）**逐项一致**；
- 31 只多两条警告：`Missing 'lexical-binding' cookie in init.el`（→ 已在待办 1 修掉）；
- `point-at-bol` / `point-at-eol` 在 31.1 里**仍然存在**（`fboundp` = t），不是它的问题。

### 3.3 包树现状（自洽但有化石）

- `elpa/` 顶层 231 个条目 = **205 个包目录** + 26 个 `.signed` 签名文件；
- 53 个包有 2–3 个版本（`compat` 三份：28.1.2.2 / 29.1.4.5 / 30.1.0.1）；
- **0 个**包要求 `emacs > 30.2` → 当前状态自洽，可安全作为基线；
- 化石来自两代：2025-04 与 2026-01/02。

### 3.4 换版本会踩的三类重编译（"很多问题"的最可能来源）

| 类别 | 证据 | 症状 |
|---|---|---|
| **动态模块失效** | `elpa/rime-20251105.1505/librime-emacs.so`（2026-01-27）、`elpa/vterm-20251119.1653/vterm-module.so` 均编译于 30.2 时代，文件名不带版本 | 输入法/终端失效——最像"很多问题"。**注**：2026-09-12 实测这两个 `.so` 在 31.1 下能加载（`(require 'rime)`/`vterm` 不报错），但只验了加载、没验功能；重编仍按待办 5 做 |
| native-comp 全量首次编译 | 那次 2h 只得 160 个 `.eln`（30.2 下已 418 个） | 启动卡、CPU 高、Warnings 一堆 |
| straight 全量重建 | 沙箱一跑即打印 `Rebuilding all packages due to change in Emacs version` | 每次换版本首次启动一次（只有 3 个 straight 包，秒级） |

### 3.5 系统与回退弹药

- Arch Linux；`extra/emacs-wayland` 已是 **31.1-2**（当前装的是 `emacs-wayland 30.2-3`）；
- 升级只牵动 `emacs-wayland` 自己，**依赖全满足**；
- 回退包在缓存里：`/var/cache/pacman/pkg/emacs-wayland-30.2-3-x86_64.pkg.tar.zst`，回退 `pacman -U <该文件>`（用户 8-28 实测可行，3 分钟）；31.1-1 的包也在缓存。

### 3.6 日用实例的入口实况

`~/.config/autostart/emacs.desktop` → systemd `app-emacs@autostart.service`，`ExecStart=:/usr/bin/emacs`（**GUI 实例**，不是 `--daemon`；`init-no-x-flag` 因此为 nil，org/note 正常加载）→ 靠 `~/.emacs.d/init.el`（chemacs stub）读 `~/.emacs-profiles.el` 的 `default` profile。配置一旦落到标准路径，**systemd 那行一个字都不用改**。

### 3.7 Emacs 31 的 user-lisp 自动处理（2026-09-12 新发现，代价很大）

31 新增 User Lisp Directory 特性（`startup.el`，`user-lisp-auto-scrape` 默认 t）：配置目录下若有 `user-lisp/`，它会被**递归 byte-compile（.elc 直接写进源码目录）、扫 autoload、子目录进 load-path**。这份配置的目录恰叫这个名字，于是：

- 第一次真实启动 31.1（非 `-Q`）就在仓库里生成 25 个 `.elc` + `user-lisp/.user-lisp-autoloads.el`；
- 它还会为了解析 `transient-define-prefix` 之类的 cookie 去 `load` 模块，**打乱加载顺序**，引发一连串看起来像"31 不兼容"的假故障：`meow/:config: Invalid function: gwp::text-edit-def`、`Cannot load org-zotero`、`org-note-search-transient` / `rust-edit-transient` 变量 void、`use-package: Unrecognized keyword: :straight`、`jka-compr Recursive load`；
- **`user-lisp-auto-scrape` 设为 nil 后，31.1 的启动日志只剩旧账 `highlight` 那一条**（30.2 也有），上面全部消失。这个变量必须在 `early-init.el` 里设（startup.el 文档明说 init.el 太晚）。

### 3.8 解包的 31.1 必须显式给 `--dump-file`

解包树的位置不是编译期假定的 `/usr/lib/emacs/31.1/x86_64-pc-linux-gnu/`，不显式指 dump 文件时 Emacs 会退回 `loadup` 现场重编一份：又慢（上一轮"31 加载 8.1s"就是这么来的）又会在 `--init-directory`/daemon 下直接崩（`Symbol's function definition is void: file-name-sans-extension`）。启动器已用 `ls .../emacs-*.pdmp` 自动取值。

### 3.9 其它已实测的小事实

- `custom-file` **不会**被 Emacs 自动加载：init 里必须显式 `load`（daemon 实测 `custom.el` 的变量没生效）；
- **`setq` 早于 `defcustom` 的值会保留**（`project-list-file`/`transient-history-file`/`tramp-persistency-file-name` 等 8 项实测）；
- 默认 `native-comp-eln-load-path` = `("<user-emacs-dir>/eln-cache/" "/usr/lib/emacs/<ver>/native-lisp/")`，改时必须保留系统那一项，否则连 Emacs 自己的 lisp 都要重编；
- **`.eln` 文件名含源文件路径的哈希**（同内容不同路径 → 不同 `.eln`）：包树一搬，`eln-cache` 必然失效，重新编译是免不了的（正是待办 5 要留日志的那一步）。

## 四、施工面（我的工作面，日用零接触）

| 路径 | 内容 |
|---|---|
| `~/Incoming/emacs-dev/` | 配置的**独立克隆**，branch `dev`；remote `stable`=旧库(`~/Install/configs/emacs/gwp-scratch`)、`github`=私有库。树里**只有代码**（`early-init.el`、`init.el`、`user-lisp/`、`snippets/`、`site-lisp/`），没有 elpa/straight/state |
| `~/Incoming/dev-emacs` | 演化轨启动器（**故意放在克隆外面**，不进仓库）。`dev-emacs` = 系统 30.2；`dev-emacs --31` = 解包版 31.1（自动带 `--dump-file`）。都走 `--init-directory`，socket = `gwp-dev` |
| `~/Incoming/accept.sh` | 验收脚本：`accept.sh gwp-dev gwp-dev [--expect-31]`（实例已在跑时）。逐条断言落点、socket 名、仓库零新增文件 |
| `~/Incoming/emacs-31.1/` | 从 pacman 缓存解包的 `emacs-wayland-31.1-1`（282MB，未安装到系统） |
| `~/Incoming/emacs-pkg-snapshots/2026-09-11/` | `elpa-30.2-baseline.tgz`（19MB）+ `manifest-30.2.txt`（205 包 + 3 个 straight 仓库） |

已实测（2026-09-12）：30.2 与 31.1 各起一次 daemon → `accept.sh` **全绿**；两版各做一次"有 X 路径"的完整 init（`init-no-x-flag=nil`，org/note 分支）→ 除旧账 `highlight` 外无错误；干净克隆（`/tmp/fresh-clone`）能起、符号链接随克隆复原、克隆内零新增文件。稳定轨 `elpa`/状态文件未被触碰。

## 五、状态映射表（已落进 `early-init.el`）

| 现在（散在仓库里，随 syncthing 跨机） | 新家（按机器） |
|---|---|
| `elpa/`、`straight/`、`eln-cache/` | `~/.cache/emacs/{elpa,straight,eln-cache}` |
| `custom.el`、`recentf`、`history`、`bookmarks`、`bm-repository`、`tramp`、`projects`、`.org-id-locations`、`transient/`、`eshell/` | `~/.local/state/emacs/` |
| 仓库只留代码 | `init.el`、`early-init.el`、`user-lisp/`、`snippets/`、`site-lisp/` |

`early-init.el` 一次设定 `package-user-dir`、`straight-base-dir`、`native-comp-eln-load-path`、`custom-file`、`recentf-save-file`、`savehist-file`、`bookmark-default-file`、`bm-repository-file`、`tramp-persistency-file-name`、`org-id-locations-file`、`project-list-file`、`transient-history-file`、`eshell-directory-name`、`server-name "gwp"`、`user-lisp-auto-scrape nil`，外加两项防御性落点（`url-configuration-directory`、`auto-save-list-file-prefix`——它们默认落在仓库里，现在没人用，先占住位置）。接受 `GWP_STATE_DIR`/`GWP_CACHE_DIR`/`GWP_SERVER_NAME` 覆盖（供多轨与测试）。

路径只有一个来源：`init-core.el` / `init-workspace.el` 里原先各自覆盖 `bookmarks`、`bm-repository` 的两行已删（bm 那行曾把 `bm-repository` 写进仓库根）。

## 六、待办（按序，判据已冻结，事前不改）

| # | 动作 | 验收判据 | 状态 |
|---|---|---|---|
| 1 | 施工面写 `early-init.el`；`init.el` 补 `lexical-binding` cookie、去掉它自己的 `(setq custom-file …)`；启动器与 `early-init.el` 共用同一份定义 | 30.2 与 31.1 各跑一次：state/cache 真落到新家；`git status` 显示施工面**零新增文件**；`server-name` = `gwp` | ✅ 2026-09-12（30.2/31.1 两次 daemon + 两次有 X 全量 init，`accept.sh` 全绿；`server-name` 日用默认 `gwp`、演化轨 `gwp-dev`） |
| 2 | 把"新克隆跑不起来"的根补进 git：4 个 `user-lisp/*.el` 符号链接、`site-lisp/org-zotero` | 干净克隆后能起，无 `site-lisp` 目录错误 | ✅ 2026-09-12（`/tmp/fresh-clone` 实测能起；`site-lisp/treesit-jump/` 故意不并入：上游仓库、配置里无人引用，要用就照 remote 重新 clone） |
| 3 | 体验清单：batch 能验的全部跑通（socket 名、`emacsclient -s gwp`、rime 谓词、附件目录左窗、denote、agenda 路径、snippet 展开、gptel 后端）；交互项列成短清单交用户点 | 清单全绿；交互项由用户确认 | ⏳ 下一步 |
| 4 | 切换准备：备份 `~/.emacs.d`、`~/.emacs-profiles.el`、`~/.local/bin/emacs` → 快照目录；仓库搬到 `~/.config/emacs`（git 历史与 remote 跟随）；旧路径留 README 指向新家；把仓库内的运行态文件（`history`/`recentf`/`custom.el`/…）搬进 `~/.local/state/emacs/` 并从仓库移除 | 备份可解包复原；`emacs` 裸命令直接起新配置；旧路径只剩 README | ⏳ |
| 5 | 31 落地：重编 rime/vterm 模块 → 全量 native 编译留日志 → 复跑清单 → 出报告 → 用户决定切系统包（`pacman -Syu emacs-wayland`） | 模块可用；清单全绿；回退命令已验证 | ⏳ |
| 6 | 提升路径：`dev` → push `github` → 用户点头 → 稳定库合入 → 重启 → 跑清单 | 提升前后 `git log` 线性；不满意可 `git reset --hard <tag>` | ⏳ 有一处要先解：稳定库 `master` 已有两个只动文档的提交（`0a3b49a` PORT 文档、`1685a0e` memo），`dev` 上没有 → `merge --ff-only` 不成立。可选：①把这两个提交 cherry-pick 进 `dev`，之后所有提交（含文档）一律走 `dev`，`master` 只接受 ff；②接受一个 merge 提交（放弃"线性"）。**需要用户点头** |

## 七、回退面

- 配置：`git reset --hard <tag>`（提升前打 tag）；
- 系统 Emacs：`pacman -U /var/cache/pacman/pkg/emacs-wayland-30.2-3-x86_64.pkg.tar.zst`；
- 包树：`tar -xzf ~/Incoming/emacs-pkg-snapshots/2026-09-11/elpa-30.2-baseline.tgz`；
- 整套回今天：解包 `~/.emacs.d` 那份备份 + 恢复 `~/.emacs-profiles.el` 与 wrapper。**注**：`~/.cache/emacs`、`~/.local/state/emacs` 是新增目录，删掉即干净回退（日用轨从未依赖它们）。

## 八、已知旧账（顺手清，不阻塞）

1. `highlight-20210318.2248` 缺 `*-autoloads.el`，30.2/31.1 都报 `Error loading autoloads`（两版唯一残留的错误行）；
2. `names-20221227.1825` 的 `defadvice` 自 30.1 起废弃告警；
3. `init-core.el` 用了废弃的 `point-at-bol`/`point-at-eol`（31 仍在，32 可能删）；
4. `custom.el` 与 `init-ui.el` 的 faces 有重复（Custom UI 写的盖住了配置）；
5. `AGENTS.md` 里"入口 wrapper 由 yadm 管理"与实测不符——**yadm 没有纳管**这三个中的任何一个；
6. `~/.emacs-profiles.el` 里 `default` 与 `gwp` 指向同一目录，纯冗余（随 chemacs 一起退役）；
7. `user-lisp/zotero.so` 是机器本地编译产物（Rust 模块），稳定库里未纳管；`emacs-zotero.el` 用 `condition-case` 包着 `(require 'zotero)`，缺了不致命——将来该进 `~/.cache`，不该进 git。

## 九、不要做的事

- 不动 `server-socket-dir`；不改 systemd/autostart；
- 不在稳定轨直接改配置（那是用户日用；一切在施工面）——只动文档不算，但文档提交会让 `dev`/`master` 分叉，见待办 6；
- 不在清单通过前升级系统 Emacs；
- 不用"整目录 copy 回来"做提升（丢 diff、丢历史，可能复制出半状态）；
- 不重推"chemacs 是否值得"（已裁决：退役）；
- 不让 31 的 `user-lisp-auto-scrape` 重新打开（会把 `.elc` 写进仓库并打乱加载顺序，见 3.7）。

## 十、新会话怎么接着干

1. 读本文件 + `AGENTS.md` + `user-lisp/AGENTS.md`；
2. 确认施工面还在（`~/Incoming/emacs-dev`、`~/Incoming/dev-emacs`、`~/Incoming/accept.sh`、`~/Incoming/emacs-31.1`、快照目录）；
3. 从"六、待办"里**第一个未完成项**继续，判据照表；做一步就把本文件的"状态"列更新；
4. 需要用户决策的只有：切换时机（第 4/5 步）、体验清单的交互项、提升路径的合并方式（第 6 步）。

## 更新记录

- **2026-09-11 建立**：目标、决议、证据、施工面、状态映射、待办与判据、回退面、旧账、禁做项。
- **2026-09-12 V1.1**：待办 1、2 完成并验收（`accept.sh` 全绿 + 干净克隆实测）；新增决议 9、10；新增证据 3.7（31 的 user-lisp 自动处理，及其引发的假故障链）、3.8（解包 31.1 需 `--dump-file`）、3.9（custom-file 不自动加载等四项）；施工面表更新（启动器移到克隆外、新增 `accept.sh`）；映射表补两项防御性落点；待办表加"状态"列并标出第 6 项的合并分叉问题。
