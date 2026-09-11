# gwp-scratch：AI 代管续聊 memo

> 更新：2026-09-11 11:40 ｜ 本文件是接续入口（先读它，再读两份 `AGENTS.md`，细节回 `transcripts.md`）。上一版 10:03 的正文除“版本管理边界”里的缓存一句、“输入法验收”、“开放问题”三处已更新外仍然有效。

## 总纲

这次讨论的核心，不是把旧配置整理成一个漂亮、统一、现代化的工程，而是**在不牺牲现有可用性的前提下，重新划分维护责任**：

- ybyygu 继续提供需求、使用体验与方向判断；
- AI 接手现有 `.el` 代码、snippet 与相关文档的维护；
- `note` 退回私人笔记，不再承担代码真源、tangle 源或同步对象的职责；
- 旧配置中的未知文件、运行态、残留结构暂时全部保留，等实际过审或实际故障驱动后再处理。

真正需要避免的是“管理项目本身变成一次重构”。因此，今后的进度以**具体问题驱动**，而不是以“整理完整”为目标。

## 已裁决的所有权边界

项目指：

`/home/ybyygu/Install/configs/emacs/gwp-scratch/`

其中：

- `/home/ybyygu/Install/configs/emacs/gwp-scratch/init.el`
- `/home/ybyygu/Install/configs/emacs/gwp-scratch/user-lisp/*.el`
- `/home/ybyygu/Install/configs/emacs/gwp-scratch/snippets/`

是当前代码与内容的权威来源，AI 直接维护。

`/home/ybyygu/Install/configs/emacs/gwp-scratch/gwp-scratch.note` 以及其 archive：

- 不作为代码真源；
- 不与 `.el` 比对；
- 不由 AI 维护；
- 不重新建立 `.note → .el` 或 `.el → .note` 的同步关系；
- note 中尚未落地的内容，不自动进入代码待办。

外层仓库 `/home/ybyygu/Install/configs/emacs/` 主要承载旧 note 的版本记录，属于用户自己的笔记侧，不纳入本项目的 AI 维护回路。

这条边界解决了此前最容易反复出现的歧义：以后看到 `.el` 文件头部残留的 `[[file:...gwp-scratch.note...]]`，它们只是旧 literate 流程留下的历史注释，不代表当前真源。

## AI 代管的工作契约

### ybyygu 负责

- 在 note 或对话中记录想法、问题与使用体验；
- 描述希望改变的行为；
- 判断一个遗留功能是否值得保留；
- 对实际交互效果做最终验收。

不要求 ybyygu 先解释完整项目，也不要求先清理旧文件才能开始维护。

### AI 负责

- 从现有代码出发定位问题；
- 直接修改权威 `.el`、snippet 或相关配置；
- 维持最小改动；
- 按影响范围验证；
- 在发生结构性变化或形成可复用隐性知识时更新文档；
- 用中文 commit 说明动机与使用影响；
- 维护 GitHub private 仓库作为远程备份。

默认工作回路：

```text
需求/体验
  → 定位模块
  → 读取当前实现
  → 明确保留什么、改变什么
  → 最小修改
  → 按影响面验证
  → 必要时更新 AGENTS.md
  → 提交并推送备份
```

每次只处理一个具体问题。没有用户需求时，不主动重排模块、清点文件、统一包管理器或引入工具链。

## fractal-docs 的最小落地

当前采用两层文档，不扩展更多文档层。

### 项目级入口

`/home/ybyygu/Install/configs/emacs/gwp-scratch/AGENTS.md`

职责：

- 项目定位与 AI 协作边界；
- `.el` 是唯一代码真源，note 不属于维护范围；
- chemacs2、profile、daemon 与 `init.el` 的加载拓扑；
- 包管理与提交约定；
- 基本验证方式；
- 会导致 AI 改错地方的跨文件隐性知识；
- 指向子目录文档。

### 模块级入口

`/home/ybyygu/Install/configs/emacs/gwp-scratch/user-lisp/AGENTS.md`

职责：

- `init-*.el` 的模块地图；
- 模块之间的加载关系；
- `cp2k-mode`、`rust-edit`、`yadm` 等局部依赖；
- 外部符号链接与懒加载模块；
- `gwp::*` 按键 map 的局部约定；
- 本目录特有的历史痕迹与修改陷阱。

根文档管项目契约，子文档管代码导航；两者不重复抄写事实。暂时不建 `docs/`、CHANGELOG 或 `ai-chats/` 过程文档层。若未来某个目录形成独立维护规则，再按实际需要分裂文档。

## 当前管理策略

### 保持结构，不追求重构

现有模块划分、加载顺序、meow/general 按键体系、package.el 与少量 straight.el 并存的状态先保持。默认新增依赖继续走现有 `:ensure` 路径，只有确实需要 GitHub 直装时才使用 `:straight`。

运行态与遗留物暂不清理，包括：

- `history`、`recentf`、`bookmarks`、`eln-cache` 等；
- `pkg/`、`a.el`、`orca.el`、`start-eaf.sh`、`run.sh` 等；
- 未确认用途的外部链接、二进制与数据文件。

“未被当前入口加载”只能说明尚未确认，不足以推出“应该删除”。

### 版本管理边界已收拢

重要的日常写作资产已经纳入 inner repository：

- `snippets/markdown-mode/` 的 markdown snippet 与 prompt 模板；
- 其它新增 snippet；
- `english-words.txt`，因为它被 `init-completion.el` 作为 ispell 备用词典引用。

`.yas-compiled-snippets.el` 属于含机器绝对路径的自动生成缓存，已忽略；**但它不会自动重建，且存在时不比对 mtime 就会被无条件加载**，所以 `snippets/` 各目录现在都不留缓存（详见 `AGENTS.md` 的 snippets 约定）。其余运行态噪声仍保留，待以后单独处理。

## 输入法问题形成的定位框架

输入法相关行为不能笼统地称为“改 rime”，需要先分层：

1. `/home/ybyygu/.local/share/fcitx5/rime/`：RIME 数据、schema、punctuator、fcitx5 与 emacs-rime 共用；
2. `/home/ybyygu/Install/configs/emacs/gwp-scratch/user-lisp/init-ui.el`：emacs-rime 的 predicate 与按键，决定 Emacs 何时自动交给中文输入或直接按英文输入；
3. `/home/ybyygu/.emacs-profiles.el`、daemon 与 socket：决定实际跑的是哪份配置，以及改动何时生效。

本次 `-` 的问题定位在第 2 层：`rime-predicate-after-alphabet-char-p` 原本把 `-` 视作可延续英文状态的字符；现已由项目内的 `gwp::` 变体排除，使英文字母连续输入后按 `-`，下一个按键交给 RIME 处理中文。RIME schema 与 fcitx5 的 `switch_key` 没有被改动。

这个例子留下两条可继续使用的认识：

- 先定位行为属于 RIME、emacs-rime 还是 daemon/profile，再修改；
- 单文件行为的特殊理由优先写在函数 docstring 或临近注释中，不为一次局部修复扩张项目文档。

**已闭环（2026-09-11）**：`gwp::rime-predicate-after-alphabet-char-p` 已装进日用实例（实例于 09-11 10:20 重启，`rime-disable-predicates` 内已是该变体），ybyygu 实测 `abc-` 之后进入中文输入，通过。若日后端到端行为不符，仍优先回到 `init-ui.el` 查，不重新打开 note/tangle 体系。

## 本轮增补：snippet 提示词库（2026-09-11）

### 两个独立故障叠成“提示词加载不出来”

1. **散文里写了 yasnippet 的元语法**：`prompt-阅读文献`、`prompt-不上东坡肉`、`prompt-grill-me`、`prompt-唐僧病` 里的字面反引号被当 elisp 求值、`${token}` 被当字段，展开时把正文静默换成 `Symbol’s value as variable is void: …`，中间的文字还被吃掉；`唐僧病` 的代码围栏整段塌成一行乱码。
   - 修法沿用仓库 2018 年 `code`/`back-quote` 的旧惯例：字面反引号写 ``\` ``，美元符写 `\$`；现已固化为约定。
2. **编译缓存不以源文件失效**：`.yas-compiled-snippets.el` 存在即被 `load`，不与源文件比对 mtime（实测：源文件 mtime 新 2 小时仍加载旧内容），并且不会自动重建。→ 6 个目录的缓存全部删除，规则改为**不留缓存**；要加载速度再 `M-x yas-recompile-all`，但要记得改完再删。

### 校验判据：展开 == 源

隔离副本 → 删缓存 → `emacs -Q --batch` 加载目录 → `yas-expand-snippet` → 与正文逐字比对（忽略 markdown-mode 的字体化属性）。结果：markdown 16 个散文类提示词 0 不一致、4 个模板类（`code`、`prompt-org-mode`、`prompt-高手回复`、`prompt-肚子里的蛔虫`）；全库 6 个目录 86 个 snippet 可解析可查询，directive 告警与同名告警均为 0。

### 顺带解决的两件事

- `yas-indent-line` 会把展开文本按 markdown 规则重排（`prompt-git-commit` 的嵌套子项、`prompt-grill-me` 的 `  > AI:` 变平级）→ 这两个文件加 `# expand-env: ((yas-indent-line 'none))` 逐字保真，校验因此回到“二值信号”（任何 △/✗ 都是新问题）。
- 库收拢：ybyygu 判断 29 个 markdown 语法 snippet 不再使用并删除（只剩 `code` + 19 个 `prompt-*`）；引用面经核查为零依赖。本轮新增 `prompt-图文并茂`。

细节落在 `AGENTS.md` 的 snippets 约定、验证表、隐性知识三处，本 memo 不重复。

## 验证与可用性原则

验证不是全项目测试，而是防止本轮修改破坏正在使用的功能：

- 普通局部修改：优先做改动文件的语法/编译检查；
- snippet 类改动：按“展开 == 源”验（隔离副本先删 `.yas-compiled-snippets.el`，batch 展开后与正文逐字比对），不能只验“没报错”——生成链上的解释器会吞掉错误并把错误文本混进输出；
- 涉及 `init.el` 或模块加载：再检查装配链；
- 涉及 GUI、Org、note 或输入法：无 X 的 batch 结果不能作为完整结论；
- daemon 已加载旧代码时，重启前的日用体验不代表新代码；
- 测试不得为了方便杀掉或污染日用 daemon，交互行为需要独立实例或明确重启后验收。

hpc44 远程服务器目前基本不用；其 broken 状态不阻塞本机维护，也不应成为补齐旧部署流程的前置任务。

## 后续值得保持开放的问题

1. ~~**输入法改动的端到端验收**~~ → **已闭环（2026-09-11）**：实例重启后 ybyygu 实测 `abc-` 后进入中文输入通过，见上节。
2. **`# expand-env` 的现场验收**：`prompt-git-commit`、`prompt-grill-me` 的缩进保真只在 `-Q --batch` 验过；日用实例插一次确认即可，最坏情形是回到平级。
3. **snippet 校验脚本是否长期化**：本轮的展开校验脚本放在 `/tmp`（易失）。若想让“改完一键验”成为常规，可考虑入库（如 `tools/`）；目前只是候选。
4. **运行态噪声是否值得隔离**：只有当 `git status`、syncthing 冲突或跨机同步持续造成实际负担时，再讨论 `.gitignore`、`var/` 或 `.stignore`。
5. **遗留物是否需要分诊**：继续遵守“逐个过审、先留后动”，不把清理清单升级为管理目标。
6. **模块地图的准确性**：文档中的职责描述是当前代码导航，不是不可修改的设计宣言；实际发现模块职责与描述不符时再校正。
7. **协作经验是否升级为通用规则**：上一轮形成的“先确立所有权，再建立最小文档，再由真实问题驱动工具化”，加上本轮的两条候选——“加速产物必须有失效检测”“校验判据必须保持二值”——都具有跨项目价值，但是否写入长期用户档案，另行决定。

## 续聊回源入口

- 项目边界、所有权、维护策略：  
  `/home/ybyygu/Install/configs/emacs/gwp-scratch/AGENTS.md`
- 模块地图与 `user-lisp` 局部知识：  
  `/home/ybyygu/Install/configs/emacs/gwp-scratch/user-lisp/AGENTS.md`
- 当前输入法 predicate 实现：  
  `/home/ybyygu/Install/configs/emacs/gwp-scratch/user-lisp/init-ui.el`
- 总装配入口与加载顺序：  
  `/home/ybyygu/Install/configs/emacs/gwp-scratch/init.el`
- RIME 外部配置：  
  `/home/ybyygu/.local/share/fcitx5/rime/`
- profile 与 daemon 入口：  
  `/home/ybyygu/.emacs-profiles.el`、`/home/ybyygu/.config/autostart/emacs.desktop`

下次接续时，先读两个 `AGENTS.md`，再按具体需求进入对应 `.el`；不要回到 note 里重新建立代码真源。

snippet 相关入口：

- 约定、缓存陷阱与展开校验：
  `/home/ybyygu/Install/configs/emacs/gwp-scratch/AGENTS.md`（snippets 约定、验证表、隐性知识三节）
- 提示词库本体：
  `/home/ybyygu/Install/configs/emacs/gwp-scratch/snippets/markdown-mode/`（19 个 `prompt-*` + `code`；无编译缓存，改完即生效）

## 更新记录

- **2026-09-11 10:03 建立**：所有权边界、AI 代管工作契约、fractal-docs 两层落地、输入法定位框架。
- **2026-09-11 11:40 增补 snippet 库一轮**：修复 4 个提示词被 yasnippet 静默损坏、确立转义约定、全库不留编译缓存、新增 `prompt-图文并茂`、库收拢（删 29 个不用的语法 snippet）、确立“展开 == 源”校验；输入法端到端验收闭环。

