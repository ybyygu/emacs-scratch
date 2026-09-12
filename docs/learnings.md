# 隐性知识（learnings）

> 版本：V1.0 ｜ 更新：2026-09-12 ｜ 创建：2026-09-12
> 角色：本项目**经验库正本**——会改变后续排错或实现选择的可迁移规则；每条按「现象 → 真相 → 错误后果」写，让读者能辨认适用情境与踩坑代价
> 关联：[AGENTS.md](../AGENTS.md)（项目入口，只放速查摘要）｜ [user-lisp/AGENTS.md](../user-lisp/AGENTS.md)（目录级局部陷阱另在其蓝图里，不进本文件）

## 1. batch 环境测不到 org / note

- **现象**：batch 下加载 `init.el` 一切正常，但改动的 org 配置毫无反应。
- **真相**：无 X 时 `init-no-x-flag` 为 t，`init-org`、`init-note` 整块被跳过。
- **错误后果**：把"没测到"当成"没问题"。

## 2. batch 加载会启动 server

- **现象**：`emacs --batch` 加载 `init.el` 时打印 "Starting a server..."。
- **真相**：`init-core.el` 末尾有 `(unless (server-running-p) (server-start))`。
- **错误后果**：与日用 daemon 抢 socket；测试需另指定 `server-name`。

## 3. snippet 改了或新增了却不出现

- **现象**：编辑或新增 `snippets/**` 后，`C-.` 的候选列表里还是旧内容，新文件完全不出现。
- **真相**：目录下若有 `.yas-compiled-snippets.el`，yasnippet 无条件 `load` 它、不比对 mtime；缓存只由 `M-x yas-recompile-all` 生成，不会自动重建；已运行的实例还要 `M-x yas-reload-all` 才会重读目录。
- **错误后果**：把“缓存陈旧”误判为格式写错、键位不对，或以为编辑没保存。

## 4. 改动只在重启后生效

- **现象**：改完 `.el`，日用 Emacs 里没有变化。
- **真相**：daemon 已加载旧版本代码。
- **错误后果**：重复修改或误判失败。

## 5. 输入法行为分三层，改之前先定位

- **现象**：改了“输入法”的设置没效果，或不知道该改哪里。
- **真相**：① RIME 数据目录 `~/.local/share/fcitx5/rime/`（**在仓库外**，fcitx5 与 emacs-rime 共用，改完要重新部署）；② emacs-rime 的 predicate 与按键（`user-lisp/init-ui.el`，决定“何时自动切英文”）；③ 跑的是哪份配置：`~/.emacs-profiles.el` 的 profile + `gwp` socket daemon（当前由 `~/.config/autostart/emacs.desktop` → systemd `app-emacs@autostart.service` 拉起）。该 RIME 方案的 `ascii_mode` 只有“中文”一个状态，所以 Emacs 里感受到的“自动中英文切换”实际由 ② 决定。
- **错误后果**：去 RIME 侧改 `switch_key` 想解决 Emacs 里的行为；或改完 `.el` 忘了重启 daemon，以为没生效。

## 6. 升级包之后 vterm / rime 打不开

- **现象**：`M-x vterm` 弹 `Vterm needs 'vterm-module' to work. Compile it now?`（batch 里变成 `end-of-file during reading stdin`，清单 V-01 会红）；或中文输入起不来。
- **真相**：`vterm-module.so`、`librime-emacs.so` 是**机器本地编译产物，不在包内容里** —— 包一升级，版本目录换新，模块就留在了旧目录里。重编要在**新版包目录**里做：`mkdir -p build && cd build && cmake -G 'Unix Makefiles' -DUSE_SYSTEM_LIBVTERM=ON .. && make`（系统有 `libvterm` 与头文件，不联网，产物落在包根）；rime 走它自己的 `make lib`。
- **错误后果**：把"包升级弄坏了配置"当真去翻 `.el`；或者清理旧版本目录时，把还没重编的那份模块一起删了。dev 轨与日用轨的包树各有一份模块，**升级后各自都要重编**。

## 7. 不要在源码目录留 `.elc`

- **现象**：编辑 `.el` 后行为仍是旧的。
- **真相**：同名 `.elc` 会被优先加载（当前仓库内没有任何 `.elc`）。
- **错误后果**：隐式状态遮蔽真源；编译验证请输出到 `/tmp`。

## 8. 仓库位于同步区内

- **现象**：仓库根出现 `*.sync-conflict-*` 文件（最近一次 2026-01-27）。
- **真相**：`~/Install` 由 syncthing 共享给 3 台设备，`elpa/`、`eln-cache/`、`history`、`recentf` 等运行态也在其中。
- **错误后果**：把动态产物写进仓库会引发跨机冲突；清理冲突文件前先确认差异。

## 9. 密钥与外部数据在仓库外

- **现象**：gptel 等配置可用，但仓库里搜不到 key。
- **真相**：密钥统一放在 `~/Install/configs/llms/*.txt`；仓库根的 `english-words.txt`（370,105 词，已入库）被 `init-completion.el` 当作 ispell 备用词典引用。
- **错误后果**：误删根目录数据文件会破坏补全；新增密钥不要写进 `.el`。
