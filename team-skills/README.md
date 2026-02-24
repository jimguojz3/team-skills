# team-skills - SAP ABAP 团队知识库

团队共享的 OpenClaw Skills，沉淀日常开发经验，支持智能检索和持续积累。

## 🎯 核心目标

- ✅ **知识沉淀**：同事的经验自动留档，新人快速上手
- ✅ **即查即用**：通过自然语言查询获取代码模板、BAPI调用、增强点
- ✅ **持续完善**：日常工作即贡献，无需额外维护成本
- ✅ **团队共享**：统一的 Skill 仓库，大家共同维护

---

## 📁 目录结构

```
team-skills/
├── README.md                 # 本文件
├── extract-skill.py          # 自动提取脚本
├── index.json                # 自动生成的索引
├── mm-module/                # MM 物料管理模块
│   ├── SKILL.md              # 主技能文档
│   └── enhancements/         # 增强点汇总
├── sd-module/                # SD 销售分销模块
│   └── SKILL.md
├── pp-module/                # PP 生产计划模块
│   └── SKILL.md
└── abap-common/              # ABAP 通用技巧
    ├── SKILL.md
    └── templates/            # 代码模板
```

---

## 🔄 两种贡献方式

### 方式 1：代码注释（推荐 ⭐）

在 ABAP 代码中添加标准注释，定期运行脚本自动提取：

```abap
* <SKILL>
* MODULE: MM
* BAPI: BAPI_PO_CREATE1
* SCENARIO: 批量创建采购订单
* AUTHOR: 张三
* DATE: 2026-02-20
* TIPS: 创建后必须调用 BAPI_TRANSACTION_COMMIT
* </SKILL>
```

**提取命令**（可加入 CI/CD 或定期执行）：
```bash
python3 team-skills/extract-skill.py /path/to/abap/code /path/to/team-skills
```

### 方式 2：直接编辑

直接修改对应模块的 `SKILL.md` 文件，提交 Git PR：

```bash
git clone <repo-url>
cd team-skills
# 编辑 mm-module/SKILL.md
git add .
git commit -m "添加 MM 模块 BAPI_PO_CHANGE 示例"
git push origin main
```

---

## 🚀 使用方式

### 在 OpenClaw 中引用

配置 OpenClaw 加载团队 Skill：

```bash
# 编辑 ~/.openclaw/openclaw.json
{
  "commands": {
    "nativeSkills": [
      "team-skills/mm-module",
      "team-skills/sd-module",
      "team-skills/abap-common"
    ]
  }
}
```

### 查询示例

然后可以直接问 OpenClaw：

```
"MM 模块怎么创建采购订单？"
"SD 模块有哪些常用 BAPI？"
"ALV 报表的标准模板是什么？"
"ME21N 采购订单有什么增强点？"
```

### 直接阅读文档

也可以直接查看 Markdown 文件：
- `mm-module/SKILL.md` - 物料管理模块知识
- `abap-common/SKILL.md` - ABAP 通用技巧

---

## 🏗️ 添加新模块

创建新模块的步骤：

1. 创建目录结构：
```bash
mkdir -p team-skills/xx-module/enhancements
touch team-skills/xx-module/SKILL.md
```

2. 使用模板创建 SKILL.md：
```markdown
---
name: xx-module
description: XX 模块开发经验
metadata:
  openclaw:
    emoji: 🏭
---

# XX 模块

## 常用 BAPI
[待补充]

## 增强点
[待补充]

## 代码模板
[待补充]
```

3. 更新 README.md 中的目录结构

---

## 👥 团队协作流程

```
同事开发完成
    │
    ├── 代码注释方式 ──> 添加 <SKILL> 注释 ──> 提交代码
    │                                            │
    ├── 直接编辑方式 ──> 修改 SKILL.md ───> Git PR ─┤
                                                     │
                                              CI/CD 自动提取
                                                     │
                                              更新团队 Skill 库
                                                     │
                                              全员可用（OpenClaw/文档）
```

---

## 📝 注释规范

### SKILL 注释块格式

```abap
* <SKILL>
* MODULE:      MM|SD|PP|FI|CO|...           (模块名，必填)
* BAPI:        BAPI_XXX_XXX                 (BAPI/函数名，可选)
* SCENARIO:    一句话描述业务场景            (场景描述，必填)
* AUTHOR:      作者姓名                      (作者，可选)
* DATE:        YYYY-MM-DD                   (日期，可选)
* TIPS:        重要提示/注意事项              (提示，可选)
* CODE:        关键代码片段                  (代码，可选)
* </SKILL>
```

### 字段说明

| 字段 | 必填 | 说明 |
|------|------|------|
| MODULE | ✅ | 模块代码：MM/SD/PP/FI/CO/ABAP |
| BAPI | ❌ | 如果有 BAPI/函数调用，填写名称 |
| SCENARIO | ✅ | 业务场景，一句话描述 |
| AUTHOR | ❌ | 贡献者姓名 |
| DATE | ❌ | 日期，格式 YYYY-MM-DD |
| TIPS | ❌ | 注意事项、坑点提示 |
| CODE | ❌ | 关键代码片段（简化版） |

---

## 🔧 工具脚本

### extract-skill.py

自动从 ABAP 代码中提取 SKILL 注释，更新到对应模块。

```bash
# 基本用法
python3 extract-skill.py /path/to/abap/code /path/to/team-skills

# 示例
python3 extract-skill.py ~/abap-projects/erp-core ./team-skills
```

### 集成到 CI/CD

`.github/workflows/extract-skills.yml`:
```yaml
name: Extract Skills
on:
  push:
    paths:
      - '**.abap'

jobs:
  extract:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Extract Skills
        run: python3 team-skills/extract-skill.py ./src ./team-skills
      - name: Commit Changes
        run: |
          git config user.name "GitHub Actions"
          git config user.email "actions@github.com"
          git add team-skills/
          git diff --quiet && git diff --staged --quiet || git commit -m "Auto-extract skills"
          git push
```

---

## 🆕 更新日志

- 2026-02-23: 创建团队 Skill 框架，包含 MM 模块、ABAP 通用技巧
- 2026-02-23: 添加自动提取脚本 extract-skill.py

---

## 🤝 贡献指南

1. **Fork** 本仓库
2. **添加 SKILL**：代码注释或直接编辑
3. **提交 PR**：简要说明添加的内容
4. **Review**：团队维护者审核合并

---

## 📞 联系方式

- 技术问题：OpenClaw Chat
- 内容贡献：GitHub PR

---

*让知识流动起来，让开发更高效！* 🚀
