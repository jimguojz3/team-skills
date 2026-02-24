#!/usr/bin/env python3
"""
analyze-abap.py - 智能分析ABAP代码，自动生成Skill

功能：
  - 无需人工注释，自动识别代码模式
  - 支持BAPI/Function调用提取
  - 识别增强点（User Exit, BAdI）
  - 检测标准表访问模式
  - 生成可搜索的Skill文档

用法：
  python3 analyze-abap.py /path/to/abap/code /path/to/team-skills

支持的代码来源：
  - Eclipse ADT 本地项目
  - abapGit 克隆的仓库
  - VS Code ABAP 本地文件
"""

import os
import re
import sys
import json
from datetime import datetime
from pathlib import Path
from collections import defaultdict

# ABAP代码分析规则
PATTERNS = {
    # BAPI调用
    'bapi': re.compile(
        r"CALL\s+FUNCTION\s+['\"](BAPI_\w+)['\"]",
        re.IGNORECASE
    ),
    
    # 普通Function Module
    'function': re.compile(
        r"CALL\s+FUNCTION\s+['\"](\w+)['\"]",
        re.IGNORECASE
    ),
    
    # 增强点 - User Exit
    'user_exit': re.compile(
        r"CALL\s+CUSTOMER-FUNCTION\s+['\"]?(\w+)['\"]?",
        re.IGNORECASE
    ),
    
    # BAdI调用
    'badi': re.compile(
        r"GET\s+BADI\s+(\w+)",
        re.IGNORECASE
    ),
    
    # 标准表访问
    'table_access': re.compile(
        r"SELECT\s+.*\s+FROM\s+(mara|marc|mard|vbak|vbap|kna1|lfa1|ekko|ekpo|bkpf|bsid)\s",
        re.IGNORECASE
    ),
    
    # 事务码调用
    'transaction': re.compile(
        r"CALL\s+TRANSACTION\s+['\"](\w+)['\"]",
        re.IGNORECASE
    ),
    
    # ALV相关
    'alv': re.compile(
        r"REUSE_ALV|CL_SALV|CL_GUI_ALV",
        re.IGNORECASE
    ),
    
    # OData/RFC
    'odata': re.compile(
        r"/IWBEP/|CL_SADL|IF_SADL",
        re.IGNORECASE
    ),
    
    # 程序信息
    'program_info': re.compile(
        r"PROGRAM\s+(\w+)|REPORT\s+(\w+)",
        re.IGNORECASE
    ),
    
    # 类定义
    'class_def': re.compile(
        r"CLASS\s+(\w+)\s+DEFINITION",
        re.IGNORECASE
    ),
}

# 模块映射表（根据代码内容推断模块）
MODULE_MAPPING = {
    'MM': ['MARA', 'MARC', 'MARD', 'MAKT', 'EKKO', 'EKPO', 'EKET', 'BAPI_PO', 
           'BAPI_GOODSMVT', 'ME21N', 'ME22N', 'ME23N', 'MB51'],
    'SD': ['VBAK', 'VBAP', 'VBKD', 'VBFA', 'KNA1', 'KNVV', 'BAPI_SALESORDER',
           'VA01', 'VA02', 'VA03', 'VL01N'],
    'PP': ['AUFK', 'AFKO', 'AFPO', 'RESB', 'BAPI_PRODORD'],
    'FI': ['BKPF', 'BSEG', 'BSID', 'BSAD', 'BAPI_ACC'],
    'CO': ['COSP', 'COEP', 'BAPI_COST'],
    'HR': ['PA0000', 'PA0001', 'HR_INFOTYPE'],
}


def infer_module(code_content, bapi_list, table_list):
    """根据代码内容推断所属模块"""
    scores = defaultdict(int)
    content_upper = code_content.upper()
    
    # 根据BAPI推断
    for bapi in bapi_list:
        bapi_upper = bapi.upper()
        for module, keywords in MODULE_MAPPING.items():
            if any(kw in bapi_upper for kw in keywords):
                scores[module] += 3
    
    # 根据表名推断
    for table in table_list:
        table_upper = table.upper()
        for module, keywords in MODULE_MAPPING.items():
            if any(kw == table_upper for kw in keywords):
                scores[module] += 2
    
    # 返回最高分的模块，如果没有则返回UNKNOWN
    if scores:
        return max(scores.items(), key=lambda x: x[1])[0]
    return 'UNKNOWN'


def extract_code_snippet(lines, start_line, context=5):
    """提取代码片段（带上下文）"""
    start = max(0, start_line - context)
    end = min(len(lines), start_line + context + 1)
    return '\n'.join(lines[start:end])


def analyze_abap_file(filepath):
    """分析单个ABAP文件"""
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            content = f.read()
            lines = content.split('\n')
    except Exception as e:
        print(f"警告: 无法读取文件 {filepath}: {e}")
        return None
    
    result = {
        'filename': os.path.basename(filepath),
        'filepath': filepath,
        'program_name': None,
        'class_name': None,
        'module': 'UNKNOWN',
        'bapis': [],
        'functions': [],
        'user_exits': [],
        'badis': [],
        'tables': [],
        'transactions': [],
        'uses_alv': False,
        'uses_odata': False,
        'snippets': [],
        'line_count': len(lines),
    }
    
    # 提取程序/类名
    prog_match = PATTERNS['program_info'].search(content)
    if prog_match:
        result['program_name'] = prog_match.group(1) or prog_match.group(2)
    
    class_match = PATTERNS['class_def'].search(content)
    if class_match:
        result['class_name'] = class_match.group(1)
    
    # 查找BAPI调用
    for match in PATTERNS['bapi'].finditer(content):
        bapi_name = match.group(1)
        line_num = content[:match.start()].count('\n')
        snippet = extract_code_snippet(lines, line_num)
        result['bapis'].append({
            'name': bapi_name,
            'line': line_num + 1,
            'snippet': snippet
        })
    
    # 查找Function Module
    for match in PATTERNS['function'].finditer(content):
        func_name = match.group(1)
        if not func_name.startswith('BAPI_'):
            line_num = content[:match.start()].count('\n')
            result['functions'].append({
                'name': func_name,
                'line': line_num + 1
            })
    
    # 查找User Exit
    for match in PATTERNS['user_exit'].finditer(content):
        exit_name = match.group(1)
        line_num = content[:match.start()].count('\n')
        snippet = extract_code_snippet(lines, line_num)
        result['user_exits'].append({
            'name': exit_name,
            'line': line_num + 1,
            'snippet': snippet
        })
    
    # 查找BAdI
    for match in PATTERNS['badi'].finditer(content):
        badi_name = match.group(1)
        line_num = content[:match.start()].count('\n')
        result['badis'].append({
            'name': badi_name,
            'line': line_num + 1
        })
    
    # 查找标准表访问
    for match in PATTERNS['table_access'].finditer(content):
        table_name = match.group(1).upper()
        if table_name not in result['tables']:
            result['tables'].append(table_name)
    
    # 查找事务码
    for match in PATTERNS['transaction'].finditer(content):
        result['transactions'].append(match.group(1))
    
    # 检测ALV使用
    result['uses_alv'] = bool(PATTERNS['alv'].search(content))
    
    # 检测OData使用
    result['uses_odata'] = bool(PATTERNS['odata'].search(content))
    
    # 推断模块
    bapi_names = [b['name'] for b in result['bapis']]
    result['module'] = infer_module(content, bapi_names, result['tables'])
    
    return result


def scan_abap_directory(directory):
    """扫描目录中的所有ABAP文件"""
    results = []
    abap_extensions = ['.abap', '.txt', '.sap']  # ABAP文件扩展名
    
    for root, dirs, files in os.walk(directory):
        # 跳过隐藏目录和node_modules等
        dirs[:] = [d for d in dirs if not d.startswith('.') and d not in ['node_modules']]
        
        for filename in files:
            # 检查扩展名或文件名模式
            if any(filename.endswith(ext) for ext in abap_extensions) or \
               filename.startswith('Z') or filename.startswith('Y') or \
               'BAPI' in filename.upper():
                filepath = os.path.join(root, filename)
                result = analyze_abap_file(filepath)
                if result and (result['bapis'] or result['user_exits'] or 
                              result['tables'] or result['badis']):
                    results.append(result)
    
    return results


def generate_skill_md(analysis_results, team_skills_dir):
    """生成Skill Markdown文件"""
    
    # 按模块分组
    by_module = defaultdict(list)
    for result in analysis_results:
        module = result['module']
        by_module[module].append(result)
    
    for module, programs in by_module.items():
        if module == 'UNKNOWN':
            module = 'ABAP-COMMON'  # 未知模块归入通用
        
        skill_dir = os.path.join(team_skills_dir, f"{module.lower()}-module")
        os.makedirs(skill_dir, exist_ok=True)
        
        skill_file = os.path.join(skill_dir, "SKILL.md")
        
        # 读取现有内容（保留手动添加的部分）
        existing_content = ""
        if os.path.exists(skill_file):
            with open(skill_file, 'r', encoding='utf-8') as f:
                existing_content = f.read()
        
        # 如果没有现有内容，创建基础结构
        if not existing_content:
            existing_content = f"""---
name: {module.lower()}-module
description: SAP {module} 模块开发经验 - 自动分析生成
metadata:
  openclaw:
    emoji: 📦
    contributors: []
    last_updated: {datetime.now().strftime('%Y-%m-%d')}
---

# {module} 模块 Skill

## 📋 说明

本文档由自动分析脚本生成，分析源代码中的BAPI调用、表访问、增强点等。

---

"""
        
        # 生成自动分析部分
        auto_content = f"""
## 🤖 自动分析结果

*分析时间: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}*  
*分析文件数: {len(programs)}*

---

"""
        
        # 收集所有BAPI
        all_bapis = {}
        for prog in programs:
            for bapi in prog['bapis']:
                bapi_name = bapi['name']
                if bapi_name not in all_bapis:
                    all_bapis[bapi_name] = {
                        'programs': [],
                        'snippet': bapi['snippet']
                    }
                all_bapis[bapi_name]['programs'].append(prog['filename'])
        
        if all_bapis:
            auto_content += "### 📞 BAPI调用\n\n"
            for bapi_name, info in sorted(all_bapis.items()):
                auto_content += f"#### {bapi_name}\n\n"
                auto_content += f"**使用位置**: {', '.join(set(info['programs']))}\n\n"
                auto_content += f"**代码示例**:\n```abap\n{info['snippet']}\n```\n\n"
                auto_content += "---\n\n"
        
        # 收集所有增强点
        all_enhancements = []
        for prog in programs:
            for exit_info in prog['user_exits']:
                all_enhancements.append({
                    'type': 'User Exit',
                    'name': exit_info['name'],
                    'program': prog['filename'],
                    'snippet': exit_info['snippet']
                })
            for badi_info in prog['badis']:
                all_enhancements.append({
                    'type': 'BAdI',
                    'name': badi_info['name'],
                    'program': prog['filename'],
                    'snippet': None
                })
        
        if all_enhancements:
            auto_content += "### 🔧 增强点\n\n"
            for enh in all_enhancements:
                auto_content += f"#### {enh['name']} ({enh['type']})\n\n"
                auto_content += f"**使用位置**: {enh['program']}\n\n"
                if enh['snippet']:
                    auto_content += f"**代码示例**:\n```abap\n{enh['snippet']}\n```\n\n"
                auto_content += "---\n\n"
        
        # 收集标准表访问
        all_tables = set()
        for prog in programs:
            all_tables.update(prog['tables'])
        
        if all_tables:
            auto_content += "### 📊 标准表访问\n\n"
            auto_content += "| 表名 | 说明 |\n"
            auto_content += "|------|------|\n"
            table_descriptions = {
                'MARA': '物料主数据 - 基本视图',
                'MARC': '物料主数据 - 工厂视图',
                'MARD': '物料主数据 - 库存视图',
                'MAKT': '物料描述',
                'EKKO': '采购订单抬头',
                'EKPO': '采购订单行项目',
                'VBAK': '销售订单抬头',
                'VBAP': '销售订单行项目',
                'KNA1': '客户主数据 - 基本视图',
                'LFA1': '供应商主数据',
                'BKPF': '会计凭证抬头',
                'BSEG': '会计凭证行项目',
                'BSID': '客户未清项',
            }
            for table in sorted(all_tables):
                desc = table_descriptions.get(table, '')
                auto_content += f"| {table} | {desc} |\n"
            auto_content += "\n---\n\n"
        
        # 程序清单
        auto_content += "### 📁 分析的程序清单\n\n"
        auto_content += "| 程序名 | 类型 | BAPI数 | 表访问 | ALV | OData |\n"
        auto_content += "|--------|------|--------|--------|-----|-------|\n"
        for prog in sorted(programs, key=lambda x: x['filename']):
            prog_name = prog['program_name'] or prog['class_name'] or prog['filename']
            prog_type = '类' if prog['class_name'] else '程序'
            auto_content += f"| {prog_name} | {prog_type} | {len(prog['bapis'])} | {len(prog['tables'])} | {'✓' if prog['uses_alv'] else ''} | {'✓' if prog['uses_odata'] else ''} |\n"
        
        # 合并内容（在标记之间插入自动分析部分）
        if '## 🤖 自动分析结果' in existing_content:
            # 替换现有自动分析部分
            parts = existing_content.split('## 🤖 自动分析结果')
            manual_part = parts[0]
            after_auto = ''
            if len(parts) > 1:
                # 找到下一个标题
                remainder = parts[1]
                next_heading = re.search(r'\n## ', remainder)
                if next_heading:
                    after_auto = remainder[next_heading.start():]
            new_content = manual_part + auto_content + after_auto
        else:
            # 追加到文件末尾
            new_content = existing_content + auto_content
        
        # 写入文件
        with open(skill_file, 'w', encoding='utf-8') as f:
            f.write(new_content)
        
        print(f"✅ 已更新 {skill_file}")


def generate_index(analysis_results, team_skills_dir):
    """生成索引文件"""
    index = {
        "generated_at": datetime.now().isoformat(),
        "total_files": len(analysis_results),
        "modules": {},
        "summary": {
            "total_bapis": 0,
            "total_enhancements": 0,
            "total_tables": set(),
        }
    }
    
    for result in analysis_results:
        module = result['module']
        if module not in index["modules"]:
            index["modules"][module] = {
                "file_count": 0,
                "bapis": [],
                "tables": [],
                "enhancements": []
            }
        
        mod_info = index["modules"][module]
        mod_info["file_count"] += 1
        mod_info["bapis"].extend([b['name'] for b in result['bapis']])
        mod_info["tables"].extend(result['tables'])
        mod_info["enhancements"].extend([e['name'] for e in result['user_exits']])
        mod_info["enhancements"].extend([b['name'] for b in result['badis']])
        
        # 汇总统计
        index["summary"]["total_bapis"] += len(result['bapis'])
        index["summary"]["total_enhancements"] += len(result['user_exits']) + len(result['badis'])
        index["summary"]["total_tables"].update(result['tables'])
    
    # 去重并转换set为list
    index["summary"]["total_tables"] = list(index["summary"]["total_tables"])
    for module in index["modules"]:
        index["modules"][module]["bapis"] = list(set(index["modules"][module]["bapis"]))
        index["modules"][module]["tables"] = list(set(index["modules"][module]["tables"]))
        index["modules"][module]["enhancements"] = list(set(index["modules"][module]["enhancements"]))
    
    index_file = os.path.join(team_skills_dir, "index.json")
    with open(index_file, 'w', encoding='utf-8') as f:
        json.dump(index, f, indent=2, ensure_ascii=False)
    
    print(f"✅ 已生成索引: {index_file}")
    return index


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        print(f"\n用法: {sys.argv[0]} <abap_code_directory> <team_skills_directory>")
        print(f"\n示例:")
        print(f"  # 分析 Eclipse ADT 项目")
        print(f"  python3 {sys.argv[0]} ~/eclipse-workspace/ERP_CORE ./team-skills")
        print(f"\n  # 分析 abapGit 仓库")
        print(f"  python3 {sys.argv[0]} ~/abap-projects/erp-code ./team-skills")
        sys.exit(1)
    
    abap_dir = sys.argv[1]
    skills_dir = sys.argv[2]
    
    if not os.path.exists(abap_dir):
        print(f"❌ 错误: ABAP代码目录不存在: {abap_dir}")
        sys.exit(1)
    
    print(f"🔍 扫描ABAP代码: {abap_dir}")
    print(f"📝 输出Skill目录: {skills_dir}")
    print()
    
    # 分析所有文件
    results = scan_abap_directory(abap_dir)
    print(f"📊 找到 {len(results)} 个有价值的程序")
    
    if results:
        # 按模块统计
        module_stats = defaultdict(int)
        for r in results:
            module_stats[r['module']] += 1
        
        print("\n📦 模块分布:")
        for module, count in sorted(module_stats.items()):
            print(f"  {module}: {count} 个程序")
        
        print()
        
        # 生成Skill文档
        generate_skill_md(results, skills_dir)
        
        # 生成索引
        index = generate_index(results, skills_dir)
        
        print(f"\n🎉 分析完成!")
        print(f"   - 总程序数: {len(results)}")
        print(f"   - BAPI调用: {index['summary']['total_bapis']}")
        print(f"   - 增强点: {index['summary']['total_enhancements']}")
        print(f"   - 涉及表: {len(index['summary']['total_tables'])}")
    else:
        print("\nℹ️ 未找到可分析的ABAP代码")
        print("提示: 确保目录中包含.abap/.txt文件，或文件名以Z/Y开头")


if __name__ == '__main__':
    main()
