#!/usr/bin/env python3
"""
extract-skill.py - 从 ABAP 代码注释自动提取 SKILL 信息

使用方法:
  python3 extract-skill.py /path/to/abap/code /path/to/team-skills

功能:
  - 扫描代码文件中的 <SKILL>...</SKILL> 注释块
  - 提取模块、BAPI、场景、作者、日期等信息
  - 自动更新对应的 SKILL.md 文件

注释格式:
  * <SKILL>
  * MODULE: MM
  * BAPI: BAPI_PO_CREATE1
  * SCENARIO: 批量创建采购订单
  * AUTHOR: 张三
  * DATE: 2026-02-20
  * TIPS: 重要提示信息
  * </SKILL>
"""

import os
import re
import sys
import json
from datetime import datetime
from pathlib import Path

# SKILL 注释解析正则
SKILL_PATTERN = re.compile(
    r'\*\s*<SKILL>(.*?)\*\s*</SKILL>',
    re.DOTALL | re.IGNORECASE
)

# 字段解析
FIELD_PATTERN = re.compile(r'\*\s*(\w+):\s*(.+?)(?=\n|\r)')


def parse_skill_block(block_text):
    """解析 SKILL 注释块"""
    fields = {}
    for match in FIELD_PATTERN.finditer(block_text):
        key = match.group(1).upper()
        value = match.group(2).strip()
        fields[key] = value
    return fields


def scan_abap_files(directory):
    """扫描目录中的所有 ABAP 文件"""
    skill_entries = []
    
    for root, dirs, files in os.walk(directory):
        # 跳过非代码目录
        dirs[:] = [d for d in dirs if not d.startswith('.')]
        
        for filename in files:
            if filename.endswith(('.abap', '.txt')):
                filepath = os.path.join(root, filename)
                try:
                    with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                    
                    # 查找所有 SKILL 块
                    for match in SKILL_PATTERN.finditer(content):
                        block = match.group(1)
                        skill_data = parse_skill_block(block)
                        if skill_data:
                            skill_data['source_file'] = filepath
                            skill_data['extracted_at'] = datetime.now().isoformat()
                            skill_entries.append(skill_data)
                            
                except Exception as e:
                    print(f"警告: 无法读取文件 {filepath}: {e}")
    
    return skill_entries


def update_skill_md(skill_entries, team_skills_dir):
    """更新 SKILL.md 文件"""
    
    # 按模块分组
    modules = {}
    for entry in skill_entries:
        module = entry.get('MODULE', 'UNKNOWN').upper()
        if module not in modules:
            modules[module] = []
        modules[module].append(entry)
    
    for module, entries in modules.items():
        skill_dir = os.path.join(team_skills_dir, f"{module.lower()}-module")
        skill_file = os.path.join(skill_dir, "SKILL.md")
        
        # 确保目录存在
        os.makedirs(skill_dir, exist_ok=True)
        
        # 读取现有内容（如果存在）
        existing_content = ""
        if os.path.exists(skill_file):
            with open(skill_file, 'r', encoding='utf-8') as f:
                existing_content = f.read()
        
        # 生成新的条目内容
        new_entries_md = []
        for entry in entries:
            bapi = entry.get('BAPI', '')
            scenario = entry.get('SCENARIO', '')
            author = entry.get('AUTHOR', 'Unknown')
            date = entry.get('DATE', datetime.now().strftime('%Y-%m-%d'))
            tips = entry.get('TIPS', '')
            
            md_entry = f"""
### {bapi} - {scenario}

**业务场景**: {scenario}  
**同事**: {author}  
**日期**: {date}  
**注意事项**: {tips}

**来源文件**: `{entry.get('source_file', 'N/A')}`

---
"""
            # 检查是否已存在（基于 BAPI 名）
            if bapi and bapi not in existing_content:
                new_entries_md.append(md_entry)
        
        if new_entries_md:
            # 追加到文件
            with open(skill_file, 'a', encoding='utf-8') as f:
                f.write('\n## 🔄 自动提取条目\n')
                f.write(f'\n*更新时间: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}*\n')
                for entry_md in new_entries_md:
                    f.write(entry_md)
            
            print(f"✅ 已更新 {skill_file} ({len(new_entries_md)} 个新条目)")
        else:
            print(f"ℹ️  {skill_file} 无新条目")


def generate_index(team_skills_dir):
    """生成索引文件"""
    index = {
        "generated_at": datetime.now().isoformat(),
        "modules": []
    }
    
    for item in os.listdir(team_skills_dir):
        item_path = os.path.join(team_skills_dir, item)
        if os.path.isdir(item_path) and item.endswith('-module'):
            module_name = item.replace('-module', '')
            skill_file = os.path.join(item_path, "SKILL.md")
            
            if os.path.exists(skill_file):
                with open(skill_file, 'r', encoding='utf-8') as f:
                    content = f.read()
                
                # 统计 BAPI 数量
                bapi_count = len(re.findall(r'BAPI[_\w]+', content))
                
                index["modules"].append({
                    "name": module_name,
                    "path": item,
                    "bapi_count": bapi_count,
                    "skill_file": f"{item}/SKILL.md"
                })
    
    index_file = os.path.join(team_skills_dir, "index.json")
    with open(index_file, 'w', encoding='utf-8') as f:
        json.dump(index, f, indent=2, ensure_ascii=False)
    
    print(f"✅ 已生成索引: {index_file}")


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        print(f"\n用法: {sys.argv[0]} <abap_code_directory> <team_skills_directory>")
        sys.exit(1)
    
    abap_dir = sys.argv[1]
    skills_dir = sys.argv[2]
    
    if not os.path.exists(abap_dir):
        print(f"错误: ABAP 代码目录不存在: {abap_dir}")
        sys.exit(1)
    
    print(f"🔍 扫描目录: {abap_dir}")
    print(f"📝 更新目标: {skills_dir}")
    print()
    
    # 提取 SKILL 条目
    entries = scan_abap_files(abap_dir)
    print(f"📊 找到 {len(entries)} 个 SKILL 条目")
    
    if entries:
        # 更新 SKILL 文件
        update_skill_md(entries, skills_dir)
        
        # 生成索引
        generate_index(skills_dir)
        
        print("\n🎉 完成!")
    else:
        print("\nℹ️ 未找到 SKILL 条目，请检查代码中的注释格式")


if __name__ == '__main__':
    main()
