---
name: mm-module
description: SAP MM 模块开发经验 - 包含 BAPI 调用、增强点、代码模板
metadata:
  openclaw:
    emoji: 📦
    contributors: []
    last_updated: 2026-02-23
---

# MM 模块 Skill - 物料管理

## 📋 快速检索

- **BAPI**: 搜索 `BAPI_`
- **增强**: 搜索 `增强` 或 `User-Exit`
- **事务码**: 搜索 `TCode:`

---

## 🛒 采购管理 (Purchasing)

### BAPI_PO_CREATE1 - 创建采购订单

```abap
DATA: ls_poheader  TYPE bapimepoheader,
      ls_poheaderx TYPE bapimepoheaderx,
      lt_poitem    TYPE TABLE OF bapimepoitem,
      lt_poitemx   TYPE TABLE OF bapimepoitemx,
      lt_return    TYPE TABLE OF bapiret2.

" 设置抬头信息
ls_poheader-comp_code  = '1000'.
ls_poheader-doc_type   = 'NB'.
ls_poheader-vendor     = '100000'.
ls_poheader-purch_org  = '1000'.
ls_poheader-pur_group  = '001'.

" 设置行项目
APPEND VALUE #( po_item = '00010'
                material = 'MAT001'
                plant = '1000'
                quantity = 10 ) TO lt_poitem.

" 创建采购订单
CALL FUNCTION 'BAPI_PO_CREATE1'
  EXPORTING
    poheader  = ls_poheader
    poheaderx = ls_poheaderx
  TABLES
    poitem    = lt_poitem
    poitemx   = lt_poitemx
    return    = lt_return.

" 重要：必须提交事务！
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING
    wait = 'X'.
```

**业务场景**: 批量从外部系统导入采购订单  
**同事**: [待填写]  
**日期**: 2026-02-23  
**注意事项**:
- 必须调用 `BAPI_TRANSACTION_COMMIT` 提交事务
- 如果返回错误，需检查物料、供应商是否存在
- 数量字段注意单位换算

---

### BAPI_PO_CHANGE - 修改采购订单

[待补充]

---

## 📦 库存管理 (Inventory Management)

### BAPI_GOODSMVT_CREATE -  Goods Movement

```abap
DATA: ls_gm_header  TYPE bapi2017_gm_head_01,
      lt_gm_item    TYPE TABLE OF bapi2017_gm_item_create,
      ls_gm_item    TYPE bapi2017_gm_item_create,
      lt_return     TYPE TABLE OF bapiret2.

" 设置抬头
ls_gm_header-pstng_date = sy-datum.
ls_gm_header-doc_date   = sy-datum.
ls_gm_header-pr_uname   = sy-uname.

" 设置行项目（101 - 收货）
ls_gm_item-material   = 'MAT001'.
ls_gm_item-plant      = '1000'.
ls_gm_item-stge_loc   = '0001'.
ls_gm_item-batch      = 'BATCH001'.
ls_gm_item-move_type  = '101'.          " 收货
ls_gm_item-entry_qnt  = 100.
ls_gm_item-entry_uom  = 'EA'.
ls_gm_item-po_number  = '4500000001'.
ls_gm_item-po_item    = '10'.

APPEND ls_gm_item TO lt_gm_item.

" 创建物料凭证
CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
  EXPORTING
    goodsmvt_header  = ls_gm_header
    goodsmvt_code    = '01'              " 收货
  IMPORTING
    goodsmvt_headret = ls_headret
  TABLES
    goodsmvt_item    = lt_gm_item
    return           = lt_return.

" 提交事务
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
  EXPORTING
    wait = 'X'.
```

**业务场景**: 采购订单收货  
**移动类型**: 101 (GR收货), 102 (GR收货冲销), 261 (发货到订单)  
**同事**: [待填写]  
**日期**: 2026-02-23

---

## 🔧 增强点汇总

### MM01/MM02/MM03 物料主数据增强

**出口**: `EXIT_SAPLMGMU_001`  
**BAdI**: `BADI_MATERIAL_REF`  
**场景**: 自定义字段校验、自动填充默认值

[代码示例待补充]

---

### ME21N/ME22N/ME23N 采购订单增强

**BAdI**: `ME_PROCESS_PO_CUST`  
**场景**: 采购订单保存前校验、自动分配审批策略

```abap
METHOD if_ex_me_process_po_cust~process_item.
  " 示例：检查物料是否允许在当前工厂采购
  DATA: lv_material TYPE mara-matnr,
        lv_plant    TYPE marc-werks.

  im_item->get_data( IMPORTING es_data = ls_item ).
  lv_material = ls_item-material.
  lv_plant    = ls_item-plant.

  " 自定义检查逻辑...
ENDMETHOD.
```

**同事**: [待填写]  
**日期**: 2026-02-23

---

## 📝 代码模板

### 读取物料主数据

```abap
" 使用函数模块
CALL FUNCTION 'MATERIAL_READ_PLANT'
  EXPORTING
    matnr         = lv_material
    werks         = lv_plant
  IMPORTING
    marc          = ls_marc
  EXCEPTIONS
    material_not_found = 1
    plant_not_found    = 2
    OTHERS             = 3.

" 或直接查表
SELECT SINGLE * FROM mara INTO @DATA(ls_mara) WHERE matnr = @lv_material.
SELECT SINGLE * FROM marc INTO @DATA(ls_marc) WHERE matnr = @lv_material AND werks = @lv_plant.
SELECT SINGLE * FROM mard INTO @DATA(ls_mard) WHERE matnr = @lv_material AND werks = @lv_plant AND lgort = @lv_storage.
```

---

## 🔍 调试技巧

### 常用事务码

| 功能 | TCode |
|------|-------|
| 采购订单 | ME21N/ME22N/ME23N |
| 物料主数据 | MM01/MM02/MM03 |
| 库存总览 | MMBE |
| 物料凭证 | MB51 |
| BAPI 测试 | SE37 |

### Debug 技巧

- 在 BAPI 调用前设置断点
- 使用 ST05 跟踪数据库访问
- SE80 查看标准程序逻辑

---

## 🆕 最新更新

- 2026-02-23: 创建 MM 模块 Skill 框架
- [待补充]

---

*有问题？联系 MM 模块负责人*
