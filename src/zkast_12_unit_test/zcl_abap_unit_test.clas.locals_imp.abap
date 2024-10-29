*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


CLASS ltc_abap_unit_test DEFINITION DEFERRED.                                   "Private Dependency Injection
CLASS zcl_abap_unit_test DEFINITION LOCAL FRIENDS ltc_abap_unit_test.


CLASS lcl_current_acc_constr_param DEFINITION.

  PUBLIC SECTION.
    INTERFACES:
      lif_current_acc_constr_param.
    METHODS:
      constructor
        IMPORTING
          iv_num TYPE int1.

  PRIVATE SECTION.
    DATA:
        mo_current_account TYPE REF TO zcl_current_acc_constr_param.

ENDCLASS.

CLASS lcl_current_acc_constr_param IMPLEMENTATION.

  METHOD constructor.
    mo_current_account = NEW zcl_current_acc_constr_param( iv_num ).
  ENDMETHOD.

  METHOD lif_current_acc_constr_param~get_current_balance.
    rv_balance = mo_current_account->get_current_balance( is_person_data ).
  ENDMETHOD.

ENDCLASS.


CLASS ltd_current_account DEFINITION.

  PUBLIC SECTION.
    INTERFACES:
      lif_current_acc_constr_param.

ENDCLASS.

CLASS ltd_current_account IMPLEMENTATION.

  METHOD lif_current_acc_constr_param~get_current_balance.
    rv_balance = 35.
  ENDMETHOD.

ENDCLASS.
