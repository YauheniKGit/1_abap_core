CLASS zcl_abap_unit_test DEFINITION PUBLIC CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          iif_kast_account   TYPE REF TO zif_kast_account OPTIONAL
          io_current_account TYPE REF TO zcl_current_account OPTIONAL,

      calc_value                                                                                  " for Simple Example
        IMPORTING
          iv_param_a TYPE int8
          iv_param_b TYPE int8 OPTIONAL
        EXPORTING
          ev_result  TYPE int8,

      get_full_balance_ext_meth_w_if                                                              " for OO TDF w IF
        IMPORTING
                  is_person_data   TYPE zif_store_constants_types=>ty_person_data
        RETURNING VALUE(rv_result) TYPE string,

      call_ext_meth_wo_if                                                                         " for OO TDF w/o IF
        IMPORTING
                  is_person_data   TYPE zif_store_constants_types=>ty_person_data
        RETURNING VALUE(rv_result) TYPE int8,

      call_ext_meth_wo_if_constr_par                                                                 " for w/o IF factory
        IMPORTING
                  is_person_data   TYPE zif_store_constants_types=>ty_person_data
        RETURNING VALUE(rv_result) TYPE int8.

  PRIVATE SECTION.

    DATA:
      mif_kast_account             TYPE REF TO zif_kast_account,
      mo_current_account           TYPE REF TO zcl_current_account,
      mif_current_acc_constr_param TYPE REF TO lif_current_acc_constr_param.


    METHODS:
      select_data,
      " for SQL TDF, private dependency injection
      modify_cds                                                                                  " for CDS TDF, private dependency injection
        RETURNING VALUE(rv_result) TYPE string,

      authorization_check                                                                        " For Authorizations Test
        RETURNING VALUE(rv_is_authorized) TYPE abap_bool,

      get_input                                                                                  " for TSET-SEAM
        RETURNING VALUE(rv_input) TYPE string.

ENDCLASS.

CLASS zcl_abap_unit_test IMPLEMENTATION.

  METHOD constructor.

    me->mif_kast_account = iif_kast_account.

    IF io_current_account IS BOUND.
      me->mo_current_account = io_current_account.
    ELSE.
      me->mo_current_account = NEW #( 1 ).
    ENDIF.

  ENDMETHOD.

  METHOD get_full_balance_ext_meth_w_if.

    DATA(rv_result_ext) = mif_kast_account->view_balances( is_person_data = is_person_data ).

    rv_result = rv_result_ext + 5.

  ENDMETHOD.

  METHOD calc_value.

    IF iv_param_b IS NOT INITIAL.
      ev_result = iv_param_a * iv_param_b.
    ELSE.
      ev_result = iv_param_a * iv_param_a.
    ENDIF.

  ENDMETHOD.

  METHOD select_data.

    SELECT * FROM zkast_cc_park
      WHERE capacity > 20
      INTO TABLE @DATA(parkings).

    LOOP AT parkings ASSIGNING FIELD-SYMBOL(<fs_parking>).
      <fs_parking>-lchg_uname = sy-uname.
    ENDLOOP.

    MODIFY zkast_cc_park FROM TABLE @parkings.

  ENDMETHOD.

  METHOD modify_cds.

*    SELECT * FROM zsht_cc_i_park
*      INTO TABLE @DATA(parkings).
*
*    LOOP AT parkings ASSIGNING FIELD-SYMBOL(<fs_parking>) .
*      IF <fs_parking>-parkstatus = 'O'.
*        rv_result = |Open Parking No { <fs_parking>-parkid } Found|.
*        RETURN.
*      ENDIF.
*    ENDLOOP.

    rv_result = 'Open Parking NOT Found'.

  ENDMETHOD.

  METHOD call_ext_meth_wo_if.

    DATA(lv_res) = mo_current_account->get_current_balance( is_person_data ).

    rv_result = lv_res + 5.

  ENDMETHOD.

  METHOD call_ext_meth_wo_if_constr_par.

    DATA(lv_res) = mif_current_acc_constr_param->get_current_balance( is_person_data ).

    rv_result = lv_res + 5.

  ENDMETHOD.

  METHOD get_input.
    " the code inside test-seam section will be replaced with the code inside test-injection while tests running
    " in fact the code inside test-seam remains untested
    TEST-SEAM fake_input.
*      cl_demo_input=>request( CHANGING field = rv_input ).
    END-TEST-SEAM.

  ENDMETHOD.

  METHOD authorization_check.

    rv_is_authorized = abap_true.

    AUTHORITY-CHECK OBJECT 'M_QTN_EKO'
            ID 'ACTVT'  FIELD '03'.
    IF sy-subrc <> 0.
      rv_is_authorized = abap_false.
      RETURN.
    ENDIF.

    AUTHORITY-CHECK OBJECT 'M_LFA1_BEK'
            ID 'ACTVT'  FIELD '01'.
    IF sy-subrc <> 0.
      rv_is_authorized = abap_false.
      RETURN.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


