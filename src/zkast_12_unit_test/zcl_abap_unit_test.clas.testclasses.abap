**"* use this source file for your ABAP unit test classes

CLASS ltc_abap_unit_test DEFINITION FOR TESTING
DURATION SHORT
RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    CLASS-DATA:
      sif_environment_osql     TYPE REF TO if_osql_test_environment,
      sif_environment_cds      TYPE REF TO if_cds_test_environment,
      environment_fm           TYPE REF TO if_function_test_environment,
      authority_object_set_cl  TYPE REF TO cl_aunit_authority_check,
      authority_object_set     TYPE REF TO if_aunit_authority_objset,
      authorization_controller TYPE REF TO if_aunit_auth_check_controller.

    CLASS-METHODS:
      class_setup,    " runs automatically at the start of the class
      class_teardown. " runs automatically at the end of the class

    DATA:
      mo_cut      TYPE REF TO zcl_abap_unit_test,
      mo_i_helper TYPE REF TO zif_kast_account.

    METHODS:
      setup, " runs automatically before each test method
      teardown. " runs automatically after each test method

    METHODS:
      check_calc_value_param_a FOR TESTING RAISING cx_static_check,                             "simple method
      check_calc_value_param_b FOR TESTING RAISING cx_static_check,

      check_tdf_sql_true_pdi FOR TESTING RAISING cx_static_check,                               "tdf sql
      check_tdf_sql_false_pdi FOR TESTING RAISING cx_static_check,

      check_tdf_cds_true_o_park_pdi FOR TESTING RAISING cx_static_check,                        "tdf cds
      check_tdf_cds_false_empty_tab FOR TESTING RAISING cx_static_check,
      check_tdf_cds_false_close_park FOR TESTING RAISING cx_static_check,

      check_tdf_oo_w_if FOR TESTING RAISING cx_static_check,                                    "tdf oop with IF
      check_tdf_oo_wo_if FOR TESTING RAISING cx_static_check,                                   "tdf oop without IF
      check_oo_wo_if_constr_param FOR TESTING RAISING cx_static_check,                          "factory

      check_test_seams FOR TESTING RAISING cx_static_check,                                     "test seam

      check_authority_dependencies FOR TESTING RAISING cx_static_check,                         "api authority check
      set_up_authorization_double
        IMPORTING
          authorizations TYPE cl_aunit_auth_check_types_def=>user_role_authorizations
        RAISING
          cx_abap_auth_check_exception,
      reset_authorization_double
        IMPORTING
          authorizations TYPE cl_aunit_auth_check_types_def=>user_role_authorizations
        RAISING
          cx_abap_auth_check_exception.

ENDCLASS.


CLASS ltc_abap_unit_test IMPLEMENTATION.

  METHOD class_setup.

    sif_environment_osql =                                            " setup SQL environment (TDF)
        cl_osql_test_environment=>create(
            i_dependency_list = VALUE #( ( 'zkast_cc_park' ) ) ).

*    sif_environment_cds =                                             " setup CDS environment (TDF)
*        cl_cds_test_environment=>create_for_multiple_cds(
*            i_for_entities = VALUE #(
*                ( i_for_entity = 'ZSHT_CC_I_PARK'
*                  i_dependency_list = VALUE #( ( 'ZSHT_CC_P_PARK' )   " for hierarchical testing
*                                               ( 'ZSHT_CC_P_CAR' ) )
*                  i_select_base_dependencies  = abap_true )           " framework loads all dependencies automatically
*            ) ).

  ENDMETHOD.

  METHOD setup.
*    DATA: lo_helper TYPE REF TO zif_kast_helper.
    mo_cut = NEW zcl_abap_unit_test( ).
  ENDMETHOD.

  METHOD check_calc_value_param_a.

    " prepare parameters, GIVEN, arrange
    DATA:
      result  TYPE int8,
      param_a TYPE int8 VALUE 10.

    " run method to be tested, WHEN, act
    mo_cut->calc_value(
     EXPORTING
       iv_param_a = param_a
     IMPORTING
       ev_result = result
       ).

    " check result, THEN, assert
    DATA(cl_result_tmp) = cl_abap_unit_assert=>assert_equals(
      act = result
      exp = 100
      msg = 'Something went wrong'
    ).

  ENDMETHOD.

  METHOD check_calc_value_param_b.

    "given, arrange

    "when, act
    mo_cut->calc_value(
      EXPORTING
        iv_param_a = 2
        iv_param_b = 4
      IMPORTING
        ev_result  = DATA(lv_result)
    ).

    "then, assert
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = lv_result
        exp                  = 8
        msg                  = 'Multiply is not correct'
    ).

  ENDMETHOD.

  METHOD check_tdf_sql_true_pdi.

    " prepare mock data
    DATA:
      zkast_cc_park_mock TYPE STANDARD TABLE OF zkast_cc_park.

    zkast_cc_park_mock = VALUE #(
                                 ( parkuuid = '123'
                                   parkid = '1'
                                   parkname =  'Mocked Parking 1 YK'
                                   capacity = '21' )
                                ).

    " insert mock data into environment
    sif_environment_osql->insert_test_data( zkast_cc_park_mock ) .

    mo_cut->select_data(  ).

    SELECT parkid,
           parkname,
           lchg_uname
      FROM zkast_cc_park
      INTO TABLE @DATA(parkings).

    cl_abap_unit_assert=>assert_equals(
      act = parkings[ parkid = '1' ]-lchg_uname
      exp = sy-uname
      msg = 'Something went wrong'
      ).

  ENDMETHOD.

  METHOD check_tdf_sql_false_pdi.

    " prepare mock data
    DATA:
      zkast_cc_park_mock TYPE STANDARD TABLE OF zkast_cc_park.

    zkast_cc_park_mock = VALUE #(
                                 ( parkuuid = '234'
                                   parkid = '2'
                                   parkname =  'Mocked Parking 2 YK'
                                   capacity = '19' )
                                ).

    " insert mock data into environment
    sif_environment_osql->insert_test_data( zkast_cc_park_mock ) .

    mo_cut->select_data(  ).

    SELECT parkid,
           parkname,
           lchg_uname
      FROM zkast_cc_park
      INTO TABLE @DATA(parkings).

    cl_abap_unit_assert=>assert_equals(
      act = parkings[ parkid = '2' ]-lchg_uname
      exp = ''
      msg = 'Something went wrong'
      ).

  ENDMETHOD.

  METHOD check_tdf_cds_true_o_park_pdi.

    " prepare mock data
*    DATA:
*      zsht_cc_p_park_mock TYPE TABLE OF zsht_cc_p_park,
*      zsht_cc_p_car_mock  TYPE TABLE OF zsht_cc_p_car.

*    zsht_cc_p_park_mock = VALUE #( ( parkuuid = '111'
*                                     parkid = '1'
*                                     parkname =  'Mocked Parking 1'
*                                     capacity = '2'
*                                   )
*                                 ).
*
*    zsht_cc_p_car_mock = VALUE #(
*                                  ( caruuid = '111' parkuuid = '111' )
*                                ).
*
*    " insert mock data into environment
*    sif_environment_cds->insert_test_data( zsht_cc_p_park_mock ) .
*    sif_environment_cds->insert_test_data( zsht_cc_p_car_mock ) .

    DATA(result_msg) = mo_cut->modify_cds(  ).

    cl_abap_unit_assert=>assert_equals(
      act = result_msg
      exp = 'Open Parking No 1 Found'
      msg = 'Something went wrong'
     ).

  ENDMETHOD.

  METHOD check_tdf_cds_false_empty_tab.

*    " prepare mock data
*    DATA:
*      zsht_cc_p_park_mock TYPE TABLE OF zsht_cc_p_park,
*      zsht_cc_p_car_mock  TYPE TABLE OF zsht_cc_p_car.
*
*    " insert mock data into environment
*    sif_environment_cds->insert_test_data( zsht_cc_p_park_mock ) .
*    sif_environment_cds->insert_test_data( zsht_cc_p_car_mock ) .

    DATA(result_msg) = mo_cut->modify_cds(  ).

    cl_abap_unit_assert=>assert_equals(
      act = result_msg
      exp = 'Open Parking NOT Found'
      msg = 'Something went wrong'
     ).

  ENDMETHOD.

  METHOD check_tdf_cds_false_close_park.

    " prepare mock data
*    DATA:
*      zsht_cc_p_park_mock TYPE TABLE OF zsht_cc_p_park,
*      zsht_cc_p_car_mock  TYPE TABLE OF zsht_cc_p_car.
*
*    zsht_cc_p_park_mock = VALUE #(
*                                    ( parkuuid = '111'
*                                     parkid = '1'
*                                     parkname =  'Mocked Parking 1'
*                                     capacity = '2'
*                                    )
*                                 ).
*
*    zsht_cc_p_car_mock = VALUE #(
*                                  ( caruuid = '111' parkuuid = '111' )
*                                  ( caruuid = '222' parkuuid = '111' )
*                                  ( caruuid = '333' parkuuid = '111' )
*                                ).
*
*    " insert mock data into environment
*    sif_environment_cds->insert_test_data( zsht_cc_p_park_mock ) .
*    sif_environment_cds->insert_test_data( zsht_cc_p_car_mock ) .

    DATA(result_msg) = mo_cut->modify_cds(  ).

    cl_abap_unit_assert=>assert_equals(
      act = result_msg
      exp = 'Open Parking NOT Found'
      msg = 'Something went wrong'
     ).

  ENDMETHOD.

  METHOD check_tdf_oo_w_if.

    DATA:
      lif_kast_account TYPE REF TO zif_kast_account,
      ls_person_data   TYPE zif_store_constants_types=>ty_person_data.

    lif_kast_account ?= cl_abap_testdouble=>create( 'zif_kast_account' ).                            " create test double for the interface which implementation needs to be replaced

    cl_abap_testdouble=>configure_call( lif_kast_account
                                        )->ignore_all_parameters(
                                        )->returning( value = 10 ).                                  " configure call

    DATA(lo_value) = lif_kast_account->view_balances( is_person_data = ls_person_data ).             " call configured method
    DATA(lo_action) = NEW zcl_abap_unit_test( iif_kast_account = lif_kast_account ).                               " create new object giving mocked value as parameter

    DATA(lv_return) = lo_action->get_full_balance_ext_meth_w_if( ls_person_data ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_return
        exp = 15
    ).

  ENDMETHOD.

  METHOD check_tdf_oo_wo_if.

    DATA:
      ls_person_data   TYPE zif_store_constants_types=>ty_person_data.

    DATA(test_double) = CAST zcl_current_account(
    cl_abap_testdouble=>create( 'zcl_current_account' ) ).                           " create test double for the interface which implementation needs to be replaced

    cl_abap_testdouble=>configure_call( test_double
                                        )->ignore_all_parameters(
                                        )->returning( value = 10 ).                  " configure call

    test_double->get_current_balance( is_person_data = ls_person_data ).             " call configured method

    mo_cut = NEW zcl_abap_unit_test( io_current_account = test_double ).        " create new object giving mocked value as parameter

    DATA(lv_return) = mo_cut->call_ext_meth_wo_if( ls_person_data ).

    cl_abap_unit_assert=>assert_equals(
        act = lv_return
        exp = 15
    ).

  ENDMETHOD.


  METHOD check_oo_wo_if_constr_param.

    DATA:
      ls_person_data  TYPE zif_store_constants_types=>ty_person_data.

    mo_cut->mif_current_acc_constr_param = NEW ltd_current_account( ).          " inject dependency
    DATA(lv_res) = mo_cut->call_ext_meth_wo_if_constr_par( ls_person_data ).

    cl_abap_unit_assert=>assert_equals(
      act   = lv_res
      exp   = 40
      ).

  ENDMETHOD.


  METHOD check_test_seams.
    " the code inside test-injection section replaces the code inside test-seam section while tests running
    TEST-INJECTION fake_input.
      rv_input = 'Fake Input'.
    END-TEST-INJECTION.

    DATA(res_input) = mo_cut->get_input( ).

    cl_abap_unit_assert=>assert_equals(
    EXPORTING
      exp = 'Fake Input'
      act = res_input ).

  ENDMETHOD.

  METHOD check_authority_dependencies.

    DATA:
      auth_result    TYPE abap_bool,
      authorizations TYPE cl_aunit_auth_check_types_def=>user_role_authorizations.

    " prepare the list of authorization objects for testing
    authorizations = VALUE cl_aunit_auth_check_types_def=>user_role_authorizations( ( role_authorizations = VALUE #(
      ( object         = 'M_QTN_EKO'
        authorizations = VALUE #(
          ( VALUE #( ( fieldname = 'ACTVT' fieldvalues = VALUE #( ( lower_value = '03' ) ) )
                     ( fieldname = 'EKORG' fieldvalues = VALUE #( ( lower_value = '' ) ) ) ) ) ) )  ) ) ).

    " configure test user
    me->set_up_authorization_double( authorizations ).

    auth_result = mo_cut->authorization_check(  ).

    cl_abap_unit_assert=>assert_equals( act = auth_result exp = '' ). " '' - unauthorized; 'X' - authorized

    " reset test user
    me->reset_authorization_double( authorizations ).

  ENDMETHOD.

  METHOD set_up_authorization_double.
    authority_object_set = cl_aunit_authority_check=>create_auth_object_set( ).      " create set of authorization objects
    authority_object_set->add_role_auths_for_users( authorizations ).                " add prepared authorizations to authority object set
    authorization_controller = cl_aunit_authority_check=>get_controller( ).          " get controller
    authorization_controller->restrict_authorizations_to( authority_object_set ).    " restrict user authorizations to authority object set
  ENDMETHOD.

  METHOD reset_authorization_double.
    authorization_controller->reset(  ). "reset controller
    authority_object_set->remove_role_auths_for_users( authorizations ). " clear authority object set
  ENDMETHOD.

  METHOD teardown.
    sif_environment_osql->clear_doubles( ). "clear sql doubles (TDF)
    sif_environment_cds->clear_doubles( ). "clear cds doubles (TDF)
  ENDMETHOD.

  METHOD class_teardown.
    sif_environment_osql->destroy( ). " destroy sql environment (TDF)
    sif_environment_cds->destroy( ). " destroy cds environment (TDF)
  ENDMETHOD.



ENDCLASS.
