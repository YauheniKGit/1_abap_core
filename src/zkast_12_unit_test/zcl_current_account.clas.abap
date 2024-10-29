CLASS zcl_current_account DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS:
      constructor
        IMPORTING
          iv_num TYPE int1 OPTIONAL,
      get_current_balance
        IMPORTING
                  is_person_data    TYPE zif_store_constants_types=>ty_person_data
        RETURNING VALUE(rv_balance) TYPE int8.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_current_account IMPLEMENTATION.

  METHOD constructor.

  ENDMETHOD.

  METHOD get_current_balance.
    rv_balance = 8.
  ENDMETHOD.



ENDCLASS.
