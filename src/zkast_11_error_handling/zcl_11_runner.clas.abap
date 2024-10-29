CLASS zcl_11_runner DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES:
      if_oo_adt_classrun.

    METHODS:
      read_file
        IMPORTING
                  file_name TYPE string
        RAISING   zcx_kast_no_check.


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_11_runner IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    DATA: exc TYPE REF TO zcx_kast_no_check.

    out->write( 'Hello 22' ).

    TRY.
        read_file( file_name = '1' ).
      CATCH zcx_kast_no_check INTO exc.
        out->write( exc->text ).
    ENDTRY.

  ENDMETHOD.


  METHOD read_file.

    RAISE EXCEPTION NEW zcx_kast_no_check( text = 'Empty file' ).

  ENDMETHOD.



ENDCLASS.
