CLASS zcx_kast_no_check DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_t100_message .
    INTERFACES if_t100_dyn_msg .

    DATA:
        text TYPE string.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        text      TYPE string OPTIONAL.



  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcx_kast_no_check IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->text = text.

  ENDMETHOD.

ENDCLASS.


