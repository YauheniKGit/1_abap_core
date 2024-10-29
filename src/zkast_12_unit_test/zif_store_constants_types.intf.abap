INTERFACE zif_store_constants_types
  PUBLIC .

  TYPES:
    BEGIN OF ty_person_data,
      id      TYPE char5,
      name    TYPE char40,
      surname TYPE char40,
    END OF ty_person_data.
  TYPES:
      tty_person_data TYPE STANDARD TABLE OF ty_person_data WITH DEFAULT KEY.





ENDINTERFACE.
