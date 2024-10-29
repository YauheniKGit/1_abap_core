INTERFACE zif_kast_account
  PUBLIC.

  METHODS:
    view_balances
      IMPORTING
                is_person_data  TYPE zif_store_constants_types=>ty_person_data
      RETURNING VALUE(rv_balance) TYPE int8.

ENDINTERFACE.
