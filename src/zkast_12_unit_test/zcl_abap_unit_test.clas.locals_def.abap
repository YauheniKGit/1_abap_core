*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
INTERFACE lif_current_acc_constr_param.

  METHODS:
    get_current_balance
      IMPORTING
                is_person_data    TYPE zif_store_constants_types=>ty_person_data
      RETURNING VALUE(rv_balance) TYPE int8.

ENDINTERFACE.
