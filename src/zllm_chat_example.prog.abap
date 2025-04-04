*&---------------------------------------------------------------------*
*& Report ZLLM_CHAT_EXAMPLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zllm_chat_example.

PARAMETERS model TYPE zllm_model MATCHCODE OBJECT zllm_model.

INCLUDE yy_llm_ide.

INITIALIZATION.
  model = lcl_app=>default_model( ).

START-OF-SELECTION.
  lcl_app=>chat( ).
