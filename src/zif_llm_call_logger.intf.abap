INTERFACE zif_llm_call_logger
  PUBLIC .
  METHODS add
    IMPORTING
      entry TYPE zllm_call_log.
ENDINTERFACE.
