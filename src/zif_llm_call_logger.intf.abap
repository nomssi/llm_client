"! <p class="shorttext synchronized" lang="en">Logging Calls</p>
INTERFACE zif_llm_call_logger
  PUBLIC .
  "! <p class="shorttext synchronized" lang="en"></p>
  "! Add an entry to the log
  "! @parameter entry | <p class="shorttext synchronized" lang="en"></p>
  METHODS add
    IMPORTING
      entry TYPE zllm_call_log.
ENDINTERFACE.
