"! <p class="shorttext synchronized" lang="en">LLM Statistics Writer</p>
INTERFACE zif_llm_statistics
  PUBLIC .
  "! <p class="shorttext synchronized" lang="en"></p>
  "! Add a statistic record
  "! @parameter record | <p class="shorttext synchronized" lang="en"></p>
  METHODS add
    IMPORTING
      record TYPE zllm_statistics.

ENDINTERFACE.
