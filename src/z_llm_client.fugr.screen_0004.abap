PROCESS BEFORE OUTPUT.
 MODULE detail_init.
*
PROCESS AFTER INPUT.
 MODULE DETAIL_EXIT_COMMAND AT EXIT-COMMAND.
 MODULE DETAIL_SET_PFSTATUS.
 CHAIN.
    FIELD ZLLM_SYSTEM-STAT_ACTIVE .
    FIELD ZLLM_SYSTEM-SAVE_CALLS .
    FIELD ZLLM_SYSTEM-CALL_FILTER_UNAME .
  MODULE SET_UPDATE_FLAG ON CHAIN-REQUEST.
 endchain.
 chain.
  module detail_pai.
 endchain.
