*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLLM_CLNT_CONFIG................................*
DATA:  BEGIN OF STATUS_ZLLM_CLNT_CONFIG              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_CLNT_CONFIG              .
CONTROLS: TCTRL_ZLLM_CLNT_CONFIG
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZLLM_CONFIG.....................................*
DATA:  BEGIN OF STATUS_ZLLM_CONFIG                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_CONFIG                   .
*.........table declarations:.................................*
TABLES: *ZLLM_CLNT_CONFIG              .
TABLES: *ZLLM_CONFIG                   .
TABLES: ZLLM_CLNT_CONFIG               .
TABLES: ZLLM_CONFIG                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
