*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZLLM_CLNT_CONFIG................................*
DATA:  BEGIN OF STATUS_ZLLM_CLNT_CONFIG              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_CLNT_CONFIG              .
CONTROLS: TCTRL_ZLLM_CLNT_CONFIG
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZLLM_SYSTEM.....................................*
DATA:  BEGIN OF STATUS_ZLLM_SYSTEM                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZLLM_SYSTEM                   .
*.........table declarations:.................................*
TABLES: *ZLLM_CLNT_CONFIG              .
TABLES: *ZLLM_SYSTEM                   .
TABLES: ZLLM_CLNT_CONFIG               .
TABLES: ZLLM_SYSTEM                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
