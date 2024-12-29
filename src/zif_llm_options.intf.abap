"! <p class="shorttext synchronized" lang="en">Options for the LLM call</p>
INTERFACE zif_llm_options
  PUBLIC .
  METHODS:
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Range [0, 2]
    "! @parameter temperature | <p class="shorttext synchronized" lang="en"></p>
    set_temperature IMPORTING temperature TYPE decfloat16 RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! > 0
    "! @parameter tokens | <p class="shorttext synchronized" lang="en"></p>
    set_max_tokens IMPORTING tokens TYPE i RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Range [0, 1]
    "! @parameter top_p | <p class="shorttext synchronized" lang="en"></p>
    set_top_p IMPORTING top_p TYPE decfloat16 RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Range [1, infinity]
    "! @parameter top_k | <p class="shorttext synchronized" lang="en"></p>
    set_top_k IMPORTING top_k TYPE i RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Seed parameter for more repeatable output, integer only
    "! @parameter seed | <p class="shorttext synchronized" lang="en"></p>
    set_seed IMPORTING seed TYPE i RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Range [-2, 2]
    "! @parameter frequency_penalty | <p class="shorttext synchronized" lang="en"></p>
    set_frequency_penalty IMPORTING frequency_penalty TYPE decfloat16 RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Range [-2, 2]
    "! @parameter presence_penalty | <p class="shorttext synchronized" lang="en"></p>
    set_presence_penalty IMPORTING presence_penalty TYPE decfloat16 RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Range [0, 1]
    "! @parameter min_p | <p class="shorttext synchronized" lang="en"></p>
    set_min_p IMPORTING min_p TYPE decfloat16 RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Range [0, 1]
    "! @parameter top_a | <p class="shorttext synchronized" lang="en"></p>
    set_top_a IMPORTING top_a TYPE decfloat16 RAISING zcx_llm_validation,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Key-Value list of parameters that will be given to the model.
    "! @parameter parameters | <p class="shorttext synchronized" lang="en"></p>
    set_custom_parameters IMPORTING parameters TYPE zllm_keyvalues,
    "! <p class="shorttext synchronized" lang="en"></p>
    "! Get all parameters for the call
    "! @parameter parameters | <p class="shorttext synchronized" lang="en"></p>
    get_paramters RETURNING VALUE(parameters) TYPE zllm_keyvalues.
ENDINTERFACE.
