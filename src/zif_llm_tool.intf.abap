interface ZIF_LLM_TOOL
  public .
  "! <p class="shorttext synchronized" lang="en"></p>
  "! Get the result of the tool in order
  "! @parameter result | <p class="shorttext synchronized" lang="en"></p>
  methods get_result_msg RETURNING VALUE(result) type zllm_tool_message.

endinterface.
