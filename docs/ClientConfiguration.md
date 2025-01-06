# Client Configuration
You can use transaction ZLLM_CLIENT_CONFIG or SM30 table ZLLM_CLNT_CONFIG.

## Configuration
The table has the following fields which can be maintained as customzing:
- Model: This is the model key you use in coding
- Section LLM Client Configuration:
    - Provider Name: Select the configured [Provider](Provider.md)
    - Model: Provider-internal model name; in case of Azure OpenAI serivces this is the name of the deployment
    - Struct. Output?: Currently information only - if the model supports structured output, not validated at runtime
    - Tools Supported?: Currently information only - if the model supports tool output, not validated at runtime
    - Default Options: **Needs testing** - Text field that allows you to set options that will be passed as-is to the provider except they are overridden by code. Entries can be separated by ';', key/value separated by ':'. Values need to be enclosed in '"' if they are string values. Better maintenance options and examples will be provided in future.

## Recommendations
Your feedback and experience is highly appreciated to extend this list:
- Setup default models for specific feature categories that can be exchanged without implementation changes, e.g.
    - Text output
    - Tool Calls
    - Structured Output
- Avoid giving more details than necessary, e.g. in case of ollama you might use the model as llama3.2 which then is mapped to llama3.2:3b-instruct-q8_0

# System Configuration
Via transaction ZLLM_SYSTEM_CONF or SM30 table ZLLM_SYSTEM statistics and tracing (saving http calls) can be enabled/disabled. Disabled by default.

Currently this is a very rudimentary implementation which will be extended in future based on demand.
- Table zllm_call_log contains the full http client request and response without headers and cookies - main use is for debugging
- Table zllm_statistics contains basic call statistics with token counts