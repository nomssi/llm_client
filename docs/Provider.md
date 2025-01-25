# Provider Configuration

## [OpenAI](https://openai.com/)
Create the SM59 destination type "G" with:
- HOST api.openai.com
- Port 443
- Path Prefix /v1
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:
- Implementation Class: ZCL_LLM_CLIENT_OPENAI
- RFC Desitination: created above
- Auth Type: A (API Key)
- Auth Value: Paste the API Key into both fields (key might be longer than 132 characters, in that case paste the remaining ones in the second field)

## [OpenRouter](https://openrouter.ai/)
Create the SM59 destination type "G" with:
- HOST openrouter.ai
- Port 443
- Path Prefix /api/v1
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:
- Implementation Class: ZCL_LLM_CLIENT_OPENROUTER
- RFC Desitination: created above
- Auth Type: A (API Key)
- Auth Value: Paste the API Key into the first field (in case it is too long you can use the second field for the rest)

## [Ollama](https://ollama.com/)
Create the SM59 destination type "G" with:
- HOST - your ollama host
- Port - your ollama port (default 11434)
- Path Prefix /api
- SSL - depends on your setup, by default ollama uses http
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:
- Implementation Class: ZCL_LLM_CLIENT_OLLAMA
- RFC Desitination: created above
- Auth Type: blank - alternative if you use some kind of reverse proxy/API Gateway that adds api-key you can use:
    - Auth Type: A
    - Auth Value: {apikey-header-name}:{apikey-value} Example: ApiKeyHeader:myApiKey123456

## [Azure OpenAI Service](https://azure.microsoft.com/de-de/products/ai-services/openai-service)
Create the SM59 destination type "G" with:
- HOST {deployment}.cognitiveservices.azure.com
- Port 443
- Path Prefix /openai/deployments
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:
- Implementation Class: ZCL_LLM_CLIENT_AZUREOAI
- RFC Desitination: created above
- Auth Type: A (API Key)
- Auth Value: Paste the API Key into the first field (in case it is too long you can use the second field for the rest)

## [Anthropic](https://docs.anthropic.com/en/home)
Create the SM59 destination type "G" with:
- HOST api.anthropic.com
- Port 443
- Path Prefix /v1
- SSL Active & SSL Certificate as the usual SSL Client that has the required certificates (by default use the DFAUL SSL Client)
- Special Options HTTP 1.1

Provider configuration via Transaction ZLLM_PROVIDER_CONFIG:
- Implementation Class: ZCL_LLM_CLIENT_ANTHROPIC
- RFC Desitination: created above
- Auth Type: A (API Key)
- Auth Value: Paste the API Key into the first field (in case it is too long you can use the second field for the rest)