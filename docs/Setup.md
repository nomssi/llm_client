## Prerequisites
- SAP NetWeaver 7.52 or higher (developed and tested on this release)
- `/UI2/CL_JSON` PL12 or higher recommended
  - Alternative: Use [abap-to-json](https://github.com/SAP/abap-to-json) for pre-PL12 systems

## Installation
1. Install via [abapGit](https://github.com/abapGit/abapGit)
2. Optionally [abap-to-json](https://github.com/SAP/abap-to-json) if /UI2/CL_JSON is outdated. See [Badi_ZLLM_IMPLEMENTATION.md](Badi_ZLLM_IMPLEMENTATION.md) for details on required further steps if you use the abap-to-json implementation.

## Configuration
1. Setup TLS 1.2 properly as described for [abapGit](https://docs.abapgit.org/user-guide/setup/ssl-setup.html#sap-crypto-library)
2. Create RFC destinations for the providers see [Provider Documentation](Provider.md) for details
    - Ollama path prefix /api
    - OpenAI path prefix /v1
    - Openrouter path prefix /api/v1
    - AzureOpenAI path prefix /openai/deployments
3. Add required SSL certificates in STRUST for the providers you want to use
4. Configure the provider via report ZLLM_PROVIDER_MAINTENANCE

For details on the model configuration see [Client Configuration](ClientConfiguration.md).

## Encryption Setup
Authorization values in the provider configuration are encrypted via SSF. Mandatory setup:
- SE16 Table SSFAPPLIC create a new entry:
    - APPLIC: ZLLMCT (this name is mandatory, otherwise you need to update the code)
    - All B_XXX fields set as 'X'
    - Descript: LLM Client Credentials (you can use any description)
- Transaction SSFA create a new entry for the application created above:
    - Hash Algorithm: SHA256
    - Encryption Algorithm: AES256-CBC
    - Keep all others as default
- Transaction STRUST
    - Right click on the new entry named as you defined above and select create, use default settings
    - Hint: this is usually the last entry in the list

Note that the application name is currently hardcoded, if you require a different value edit class zcl_llm_encryption. You might also open an issue to request this to be a configurable setting based on demand I might consider this, however as this is a quite rare setting in an ABAP system I do not expect any collisions with existing custom values.