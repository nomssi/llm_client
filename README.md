# ABAP LLM Client

## Release Status
**ALPHA** - Preview release. Major breaking changes expected.
Beta release targeted for January 2025.

## Overview
ABAP LLM Client provides a universal interface to multiple major on-premise and cloud-based LLM APIs, focusing primarily on chat endpoints. The client is designed to seamlessly integrate with various LLM providers while maintaining a consistent interface for ABAP applications.

Currently supported providers:
- OLLAMA
- OpenAI
- OpenRouter

## Features
- Chat message support
- Structured output (provider-dependent)
- Multiple provider integration
- Customizable authentication

## Known Limitations
- Tested only on 7.52 (likely to run on higher releases) - S/4 tests planned with Beta release
- Only API-Key authentication currently supported
- No usage monitoring or limiting features
- Storage of API Keys in a usual DB table --> use a custom implementation or SM59 compatible auth settings

## Not planned
The following topics are currently not considered but feel free to challenge that in discussions if you have a specific use case.
- Downport lower than 7.52 due to efforts to test this
- Port to ABAP Cloud - if you use BTP you might aswell consider using Python or other languages with better GenAI library support
- Non-Chat (e.g. image, audio, video) support

## Why ABAP LLM Client?

ABAP might not be the first environment that comes to mind for GenAI projects, but it's the foundation for critical business processes in many organizations. This client exists to bridge this gap: It provides a straightforward way to integrate LLM capabilities into existing ABAP business logic.

### Core Ideas
- **Simple Integration**: Focus on the most commonly needed LLM features (mainly chat endpoints) rather than trying to support every possible API feature
- **Practical Approach**: Built for ABAP developers who need to enhance existing business functions with LLM capabilities, not for ML experiments
- **Stable Integration**: As tools and structured output become standard features across LLM providers, integration becomes more reliable and maintainable
- **Real-World Usage**: Designed for actual business scenarios where you might want to:
  - Enhance existing ABAP reports with AI-powered insights
  - Add LLM capabilities to business workflows
  - Prototype AI features in existing ABAP applications

### No Frills
This is not a full-featured ML framework or a complex AI platform. It's a practical tool for ABAP developers who need to call LLM APIs and process their responses within their ABAP environment. Nothing more, nothing less. If you need to integrate LLM capabilities into your ABAP systems and want to do it with minimal overhead, this client might be useful for you.

## Prerequisites
- SAP NetWeaver 7.52 or higher (developed and tested on this release)
- `/UI2/CL_JSON` PL12 or higher recommended
  - Alternative: Use [abap-to-json](https://github.com/SAP/abap-to-json) for pre-PL12 systems

## Installation
1. Install via [abapGit](https://github.com/abapGit/abapGit)
2. Optionally [abap-to-json](https://github.com/SAP/abap-to-json) if /UI2/CL_JSON is outdated

## Configuration
1. Setup TLS 1.2 properly as described for [abapGit](https://docs.abapgit.org/user-guide/setup/ssl-setup.html#sap-crypto-library)
2. Create RFC destinations for the providers
    - Ollama path prefix /api
    - OpenAI path prefix /v1
    - Openrouter path prefix /api/v1
3. Add required SSL certificates in STRUST for the providers you want to use
4. Configure entries in table `ZLLM_CLNT_CONFIG` with:
   - ABAP internal model name
   - Provider class (e.g., `ZCL_LLM_CLIENT_OLLAMA`)
   - Provider internal model name
   - RFC destination (SM59)
   - Auth Type (currently supports API-Key only)
   - Auth Value

## Usage
Refer to the [llm_client_test](https://github.com/abap-ai/llm_client_tests) repository on now to use the llm client. 

⚠️ **Security Notice**: Currently no safeguards. Use API keys with appropriate usage limits to prevent accidental charges e.g. for unexpected endless loops, etc.

## Extension
Custom Authenticaton implementations can be created by:
1. Sub-classing the specific provider class
2. Redefining the `SET_AUTH` method
In general for most parts interfaces are available to easily use custom implementations preferrably via sub-classes.

## Roadmap
### Beta Release Prerequisites
- [ ] Tool support implementation
- [ ] Logging functionality
- [ ] Custom auth object for access control
- [ ] Additional API provider integration
- [ ] Enhanced security features for API key management

## Contributing
Feel free to:
- Open issues for bug reports
- Use discussions for feature requests
- Request additional provider implementations
- Suggest concept improvements

## Security Recommendations
- Use SM59 supported authentication methods
- Implement API Gateway for credential mapping
- Set appropriate usage limits on API keys
- If the system is not your personal test instance consider using a custom implementation to securely store API Keys