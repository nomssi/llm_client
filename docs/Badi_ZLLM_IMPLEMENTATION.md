# BAdI ZLLM_IMPLEMENTATION

Via this BAdI you can change the default implementation for mutliple basic client components with the most important one beeing implementing custom authorization checks.

Use SE19 to create a new implementation for Enahncement Spot ZLLM_DEFAULTS. It is suggested to create a subclass of the default class to avoid issues in case it is extended at a later point in time.

Methods:

- ZIF_LLM_DEFAULT_IMPL~GET_JSON_IMPL\
Allows you to define a different JSON implementation name (as string) if you cannot use /UI2/CL_JSON. For OLLAMA to properly run with tool use you must change the class_constructor to change the value of mc_json_type:

```abap
    "Need to refer to main class instead of local definition
    "CONCATENATE mc_json_type lo_json_type_descr->absolute_name INTO mc_json_type.
    DATA cl_ui2_json TYPE /ui2/cl_json=>json.
    DATA(ui2_json_desc) = cl_abap_typedescr=>describe_by_data( cl_ui2_json ).
    CONCATENATE mc_json_type ui2_json_desc->absolute_name INTO mc_json_type.
```

- ZIF_LLM_DEFAULT_IMPL~GET_ENCRYPTION_IMPL\
Allows you to use a custom encryption implementation if you cannot/don't want to use the default SSF implementation. Your custom impelementation must use the interface ZIF_LLM_ENCRYPTION.
- ZIF_LLM_DEFAULT_IMPL~GET_CALL_LOGGER_IMPL - Custom Logger
- ZIF_LLM_DEFAULT_IMPL~GET_STATISTICS_IMPL - Custom Statistics
- ZIF_LLM_DEFAULT_IMPL~GET_AUTHORIZATION_IMPL\
**Authorization Checks** - the default implementation does NO checks. This is intentional as I don't want to deliver a custom Auth Object that might conflict with existing ones or violate your conventions. Implement a custom class with the interface ZIF_LLM_AUTH and raise ZCX_LLM_AUTHORIZATION if the caller is not allowed to execute the function.
