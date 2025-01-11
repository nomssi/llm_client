# ABAP Template Parser Documentation 

A simple jinja-like template parser is available via class zcl_llm_template_parser. The main use case is to simplify template usage with the llm client and further features.

## Basic Usage
```abap
DATA(parser) = NEW zcl_llm_template_parser( ).
parser->add_template( 
    name    = 'mytemplate'
    content = 'Hello {{ name }}!' ).
DATA(result) = parser->render( 
    template_name = 'mytemplate'
    context      = lr_data ).
```

## Template Syntax

### Variables
```abap
{{ variable }}                "Basic variable
{{ user.name }}              "Nested property
{{ users[0].name }}          "Array/table access
{{ items[1].details.price }} "Combined access
```

### Variable Filters
Available filters are:
```abap
{{ name|upper }}            "Convert to uppercase
{{ name|lower }}            "Convert to lowercase
{{ name|capitalize }}       "Capitalize first letter
{{ name|default("value") }} "Use default if value is empty
```

### Control Structures

#### If Statements
```abap
{% if condition %}
  content
{% elif other_condition %}
  alternative
{% else %}
  default content
{% endif %}
```

Supported Conditions:
- Comparison Operators: `==`, `!=`, `>=`, `<=`, `>`, `<`
- Logical Operators: `and`, `or`, `not`
- Boolean Values: `true`, `false`, `1`, `0`, `X`, `x`

Examples:
```abap
{% if user.age > 18 %}
  Adult content
{% endif %}

{% if not is_active %}
  Account inactive
{% endif %}

{% if status == 'valid' and count > 0 %}
  Valid and not empty
{% endif %}
```

#### For Loops
```abap
{% for item in items %}
  {{ item.name }}
{% endfor %}
```

Loop Context Variables:
```abap
{{ loop.index }} "Current iteration (1-based)
{{ loop.first }} "True if first iteration
{{ loop.last }}  "True if last iteration
```

### Comments
```abap
{# This is a comment and will not appear in output #}
```

## Data Type Support

### Elementary Types
- Strings
- Integers
- Dates (formatted in user format)
- Times (formatted in user format)
- Boolean values (output as 'true'/'false')

### Complex Types
- Structures
- Tables
- Nested structures and tables

### Table Output Formatting
Direct table reference formatting:
- Elementary tables: comma-separated list
- Structure tables: `[KEY1: value1, KEY2: value2; KEY1: value1, KEY2: value2]`
- Nested tables: `[NESTED TABLE]`

## Special Characters
Escape sequences supported:
```abap
\n     "Newline
\t     "Tab
\{     "Literal curly brace
\}     "Literal curly brace
\\     "Literal backslash
```

## Error Handling
The parser raises `ZCX_LLM_TEMPLATE_PARSER` exception for:
- Invalid template syntax
- Unclosed tokens
- Invalid variable paths
- Missing templates
- Unsupported variable types
- Invalid loop syntax
- Invalid conditions
- Invalid table indexes

## Complete Example
```abap
DATA(template) =
  |{% if user.active %}| &&
  |  Welcome {{ user.name|capitalize }}!\n| &&
  |  Your recent orders:\n| &&
  |  {% for order in user.orders %}| &&
  |    {% if loop.first %}First order:{% endif %}| &&
  |    {{ loop.index }}. {{ order.product }} ({{ order.date }})\n| &&
  |  {% endfor %}| &&
  |{% else %}| &&
  |  Account inactive| &&
  |{% endif %}|.

DATA(parser) = NEW zcl_llm_template_parser( ).
parser->add_template( name = 'welcome' content = template ).
DATA(output) = parser->render( 
    template_name = 'welcome'
    context      = user_data ).
```

## Limitations
- No custom filters supported
- No template inheritance or includes
- No whitespace control
- No complex table operations (sorting, filtering, etc.)
- No arithmetic operations in expressions
- No inline expressions

This documentation covers all currently implemented features in the template parser class.