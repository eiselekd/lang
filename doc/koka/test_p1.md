
# P1

## P2: Hello

Test

|~~~~~~~~~~~~~~~~~~~~~~~~~|~~~~~~~~|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~|~~~|
| _module_~[_lex_]{.opt}~ | ::=    | [_moduledecl_]{.opt} _modulebody_                         |   |
| &nbsp;                  |        |                                                           |   |
| _moduledecl_            | ::=    | _semis_  [_visibility_]{.opt} `module` _moduleid_ |   |
| _moduleid_              | ::=    | _qvarid_ []{.bar} _varid_                                 |   |
| &nbsp;                  |        |                                                           |   |
| _modulebody_            | ::=    | `{` _semis_ _declarations_ `}` _semis_    |   |
|                         | &bar;  | _semis_ _declarations_                            |   |
| &nbsp;                  |        |                                                           |   |
| _visibility_            | ::=    | `public` []{.bar} `private`                               |   |
| _semis_                 | ::=    | [`;`]{.many}                                          |   |


