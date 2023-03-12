gpb_text
=====

An OTP library

Build
-----

    $ rebar3 compile


```
leex:file("gpb_text_lexer.xrl").
c(gpb_text_lexer).
```

```
yecc:file("gpb_text_parser.yrl").
c(gpb_text_parser).
gpb_text_parser:parse(Symbols).

```

