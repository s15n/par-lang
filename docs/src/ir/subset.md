# Process Language

Process language is an IR of Par, which is essentially just a subset of itself.

Typechecking is performed in the lowering, so the process language is untyped.

## Syntax

Its syntax is much simpler than the base language. How it is lowered is explained on the respective pages, like the one on [constructions](../expressions/construction.md).


> **<sup>Syntax</sup>**\
> [_Item_](../items.md) :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Definition_
>
> [_Definition_](../items.md#definitions) :\
> &nbsp;&nbsp; &nbsp;&nbsp; `def` [ID] `=` _Expression_
> 
> [_Expression_](../expressions.md) :\
> &nbsp;&nbsp; &nbsp;&nbsp; _PrimaryExpression_ \
> &nbsp;&nbsp; | _ChanExpression_
>
> [_PrimaryExpression_](../expressions.md#primary-expressions) :\
> &nbsp;&nbsp; &nbsp;&nbsp; [ID]
>
> [_ChanExpression_](../expressions.md#channel-expressions) : `chan` [ID] [_Captures_] `{` _Process_ `}`
>
> [_Process_](../statements.md) : (_Statement_ (`;`<sup>?</sup> _Statement_) (`;`<sup>?</sup> _TerminatingStatement_)<sup>?</sup>)<sup>?</sup>
>
> [_Statement_](../statements.md#statements) :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Command_ \
> &nbsp;&nbsp; | _LetStatement_
>
> [_TerminatingStatement_](../statements.md#statements) :\
> &nbsp;&nbsp; &nbsp;&nbsp; _TerminatingCommand_

Statements are basically the whole process language and they are almost identical to the ones in regular Par, except untyped.

Here are the changes:

> **<sup>Syntax</sup>**\
> [_LetStatement_](../statements.md#let-statements) : <sup>(No patterns, only single IDs)</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; `let` [ID] `=` _Expression_
>
> [_SendCommand_](../statements/commands.md#send-commands) : <sup>(No syntax sugar for multiple arguments)</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; _Receiver_ `(` _Expression_ `)`
>
> [_ReceiveCommand_](../statements/commands.md#receive-commands) : <sup>(No patterns, only single IDs; no syntax sugar for multiple names)</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; _Receiver_ `[` [ID] `]`
>
> [_MatchCommand_](../statements/commands.md#match-commands) : <sup>(No patterns, only labels)</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; _Receiver_ `{` ([_Label_] `=>` `{` _Process_ `}`)<sup>\*</sup> `}`
>
> [_RecursiveCommand_](../statements/commands.md#recursive-commands) : <sup>(Captures)</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; _Receiver_ `unfounded`<sup>?</sup> `begin` [_LoopLabel_]<sup>?</sup> [_Captures_]
>
> [_LoopCommand_](../statements/commands.md#recursive-commands) : <sup>(Captures)</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; _Receiver_ `loop` [_LoopLabel_]<sup>?</sup> [_Captures_]

TODO: why are these used in untyped code?
> [_SendTypeCommand_](../statements/commands.md#send-type-commands) : <sup>(No syntax sugar for multiple types)</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; _Receiver_ `(` _Type_ `)`
>
> [_ReceiveTypeCommand_](../statements/commands.md#receive-type-commands) : <sup>(No syntax sugar for multiple names)</sup>\
> &nbsp;&nbsp; &nbsp;&nbsp; _Receiver_ `[` [ID] `]`

## Captures

> **<sup>Syntax</sup>**\
> _Captures_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `|` TODO `|`

TODO


[ID]: ../lexical.md#names
[_Label_]: ../types.md
[_LoopLabel_]: ../statements/commands.md#recursive-commands
[_Captures_]: #captures