# Application Expressions

> **<sup>Syntax</sup>**\
> _Application_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Applicable_ \
> &nbsp;&nbsp; | [_MatchExpression_](#match-expressions) \
> &nbsp;&nbsp; | [_LoopApplication_](#recursive-destructions)
>
> _Applicable_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_PrimaryExpression_] \
> &nbsp;&nbsp; | [_FunctionCall_](#function-calls) \
> &nbsp;&nbsp; | [_ChoiceSelection_](#choice-selections) \
> &nbsp;&nbsp; | [_RecursiveDestruction_](#recursive-destructions) \
> &nbsp;&nbsp; | [_UniversalSpecialization_](#universal-specializations)

While [constructions](construction.md) construct values, applications destruct them.

An application is always of the form: _Applicable_ _Suffix_.
Every application corresponds to a [command](../statements/commands.md), which looks the same. In contrast to commmand receivers however, applicables don't become the result of this expression.

All applications can be linked via:
```par
dual <> app suffix
// is equivalent to
let a = app
a suffix
dual <> a
```
Note that if app is a local variable, the `let` is not needed.

## Function Calls

> **<sup>Syntax</sup>**\
> _FunctionCall_ : _Applicable_ `(` [_ExpressionList_] `)`

*<sup>
[Destructs Type](../types.md#function-types)
| [Destructs Expression](construction.md#function-expressions)
| [Statement](../statements/commands.md#send-commands)
</sup>*

If `f` is of type `[A] B` and `a` is of type `A`, the function call `f(a)` is of type `B`.

A function call is equivalent to a [send command](../statements/commands.md#send-commands):
```par
// in process syntax
let b = f(a)
// is equivalent to
let b = f
b(a)
```

## Choice Selections

> **<sup>Syntax</sup>**\
> _ChoiceSelection_ : _Applicable_ [_Label_]

*<sup>
[Destructs Type](../types.md#choice-types)
| [Destructs Expression](construction.md#choice-constructions)
| [Statement](../statements/commands.md#signal-commands)
</sup>*

If `x` is of type `{ ..., .label => T, ... }`, the choice selection `x.label` is of type `T`.

A choice selection is equivalent to a [signal command](../statements/commands.md#signal-commands):
```par
// in process syntax
let y = x.label
// is equivalent to
let y = x
y.label
```

## Match Expressions

<!--
> **<sup>Syntax</sup>**\
> _MatchExpression_ : _Applicable_ `{` (_Pattern_ `=>` _Expression_ `,`<sup>?</sup>)<sup>\*</sup> `}`
-->
> **<sup>Syntax</sup>**\
> _MatchExpression_ : _Applicable_ `{` ([_Label_] (`(` [_ReceivePatterns_] `)`)<sup>\*</sup> ([ID] | `!`) `=>` [_Expression_] `,`<sup>?</sup>)<sup>\*</sup> `}`

*<sup>
[Destructs Type](../types.md#either-types)
| [Destructs Expression](construction.md#either-selections)
| [Statement](../statements/commands.md#match-commands)
</sup>*

Match expressions are similar to `match` in Rust or `case` in Haskell. Their pattern matching abilities are currently limited but will expand in the [future](../future.md).

If `x` is of type `either { .l1 T1, ..., .ln Tn }`
and
- `p1` is a [pattern](../patterns.md) for type `T1`, and so on, until `pn` is a pattern for type `Tn`
- `y1`, ..., `yn` are all of type `U`

then `x { .l1 p1 => y1, ..., .ln pn => yn }` is of type `U`. It evaluates to the `yi` which label `.li` was matched.

A match expression is equivalent to a [match command](../statements/commands.md#match-commands):
```par
// in process syntax
let y = x { .l1 p1 => y1, ..., .ln pn => yn }
// is equivalent to
let y = x
y { .l1 => { q1; ... }, ..., .ln => { q1; ... } }
```
where `qi` is the command corresponding to `pi`. todo: how is this defined.


## Recursive Destructions

> **<sup>Syntax</sup>**\
> _RecursiveDestruction_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Applicable_ `begin` [_LoopLabel_]<sup>?</sup>
>
> _LoopApplication_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Applicable_ `loop` [_LoopLabel_]<sup>?</sup>

*<sup>
[Destructs Type](../types.md#recursive-types)
| [Statement](../statements/commands.md#recursive-commands)
</sup>*

A `loop` corresponds to the innermost `begin` with the same loop label. `loop` without a label can only correspond to `begin` without a label.

If `x` is of type `recursive T`, then `begin x` is of type `T`, replacing each corresponding `self` of `T` with `recursive T`.

A `loop` corresponding to this `begin` can be used on values of type `recursive T`. Its behavior is equivalent to "pasting" the `begin` and every application that follows it.

<div class="warning" style="--warning-border: var(--note-border)">

With [totality](../future.md), `loop` can only be called on a direct descendant of the value `begin` was called on. I.e. on a value which type is a "`self`" correponding to the recursive type which `begin` was called on.
</div>

## Universal Specializations

> **<sup>Syntax</sup>**\
> _UniversalSpecialization_ : _Applicable_ `(` `type` [_TypeList_] `)`

*<sup>
[Destructs Type](../types.md#universal-types)
| [Destructs Expression](construction.md#universal-constructions)
| [Statement](../statements/commands.md#receive-type-commands)
</sup>*

If `x` is of the universal type `[type T] R`, the specialization `f(type X)` is of type `R`.


[ID]: ../lexical.md#names
[_PrimaryExpression_]: ../expressions.md#primary-expressions
[_Expression_]: ../expressions.md
[_ExpressionList_]: ../expressions.md
[_Label_]: ../types.md
[_ReceivePatterns_]: construction.md#choice-constructions
[_LoopLabel_]: ../statements/commands.md#recursive-commands
[_TypeList_]: ../types.md