# Construction Expressions

> **<sup>Syntax</sup>**\
> _Construction_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_Unit_] \
> &nbsp;&nbsp; | [_PairExpression_] \
> &nbsp;&nbsp; | [_FunctionExpression_] \
> &nbsp;&nbsp; | [_EitherSelection_] \
> &nbsp;&nbsp; | [_ChoiceConstruction_] \
> &nbsp;&nbsp; | [_IterativeConstruction_] \
> &nbsp;&nbsp; | [_Loop_] \
> &nbsp;&nbsp; | [_ExistentialConstruction_] \
> &nbsp;&nbsp; | [_UniversalConstruction_]


## The Unit Expression

> **<sup>Syntax</sup>**\
> _Unit_ : `!`

*<sup>
[Type](../types.md#the-unit-type)
| [Pattern](../patterns.md#todo)
| [Statement](../statements/commands.md#the-break-command)
| [Destructing Statement](../statements/commands.md#the-continue-command)
</sup>*

The unit expression `!` is of the [unit type](../types.md#the-unit-type) `!`.

It's the only value of its type:
```par
def unit: ! = !
```

Unit expressions can be linked via:
```par
dual <> !
// is equivalent to
dual!
```


## Pair Expressions

> **<sup>Syntax</sup>**\
> _PairExpression_ : `(` [_ExpressionList_] `)` [_Expression_]

*<sup>
[Type](../types.md#pair-types)
| [Pattern](../patterns.md#todo)
| [Statement](../statements/commands.md#send-commands)
| [Destructing Statement](../statements/commands.md#receive-commands)
</sup>*

If `a` is of type `A` and `b` is of type `B`, the pair expression `(a) b` is of the [pair type](../types.md#pair-types) `(A) B`.

todo

Pair expressions can be linked via:
```par
dual <> (a) b
// is equivalent to
dual(a)
dual <> b
```


## Function Expressions

> **<sup>Syntax</sup>**\
> _FunctionExpression_ : `[` [_PatternList_] `]` [_Expression_]

*<sup>
[Type](../types.md#function-types)
| [Destructing Expression](application.md#function-calls)
| [Statement](../statements/commands.md#receive-commands)
| [Destructing Statement](../statements/commands.md#send-commands)
</sup>*

If `p` is an [irrefutable](patterns.md#irrefutable-note) pattern for type `A` and `b` (wich must use the bindings of `p`) is of type `B`, the function expression `[p] b` is of the [function type](../types.md#function-types) `[A] B`.

todo

Function expressions can be linked via:
```par
dual <> [p] b
// is equivalent to
dual[p]
dual <> b
```

## Either Selections

> **<sup>Syntax</sup>**\
> _EitherSelection_ : [_Label_] [_Expression_]

*<sup>
[Type](../types.md#either-types)
| [Destructing Expression](application.md#either-destructions)
| [Statement](../statements/commands.md#signal-commands)
| [Destructing Statement](../statements/commands.md#match-commands)
</sup>*

The type of an either selection cannot be inferred from itself. \
A selection of the [either type](../types.md#either-types) `either { .a A, .b B }` is either `.a a` if `a` is of type `A` or `.b b` if `b` is of type `B`.

todo

Either selections can be linked via:
```par
dual <> .a a
// is equivalent to
dual.a
dual <> a
```


## Choice Constructions

> **<sup>Syntax</sup>**\
> _ChoiceConstruction_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `{` ([_Label_] (`(` _ReceivePatterns_ `)`)<sup>\*</sup> `=>` _Expression_ `,`<sup>?</sup>)<sup>\*</sup> `}`
>
> _ReceivePatterns_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _PatternList_ \
> &nbsp;&nbsp; | `type` [_ID_List_]

*<sup>
[Type](../types.md#choice-types)
| [Destructing Expression](application.md#choice-selections)
| [Statement](../statements/commands.md#match-commands)
| [Destructing Statement](../statements/commands.md#signal-commands)
</sup>*

If `a` is of type `A` and `b` is of type `B`, the choice construction `{ .a => a, .b => b }` is of the [choice type](../types.md#choice-types) `{ .a => A, .b => B }`.

Some patterns can be used on the left side:
- `{ .a(p) => a }` is equivalent to `{ .a => [p] a }`
- `{ .a(type T) => a }` is equivalent to `{ .a => [type T] a }`

todo

Choice constructions can be linked via:
```par
dual <> { .a => a, .b => b }
// is equivalent to
dual { 
  .a => { dual <> a }
  .b => { dual <> b }
}
```

## Iterative Constructions

> **<sup>Syntax</sup>**\
> _IterativeConstruction_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `begin` [_LoopLabel_]<sup>?</sup> [_Expression_]
>
> _Loop_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `loop` [_LoopLabel_]<sup>?</sup>

*<sup>
[Type](../types.md#iterative-types)
| [Statement](../statements/commands.md#recursive-commands)
</sup>*

If --- given every `loop` in `a` is of type `iterative A` --- `a` is of type `A`, the iterative construction `begin a` is of the [iterative type](../types.md#iterative-types) `iterative A`.

A `loop` corresponds to the innermost `begin` with the same loop label. `loop` without a label can only correspond to `begin` without a label.

todo

Iterative constructions can be linked via:
```par
dual <> begin a
// is equivalent to
dual begin
dual <> a // with loop in a replaced by begin a
```


## Existential Constructions

> **<sup>Syntax</sup>**\
> _ExistentialConstruction_ : `(` `type` [_TypeList_] `)` [_Expression_]

*<sup>
[Type](../types.md#existential-types)
| [Pattern](../patterns.md#todo)
| [Statement](../statements/commands.md#send-type-commands)
| [Destructing Statement](../statements/commands.md#receive-type-commands)
</sup>*

If `a` is of type `A`, the existential construction `(type T) a` is of the [existential type](../types.md#existential-types) `(type T) A`.

todo

Existential constructions can be linked via:
```par
dual <> (type T) a
// is equivalent to
dual(type T)
dual <> a
```

## Universal Constructions

> **<sup>Syntax</sup>**\
> _UniversalConstruction_ : `[` `type` [_ID_List_] `]` [_Expression_]

*<sup>
[Type](../types.md#universal-types)
| [Destructing Expression](application.md#universal-specializations)
| [Statement](../statements/commands.md#receive-type-commands)
| [Destructing Statement](../statements/commands.md#send-type-commands)
</sup>*

If `a` is of type `A`, the universal construction `[type T] a` (where `a` can use the type `T`) is of the [universal type](../types.md#universal-types) `[type T] A`.

todo

Universal constructions can be linked via:
```par
dual <> [type T] a
// is equivalent to
dual[type T]
dual <> a
```