# Types

At the heart of Par lies its type system, representing linear logic.

> **<sup>Syntax</sup>**\
> _Type_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_NamedType_](#named-types) \
> &nbsp;&nbsp; | [_ChannelType_](#channel-types) \
> &nbsp;&nbsp; | [_TupleType_](#tuple-types) \
> &nbsp;&nbsp; | [_FunctionType_](#function-types) \
> &nbsp;&nbsp; | [_EitherType_](#either-types) \
> &nbsp;&nbsp; | [_ChoiceType_](#choice-types) \
> &nbsp;&nbsp; | [_Unit_](#the-unit-type) \
> &nbsp;&nbsp; | [_Bottom_](#the-bottom-type) \
> &nbsp;&nbsp; | [_RecursiveType_](#recursive-types) \
> &nbsp;&nbsp; | [_IterativeType_](#iterative-types) \
> &nbsp;&nbsp; | [_Self_](#recursive-types) \
> &nbsp;&nbsp; | [_Loop_](#iterative-types) \
> &nbsp;&nbsp; | _ExistentialType_ \
> &nbsp;&nbsp; | _UniversalType_ <!--\
> &nbsp;&nbsp; | _ReplicableType_ \
> &nbsp;&nbsp; | _TaggedType_ -->
>
> _TypeList_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _Type_ (`,` _Type_)<sup>\*</sup> `,`<sup>?</sup>
>
> _Constructor_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `.` [_ID_]

## Named Types

> **<sup>Syntax</sup>**\
> _NamedType_ : [_ID_] [_TypeArguments_]<sup>?</sup>

Defined via type aliases, named types can always be replaced with their underlying representation.

```par
let x: Option<?> = .none!
// is equivalent to
let x: either { .none!, .some? } = .none!
```

## Channel Types

> **<sup>Syntax</sup>**\
> _ChannelType_ : `chan` _Type_

`chan A` represents a channel accepting an `A`:
```par
def just_true: Bool = chan yield {
  let c: chan Bool = yield
  yield.true!
}
```

Mathematically, `chan A` is \\(A^\perp\\) and therefore equivalent to patterns for type `A` (although those have dual syntax). We get for example:

- `chan A ≅ [A]?`

- `chan ! ≅ ?` and `chan ? ≅ !`

- `chan (A) B ≅ [A] chan B` and `chan [A] B ≅ (A) chan B`

- `chan either { .a A, .b B } ≅ { .a => chan A, .b => chan B }` and <br>
  `chan { .a => A, .b => B } ≅ either { .a chan A, .b chan B }`

- `chan chan T ≅ T`

todo: Is this all correct?

A more elaborate example can be seen [here](https://github.com/faiface/par-lang/blob/main/examples/flatten.par)

## Tuple Types

> **<sup>Syntax</sup>**\
> _TupleType_ : `(` _TypeList_ `)` _Type_

Having multiple types between the `()` is just syntax sugar:
```par
type T = (A, B)!
// is equivalent to
type T = (A) (B) !
```

`!` is the unit for tuples, i.e. `(A, B)!` and `(A) B` are equivalent.

Values are created using [tuple expressions](./expressions.md#tuple-expressions):
```par
let a: A = ...
let b: B = ...

let pair: (A) B = (a) b
```

Mathematically, `(A) B` is \\(A \otimes B\\). For session types, it means "send `A` and continue as `B`".

## Function Types

> **<sup>Syntax</sup>**\
> _FunctionType_ : `[` _TypeList_ `]` _Type_

Having multiple types between the `[]` is just syntax sugar:
```par
type T = [A, B] R
// is equivalent to
type T = [A] [B] R
```

Values are created using [function expressions](./expressions.md#function-expressions):
```par
let id: [A] A = [a] a
```

Mathematically, `[A] B` is a [linear](./linearity.md) function \\(A \multimap B\\). For session types, it means "receive `A` and continue as `B`".

## Either Types

> **<sup>Syntax</sup>**\
> _EitherType_ : `either` `{` (_Constructor_ _Type_ `,`<sup>?</sup>)<sup>\*</sup> `}`

An either type is a classical sum type aka. tagged union. Every value of such a type consists of a label (called "constructor") and a value of the type corresponding to the label (its "payload").

```par
// the most basic sum type
type Bool = either {
  .true!  // constructor "true" with payload !
  .false! // constructor "false", also with payload !
}

// a slightly more complex example
type TwoOrNone<T> = either {
  .none!      // constructor "none" with "no" payload (using !)
  .two(T, T)! // constructor "some" with "two" payloads
}
```

Values are created by calling one of the constructors with its payload.
```par
let no_bool: TwoOrNone<Bool> = .none!

let both_bools: TwoOrNone<Bool> = .two(.true!, .false!)!
```

Mathematically, `either { .a A, .b B }` is \\(A \oplus B\\). For session types, it means "select from `A` or `B`".
An empty either type `either {}` is therefore \\(\mathbf{0}\\). There is a function from it to every type:
```par
def absurd: [type T] [either {}] T = [type T] [x] x {}
```
This function can never be called though.

Either types are often used as [recursive] types.

## Choice Types

> **<sup>Syntax</sup>**\
> _ChoiceType_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `{` (_Constructor_ (`(` _ReceiverList_ `)`)<sup>\*</sup> `=>` _Type_ `,`<sup>?</sup>)<sup>\*</sup> `}`
>
> _ReceiverList_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _TypeList_ \
> &nbsp;&nbsp; | `type` [_ID_List_]

A choice type is dual to an [either](#either-types) type. Constructing a value of an either type is "making a choice" and similarly, destructing such a value looks exactly like constructing a value of a choice type. 

```par
// choice of two
type BoolChoice<A, B> = {
  .true => A
  .false => B
}

// destruct a Bool
def negate(b: Bool): Bool = b {
  .true! => .false!
  .false! => .true!
}

// construct a choice
def negate_choice: BoolChoice<Bool, Bool> = {
  .true => .false!
  .false => .true!
}

// define negate using the choice
// featuring selecting from the choice type value
def also_negate: [Bool] Bool = [b] b {
  .true! => negate_choice.true
  .false! => negate_choice.false
}
```
`.cons => [A] B` can also be written as `.cons(A) => B`

A choice type represents an interface for interacting with data. While an either type describes its underlying data, a choice type describes what can be done with it.
```par
// creating an interface
type Stack<Unwrap, T> = iter {
  .push(T) => loop
  .pop => (Option<T>) loop
  .unwrap => Unwrap
}

// implementing it
dec list_stack : [type T] [List<T>] Stack<List<T>, T>
def list_stack = [type T] [list] begin {
  .push(x) => let list: List<T> = .item(x) list in loop
  .pop => list {
    .empty! => (.none!) let list: List<T> = .empty! in loop,
    .item(head) tail => (.some head) let list = tail in loop
  }
  .unwrap => list
}

def main = do {
  let list: List<Bool> = .item(.false!).empty!
  let stack: Stack<List<Bool>, Bool>
    = list_stack(type Bool)(list)

  stack.push(.true!)
  stack.push(.false!)
  stack.pop
} in stack
```
todo: Is this a good example?

Mathematically, `{ .a => A, .b => B }` is \\(A \mathbin{\\&} B\\). For session types, it means "offer a choice of `A` or `B`".
An empty choice `{}` is therefore \\(\top\\) and has exactly one value, `{}`. There is a function to it from every type:
```par
def terminate: [type T] [T] {} = [type T] [x] {}
```
The result of this function can never be used though (todo: correct?).

Choice types are often used as [iterative] types.

## The Unit Type

> **<sup>Syntax</sup>**\
> _Unit_ : `!`

Unit is a type providing no information. In C(++) it's called `void`, in Rust it's `()` (and it can be thought of as an empty tuple in Par as well). There is exactly one value of type `!`, and it's also `!`.
```par
let unit: ! = !
```
Every value of a type `A` corresponds to a function `[!] A`:
```par
def select: [type T] [T] [!] T = [type T] [x] [!] x
// uncurrying makes this clear
// [T] [!] T = [T, !] T ≅ [(T) !] T ≅ [T] T

def extract: [type T] [[!] T] T = [type T] [f] f(!)
```

A type `A` is called _droppable_, if there is a function `[A] !`.
```par
// Types constructed only from ! are droppable
def drop_bool: [Bool] ! = [b] b {
  .true! => !
  .false! => !
}

// Replicables are droppable
def drop_repl: [type T] [&T] ! = !

// Functions are not droppable in general
def drop_impossible: [[Bool] Bool] ! = todo
```

Mathematically, `!` is \\(\mathbf{1}\\), the unit for \\(\otimes\\).

## The Bottom Type

> **<sup>Syntax</sup>**\
> _Bottom_ : `?`

Bottom is an empty type. In Haskell, it's `void`, in Rust it's (perhaps confusingly) `!`.

`?` dual to `!`. It's mostly used in process syntax to destruct the unit:
```par
let triple: (A, B, C)! = ...

let reverse = do {
  triple[a, b, c]?
} in (c, b, a)!
```

Mathematically, `?` is \\(\bot\\), the unit for \\(⅋\\). So \\(\bot \mathbin{⅋} A = \mathbf{1} \multimap A \cong A\\) (as seen before for the [unit](#the-unit-type) type).

todo: are `!` and `?` Rust's `()` and `!` or is that `{}` and `either {}`?

## Recursive Types

> **<sup>Syntax</sup>**\
> _RecursiveType_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `rec` [_LoopLabel_]<sup>?</sup> _Type_
>
> _Self_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `self` [_LoopLabel_]<sup>?</sup>

todo: General, mathematical, constraints

A recursive type can be used within itself via `self`

Recursive types are mostly used in conjunction with either types:
```par
type List<T> = rec either {
  .empty!
  .item(T) self
}
```

Values of recursive types terminate (todo: correct?)
```par
// a simple List
let l: List<Bool> = .item(.true!).item(.false!).empty!
```

## Iterative Types

> **<sup>Syntax</sup>**\
> _IterativeType_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `iter` [_LoopLabel_]<sup>?</sup> _Type_
>
> _Loop_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `loop` [_LoopLabel_]<sup>?</sup>

todo: General, mathematical, constraints

An iterative type can be used within itself via `loop`

Iterative types are mostly used in conjunction with choice types, for example:
```par
type Repl<T> = iter {
  .copy => (loop, loop)!
  .drop => !
  .unwrap => T
}
```

Here we construct
```par
?type Bool = either { .true!, .false! }
?type Nat = rec either { .zero!, .succ self }
?type List<T> = rec either { .empty!, .item(T) self }
?
// construct a list of Repl<Bool>
// i.e. a value of a recursive type
dec repeat : [Nat, Repl<Bool>] List<Bool>
def repeat = [n, b] n begin {
  .zero! => do { b.drop? } in .empty!
  .succ pred => let (b, c)! = b.copy in .item(c.unwrap) pred loop
}

// construct a value of the iterative Repl<Bool>
dec repl_bool : [Bool] Repl<Bool>
def repl_bool = [b] begin {
  .copy => b {
    .true! => (let b: Bool = .true! in loop, let b: Bool = .true! in loop)!
    .false! => (let b: Bool = .false! in loop, let b: Bool = .false! in loop)!
  }
  .drop => b {
    .true! => !
    .false! => !
  }
  .unwrap => b
}
?
?def main = repeat(.succ.succ.succ.zero!, repl_bool(.true!))
```

Values of iterative types may be infinite (todo: correct?)
```par
type Inf<T> = iter (T) loop

def infinite_bools: Inf<Bool> = begin (.true!) loop
```

## Existential Types

> **<sup>Syntax</sup>**\
> _ExistentialType_ : `(` `type` [_ID_List_] `)` _Type_

Having multiple types between the `()` is just syntax sugar:
```par
type T = (type A, B) X
// is equivalent to
type T = (type A) (type B) X
```

Existential types mirror tuple types but they're qualified over types rather than values.
They are used to obscure their underlying type.
```par
?type Bool  = either { .true!, .false! }
?type Nat = recursive either { .zero!, .succ self }
?
type Any = (type T) T

?def main = chan user {
// create values of the existential Any type
// note that both have exactly the same type!
let any1: Any = (type Bool) .true!
let any2: Any = (type Nat) .succ.zero!
?
?user(any1)
?user(any2)
?user!
?}
```

The qualifying types and values can both be extracted from a value of such a type:
```par
let any: Any = ...
let (type X) x = any
let y: X = x
```
todo: This doesn't work in Par currently. Is this intended?

## Universal Types