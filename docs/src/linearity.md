# Linearity

TODO: explain linearity, data, etc. here

## Data

Some types can be dropped implicitly. Those are called **data** types. They are not subject to linearity. Those types are:
- The [unit](./types.md#the-unit-type) `!`
- [Pairs](./types.md#pair-types) `(A) B` if `A` and `B` are data
- [Either types](./types.md#either-types) `either { .d1 D1, ..., .dn Dn }` if `D1, ..., Dn` are data
- `recursive D`, `iterative D`, `[type T] D`, and `(type T) D` if `D` is data.

Other types are not data, but can behave similarly. For example, a [choice type](./types.md#choice-types) can behave similarly by including `copy` and `drop` labels:
```par
// the choice
type If<A, B> = {
  .true => A,
  .false => B,
}
// becomes "data" by:

// make it iterative
type If<A, B> = iterative {
  .true => A,
  .false => B,

  // add copy label
  .copy => (self, self)!
  // add drop label
  .drop => !
}
```
Note that for now:
- `copy` and `drop` are for exact and cheap duplicates (Data)
- `clone` and `delete` are for copies that behave exactly the same
- `fork` and `close` are for copies that allow parallelization, but using one forked element might influence the other

This distinction will get more useful with [traits](./future.md#traits).

The same pattern can be used to make functions "data":
```par
// the function type
type Mapper<A, B> = [A] B
// becomes "data" by
// wrapping it in a choice
type Mapper<A, B> = iterative {
  // the original
  .unwrap => [A] B,

  .copy => (self, self)!,
  .drop => !
}

// the value
def negate: Mapper<Bool, Bool> = [b] b {
  .true! => .false!,
  .false! => .true!,
}
// becomes
def negate: Mapper<Bool, Bool> = begin {
  .unwrap => [b] b {
    .true! => .false!,
    .false! => .true!,
  }

  .copy => (loop, loop)!,
  .drop => !
}
```
This construction allows a `map` function:
```par
dec map : [type T, U] [List<T>, Mapper<T, U>] List<U>
def map = [type T, U] [list, mapper] list begin {
  .empty! => do {
    // need to drop explicitly
    mapper.drop?
  } in .empty!,
  .item(head) tail => do {
    // need to copy explicitly
    let (mapper, f)! = mapper.copy
    // unwrap f: Mapper<T, U> to [T] U
    f.unwrap
  } in .item(f(head)) tail loop
}
```