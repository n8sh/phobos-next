# D Programming Language TODO

## Tools

Add flycheck target that compiles and runs current module with `ldmd2
-fsanitize=address -unittest`.

## Language

- Add `__traits(isLvalue)` and `__traits(isRvalue)` working also with `this` and
use to improve `gmp-d` and specialize `HashMapOrSet.byElement()` and
`HashMapOrSet.opSlice()`.

### Add Rust-style pass by move

for types qualified with `unique` as in

```D
class C;

unique C x;
y = x;
f(x); // should error, x already moved

unique C y;
f(y);
z = y; // should error, y already moved
```

### Cast
Because `cast()` is allowed for casting away `const` or `immutable` it should
also allow the reverse casting with the syntax `cast(const)` and
`cast(immutable)`. See for instance the doc example
for
[BigInt.opBinaryRight](http://dlang.org/phobos/std_bigint.html#.BigInt.opBinaryRight).

### Stricter enums (onging)
Disallow comparison of enumerators from different enums such as here

```D
enum E { e }
enum D { d }
assert(E.e == D.d);
```
## Determinism

New attribute `@deterministic` for data and code that becomes deterministic in
both space and time.

## Compiler

## Make inout types usable as structure template parameters
https://issues.dlang.org/show_bug.cgi?id=9983

AFAICT, shouldn't be that hard...

## Infer scoped GC allocations by tracing their references and if they are passed only as scope parameters to functions in their defined scope or as aliases

And make their destruction deterministic.

For instance:

```D

T sum(T)(in T[] x) // x cannot escape scope of `sum`
{
    /// calculate and return sum of `x` ...
}

double f(size_t n)
{
    auto x = new int[n]; // scoped (deallocation) should be inferred
    auto y = sum(x); // cannot alias `x`
    return y;
}
```

For details see: http://forum.dlang.org/post/bsmoxhxakxgmkipksvef@forum.dlang.org

## Make template instantiations fully lazy
1. function members of template structs and classes

This will speed up container instantations.

## Automatic `move` when passing last reference

## Add builtin property `x.moveSelf` and `x.moveTo(y)` that avoids zero-init at the end and forbids use after movee like in Rust.

### Diagnostics

- Detect infinite functions recursion that overflow the stack for instance

```D
struct S
{
   void f() { f(); }
   int g() { return g(); }
}
void f() { f(); }
int g() { return g(); }
```

Pure functions should be easier.

- Better diagnostics when calling abstract members

```D
/// Test diagnostics for abstract members.
module abstract_members;

enum Rel { subkindOf, partOf }

class Edge
{
    @safe pure:
    abstract Rel rel() const nothrow @nogc;
}

class SubkindOf : Edge
{
    @safe pure:
}

@safe pure nothrow unittest
{
    auto subkindOf = new SubkindOf();
}
```

errors as


```
/home/per/Work/knet/phobos-next/snippets/abstract_members.d(19,22): Error: cannot create instance of abstract class `SubkindOf`
/home/per/Work/knet/phobos-next/snippets/abstract_members.d(19,22):        function `Rel rel() const pure nothrow @nogc @safe` is not implemented
```

should something like be

```
/home/per/Work/knet/phobos-next/snippets/abstract_members.d(19,22): Error: cannot create instance of abstract class `SubkindOf`
/home/per/Work/knet/phobos-next/snippets/abstract_members.d(9,4):          because abstract function `Rel rel() const pure nothrow @nogc @safe` defined here
/home/per/Work/knet/phobos-next/snippets/abstract_members.d(12,22):        is not implemented in class `SubkindOf` defined here
```

- Overriding

```D
abstract scope inout(Zing)[] nthActors(size_t i)() inout return nothrow @nogc;
```

with

```D
override scope inout(Zing)[] nthActors(size_t i)() inout return nothrow @nogc;
```

in subclass

says

```
src/knet/edge.d(829,34): Error: function knet.edge.EdgeM.nthActors!0LU.nthActors cannot override a non-virtual function
```

when is should say

```
virtual functions cannot be templates
```

- Detect possible null pointer dereferences

- Pair calls to `!x.empty()` with `x.front() x.popFront()` with no mutation in
  between.

- Error/Warn about statements with no effect such as
  - x = x;

- Better diagnostics for Error: `pure function 'knet.base.Db.at' cannot call
  impure function 'hashmap_or_hashset.HashMapOrSet!(Lemma, Ix!(NodeInst, uint,
  false), null, FNV!(64LU, true), 1u, 2u, 1u).HashMapOrSet.opIndex!().opIndex'`

  by adding

  `defined at ...`

- Lookup transitively failing template restrictions qualifiers for pure, nothrow etc

- Hint about using `dup` when copy ctor is disabled and type has `dup` member
  and instance needs to be copied either explicitly or implicitly

- The message

`struct basic_array.S is not copyable because it is annotated with @disable`

should be more descriptive and show location where copy construction was
disabled possibly implicitly because of uncopyable members.

- Explain that template braces need to be added for non-templated functions with
  auto ref parameters

- Show unused symbols (imports, aliases, variables, constants, etc).

## My tasks
- [DIP-90](https://wiki.dlang.org/DIP90)

## Libraries
- Allow non-copyable containers with `toString` member to be printed via
  `writeln` via `std.conv:to`

- Make returned struct types of `findSplitBefore`, `findSplitAfter` and
  `findSplit` contain lazy definitions of `first`, `middle` and `second`. These
  are calculated from combination of original slice and hit begin and hit
  offset. Will need to be `@trusted` for fast unchecked pointer arithmetic.

- Add ASCII-optimized versions of `find`, `canFind`, `startsWith`, `endsWith`,
`findSplit*` when haystack is an array of `char` etc. Implemented first as
function overload taking the char as template parameter and then reused in
existing algorithms when needle is a `char` or perhaps also when `string` of
length 1.

- Make libraries and compile faster and more robust by using `__traits(compiles,
  { ... } )` instead of `is(typeof( { ... } ))`. See
  http://forum.dlang.org/post/rjrdgmijsmvvsbpinidz@forum.dlang.org

- Specialize `std.algorithm.comparison.equal()` for unordered (set-like)
  containers. Use my trait `isSetLike`.

## Packages

- Create D bindings for PCRE2 and name it `pcre2-d`. Borrow from
  https://docs.neurobin.org/jpcre2/

## DUB

- Add Bash completion for dub parameters such as `--build=BUILD_TYPE`, where
  alternatives for `BUILD_TYPE` are a combination of predefined values and
  parsed from `dub.sdl` and `dub.json`.

## Questions
- Is compilation performance improved when a template instantiation is moved to
  an alias declaration in a separate D module? For instance, as done with
  `NodeIxs` in `knet/base.d`?

## Convert this document to HTML on Ubuntu via

```Shell
TMP=`tempfile --suffix=.html` && pandoc -s -f markdown_github -o $TMP D_TODO.md && $BROWSER $TMP
```
