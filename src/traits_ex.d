#!/usr/bin/env rdmd-dev-module

/** Various extensions to std.traits.
    Copyright: Per Nordlöw 2016-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
    See also: http://forum.dlang.org/thread/jbyixfbuefvdlttnyclu@forum.dlang.org#post-mailman.2199.1353742037.5162.digitalmars-d-learn:40puremagic.com
*/
module traits_ex;

import std.traits: isArray, ParameterTypeTuple, isStaticArray, isDynamicArray, isSomeChar, isSomeString, isExpressions, isIntegral, isSigned, isUnsigned;
import std.range: ElementType, isForwardRange, isRandomAccessRange, isInputRange, isBidirectionalRange, isOutputRange, isIterable;
import std.typecons : Tuple;

public import std.traits : isCopyable;

/** Returns: true iff $(D ptr) is handled by the garbage collector (GC). */
bool isGCPointer(const void* ptr)
    @trusted nothrow
{
    import core.memory : GC;
    return !!GC.addrOf(ptr);
}
alias inGC = isGCPointer;
alias isGCed = isGCPointer;

nothrow unittest
{
    int s;
    int* sp = &s;
    assert(!sp.isGCPointer);
    int* ip = new int;
    assert(ip.isGCPointer);
}

/** Returns: true iff all values $(D V) are the same.
    See also: http://forum.dlang.org/post/iflpslqgrixdjwrlqqvn@forum.dlang.org
    See also: http://forum.dlang.org/post/mheumktihihfsxxxapff@forum.dlang.org
*/
template allSame(V ...)
    if (isExpressions!V)
{
    static if (V.length <= 1)
        enum allSame = true;
    else static if (V.length & 1)
        enum allSame = (V[$ - 1] == V[0] &&
                        V[0 .. $/2] == V[$/2 .. $-1] &&
                        allSame!(V[0 .. $/2]));
    else
        enum allSame = (V[0 .. $/2] == V[$/2 .. $] &&
                        allSame!(V[0 .. $/2]));
}

@safe pure nothrow @nogc unittest
{
    static assert( allSame!());
    static assert( allSame!(42));
    static assert( allSame!(42, 42, 42));
    static assert(!allSame!(42, 43, 42));
}

version(none)                   // disable for now
{
/** Iterative `allSame`. */
template allSameIterative(V...)
    if (isExpressions!V)
{
    bool impl_(V...)()
    {
        static if (V.length >= 2)
        {
            foreach (i, _; V[0 .. $ - 1])
            {
                if (V[i] != V[i + 1])
                {
                    return false;
                }
            }
            return true;        // TODO remove warning: statement not reachable
        }
        else
        {
            return true;
        }
    }
    enum allSameIterative = impl_!V();
}

@safe pure nothrow @nogc unittest
{
    static assert( allSameIterative!());
    static assert( allSameIterative!(42));
    static assert( allSameIterative!(42, 42, 42));
    static assert(!allSameIterative!(42, 43, 42));
}
}

/** Recursive `allSame`. */
template allSameRecursive(V...)
    if (isExpressions!(V))
{
    static if (V.length <= 1)
        enum allSameRecursive = true;
    else
        enum allSameRecursive = V[0] == V[1] && allSameRecursive!(V[1..$]);
}

@safe pure nothrow @nogc unittest
{
    static assert(allSameRecursive!());
    static assert(allSameRecursive!(42));
    static assert(!allSameRecursive!(41, 42));
    static assert(allSameRecursive!(42, 42, 42));
}

/** Returns: true iff all types $(D T) are the same. */
template allSameType(T...)
{
    static if (T.length <= 1)
        enum allSameType = true;
    else
        enum allSameType = is(T[0] == T[1]) && allSameType!(T[1..$]);
}

enum allSameType1(T...) = !T.length || (is(T[0] == T[T.length > 1]) && allSameType1!(T[1 .. $]));

@safe pure nothrow @nogc unittest
{
    static assert(allSameType!(int, int));
    static assert(!allSameType!(int, double));
    static assert(!allSameType!(int, int, double));
    static assert(allSameType!(Tuple!(int, int, int).Types, int));
}

alias allTypesSame = allSameType;
alias isHomogeneous = allSameType;
enum isHomogeneousTuple(T) = isHomogeneous!(T.Types);

@safe pure nothrow @nogc unittest
{
    static assert(isHomogeneousTuple!(Tuple!(int, int, int)));
    static assert(isHomogeneousTuple!(Tuple!(float, float, float)));
    static assert(!isHomogeneousTuple!(Tuple!(int, float, double)));
}

enum isHomogeneousTupleOf(T, E) = (isHomogeneous!(T.Types) &&
                                   is(Unqual!(T.Types[0]) == Unqual!E));

@safe pure nothrow @nogc unittest
{
    import std.typecons : Tuple;
    static assert(isHomogeneousTupleOf!(Tuple!(int, int, int), int));
    static assert(isHomogeneousTupleOf!(Tuple!(float, float, float), float));
    static assert(!isHomogeneousTupleOf!(Tuple!(float, float, float), int));
}

/**
   Returns $(D true) if at least one type in the $(D Tuple T)
   is not the same as the others.
*/
enum isHeterogeneous(T) = !isHomogeneous!T;

import std.typecons : isTuple;

/**
   Returns $(D true) if all types in the $(D Tuple T) are the same.
   TODO: Remove when this is merged: https://github.com/D-Programming-Language/phobos/pull/3395
   See also: https://github.com/D-Programming-Language/phobos/pull/1672/files
*/
template allSameTypesInTuple(T)
    if (isTuple!T)
{
    alias types = T.Types;
    static if (types.length > 0)
    {
        template isSameTypeAsHead(U)
        {
            enum isSameTypeAsHead = is(U == types[0]);
        }
        import std.meta : allSatisfy;
        enum allSameTypesInTuple = allSatisfy!(isSameTypeAsHead, types);
    }
    else
        enum allSameTypesInTuple = true;
}

@safe pure nothrow unittest
{
    import std.typecons : Tuple;
    alias HOTUP = Tuple!(int, int, int);
    static assert(allSameTypesInTuple!HOTUP);

    const HOTUP hotup = HOTUP(1, 2, 3);
    static assert(allSameTypesInTuple!(typeof(hotup)));

    alias HETUP = Tuple!(string, bool, float);
    static assert(!allSameTypesInTuple!(HETUP));

    const HETUP hetup = HETUP("test", false, 2.345);
    static assert(!allSameTypesInTuple!(typeof(hetup)));

    alias ZTUP = Tuple!();
    static assert(allSameTypesInTuple!ZTUP);

    const ZTUP ztup = ZTUP();
    static assert(allSameTypesInTuple!(typeof(ztup)));
}

/** Return Tuple $(D tup) as a Static Array.
    See also: http://dpaste.dzfl.pl/d0059e6e6c09
*/
inout (T.Types[0])[T.length] asStaticArray(T)(inout T tup)
    if (allSameType!(T.Types))
{
    return *cast(T.Types[0][T.length]*)&tup; // hackish
}

pure nothrow @nogc unittest
{
    import std.typecons: tuple;
    const auto tup = tuple("a", "b", "c", "d");
    const string[4] arr = ["a", "b", "c", "d"];
    static assert(is(typeof(tup.asStaticArray()) == typeof(arr)));
    assert(tup.asStaticArray() == arr);
}

/** Return Tuple $(D tup) as a Dynamic Array.
*/
auto asDynamicArray(T)(inout T tup)
    if (allSameType!(T.Types))
{
    alias E = T.Types[0];
    E[] a = new E[T.length];
    a.length = T.length;
    foreach (const i, e; tup)
    {
        a[i] = e;
    }
    return a;
}

pure nothrow unittest
{
    import std.typecons: tuple;
    auto tup = tuple("a", "b", "c", "d");
    string[4] arr = ["a", "b", "c", "d"];
    assert(tup.asDynamicArray() == arr);
}

/** Is `true` if `R` is iterable over references to its elements.
    TODO Add to Phobos.
 */
enum bool isRefIterable(T) = is(typeof({ foreach (ref elem; T.init) {} }));

/// Useful aliases for combinations of range predicates.
enum isIterableOf(R, E) = isIterable!R && is(ElementType!R == E);
enum isIterableOfUnqual(R, E) = isIterable!R && is(Unqual!(ElementType!R) == Unqual!E);
enum isIterableOfSomeString(R) = (isIterable!R && isSomeString!(ElementType!R));

@safe pure nothrow @nogc unittest
{
    alias E = string;
    alias I = int;
    alias R = typeof(["a", "b"]);
    static assert(isIterableOf!(R, E));
    static assert(isIterableOfUnqual!(R, const(E)));
    static assert(isIterableOfSomeString!(R));
    static assert(!isIterableOf!(R, I));
}

/// Useful aliases for combinations of range predicates.
enum isRandomAccessRangeOf(R, E) = isRandomAccessRange!R && is(ElementType!R == E);
enum isForwardRangeOf(R, E) = isForwardRange!R && is(ElementType!R == E);
enum isInputRangeOf(R, E) = isInputRange!R && is(ElementType!R == E);
enum isInputRangeOfUnqual(R, E) = isInputRange!R && is(Unqual!(ElementType!R) == E);
enum isBidirectionalRangeOf(R, E) = isBidirectionalRange!R && is(ElementType!R == E);
enum isOutputRangeOf(R, E) = isOutputRange!R && is(ElementType!R == E);
enum isArrayOf(R, E) = isArray!R && is(ElementType!R == E);
enum isArrayOfSomeString(R) = isArray!R && isSomeString!(ElementType!R);

/// TODO Move to Phobos?
enum isSomeCharString(T) = (is(T == string) ||
                            is(T == const(char)[]) ||
                            is(T == char[]));

unittest
{
    alias R = typeof(["a", "b"]);
    static assert(isArrayOf!(R, string));
    static assert(isArrayOfSomeString!(R));
}

alias isSource = isInputRange;
alias isRange = isInputRange;

alias isSourceOf = isInputRangeOf;
alias isSourceOfUnqual = isInputRangeOfUnqual;
alias isSink = isOutputRange;
alias isSinkOf = isOutputRangeOf;

enum isSourceOfSomeChar(R) = (isSource!R && isSomeChar!(ElementType!R));
alias isSomeCharSource = isSourceOfSomeChar;
alias isSomeLazyString = isSourceOfSomeChar;

unittest
{
    import std.meta : AliasSeq;
    foreach (Ch; AliasSeq!(char, wchar, dchar))
    {
        assert(isSourceOfSomeChar!(Ch[]));
        assert(isSourceOfSomeChar!(const(Ch)[]));
        assert(isSourceOfSomeChar!(immutable(Ch)[]));
    }
}

enum isSourceOfSomeString(R) = (isSource!R && isSomeString!(ElementType!R));
alias isSomeStringSource = isSourceOfSomeString;

import std.functional: unaryFun, binaryFun;

/* TODO Do we need use of unaryFun and binaryFun here? */
alias isEven = unaryFun!(a => (a & 1) == 0); // Limit to Integers?
alias isOdd = unaryFun!(a => (a & 1) == 1); // Limit to Integers?
alias lessThan = binaryFun!((a, b) => a < b);
alias greaterThan = binaryFun!((a, b) => a > b);

/** Check if $(D T) has an even length. */
enum hasEvenLength(T...) = !(T.length & 1);
unittest
{
    static assert(!hasEvenLength!(1));
    static assert(hasEvenLength!(1, 2));
    static assert(!hasEvenLength!(1, 2, 3));
    static assert(hasEvenLength!(1, 2, 3, 4));
}

enum isSignedIntegral(T) = isIntegral!T && isSigned!T;
enum isUnsignedIntegral(T) = isIntegral!T && isUnsigned!T;

enum isString (T) = is(T == string);
enum isWString(T) = is(T == wstring);
enum isDString(T) = is(T == dstring);

enum isEnum(T) = is(T == enum);
unittest
{
    interface I {}
    class A {}
    class B( T ) {}
    class C : B!int, I {}
    struct S {}
    enum E { X }
    static assert(!isEnum!A );
    static assert(!isEnum!( B!int ) );
    static assert(!isEnum!C );
    static assert(!isEnum!I );
    static assert(isEnum!E );
    static assert(!isEnum!int );
    static assert(!isEnum!( int* ) );
}

/* See also: http://d.puremagic.com/issues/show_bug.cgi?id=4427 */
enum isStruct(T) = is(T == struct);
unittest
{
    interface I {}
    class A {}
    class B( T ) {}
    class C : B!int, I {}
    struct S {}
    static assert(!isStruct!A );
    static assert(!isStruct!( B!int ) );
    static assert(!isStruct!C );
    static assert(!isStruct!I );
    static assert(isStruct!S );
    static assert(!isStruct!int );
    static assert(!isStruct!( int* ) );
}

enum isClass(T) = is(T == class);
unittest
{
    interface I {}
    class A {}
    class B( T ) {}
    class C : B!int, I {}
    struct S {}
    static assert(isClass!A );
    static assert(isClass!( B!int ) );
    static assert(isClass!C );
    static assert(!isClass!I );
    static assert(!isClass!S );
    static assert(!isClass!int );
    static assert(!isClass!( int* ) );
}

enum isInterface(T) = is(T == interface);
unittest
{
    interface I {}
    class A {}
    class B( T ) {}
    class C : B!int, I {}
    struct S {}
    static assert(!isInterface!A );
    static assert(!isInterface!( B!int ) );
    static assert(!isInterface!C );
    static assert(isInterface!I );
    static assert(!isInterface!S );
    static assert(!isInterface!int );
    static assert(!isInterface!( int* ) );
}

template isType(T)       { enum isType = true; }
template isType(alias T) { enum isType = false; }

unittest
{
    struct S { alias int foo; }
    static assert(isType!int );
    static assert(isType!float );
    static assert(isType!string );
    //static assert(isType!S ); // Bugzilla 4431
    static assert(isType!( S.foo ) );
    static assert(!isType!4 );
    static assert(!isType!"Hello world!" );
}

/** Note that `NotNull!T` is not `isNullable`. */
template isNullable(T)
{
    import std.traits: isAssignable;
    enum isNullable = isAssignable!(T, typeof(null));
}
///
unittest
{
    static assert(isNullable!(int*));
}

enum nameOf(alias a) = a.stringof;
///
unittest
{
    int var;
    static assert(nameOf!var == var.stringof);
}

/** Is $(D ElementType) of type of $(D a). */
alias ElementTypeOf(alias a) = ElementType!(typeof(a));
///
unittest
{
    int[] var;
    static assert(is(ElementTypeOf!var == int));
}

template Chainable()
{
    import std.range: chain;
    auto ref opCast(Range)(Range r)
    {
        return chain(this, r);
    }
}
unittest { mixin Chainable; }

/** Returns true if `T` is an instance of the template `S`.
    See also: http://forum.dlang.org/thread/mailman.2901.1316118301.14074.digitalmars-d-learn@puremagic.com#post-zzdpfhsgfdgpszdbgbbt:40forum.dlang.org
*/
template isA(alias S, T)
{
    import std.traits : isInstanceOf;
    enum isA = isInstanceOf!(S, T);
}

unittest
{
    import std.traits : isInstanceOf;
    import std.range : SortedRange, assumeSorted;
    const x = [1, 2, 3].assumeSorted;
    static assert(isInstanceOf!(SortedRange, typeof(x)));
    static assert(isA!(SortedRange, typeof(x)));
}

/** See also: http://forum.dlang.org/thread/bug-6384-3@http.d.puremagic.com/issues/
    See also: http://forum.dlang.org/thread/jrqiiicmtpenzokfxvlz@forum.dlang.org */
enum isOpBinary(T, string op, U) = is(typeof(mixin("T.init" ~ op ~ "U.init")));

enum isComparable(T) = is(typeof({ return T.init <  T.init; })); /// TODO Move to Phobos' std.traits
enum isEquable   (T) = is(typeof({ return T.init == T.init; })); /// TODO Move to Phobos' std.traits
enum isNotEquable(T) = is(typeof({ return T.init != T.init; })); /// TODO Move to Phobos' std.traits

unittest
{
    static assert(isComparable!int);
    static assert(isComparable!string);
    static assert(!isComparable!creal);
    static struct Foo {}
    static assert(!isComparable!Foo);
    static struct Bar { bool opCmp(Bar) { return true; } }
    static assert(isComparable!Bar);
}

// TODO  variadic
enum areComparable(T, U) = is(typeof({ return T.init <  U.init; })); /// TODO Move to Phobos' std.traits
enum areEquable   (T, U) = is(typeof({ return T.init == U.init; })); /// TODO Move to Phobos' std.traits
enum areNotEquable(T, U) = is(typeof({ return T.init != U.init; })); /// TODO Move to Phobos' std.traits

unittest
{
    static assert(areComparable!(int, float));
    static assert(areEquable!(int, float));
    static assert(areNotEquable!(int, float));

    static assert(!areComparable!(int, string));
    static assert(!areEquable!(int, string));
    static assert(!areNotEquable!(int, string));
}

enum isValueType(T) = !hasIndirections!T;
// enum isValueType(T) = isScalarType!T || isStaticArray!T || isStruct!T;
enum hasValueSemantics(T) = !hasIndirections!T; // TODO merge with isValueType

/* See also: http://forum.dlang.org/thread/hsfkgcmkjgvrfuyjoujj@forum.dlang.org#post-hsfkgcmkjgvrfuyjoujj:40forum.dlang.org */
enum isReferenceType(T) = isDynamicArray!T || isSomeString!T || isClass!T;

enum arityMin0(alias fun) = __traits(compiles, fun());

/** TODO Unite into a variadic.
    See also: http://forum.dlang.org/thread/bfjwbhkyehcloqcjzxck@forum.dlang.org#post-atjmewbffdzeixrviyoa:40forum.dlang.org
*/
enum isCallableWith(alias fun, T) = (is(typeof(fun(T.init))) ||
                                     is(typeof(T.init.fun))); // TODO Are both these needed?
unittest
{
    auto sqr(T)(T x) { return x*x; }
    assert(isCallableWith!(sqr, int));
    assert(!isCallableWith!(sqr, string));
}

/* TODO Unite into a variadic.
   See also: http://forum.dlang.org/thread/bfjwbhkyehcloqcjzxck@forum.dlang.org#post-atjmewbffdzeixrviyoa:40forum.dlang.org
 */
enum isCallableWith(alias fun, T, U) = (is(typeof(fun(T.init,
                                                      U.init))) ||
                                        is(typeof(T.init.fun(U)))); // TODO Are both these needed?
unittest
{
    auto sqr2(T)(T x, T y) { return x*x + y*y; }
    assert(isCallableWith!(sqr2, int, int));
    assert(!isCallableWith!(sqr2, int, string));
}

/** Check if $(D T) is a Sorted Range.
    See also: http://forum.dlang.org/thread/lt1g3q$15fe$1@digitalmars.com
*/
template isSortedRange(T)
{
    import std.traits : isInstanceOf;
    import std.range: SortedRange;
    enum isSortedRange = isInstanceOf!(SortedRange, T); // TODO Or use: __traits(isSame, TemplateOf!R, SortedRange)
}

/** Check if Function $(D expr) is callable at compile-time.
    See also: http://forum.dlang.org/thread/owlwzvidwwpsrelpkbok@forum.dlang.org
*/
template isCTFEable(alias fun)
{
    template isCTFEable_aux(alias T)
    {
        enum isCTFEable_aux = T;
    }
    enum isCTFEable = __traits(compiles, isCTFEable_aux!(fun()));
}

template isCTFEable2(fun...)
{
    enum isCTFEable2 = true;
}

unittest
{
    int fun1() { return 1; }
    auto fun1_N()
    {
        import std.array;
//would return Error: gc_malloc cannot be interpreted at compile time,
        /* because it has no available source code due to a bug */
            return [1].array;
    }
    int fun2(int x)
    {
        return 1;
    }
    auto fun2_N(int x){
        import std.array;
//same as fun1_N
        return [1].array;
    }

    int a1;
    enum a2=0;

    static assert(!isCTFEable!(()=>a1));
    static assert(isCTFEable!(()=>a2));

    static assert(isCTFEable!fun1);
    /* static assert(!isCTFEable!fun1_N); */

    static assert(isCTFEable!(()=>fun2(0)));
    /* static assert(!isCTFEable!(()=>fun2_N(0))); */
//NOTE:an alternate syntax which could be implemented would be: static
    /* assert(!isCTFEable!(fun2_N,0)); */
}

/** Check if the value of $(D expr) is known at compile-time.
    See also: http://forum.dlang.org/thread/owlwzvidwwpsrelpkbok@forum.dlang.org
*/
enum isCTEable(alias expr) = __traits(compiles, { enum id = expr; });

unittest
{
    static assert(isCTEable!11);
    enum x = 11;
    static assert(isCTEable!x);
    auto y = 11;
    static assert(!isCTEable!y);
}

import std.traits: functionAttributes, FunctionAttribute, isCallable, ParameterTypeTuple, Unqual;

/** Returns $(D true) if $(D T) is not $(D const) or $(D immutable).
    Note that isConst is true for string, or immutable(char)[], because the
    'head' is mutable.
*/
import std.traits : isMutable;
enum isConst(T) = !isMutable!T;

unittest
{
    static assert(isConst!(const(int)));
    static assert(!isConst!int);
}

import std.traits : CommonType;

/// Is `true` iff `Types` all share a common type.
enum bool haveCommonType(Types...) = !is(CommonType!Types == void);
unittest
{
    static assert(haveCommonType!(bool, int, long));
    static assert(!haveCommonType!(bool, int, string));
}

/** Check if $(D fun) is a pure function. */
enum bool isPure(alias fun) = (isCallable!fun &&
                               (functionAttributes!fun &
                                FunctionAttribute.pure_));

/** Check if $(D fun) is a function purely callable with arguments T. */
enum bool isPurelyCallableWith(alias fun, T...) = (isPure!fun &&
                                                   is(T == ParameterTypeTuple!fun));

unittest
{
    int foo(int x) @safe pure nothrow { return x; }
    static assert(isPure!foo);
    static assert(isPurelyCallableWith!(foo, int));
}

/** Persistently Call Function $(D fun) with arguments $(D args).

    Hash Id Build-Timestamp (Code-Id because we currently have stable way of hashing-algorithms) is Constructed from Data Structure:
    - Hierarchically Mangled Unqual!typeof(instance)
    - Use msgpack in combination with sha1Of or only sha1Of (with extended
    overloads for sha1Of) if available.

    Extend std.functional : memoize to accept pure functions that takes an
    immutable mmap as input. Create wrapper that converts file to immutable mmap
    and performs memoization on the pure function.

*/
auto persistentlyMemoizedCall(alias fun, T...)(T args)
    if (isPure!fun &&
        isCallable!(fun, args))
{
    import std.functional: memoize;
    return fun(args);
}

/** Move std.uni.newLine?
    TODO What to do with Windows style endings?
    See also: https://en.wikipedia.org/wiki/Newline
*/
bool isNewline(C)(C c) @safe pure nothrow @nogc
    if (isSomeChar!C)
{
    import std.ascii: newline; // TODO Probably not useful.
    static if (newline == "\n")
    {
        return (c == '\n' || c == '\r'); // optimized for systems with \n as default
    }
    else static if (newline == "\r")
    {
        return (c == '\r' || c == '\n'); // optimized for systems with \r as default
    }
    else
    {
        static assert(false, "Support Windows?");
    }
}

bool isNewline(S)(S s) @safe pure nothrow @nogc
    if (isSomeString!S)
{
    import std.ascii: newline; // TODO Probably not useful.
    static if (newline == "\n")
    {
        return (s == '\n' || s == '\r'); // optimized for systems with \n as default
    }
    else static if (newline == "\r")
    {
        return (s == '\r' || s == '\n'); // optimized for systems with \r as default
    }
    else static if (newline == "\r\n")
    {
        return (s == "\r\n" || s == '\r' || s == '\n'); // optimized for systems with \r\n as default
    }
    else static if (newline == "\n\r")
    {
        return (s == "\n\r" || s == '\r' || s == '\n'); // optimized for systems with \n\r as default
    }
    else
    {
        static assert(false, "Support windows?");
    }
}

/** Dynamic Variant of $(D EnumMembers).
    See also: http://forum.dlang.org/thread/bspwlfypfishykezzocx@forum.dlang.org#post-dguqnroxbfewerepomwq:40forum.dlang.org
*/
auto enumMembers(T)()
{
    import std.traits : EnumMembers;
    return [EnumMembers!T];
}
alias enumEnumerators = enumMembers;
alias enumConstants = enumMembers;

/** Dynamic Variant of $(D EnumMembers) excluding the enumerator aliases.

    TODO implement with an array and a bitarrayn indiciating if it has been found
    yet or not. Should have linear complexity and much less memory usage than
    `uniqueEnumMembersHashed`.

    See also: http://forum.dlang.org/post/ziappmtvucmuefphblse@forum.dlang.org
    See also: http://forum.dlang.org/post/awihyvzjswwayeqtklly@forum.dlang.org
    See also: http://forum.dlang.org/thread/bspwlfypfishykezzocx@forum.dlang.org#post-dguqnroxbfewerepomwq:40forum.dlang.org
*/
auto uniqueEnumMembers(T)()
    if (is(T == enum))
{
    import std.traits : EnumMembers;
    import std.algorithm : sort, uniq;
    return [EnumMembers!T].sort().uniq; // TODO isn't really only uniq needed?
}

/** Hash-table version of `uniqueEnumMembers`. */
auto uniqueEnumMembersHashed(T)()
    if (is(T == enum))
{
    import std.traits : EnumMembers;
    bool[T] uniquifier;
    foreach (const member; EnumMembers!T)
    {
        uniquifier[member] = true;
    }
    return uniquifier.keys; // `keys` can be evaluate at compile-time but not `byKey`
}

///
@safe pure nothrow /*@nogc*/ unittest
{
    enum E { x, y, z, Z = z, Y = y }
    import std.algorithm.comparison : equal;
    assert(uniqueEnumMembers!E.equal([E.x, E.y, E.z])); // run-time
    static assert(uniqueEnumMembers!E.equal([E.x, E.y, E.z])); // compile-time
    static assert(E.x == 0);
    static assert(E.y == 1);
    static assert(E.z == 2);
    static assert(E.Z == E.z);
    static assert(E.Y == E.y);
    static assert(uniqueEnumMembers!E.equal(uniqueEnumMembersHashed!E));
}

enum sizeOf(T) = T.sizeof;      // TODO Add to Phobos
template sizesOf(T...)          // TODO Add to Phobos
{
    import std.meta : staticMap;
    enum sizesOf = staticMap!(sizeOf, T);
}

@safe pure nothrow @nogc unittest
{
    enum sizes = sizesOf!(bool, short, int, long);

    // static use
    static assert(sizes[0] == 1);
    static assert(sizes[1] == 2);
    static assert(sizes[2] == 4);
    static assert(sizes[3] == 8);

    // dynamic use
    const i = 0;
    assert([sizes][i] == 1);
}

enum stringOf(T) = T.stringof;  // TODO Add to Phobos
template stringsOf(T...)        // TODO Add to Phobos
{
    import std.meta : staticMap;
    enum stringsOf = staticMap!(stringOf, T);
}

@safe pure nothrow @nogc unittest
{
    enum strings = stringsOf!(bool, short, int, long);
}

/** Number of bits required to store a packed instance of $(D T).
    See also: http://forum.dlang.org/thread/okonqhnxzqlqtxijxsfg@forum.dlang.org

    TODO Extend to continuous version; use std.numeric.sumOfLog2s. Ask on
    StackExchange Computer Science for the correct terminology.

    See: http://dlang.org/phobos/std_numeric.html#.sumOfLog2s

    TODO merge with `UsageOf`
   */
template packedBitSizeOf(T)
{
    static if (is(T == enum))
    {
        static assert(T.min != T.max, "enum T must have at least two enumerators");
        import core.bitop : bsr;
        enum range = T.max - T.min; // TODO use uniqueEnumMembers.length instead?
        enum packedBitSizeOf = range.bsr + 1;
    }
    // TODO
    // else static if (isAggregate!T)
    // {
    //     foreach (E; T.tupleof)
    //     {
    //         ....;
    //     }
    // }
    else
    {
        enum packedBitSizeOf = 8*T.sizeof;
    }
}

@safe pure nothrow @nogc unittest
{
    static assert(packedBitSizeOf!ubyte == 8);
    static assert(!__traits(compiles, { enum E1 { x } static assert(packedBitSizeOf!E1 == 1);}));
    enum E2 { x, y }
    static assert(packedBitSizeOf!E2 == 1);
    enum E3 { x, y, z }
    static assert(packedBitSizeOf!E3 == 2);
    enum E4 { x, y, z, w }
    static assert(packedBitSizeOf!E4 == 2);
    enum E5 { a, b, c, d, e }
    static assert(packedBitSizeOf!E5 == 3);
    enum E6 { a, b, c, d, e, f }
    static assert(packedBitSizeOf!E6 == 3);
    enum E7 { a, b, c, d, e, f, g }
    static assert(packedBitSizeOf!E7 == 3);
    enum E8 { a, b, c, d, e, f, g, h }
    static assert(packedBitSizeOf!E8 == 3);
    enum E9 { a, b, c, d, e, f, g, h, i }
    static assert(packedBitSizeOf!E9 == 4);
}

/** Get Dimensionality of Type $(D T).
   See also: http://forum.dlang.org/thread/hiuhqdxtpifhzwebewjh@forum.dlang.org?page=2
*/

template dimensionality (T)
{
    import std.range.primitives : isInputRange;
    template count_dim (uint i = 0)
    {
        static if (is(typeof(T.init.opSlice!i(0, 0))))
        {
            enum count_dim = count_dim!(i+1);
        }
        else static if (i == 0 &&
                        (isInputRange!T ||
                         is(typeof(T.init[0]))))
        {
            enum count_dim = 1;
        }
        else
        {
            enum count_dim = i;
        }
    }
    alias dimensionality = count_dim!();
}

@safe pure nothrow @nogc unittest
{
    static assert(dimensionality!(int[]) == 1);
}

/// Rank of type `T`.
template rank(T)
{
    import std.range.primitives : isInputRange;
    static if (isInputRange!T) // is T a range?
        enum rank = 1 + rank!(ElementType!T); // if yes, recurse
    else
        enum rank = 0; // base case, stop there
}

@safe pure nothrow @nogc unittest
{
    import array_help : s;
    import std.range : cycle;

    auto c = cycle([[0,1].s[],
                    [2,3].s[]].s[]); // == [[0,1],[2,3],[0,1],[2,3],[0,1]...

    assert(rank!(typeof(c)) == 2); // range of ranges

    static assert(rank!(int[]) == 1);
    static assert(rank!(int[][]) == 2);
}

/// Returns: `true` iff `T` is a template instance, `false` otherwise.
template isTemplateInstance(T)
{
    import std.traits : TemplateOf;
    enum isTemplateInstance = is(typeof(TemplateOf!(T)));
}

///
@safe pure nothrow @nogc unittest
{
    struct S(T) { T x; }
    static assert(isTemplateInstance!(S!int));
    static assert(!isTemplateInstance!(int));
}

/** Get identifier (name) string of template instance `I`, or `null` if `I` is
    not a template instance. */
template templateIdentifierOf(I)
{
    import std.traits : TemplateOf;
    static if (isTemplateInstance!I)
    {
        enum templateIdentifierOf = __traits(identifier, TemplateOf!I);
    }
    else
    {
        enum templateIdentifierOf = null;
    }
}
alias templateNameOf = templateIdentifierOf;

///
@safe pure nothrow @nogc unittest
{
    struct S(T) { T x; }
    static assert(templateIdentifierOf!(S!int) == "S");
    static assert(templateIdentifierOf!(int) == null);
}

/** Get number of bits needed to represent the range (0 .. `length`-1). */
template bitsNeeded(size_t length)
{
    static      if (length <= 2)   { enum bitsNeeded = 1; }
    else static if (length <= 4)   { enum bitsNeeded = 2; }
    else static if (length <= 8)   { enum bitsNeeded = 3; }
    else static if (length <= 16)  { enum bitsNeeded = 4; }
    else static if (length <= 32)  { enum bitsNeeded = 5; }
    else static if (length <= 64)  { enum bitsNeeded = 6; }
    else static if (length <= 128) { enum bitsNeeded = 7; }
    else static if (length <= 256) { enum bitsNeeded = 8; }
    else                           { static assert(false, `Too large length`); }
}

/** Get entropy in number of bits of `T`. */
template EntropyBitsOf(T)
{
    import std.traits : isAggregateType, isArray;
    static if (isAggregateType!T)
    {
        // foreach (memberName; __traits(allMembers, T)) // for each member name in `struct TypedKey`
        // {
        //     const member = __traits(getMember, T.init, memberName); // member
        // }
        enum EntropyBitsOf = 8*T.sizeof;
    }
    else
    {
        enum EntropyBitsOf = 8*T.sizeof;
    }
}

@safe pure nothrow @nogc unittest
{
    static assert(EntropyBitsOf!int == 8*int.sizeof);
}

/** Is `true` if `sym` is an l-value, `false` otherwise.
    See also: https://forum.dlang.org/post/mailman.4192.1454351296.22025.digitalmars-d-learn@puremagic.com
    TODO Add to Phobos
*/
enum isLvalue(alias sym) = is(typeof((ref _){}(sym)));

/** Is `true` if `sym` is an l-value, `false` otherwise.
 */
enum isRvalue(alias sym) = !isLvalue!sym;

@safe pure nothrow @nogc unittest
{
    int i;
    string s;
    static assert(isLvalue!i);
    static assert(isLvalue!s);
    static assert(!isLvalue!13);
    static assert(!isLvalue!"a");
}

template ownsItsElements(C)
{
    import std.traits : isCopyable, hasIndirections;
    import std.range.primitives : ElementType;
    enum ownsItsElements = !isCopyable!C && !hasIndirections!(ElementType!C);
}
