module meta_ex;

@safe:

import std.range : isInputRange;
import std.meta : NoDuplicates, AliasSeq;

import std.meta : aliasSeqOf;

alias toAliasSeq = aliasSeqOf;

@safe pure nothrow @nogc unittest
{
    import std.range : iota;
    foreach(i; aliasSeqOf!(iota(10)))
    {
        // pragma(msg, i);
    }
}

alias Deduplicate = NoDuplicates;
alias Uniq = NoDuplicates;

/**
   See also: http://forum.dlang.org/post/sulxqtfprmkeekjatqup@forum.dlang.org
*/
template Merge1(A...)
    if (!(A.length & 1))
{
    static if (A.length == 0)
    {
        alias Merge1 = AliasSeq!();
    }
    else
    {
        alias Left = A[0 .. $ / 2];
        alias Right = A[$ / 2 .. $];
        alias Merge1 = AliasSeq!(Left[0], Right[0], Merge1!(Left[1 .. $], Right[1 .. $]));
    }
}

@safe pure nothrow @nogc unittest
{
    struct S(A...) {} // needed to reliably compare AliasSeq's for equality

    alias first = AliasSeq!(int, string, bool);
    alias second = AliasSeq!("abc", "def", "ghi");
    alias third = Merge1!(first, second);

    static assert(is(S!third == S!(int,    "abc",
                                   string, "def",
                                   bool,   "ghi")));
}

/**
   See also: http://forum.dlang.org/post/sulxqtfprmkeekjatqup@forum.dlang.org
*/
template Merge(A...)
{
    template With(B...)
    {
        static if (A.length == 0 ||
                   B.length == 0)
            alias With = AliasSeq!(A, B); // or static assert(0) if you require equal lengths
        else
            alias With = AliasSeq!(A[0], B[0], Merge!(A[1 .. $]).With!(B[1 .. $]));
    }
}

@safe pure nothrow @nogc unittest
{
    struct S(A...) {} // needed to reliably compare AliasSeq's for equality

    alias first = AliasSeq!(int, string, bool);
    alias second = AliasSeq!("abc", "def", "ghi");
    alias third = Merge!first.With!second;

    static assert(is(S!third == S!(int, "abc",
                                   string, "def",
                                   bool, "ghi")));

    alias fourth = Merge!(first[0 .. 2]).With!second;

    static assert(is(S!fourth == S!(int, "abc",
                                    string, "def",
                                    "ghi")));
}

/** Mixin for generating `struct` member `byRef`.
    See also: http://forum.dlang.org/post/o0vk14$2j89$1@digitalmars.com
 */
mixin template RvalueRef()
{
    alias T = typeof(this);
    static assert (is(T == struct));

    @nogc @safe
    ref const(T) byRef() const return
    {
        return this;
    }
}

@safe pure nothrow @nogc unittest
{
    struct Vector
    {
        float x, y;
        mixin RvalueRef;
    }

    void useVector(ref const Vector pos) {}

    Vector v = Vector(42, 23);

    useVector(v);                     // works
    useVector(Vector(42, 23).byRef);  // works as well, and use the same function
}

// Use same as staticIndexOf
template staticAssignableTypeIndexOf(U)
{
    static auto f(U)()
    {
        static foreach (i, T; Types)
        {
            import std.traits : isAssignable;
            static if (isAssignable!(T, U))
            {
                return i;
            }
        }
        return 0;
    }
    enum canStore = f!U;
}

import std.functional : unaryFun;

/** Returns: `xs` forwarded through calls to `fun` and packed into a `std.typecons.Tuple`.
 *
 * See also: https://forum.dlang.org/post/zjxmreegqkxgdzvihvyk@forum.dlang.org
 */
auto forwardMap(alias fun, Ts...)(Ts xs)
    if (is(typeof(unaryFun!(fun))))
{
    import std.meta : staticMap;
    alias MappedTypeOf(T) = typeof(fun(T.init));
    alias NewTypes = staticMap!(MappedTypeOf, Ts);

    import std.typecons : Tuple;
    Tuple!NewTypes ys = void;

    alias fun_ = unaryFun!(fun);

    import std.conv : emplace;
    static foreach (immutable i, x; xs)
    {
        emplace(&ys[i], fun_(x));
    }

    return ys;
}

@safe pure unittest
{
    import std.typecons : Tuple;
    alias X = Tuple!(int, float, double);
    auto x = X(42, 42f, 42);
    auto y = forwardMap!(_ => _ + 1)(x.tupleof);
    static assert(is(typeof(x) == typeof(y)));
    assert(y == X(43, 43f, 43));
}

/** Flattens a list of ranges and non ranges.
 *
 * If a type is a range then its `ElementType` is used
 */
template FlattenedRanges(Values...)
{
    import std.meta : AliasSeq;
    static if (Values.length)
    {
        import std.range : isInputRange;
        alias Head = Values[0];
        alias Tail = Values[1 .. $];
        static if (isInputRange!Head)
        {
            import std.range : ElementType;
            alias FlattenedRanges = FlattenedRanges!(ElementType!Head, FlattenedRanges!Tail);
        }
        else
        {
            alias FlattenedRanges = AliasSeq!(Head, FlattenedRanges!Tail);
        }
    }
    else
    {
        alias FlattenedRanges = AliasSeq!();
    }
}

///
@safe unittest
{
    import std.algorithm : filter;
    import std.meta : AliasSeq;

    alias R1 = typeof([1, 2, 3].filter!"true");
    alias R2 = typeof([1.0, 2.0, 3.0]);

    static assert(is(FlattenedRanges!(int, double) == AliasSeq!(int, double)));
    static assert(is(FlattenedRanges!(int, R1, R2) == AliasSeq!(int, int, double)));

    import std.traits : CommonType;

    static assert(is(CommonType!(FlattenedRanges!(int, R1, R2, float)) == double));
}

/** Returns the types of all values given.
 *
 * If a `T` is an expression it is resolved with `typeof` else it is just
 * appended.
 *
 * Returns: `AliasSeq` of the resulting types
*/
template TypesOf(Values...)
{
    import std.meta : AliasSeq;
    import std.traits : isExpressions;
    static if (Values.length)
    {
        static if (isExpressions!(Values[0]))
        {
            alias T = typeof(Values[0]);
        }
        else
        {
            alias T = Values[0];
        }
        alias TypesOf = AliasSeq!(T, TypesOf!(Values[1 .. $]));
    }
    else
    {
        alias TypesOf = AliasSeq!();
    }
}

///
@safe pure nothrow @nogc unittest
{
    import std.meta : AliasSeq;
    static assert(is(TypesOf!("hello", 1, 2, 3.0, real) ==
                     AliasSeq!(string, int, int, double, real)));
}

/** Can be used to construct a meta function that checks if a symbol is of a type.
 */
template typeOf(T)
{
    auto typeOf(U)(U)
    {
        return is(U == T);
    }
    enum typeOf(alias a) = typeOf!T(a);
}

///
@safe pure nothrow @nogc unittest
{
    import std.meta : allSatisfy, AliasSeq;
    static assert(typeOf!int(3));
    static assert(allSatisfy!(typeOf!int, 3));
}

template from(string moduleName)
{
    mixin("import from = " ~ moduleName ~ ";");
}
