module meta_ex;

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

/** Returns: `things` forwarded through calls to `fun`.
 *
 * See also: https://forum.dlang.org/post/zjxmreegqkxgdzvihvyk@forum.dlang.org
 */
auto forwardMap(alias fun, Ts ...)(Ts things) @trusted
{
    import std.meta : aliasSeqOf, staticMap;
    import std.range : iota;
    import std.typecons : Tuple;
    import std.conv : emplace;

    alias NewType(size_t i) = typeof(fun(things[i]));
    alias NewTypes = staticMap!(NewType,
                                aliasSeqOf!(iota(things.length)));
    Tuple!NewTypes results = void;
    static foreach (i, thing; things)
    {
        emplace(&results[i], fun(thing));
    }
    return results;
}

@safe pure unittest
{
    import std.typecons : Tuple;
    alias X = Tuple!(int, float, double, string);
    auto x = X(1, 1.2f, 1.2, "1.2");
    auto y = forwardMap!(_ => _)(x.tupleof);
    static assert(is(typeof(x) == typeof(y)));
}
