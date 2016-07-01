module meta_ex;

import std.range : isInputRange;
import std.meta : NoDuplicates, AliasSeq;

static if (__VERSION__ >= 2070)
{
    import std.meta : aliasSeqOf;
}
else
{
    /** Create $(D AliasSeq) from Static Array.
        See also: http://forum.dlang.org/thread/cxahuyvnygtpsaseieeh@forum.dlang.org
        See also: https://github.com/D-Programming-Language/phobos/pull/3785
    */
    template aliasSeqOf(TL...)
        if (TL.length == 1 &&
            isInputRange!(typeof(TL[0])))
    {
        import std.range: front, empty, popFront;
        alias TT = AliasSeq;
        enum r = TL[0];
        static if (r.empty)
        {
            alias aliasSeqOf = TT!();
        }
        else
        {
            enum f = r.front;
            alias aliasSeqOf = TT!(
                f,
                aliasSeqOf!(
                    { auto tmp = r; tmp.popFront(); return tmp; }()
                    )
                                   );
        }
    }
}

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
