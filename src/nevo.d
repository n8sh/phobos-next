#!/usr/bin/env rdmd-dev-module

/** Neuroevolution.

   See also: https://en.wikipedia.org/wiki/Neuroevolution

   TODO Profile use of cellops and logops in a specific domain or pattern och
   and use roulette wheel sampling based on these in future patterns.
*/
module evo;

version(unittest)
{
    import dbgio : dln;
}

@safe pure:

/** Low-Level (Genetic Programming) Operation Network Node Types often implemented
    in a modern hardware (CPU/GPU).

    See also: http://llvm.org/docs/LangRef.html#typesystem
    See also: http://llvm.org/docs/LangRef.html#instref
*/
enum LOp : ubyte
{
    sum, prod, min, max
}

/** Obselete.
*/
enum LOp_ : ubyte
{
    id, /**< Identity. */

    /* \name arithmetical: additive */
    /* @{ */
    add,                      /**< add (mi1o) */
    sub,                      /**< subtract (mi1o) */
    neg,                      /**< negation (1i1o) */
    /* @} */

    /* \name arithmetical: multiplicative */
    /* @{ */
    mul,                      /**< multiply (mimo) */
    div,                      /**< divide (mi1o) */
    pow,                      /**< power (2i1o) */
    inv,                      /**< inverse (1i1o) */
    /* @} */

    /* \name Harmonic */
    /* @{ */
    sin,                      /**< sine (1i1o) */
    cos,                      /**< cosine (1i1o) */
    tan,                      /**< tangens (1i1o) */

    /* \name Inverse Harmonic */
    csc,                      /**< 1/sin(a) */
    cot,                      /**< cotangens (1i1o) */
    sec,                      /**< 1/cos(a) */

    // Arcus
    acos,                     /**< arcus cosine (1i1o) */
    asin,                     /**< arcus sine (1i1o) */
    atan,                     /**< arcus tanges (1t2i1o) */
    acot,                     /**< arcus cotangens (1i1o) */
    acsc,                     /**< arcus css (1i1o) */
    /* @} */

    /* \name hyperbolic */
    /* @{ */
    cosh, sinh, tanh, coth, csch,
    acosh, asinh, atanh, acoth, acsch,
    /* @} */

    // /* \name imaginary numbers */
    // /* @{ */
    // real, imag, conj,
    // /* @} */

    /* \name reorganizer/permutators/permutations */
    /* @{ */
    identity,                 /**< identity (copy) (1imo) */
    interleave,               /**< interleave (1imo) */
    flip,                     /**< flip (1i1o) */
    mirror,                   /**< mirror (1i1o) */
    shuffle,                  /**< shuffle (1i1o) */
    /* @} */

    /* \name generators */
    /* @{ */
    ramp,                     /**< linear ramp (2imo) */
    zeros,                    /**< zeros (0imo) */
    ones,                     /**< ones (0imo) */
    rand,                     /**< ranodm (0imo) */
    /* @} */

    /* \name comparison */
    /* @{ */
    eq,                       /**< equality (mi1o) */
    neq,                      /**< non-equality (mi1o) */
    lt,                       /**< less-than (2i1o) */
    lte,                      /**< less-than or equal (2i1o) */
    gt,                       /**< greater-than (2i1o) */
    gte,                      /**< greater-than or equal (2i1o) */
    /* @} */

    /* \name logical */
    /* @{ */
    all,                      /**< all (non-zero) (mi1o) */
    any,                      /**< any (non-zero) (mi1o) */
    none,                     /**< none (zero) (mi1o) */
    /* @} */
}

version(none)
{
/** Return non-zero if \p lop is an \em Arithmetical \em Additive Operation. */
bool isAdd(LOp lop)
{
    return (lop == LOp.add ||
            lop == LOp.sub ||
            lop == LOp.neg);
}

/** Return non-zero if \p lop is an \em Arithmetical \em Additive Operation. */
bool isMul(LOp lop)
{
    return (lop == LOp.mul ||
            lop == LOp.div ||
            lop == LOp.pow ||
            lop == LOp.inv);
}

bool isArithmetic(LOp lop)
{
    return isAdd(lop) && isArithmetic(lop);
}

/** Return non-zero if \p lop is an \em Trigonometric Operation. */
bool isTrigonometric(LOp lop)
{
    return (lop == LOp.cos ||
            lop == LOp.sin ||
            lop == LOp.tan ||
            lop == LOp.cot ||
            lop == LOp.csc ||
            lop == LOp.acos ||
            lop == LOp.asin ||
            lop == LOp.atan ||
            lop == LOp.acot ||
            lop == LOp.acsc
        );
}

/** Return non-zero if \p lop is an \em Hyperbolic Operation. */
bool isHyperbolic(LOp lop)
{
    return (lop == LOp.cosh ||
            lop == LOp.sinh ||
            lop == LOp.tanh ||
            lop == LOp.coth ||
            lop == LOp.csch ||
            lop == LOp.acosh ||
            lop == LOp.asinh ||
            lop == LOp.atanh ||
            lop == LOp.acoth ||
            lop == LOp.acsch);
}

/** Return non-zero if \p lop is a \em Generator Operation. */
bool isGen(LOp lop)
{
    return (lop == LOp.ramp ||
            lop == LOp.zeros ||
            lop == LOp.ones ||
            lop == LOp.rand);
}

/** Return non-zero if \p lop is a \em Comparison Operation. */
bool isCmp(LOp lop)
{
    return (lop == LOp.eq ||
            lop == LOp.neq ||
            lop == LOp.lt ||
            lop == LOp.lte ||
            lop == LOp.gt ||
            lop == LOp.gte);
}

/** Return non-zero if \p lop is a \em Permutation/Reordering Operation. */
bool isPermutation(LOp lop)
{
    return (lop == LOp.identity ||
            lop == LOp.interleave ||
            lop == LOp.flip ||
            lop == LOp.mirror ||
            lop == LOp.shuffle);
}
}

/**m Network (Transformation) Operation Type Code.
 *
 * Instructions for a Program that builds \em Computing Networks (for
 * example Artificial Nerual Networks ANN).
 *
 * TODO What does \em nature cell this information bearer: Closest I
 * have found is http://en.wikipedia.org/wiki/Allele.
 */
enum GOp : ubyte
{
    /* \name Structural: Inter-Node */
    /* @{ */
    seq,                    /**< Sequential Growth. Inherit Initial Value. Set weight to +1. State is \em ON. */
    par,                    /**< Parallel Growth */

    clone,                  /**< Clone. Like \c PAR but  */
    /* @} */

    /* \name Local: Intra-Node */
    /* @{ */
    set_lop,                /**< Set Node Logic Operation (see \c LOp). */

    set_otype, /**< Set Out-Type. Either \c int, \c float or \c double for now. */

    inc_bias,               /**< Increase Threshold */
    dec_bias,               /**< Decrease Threshold */

    /* There is \em no need for "in-precision", since these are the same
       as in-precision the "previous" nodes in the network. */
    inc_oprec,              /**< Increase Out-Precision */
    dec_oprec,              /**< Decrease Out-Precision */

    inc_linkreg,            /**< Increase In-Link-Register */
    dec_linkreg,            /**< Decrease In-Link-Register */

    on,                     /**< \em Activate Current In-Link. */
    off,                    /**< \em Deactivate Current In-Link. */

    /* @} */

    wait,                   /**< Wait (Delay) one clock-cycle? */
    rec,                    /**< \em Recurse up to top (decreasing life with one). */

    jmp,                    /**< \em Jump to sub-Network/Procedure/Function. */
    def,                    /**< \em Define sub-Network/Procedure/Function. */

    end,                    /**< \em Recursion Terminator in serialized code (stream of t's). */
    null_,                   /**< \em Network Terminator in serialized code (stream of t's). */
}

// wchar
// wchar_from_LOP(LOp lop)
// {
//     wchar_t wc = UNICODE_UNKNOWN;
//     switch (lop) {
//     case LOp.add: wc = UNICODE_CIRCLED_PLUS; break;
//     case LOp.sub: wc = UNICODE_CIRCLED_MINUS; break;
//     case LOp.neg: wc = UNICODE_SQUARED_MINUS; break;
//     case LOp.mul: wc = UNICODE_CIRCLED_TIMES; break;
//     case LOp.div: wc = UNICODE_CIRCLED_DIVISION_SLASH; break;
//     case LOp.pow: wc = UNICODE_UNKNOWN; break;
//     case LOp.inv: wc = UNICODE_CIRCLED_DIVISION_SLASH; break;
//     default: break;
//     }
//     return wc;
// }

import array_ex : UncopyableArray;
import std.bitmanip : BitArray;

import vary : FastVariant;

alias Data = FastVariant!(long, double);
alias Datas = UncopyableArray!Data;

/// Parameter Link.
struct Link
{
    size_t inIx;                // input `Cell` index, relative to current
    size_t outIx;               // output `Cell` index, relative to current
}
alias Links = UncopyableArray!Link;

/// Scalar Operation Count.
alias OpCount = size_t;

/// Calculating Cell
struct Cell
{
    import std.algorithm.iteration : map, filter, fold;

    @safe pure nothrow:

    OpCount sum(const ref Datas ins, ref Datas outs) const @trusted
    {
        import std.algorithm.iteration : sum;
        outs.length = 1;
        outs[0] = ins[].map!(_ => _.commonValue)
                       .sum();
        return ins.length - 1;
    }

    OpCount prod(const ref Datas ins, ref Datas outs) const @trusted
    {
        outs.length = 1;
        outs[0] = ins[].filter!(_ => _.hasValue)
                       .map!(_ => _.commonValue)
                       .fold!((a, b) => a * b)(cast(Data.CommonType)1.0);
        return ins.length - 1;
    }

    OpCount emin(const ref Datas ins, ref Datas outs) const @trusted
    {
        import std.algorithm : minElement;
        outs.length = 1;
        outs[0] = ins[].filter!(_ => _.hasValue)
                       .map!(_ => _.commonValue)
                       .minElement(+Data.CommonType.max);
        return ins.length - 1;
    }

    OpCount emax(const ref Datas ins, ref Datas outs) const @trusted
    {
        import std.algorithm : maxElement;
        outs.length = 1;
        outs[0] = ins[].filter!(_ => _.hasValue)
                       .map!(_ => _.commonValue)
                       .maxElement(-Data.CommonType.max);
        return ins.length - 1;
    }

    OpCount execute(const ref Datas ins, ref Datas outs) const
    {
        typeof(return) opCount = 0;

        if (ins.empty) { return opCount; }

        import std.algorithm.comparison : min, max;

        final switch (lop)
        {
        case LOp.sum:
            opCount += sum(ins, outs);
            break;
        case LOp.prod:
            opCount += prod(ins, outs);
            break;
        case LOp.min:
            opCount += emin(ins, outs);
            break;
        case LOp.max:
            opCount += emax(ins, outs);
            break;
        }
        return opCount;
    }

    LOp lop;                    /// operation
}
alias Cells = UncopyableArray!Cell;

/// Network of cells.
struct Network
{
    @safe pure /*TODO nothrow @nogc*/:

    this(size_t cellCount, size_t linkCount)
    {
        import std.random : Random, uniform;
        auto gen = Random();

        cells.reserve(cellCount);
        temps.reserve(cellCount);

        foreach (immutable i; 0 .. cellCount)
        {
            cells ~= Cell(gen.uniform!LOp);
            temps ~= Data(gen.uniform!long);
        }

        links.reserve(linkCount);
        foreach (immutable i; 0 .. linkCount)
        {
            links ~= Link(uniform(0, cellCount, gen),
                          uniform(0, cellCount, gen));
        }
    }

    Datas[] getIns() @trusted
    {
        typeof(return) ins;
        ins.length = cells.length;
        foreach (immutable link; links[])
        {
            ins[link.inIx] ~= temps[link.inIx];
        }
        return ins;
    }

    /// One step forward.
    OpCount step() @trusted
    {
        typeof(return) opCount = 0;

        const ins = getIns();

        foreach (immutable i, ref cell; cells)
        {
            Datas tempOuts;         // data to be filled
            if (immutable opCount_ = cell.execute(ins[i], tempOuts))
            {
                opCount += opCount_;
                temps[i] = tempOuts[0];
            }
        }
        return opCount;
    }

    BitArray pack() nothrow @nogc
    {
        typeof(return) bits;
        return bits;
    }

    static typeof(this) unpack(in BitArray bits) nothrow @nogc
    {
        typeof(this) that;
        return that;
    }

    Cells cells;                /// operation/function cells
    Datas temps;                /// temporary outputs from cells
    Links links;                /// input-to-output links
}

unittest
{
    immutable cellCount = 1_000;
    immutable linkCount = 1_000;
    auto task = Network(cellCount, linkCount);

    immutable stepCount = 1_0;
    foreach (immutable i; 0 .. stepCount)
    {
        task.step();
    }

    dln("done");
}
