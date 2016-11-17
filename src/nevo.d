#!/usr/bin/env rdmd-dev-module

/** Neuroevolution.

   See also: https://en.wikipedia.org/wiki/Neuroevolution

   TODO Profile use of cellops and logops in a specific domain or pattern och
   and use roulette wheel sampling based on these in future patterns.

   TODO reuse traits_ex.packedBitSizeOf

   TODO reuse `IndexedBy` `Cells` and `CellIx`
*/
module nevo;

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
enum LOP : ubyte
{
    summ,                       /// Sum.
    prod,                       /// Product.
    emin,                       /// Mininum.
    emax                        /// Maximum.
}

/** Obselete.
*/
enum LOPobseleted : ubyte
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
bool isAdd(LOP lop)
{
    return (lop == LOP.add ||
            lop == LOP.sub ||
            lop == LOP.neg);
}

/** Return non-zero if \p lop is an \em Arithmetical \em Additive Operation. */
bool isMul(LOP lop)
{
    return (lop == LOP.mul ||
            lop == LOP.div ||
            lop == LOP.pow ||
            lop == LOP.inv);
}

bool isArithmetic(LOP lop)
{
    return isAdd(lop) && isArithmetic(lop);
}

/** Return non-zero if \p lop is an \em Trigonometric Operation. */
bool isTrigonometric(LOP lop)
{
    return (lop == LOP.cos ||
            lop == LOP.sin ||
            lop == LOP.tan ||
            lop == LOP.cot ||
            lop == LOP.csc ||
            lop == LOP.acos ||
            lop == LOP.asin ||
            lop == LOP.atan ||
            lop == LOP.acot ||
            lop == LOP.acsc
        );
}

/** Return non-zero if \p lop is an \em Hyperbolic Operation. */
bool isHyperbolic(LOP lop)
{
    return (lop == LOP.cosh ||
            lop == LOP.sinh ||
            lop == LOP.tanh ||
            lop == LOP.coth ||
            lop == LOP.csch ||
            lop == LOP.acosh ||
            lop == LOP.asinh ||
            lop == LOP.atanh ||
            lop == LOP.acoth ||
            lop == LOP.acsch);
}

/** Return non-zero if \p lop is a \em Generator Operation. */
bool isGen(LOP lop)
{
    return (lop == LOP.ramp ||
            lop == LOP.zeros ||
            lop == LOP.ones ||
            lop == LOP.rand);
}

/** Return non-zero if \p lop is a \em Comparison Operation. */
bool isCmp(LOP lop)
{
    return (lop == LOP.eq ||
            lop == LOP.neq ||
            lop == LOP.lt ||
            lop == LOP.lte ||
            lop == LOP.gt ||
            lop == LOP.gte);
}

/** Return non-zero if \p lop is a \em Permutation/Reordering Operation. */
bool isPermutation(LOP lop)
{
    return (lop == LOP.identity ||
            lop == LOP.interleave ||
            lop == LOP.flip ||
            lop == LOP.mirror ||
            lop == LOP.shuffle);
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
enum GOP : ubyte
{
    /* \name Structural: Inter-Node */
    /* @{ */
    seq,                    /**< Sequential Growth. Inherit Initial Value. Set weight to +1. State is \em ON. */
    par,                    /**< Parallel Growth */

    clone,                  /**< Clone. Like \c PAR but  */
    /* @} */

    /* \name Local: Intra-Node */
    /* @{ */
    setLOp,                /**< Set Node Logic Operation (see \c LOP). */

    setOType, /**< Set Out-Type. Either \c int, \c float or \c double for now. */

    incBias,               /**< Increase Threshold */
    decBias,               /**< Decrease Threshold */

    /* There is \em no need for "in-precision", since these are the same
       as in-precision the "previous" nodes in the network. */
    incOpRec,              /**< Increase Out-Precision */
    decOpRec,              /**< Decrease Out-Precision */

    incLinkreg,            /**< Increase In-Link-Register */
    decLinkreg,            /**< Decrease In-Link-Register */

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
// wchar_from_LOP(LOP lop)
// {
//     wchar_t wc = UNICODE_UNKNOWN;
//     switch (lop) {
//     case LOP.add: wc = UNICODE_CIRCLED_PLUS; break;
//     case LOP.sub: wc = UNICODE_CIRCLED_MINUS; break;
//     case LOP.neg: wc = UNICODE_SQUARED_MINUS; break;
//     case LOP.mul: wc = UNICODE_CIRCLED_TIMES; break;
//     case LOP.div: wc = UNICODE_CIRCLED_DIVISION_SLASH; break;
//     case LOP.pow: wc = UNICODE_UNKNOWN; break;
//     case LOP.inv: wc = UNICODE_CIRCLED_DIVISION_SLASH; break;
//     default: break;
//     }
//     return wc;
// }

import std.bitmanip : BitArray;
import std.random : Random, uniform;
import array_ex : UncopyableArray, CopyableArray;
import traits_ex : packedBitSizeOf;
import typecons_ex : IndexedBy;
import borrown : Owned;

import vary : FastVariant;

alias Data = FastVariant!(long, double);
alias Datas = Owned!(UncopyableArray!Data);

/// Scalar Operation Count.
alias OpCount = size_t;

/// Relative Cell index.
alias CellRIx = ptrdiff_t;

/// Relative Cell indexs.
alias CellRIxs = Owned!(CopyableArray!CellRIx);

/// Calculating Cell.
struct Cell
{
    import std.algorithm.iteration : map, filter, fold;

    @safe pure:

    this(LOP lop, size_t inputRIxsLength, size_t cellCount)
    {
    }

    /// Randomize a cell.
    void randomize(LOP lop, size_t inputRIxsLength, size_t cellCount) @trusted
    {
        auto gen = Random();
        this.lop = lop;
        inputCellRIxs.length = inputRIxsLength;
        foreach (ref rix; inputCellRIxs[])
        {
            rix = uniform(cast(CellRIx)0, cast(CellRIx)cellCount/10, gen);
        }
    }

    nothrow:

    OpCount execute(const ref Datas ins, ref Datas outs) const
    {
        if (ins.empty) { return typeof(return).init; }
        final switch (lop)
        {
        case LOP.summ: return summ(ins, outs);
        case LOP.prod: return prod(ins, outs);
        case LOP.emin: return emin(ins, outs);
        case LOP.emax: return emax(ins, outs);
        }
    }

    /// Sum of `ins`.
    OpCount summ(const ref Datas ins, ref Datas outs) const @trusted
    {
        import std.algorithm.iteration : sum;
        outs.length = 1;
        outs[0] = ins[].filter!(_ => _.hasValue)
                       .map!(_ => _.commonValue)
                       .sum();
        return ins.length - 1;
    }

    /// Product of `ins`.
    OpCount prod(const ref Datas ins, ref Datas outs) const @trusted
    {
        outs.length = 1;
        outs[0] = ins[].filter!(_ => _.hasValue)
                       .map!(_ => _.commonValue)
                       .fold!((a, b) => a * b)(cast(Data.CommonType)1.0);
        return ins.length - 1;
    }

    /// Minimum of `ins`.
    OpCount emin(const ref Datas ins, ref Datas outs) const @trusted
    {
        import std.algorithm : minElement;
        outs.length = 1;
        outs[0] = ins[].filter!(_ => _.hasValue)
                       .map!(_ => _.commonValue)
                       .minElement(+Data.CommonType.max);
        return ins.length - 1;
    }

    /// Maximum of `ins`.
    OpCount emax(const ref Datas ins, ref Datas outs) const @trusted
    {
        import std.algorithm : maxElement;
        outs.length = 1;
        outs[0] = ins[].filter!(_ => _.hasValue)
                       .map!(_ => _.commonValue)
                       .maxElement(-Data.CommonType.max);
        return ins.length - 1;
    }

    LOP lop;                  /// operation
    CellRIxs inputCellRIxs;   /// relative indexes to (neighbouring) input cells
}
alias Cells = IndexedBy!(Owned!(UncopyableArray!Cell), `Ix`);

/// Network/Graph of `Cells`.
struct Network
{
    @safe pure /*TODO nothrow @nogc*/:

    this(size_t cellCount)
    {
        auto gen = Random();

        cells.reserve(cellCount);
        temps.reserve(cellCount);

        foreach (immutable i; 0 .. cellCount)
        {
            cells ~= Cell(gen.uniform!LOP, 10, cellCount);
            temps ~= Data(gen.uniform!long);
        }
    }

    /// One step forward.
    OpCount step() @trusted
    {
        typeof(return) opCount = 0;

        foreach (immutable i, ref cell; cells)
        {
            Datas ins;
            ins.reserve(cell.inputCellRIxs.length);
            foreach (immutable rix; cell.inputCellRIxs)
            {
                ins ~= temps[(i + rix) % cells.length]; // relative index
            }

            Datas tempOuts;         // data to be filled
            if (immutable opCount_ = cell.execute(ins, tempOuts))
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
}

/// Cell Operation.
enum CellOp : ubyte
{
    seqClone,                   /// sequential clone
    parClone,                   /// parallel clone
}
alias COps = Owned!(UncopyableArray!CellOp);

/// Generative Code.
struct Code
{
    @safe pure nothrow:

    COps cOps; // cell operations, an indirect generative network encoding
    alias cOps this;

    Cells.Ix writerIxs;          /// index to writers

    Network generate() const
    {
        typeof(return) network;
        foreach (immutable cOp; cOps[])
        {
        }
        return network;
    }
}

unittest
{
    immutable cellCount = 1_000;
    auto task = Network(cellCount);

    immutable stepCount = 1_0;
    foreach (immutable i; 0 .. stepCount)
    {
        task.step();
    }

    dln("DONE");
}
