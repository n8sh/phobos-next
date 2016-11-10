#!/usr/bin/env rdmd-dev-module

/** Neuroevolution.

   See also: https://en.wikipedia.org/wiki/Neuroevolution

   TODO Profile use of cellops and logops in a specific domain or pattern och
   and use roulette wheel sampling based on these in future patterns.
*/
module evo;

import std.stdio, std.algorithm;

@safe pure nothrow:

enum LOp
{
    sum, prod, min, max
}

/** Low-Level (Genetic Programming) Operation Graph Node Types often implemented
    in a modern hardware (CPU/GPU).

    See also: http://llvm.org/docs/LangRef.html#typesystem
    See also: http://llvm.org/docs/LangRef.html#instref
*/
enum LOp_
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

/**m Graph (Transformation) Operation Type Code.
 *
 * Instructions for a Program that builds \em Computing Networks (for
 * example Artificial Nerual Networks ANN).
 *
 * TODO What does \em nature call this information bearer: Closest I
 * have found is http://en.wikipedia.org/wiki/Allele.
 */
enum GOp
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

import array_ex : UCA = UncopyableArray;

import vary : FastVariant;

alias Data = FastVariant!(long, double);
alias Datas = UCA!Data;

/// Parameter Pipe.
struct Pipe
{
    size_t inIx;                // input `Call` index
    size_t outIx;               // output `Call` index
}
alias Pipes = UCA!Pipe;

/// Operation Call
struct Call
{
    pure nothrow:

    OpCount execute(const ref Datas ins, ref Datas outs) @trusted
    {
        assert(ins.length != 0); // no use having empty input

        typeof(return) opCount = 0;

        import std.algorithm.iteration : fold, sum;
        import std.algorithm.comparison : min, max;

        final switch (lop)
        {
        case LOp.sum:
            outs.length = 1;
            outs[0] = ins[].map!(_ => _.commonValue)
                           .sum();
            opCount += ins.length;
            break;
        case LOp.prod:
            outs.length = 1;
            outs[0] = ins[].map!(_ => _.commonValue)
                           .fold!((a, b) => a * b)(cast(Data.CommonType)1.0);
            opCount += ins.length;
            break;
        case LOp.min:
            outs.length = 1;
            outs[0] = ins[].map!(_ => _.commonValue)
                           .fold!((a, b) => min(a, b))(Data.CommonType.max);
            opCount += ins.length;
            break;
        case LOp.max:
            outs.length = 1;
            outs[0] = ins[].map!(_ => _.commonValue)
                           .fold!((a, b) => min(a, b))(-Data.CommonType.max);
            opCount += ins.length;
            break;
        }
        return opCount;
    }

    LOp lop;
}
alias Calls = UCA!Call;

struct Graph
{
    Calls calls;                // operation/function calls
    Datas temps;                // temporary outputs from calls
    Pipes pipes;                // input to output pipes
}

/// Scalar Operation Count.
alias OpCount = size_t;

/// Task/Process.
struct Task
{
    @safe pure nothrow:

    this(size_t callCount, size_t pipeCount)
    {
        graph.calls.reserve(callCount);
        graph.temps.reserve(callCount);

        graph.temps.reserve(pipeCount);
    }

    /// Step forward one step.
    OpCount step() @trusted
    {
        typeof(return) opCount = 0;
        foreach (immutable i, ref call; graph.calls)
        {
            Datas ins;            // copy from temps[pipes[].inIx]
            Datas outs;
            opCount += call.execute(ins, outs);
            // copy from outs to data[pipes[].outIx]
        }
        return opCount;
    }

private:
    Graph graph;
}

unittest
{
    immutable callCount = 10_000;
    immutable pipeCount = 10_000;
    auto task = Task(callCount, pipeCount);

    immutable stepCount = 1_000;
    foreach (immutable i; 0 .. stepCount)
    {
        task.step();
    }
}
