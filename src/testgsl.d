#!/usr/bin/env dub
/+ dub.json:
 {
 "name": "testgsl",
 "dependencies": {"gsl": "~>0.1.8"}
 }
 +/

/* Run as: dub run --single testgsl.d --compiler=dmd --build=release-nobounds
 */

import std.stdio : writeln;
import std.datetime.stopwatch;

import gsl.monte;
import gsl.rng;

struct my_f_params { double a; double b; double c; }

extern(C) double my_f(const scope double* x,
                      size_t dim,
                      const scope void* params) @trusted pure nothrow @nogc
{
    const typed_params = cast(my_f_params*)params;
    assert(dim == 2);
    return (typed_params.a * x[0] * x[0] +
            typed_params.b * x[0] * x[1] +
            typed_params.c * x[1] * x[1]);
}

double eval(scope gsl_monte_function* fn,
            const scope double[] x) @trusted
{
    return (*(fn.f))(cast(double*)x, fn.dim, fn.params);
}

/// Integration result and absolute error.
struct IntegrationResult
{
    double value;
    double absoluteError;
}

gsl_rng* rng;                   // thread-local rng

shared static this()
{
    gsl_rng_env_setup();
}

static this()
{
    const gsl_rng_type* T = gsl_rng_default;
    rng = gsl_rng_alloc(T);
}

static ~this()
{
    gsl_rng_free(rng);
}

/** High-level wrapper of `gsl_monte_plain_integrate`.
 *
 */
IntegrationResult montePlainIntegrate(scope const ref gsl_monte_function fn,
                                      scope const double[] lowerLimit, // lower limit of hypercubic region
                                      scope const double[] upperLimit, // upper limit of hypercubic region
                                      const size_t calls = 500_000) @trusted
{
    assert(fn.dim == lowerLimit.length);
    assert(lowerLimit.length == upperLimit.length);
    foreach (const i; 0 .. fn.dim)
    {
        assert(lowerLimit[i] < upperLimit[i]);
    }

    gsl_monte_plain_state* state = gsl_monte_plain_alloc(fn.dim);
    typeof(return) ir;

    const int i = gsl_monte_plain_integrate(cast(gsl_monte_function*)&fn,
                                            lowerLimit.ptr,
                                            upperLimit.ptr,
                                            fn.dim,
                                            calls,
                                            rng,
                                            state,
                                            &ir.value,
                                            &ir.absoluteError);

    gsl_monte_plain_free(state);

    return ir;
}

/** High-level wrapper of `gsl_monte_miser_integrate`.
 *
 */
IntegrationResult monteMISERIntegrate(scope const ref gsl_monte_function fn,
                                      scope const double[] lowerLimit, // lower limit of hypercubic region
                                      scope const double[] upperLimit, // upper limit of hypercubic region
                                      const size_t calls) @trusted
{
    assert(fn.dim == lowerLimit.length);
    assert(lowerLimit.length == upperLimit.length);
    foreach (const i; 0 .. fn.dim)
    {
        assert(lowerLimit[i] < upperLimit[i]);
    }

    gsl_monte_miser_state* state = gsl_monte_miser_alloc(fn.dim);
    typeof(return) ir;

    const int i = gsl_monte_miser_integrate(cast(gsl_monte_function*)&fn,
                                            lowerLimit.ptr,
                                            upperLimit.ptr,
                                            fn.dim,
                                            calls,
                                            rng,
                                            state,
                                            &ir.value,
                                            &ir.absoluteError);

    gsl_monte_miser_free(state);

    return ir;
}

/** High-level wrapper of `gsl_monte_vegas_integrate`.
 *
 */
version(none)                   // TODO add wrappers missing in gsl bindings
IntegrationResult monteVEGASIntegrate(scope const ref gsl_monte_function fn,
                                      scope const double[] lowerLimit, // lower limit of hypercubic region
                                      scope const double[] upperLimit, // upper limit of hypercubic region
                                      const size_t calls) @trusted
{
    assert(fn.dim == lowerLimit.length);
    assert(lowerLimit.length == upperLimit.length);
    foreach (const i; 0 .. fn.dim)
    {
        assert(lowerLimit[i] < upperLimit[i]);
    }

    gsl_monte_vegas_state* state = gsl_monte_vegas_alloc(fn.dim);
    typeof(return) ir;

    const int i = gsl_monte_vegas_integrate(cast(gsl_monte_function*)&fn,
                                            lowerLimit.ptr,
                                            upperLimit.ptr,
                                            fn.dim,
                                            calls,
                                            rng,
                                            state,
                                            &ir.value,
                                            &ir.absoluteError);

    gsl_monte_vegas_free(state);

    return ir;
}

void test_gsl_monte_integration()
{
    gsl_monte_function fn;
    my_f_params params = { 3.0, 2.0, 1.0 };

    fn.f = &my_f;
    fn.dim = 2;
    fn.params = &params;

    const double[2] x = [2, 2];
    assert(eval(&fn, x) == 24);

    auto sw = StopWatch(AutoStart.yes);

    const size_t calls = 10_000;

    const double[2] lowerLimit = [0.0, 0.0];
    const double[2] upperLimit = [1.0, 1.0];

    // plain
    {
        sw.reset();
        const ir = montePlainIntegrate(fn, lowerLimit[], upperLimit, 12*calls);
        sw.stop();
        writeln("Plain: ", ir, " took ", sw.peek);
    }

    // MISER
    {
        sw.reset();
        const ir = monteMISERIntegrate(fn, lowerLimit[], upperLimit, calls);
        sw.stop();
        writeln("MISER: ", ir, " took ", sw.peek);
    }

    // VEGAS
    version(none)               // TODO activate
    {
        sw.reset();
        const ir = monteVEGASIntegrate(fn, lowerLimit[], upperLimit, calls);
        sw.stop();
        writeln("VEGAS: ", ir, " took ", sw.peek);
    }
}

void main()
{
    test_gsl_monte_integration();
}
