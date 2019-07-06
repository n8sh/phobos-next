#!/usr/bin/env dub
/+ dub.json:
 {
 "name": "testgsl",
 "dependencies": {"gsl": "~>0.1.8"}
 }
 +/

/* Run as:  dub run --single testgsl.d */

import std.range : iota, array;
import std.stdio : writeln;

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

IntegrationResult integrate(scope const gsl_monte_function* fn,
                            scope const double[] xl,
                            scope const double[] xu,
                            const size_t calls = 500_000) @trusted
{
    assert(fn.dim == xl.length);
    assert(xl.length == xu.length);
    const size_t dim = xl.length;
    foreach (const i; 0 .. dim)
    {
        assert(xl[i] < xu[i]);
    }

    gsl_rng_env_setup();
    const gsl_rng_type* T = gsl_rng_default;
    gsl_rng* rng = gsl_rng_alloc(T);

    gsl_monte_plain_state* state = gsl_monte_plain_alloc(dim);
    typeof(return) ir;
    int i = gsl_monte_plain_integrate(cast(gsl_monte_function*)fn,
                                      xl.ptr,
                                      xu.ptr,
                                      dim,
                                      calls,
                                      rng,
                                      state,
                                      &ir.value,
                                      &ir.absoluteError);
    gsl_monte_plain_free(state);
    gsl_rng_free(rng);
    return ir;
}

void test_gsl_monte_plain_integration()
{
    gsl_monte_function fn;
    my_f_params params = { 3.0, 2.0, 1.0 };

    fn.f = &my_f;
    fn.dim = 2;
    fn.params = &params;

    const double[2] x = [2, 2];
    assert(eval(&fn, x) == 24);

    writeln(integrate(&fn, [0.0, 0.0], [1.0, 1.0]));
}

void main()
{
    test_gsl_monte_plain_integration();
}
