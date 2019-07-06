#!/usr/bin/env dub
/+ dub.json:
 {
 "name": "testgsl",
 "dependencies": {"gsl": "~>0.1.8"}
 }
 +/

/*
 *  Compile example:
 *  dub run --single testgsl.d
 */

import std.range : iota, array;
import std.stdio : writeln;
import gsl.monte;

struct my_f_params { double a; double b; double c; }

extern(C) double my_f(const scope double* x,
                      size_t dim,
                      const scope void* params) @trusted pure nothrow @nogc
{
    auto fp = cast(my_f_params*)params;
    assert(dim == 2);
    return (fp.a * x[0] * x[0] +
            fp.b * x[0] * x[1] +
            fp.c * x[1] * x[1]);
}

double eval(scope gsl_monte_function* fn,
            const scope double[] x) @trusted
{
    return (*(fn.f))(cast(double*)x, fn.dim, fn.params);
}

double integrate(scope gsl_monte_function* fn)
{
    return typeof(return).init;
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
}

void main()
{
    test_gsl_monte_plain_integration();
}
