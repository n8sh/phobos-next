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

extern(C) double my_f(scope double* x,
                      size_t dim,
                      scope void* params)
{
    auto fp = cast(my_f_params*)params;
    assert(dim == 2);
    return (fp.a * x[0] * x[0] +
            fp.b * x[0] * x[1] +
            fp.c * x[1] * x[1]);
}

double gsl_monte_fn_eval(scope gsl_monte_function* F,
                         const scope double[] x) @trusted
{
    return (*(F.f))(cast(double*)x, F.dim, F.params);
}

void test_gsl_integration()
{
    gsl_monte_function F;
    my_f_params params = { 3.0, 2.0, 1.0 };

    F.f = &my_f;
    F.dim = 2;
    F.params = &params;

    const double[2] x = [2, 2];
    assert(gsl_monte_fn_eval(&F, x) == 24);
}

void main()
{
    test_gsl_integration();
}
