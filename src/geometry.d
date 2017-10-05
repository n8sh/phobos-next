/**
   Special thanks to:
   $(UL
   $(LI Tomasz Stachowiak (h3r3tic): allowed me to use parts of $(LINK2 https://bitbucket.org/h3r3tic/boxen/src/default/src/xf/omg, omg).)
   $(LI Jakob Øvrum (jA_cOp): improved the code a lot!)
   $(LI Florian Boesch (___doc__): helps me to understand opengl/complex maths better, see: $(LINK http://codeflow.org/).)
   $(LI #D on freenode: answered general questions about D.)
   )

   Note: All methods marked with pure are weakly pure since, they all access an instance member.
   All static methods are strongly pure.

   TODO Support radian and degree types
   TODO Replace toMathML() with fmt argument %M to toString functions
   TODO Replace toLaTeX() with fmt argument %L to toString functions
   TODO Optimize using core.simd or std.simd
   TODO Merge with analyticgeometry
   TODO Merge with https://github.com/CyberShadow/ae/blob/master/utils/geometry.d
   TODO Integrate with http://code.dlang.org/packages/blazed2
   TODO logln, log.warn, log.error, log.info, log.debug
   TODO Make use of staticReduce etc when they become available in Phobos.
   TODO Go through all usages of real and use CommonType!(real, E) to make it work when E is a bignum.
   TODO ead and perhaps make use of http://stackoverflow.com/questions/3098242/fast-vector-struct-that-allows-i-and-xyz-operations-in-d?rq=1
   TODO Tag member functions in t_geom.d as pure as is done https://github.com/$(D D)-Programming-Language/phobos/blob/master/std/bigint.d
   TODO Remove need to use [] in x[] == y[]

   See: https://www.google.se/search?q=point+plus+vector
   See: http://mosra.cz/blog/article.php?a=22-introducing-magnum-a-multiplatform-2d-3d-graphics-engine
*/

module geometry;

version = unittestAllInstances;

// version = print;

version(NoReciprocalMul)
{
    private enum rmul = false;
}
else
{
    private enum rmul = true;
}

version(LDC) static if (__VERSION__ >= 2076) { static assert(0, "TODO use static foreach inplace of iota!(...)"); }

// TODO use import core.simd;
import std.math: sqrt, isNaN, isInfinity, PI, sin, cos, acos;
import std.conv: to;
import std.traits: isSomeString, isIntegral, isFloatingPoint, isNumeric, isSigned, isStaticArray, isDynamicArray, isImplicitlyConvertible, isAssignable, isArray, CommonType;
import std.string: format, rightJustify;
import std.array: join;
import std.algorithm : map, all, any, min, max, reduce;
import std.random: uniform;

import mathml;
import static_iota : iota;
import traits_ex: haveCommonType;

enum isVector(E)     = is(typeof(isVectorImpl(E.init)));
enum isPoint(E)      = is(typeof(isPointImpl(E.init)));
enum isMatrix(E)     = is(typeof(isMatrixImpl(E.init)));
enum isQuaternion(E) = is(typeof(isQuaternionImpl(E.init)));
enum isPlane(E)      = is(typeof(isPlaneImpl(E.init)));

private void isVectorImpl    (E, uint D)        (Vector    !(E, D)    vec) {}
private void isPointImpl     (E, uint D)        (Point     !(E, D)    vec) {}
private void isMatrixImpl    (E, uint R, uint C)(Matrix    !(E, R, C) mat) {}
private void isQuaternionImpl(E)                (Quaternion!(E)        qu) {}
private void isPlaneImpl     (E)                (PlaneT    !(E)         p) {}

enum isFixVector(E) = isFix(typeof(isFixVectorImpl(E.init)));
enum isFixPoint(E)  = isFix(typeof(isFixPointImpl (E.init)));
enum isFixMatrix(E) = isFix(typeof(isFixMatrixImpl(E.init)));

private void isFixVectorImpl (E, uint D)        (Vector!(E, D)    vec) {}
private void isFixPointImpl  (E, uint D)        (Point !(E, D)    vec) {}
private void isFixMatrixImpl (E, uint R, uint C)(Matrix!(E, R, C) mat) {}

// ==============================================================================================

version(unittestAllInstances)
{
    static immutable defaultElementTypes = ["float", "double", "real"];
}
else
{
    static immutable defaultElementTypes = ["double"];
}

// See also: http://stackoverflow.com/questions/18552454/using-ctfe-to-generate-set-of-struct-aliases/18553026?noredirect=1#18553026
string makeInstanceAliases(in string templateName,
                           string aliasName = "",
                           in uint minDimension = 2,
                           in uint maxDimension = 4,
                           in string[] elementTypes = defaultElementTypes)
in
{
    assert(templateName.length);
    assert(minDimension <= maxDimension);
}
body
{
    import std.string : toLower;
    import std.conv : to;

    string code;
    if (!aliasName.length)
    {
        aliasName = templateName.toLower;
    }
    foreach (const n; minDimension .. maxDimension + 1)
    {
        foreach (const et; elementTypes) // for each elementtype
        {
            immutable prefix = ("alias " ~ templateName ~ "!("~et~", " ~
                                to!string(n) ~ ") " ~ aliasName ~ "" ~
                                to!string(n));
            if (et == "float")
            {
                code ~= (prefix ~ ";\n"); // GLSL-style prefix-less single precision
            }
            code ~= (prefix ~ et[0] ~ ";\n");
        }
    }
    return code;
}

/* Copied from https://github.com/CyberShadow/ae/blob/master/utils/geometry.d */
auto sqrtx(T)(T x)
{
    static if (is(T : int))
    {
        return std.math.sqrt(cast(float)x);
    }
    else
    {
        return std.math.sqrt(x);
    }
}

// ==============================================================================================

/// $(D D)-Dimensional Cartesian Point with Coordinate Type (Precision) $(D E).
struct Point(E, uint D)
    if (D >= 1 /* && TODO extend trait : isNumeric!E */)
{
    alias ElementType = E;

    this(T...)(T args)
    {
        foreach (const ix, arg; args)
        {
            _point[ix] = arg;
        }
    }

    /** Element data. */
    E[D] _point;
    enum dimension = D;

    @property void toString(scope void delegate(const(char)[]) sink) const
    {
        import std.format : formattedWrite;
        sink.formattedWrite("Point(%s)", _point);
    }

    @property string toMathML() const
    {
        // opening
        string str = `<math><mrow>
  <mo>(</mo>
  <mtable>`;

        foreach (const i; iota!(0, D))
        {
            str ~= `
    <mtr>
      <mtd>
        <mn>` ~ _point[i].toMathML ~ `</mn>
      </mtd>
    </mtr>`;
        }

        // closing
        str ~= `
  </mtable>
  <mo>)</mo>
</mrow></math>
`;
        return str;
    }

    /** Returns: Area 0 */
    @property E area() const { return 0; }

    inout(E)[] opSlice() inout { return _point[]; }

    /** Points +/- Vector => Point */
    auto opBinary(string op, F)(Vector!(F, D) r) const
        if ((op == "+") ||
            (op == "-"))
    {
        Point!(CommonType!(E, F), D) y;
        foreach (const i; iota!(0, D))
        {
            y._point[i] = mixin("_point[i]" ~ op ~ "r._vector[i]");
        }
        return y;
    }
}

/** Instantiator for `Point`. */
auto point(Ts...)(Ts args)
    if (haveCommonType!Ts)
{
    return Point!(CommonType!Ts, args.length)(args);
}

mixin(makeInstanceAliases("Point", "point", 2,3, ["int", "float", "double", "real"]));

@safe pure nothrow @nogc unittest
{
    assert(point(1, 2) + vector(1, 2) == point(2, 4));
    assert(point(1, 2) - vector(1, 2) == point(0, 0));
}

enum Orient { column, row } // Vector Orientation.

/** $(D D)-Dimensional Vector with Coordinate/Element (Component) Type $(D E).
    See also: http://physics.stackexchange.com/questions/16850/is-0-0-0-an-undefined-vector
*/
struct Vector(E, uint D,
              bool normalizedFlag = false, // set to true for UnitVectors
              Orient orient = Orient.column)
    if (D >= 1 &&
        (!normalizedFlag ||
         isFloatingPoint!E)) // only normalize fp for now
{
    // Construct from vector.
    this(V)(V vec)
        if (isVector!V &&
            // TOREVIEW: is(T.E : E) &&
            (V.dimension >= dimension))
    {
        static if (normalizedFlag)
        {
            if (vec.normalized)
            {
                immutable vec_norm = vec.magnitude;
                foreach (const i; iota!(0, D))
                {
                    _vector[i] = vec._vector[i] / vec_norm;
                }
                return;
            }
        }
        foreach (i; iota!(0, D))
        {
            _vector[i] = vec._vector[i];
        }
    }

    /** Construct from Scalar $(D VALUE). */
    this(S)(S scalar)
        if (isAssignable!(E, S))
    {
        static if (normalizedFlag)
        {
            clear(1/sqrt(cast(E)D));
        }
        else
        {
            clear(scalar);
        }
    }

    /** Construct from combination of arguments. */
    this(Args...)(Args args) { construct!(0)(args); }

    enum dimension = D;

    @property const
    {
        string orientationString() { return orient == Orient.column ? `Column` : `Row`; }
        string joinString() { return orient == Orient.column ? ` \\ ` : ` & `; }
    }

    @property void toString(scope void delegate(const(char)[]) sink) const
    {
        import std.format : formattedWrite;
        sink(orientationString);
        sink.formattedWrite("Vector(%s)", _vector);
    }

    /** Returns: LaTeX Encoding of Vector. http://www.thestudentroom.co.uk/wiki/LaTex#Matrices_and_Vectors */
    @property string toLaTeX() const
    {
        return `\begin{pmatrix} ` ~ map!(to!string)(_vector[]).join(joinString) ~ ` \end{pmatrix}` ;
    }

    @property string toMathML() const
    {
        // opening
        string str = `<math><mrow>
  <mo>⟨</mo>
  <mtable>`;

        if (orient == Orient.row)
        {
            str ~=  `
    <mtr>`;
        }

        foreach (i; iota!(0, D))
        {
            final switch (orient)
            {
            case Orient.column:
                str ~= `
    <mtr>
      <mtd>
        <mn>` ~ _vector[i].toMathML ~ `</mn>
      </mtd>
    </mtr>`;
                break;
            case Orient.row:
                str ~= `
      <mtd>
        <mn>` ~ _vector[i].toMathML ~ `</mn>
      </mtd>`;
                break;
            }
        }

        if (orient == Orient.row)
        {
            str ~=  `
    </mtr>`;
        }

        // closing
        str ~= `
  </mtable>
  <mo>⟩</mo>
</mrow></math>
`;
        return str;
    }

    auto randInPlace(E scaling = 1)
    {
        import random_ex: randInPlace;
        static if (normalizedFlag &&
                   isFloatingPoint!E) // cannot use normalized() here (yet)
        {
            static if (D == 2)  // randomize on unit circle
            {
                alias P = real; // precision
                immutable angle = uniform(0, 2*cast(P)PI);
                _vector[0] = scaling*sin(angle);
                _vector[1] = scaling*cos(angle);
            }
            static if (D == 3)  // randomize on unit sphere: See also: http://mathworld.wolfram.com/SpherePointPicking.html
            {
                alias P = real; // precision
                immutable u = uniform(0, cast(P)1);
                immutable v = uniform(0, cast(P)1);
                immutable theta = 2*PI*u;
                immutable phi = acos(2*v-1);
                _vector[0] = scaling*cos(theta)*sin(phi);
                _vector[1] = scaling*sin(theta)*sin(phi);
                _vector[2] = scaling*cos(phi);
            }
            else
            {
                _vector.randInPlace();
                normalize(); // TODO Turn this into D data restriction instead?
            }
        }
        else
        {
            _vector.randInPlace();
        }
    }

    /// Returns: `true` if all values are not `nan` nor `infinite`, otherwise `false`.
    @property bool ok() const
    {
        static if (isFloatingPoint!E)
        {
            foreach (v; _vector)
            {
                if (isNaN(v) ||
                    isInfinity(v))
                {
                    return false;
                }
            }
        }
        return true;
    }
    // NOTE: Disabled this because I want same behaviour as MATLAB: bool opCast(T : bool)() const { return ok; }
    bool opCast(T : bool)() const
    {
        return all!"a"(_vector[]);
    }

    /// Returns: Pointer to the coordinates.
    // @property auto value_ptr() { return _vector.ptr; }

    /// Sets all values to $(D value).
    void clear(E value)
    {
        foreach (i; iota!(0, D))
        {
            _vector[i] = value;
        }
    }

    /** Returns: Whole Internal Array of E. */
    auto opSlice()
    {
        return _vector[];
    }
    /** Returns: Slice of Internal Array of E. */
    auto opSlice(uint off, uint len)
    {
        return _vector[off .. len];
    }
    /** Returns: Reference to Internal Vector Element. */
    ref inout(E) opIndex(uint i) inout
    {
        return _vector[i];
    }

    bool opEquals(S)(in S scalar) const
        if (isAssignable!(E, S)) // TOREVIEW: Use isNotEquable instead
    {
        foreach (i; iota!(0, D))
        {
            if (_vector[i] != scalar)
            {
                return false;
            }
        }
        return true;
    }
    bool opEquals(F)(in F vec) const
        if (isVector!F &&
            dimension == F.dimension) // TOREVIEW: Use isEquable instead?
    {
        return _vector == vec._vector;
    }
    bool opEquals(F)(const F[] array) const
        if (isAssignable!(E, F) &&
            !isArray!F &&
            !isVector!F) // TOREVIEW: Use isNotEquable instead?
    {
        if (array.length != dimension)
        {
            return false;
        }
        foreach (i; iota!(0, D))
        {
            if (_vector[i] != array[i])
            {
                return false;
            }
        }
        return true;
    }

    static void isCompatibleVectorImpl(uint d)(Vector!(E, d) vec) if (d <= dimension) {}
    static void isCompatibleMatrixImpl(uint r, uint c)(Matrix!(E, r, c) m) {}

    enum isCompatibleVector(T) = is(typeof(isCompatibleVectorImpl(T.init)));
    enum isCompatibleMatrix(T) = is(typeof(isCompatibleMatrixImpl(T.init)));

    private void construct(uint i)()
    {
        static assert(i == D, "Not enough arguments passed to constructor");
    }
    private void construct(uint i, T, Tail...)(T head, Tail tail)
    {
        static        if (i >= D)
        {
            static assert(0, "Too many arguments passed to constructor");
        }
        else static if (is(T : E))
        {
            _vector[i] = head;
            construct!(i + 1)(tail);
        }
        else static if (isDynamicArray!T)
        {
            static assert((Tail.length == 0) && (i == 0), "Dynamic array can not be passed together with other arguments");
            _vector[] = head[];
        }
        else static if (isStaticArray!T)
        {
            _vector[i .. i + T.length] = head[];
            construct!(i + T.length)(tail);
        }
        else static if (isCompatibleVector!T)
        {
            _vector[i .. i + T.dimension] = head._vector[];
            construct!(i + T.dimension)(tail);
        }
        else
        {
            static assert(0, "Vector constructor argument must be of type " ~ E.stringof ~ " or Vector, not " ~ T.stringof);
        }
    }

    // private void dispatchImpl(int i, string s, int size)(ref E[size] result) const {
    //     static if (s.length > 0) {
    //         result[i] = _vector[coordToIndex!(s[0])];
    //         dispatchImpl!(i + 1, s[1..$])(result);
    //     }
    // }

    // /// Implements dynamic swizzling.
    // /// Returns: a Vector
    // @property Vector!(E, s.length) opDispatch(string s)() const {
    //     E[s.length] ret;
    //     dispatchImpl!(0, s)(ret);
    //     Vector!(E, s.length) ret_vec;
    //     ret_vec._vector = ret;
    //     return ret_vec;
    // }

    ref inout(Vector) opUnary(string op : "+")() inout { return this; }

    Vector   opUnary(string op : "-")() const
        if (isSigned!(E))
    {
        Vector y;
        foreach (i; iota!(0, D))
        {
            y._vector[i] = - _vector[i];
        }
        return y;
    }

    auto opBinary(string op, F)(Vector!(F, D) r) const
        if ((op == "+") ||
            (op == "-"))
    {
        Vector!(CommonType!(E, F), D) y;
        foreach (i; iota!(0, D))
        {
            y._vector[i] = mixin("_vector[i]" ~ op ~ "r._vector[i]");
        }
        return y;
    }

    Vector opBinary(string op : "*", F)(F r) const
    {
        Vector!(CommonType!(E, F), D) y;
        foreach (i; iota!(0, dimension))
        {
            y._vector[i] = _vector[i] * r;
        }
        return y;
    }

    Vector!(CommonType!(E, F), D) opBinary(string op : "*", F)(Vector!(F, D) r) const
    {
        // MATLAB-style Product Behaviour
        static if (orient == Orient.column &&
                   r.orient == Orient.row)
        {
            return outer(this, r);
        }
        else static if (orient == Orient.row &&
                        r.orient == Orient.column)
        {
            return dot(this, r);
        }
        else
        {
            static assert(0, "Incompatible vector dimensions.");
        }
    }

    /** Multiply this Vector with Matrix. */
    Vector!(E, T.rows) opBinary(string op : "*", T)(T inp) const
        if (isCompatibleMatrix!T &&
            (T.cols == dimension))
    {
        Vector!(E, T.rows) ret;
        ret.clear(0);
        foreach (c; iota!(0, T.cols))
        {
            foreach (r; iota!(0, T.rows))
            {
                ret._vector[r] += _vector[c] * inp.at(r,c);
            }
        }
        return ret;
    }

    /** Multiply this Vector with Matrix. */
    auto opBinaryRight(string op, T)(T inp) const
        if (!isVector!T &&
            !isMatrix!T &&
            !isQuaternion!T)
    {
        return this.opBinary!(op)(inp);
    }

    /** TODO Suitable Restrictions on F. */
    void opOpAssign(string op, F)(F r) /* if ((op == "+") || (op == "-") || (op == "*") || (op == "%") || (op == "/") || (op == "^^")) */
    {
        foreach (i; iota!(0, dimension))
        {
            mixin("_vector[i]" ~ op ~ "= r;");
        }
    }
    unittest
    {
        auto v2 = vec2(1, 3);
        v2 *= 5.0f; assert(v2[] == [5, 15].s);
        v2 ^^= 2; assert(v2[] == [25, 225].s);
        v2 /= 5; assert(v2[] == [5, 45].s);
    }

    void opOpAssign(string op)(Vector r)
        if ((op == "+") ||
            (op == "-"))
    {
        foreach (i; iota!(0, dimension))
        {
            mixin("_vector[i]" ~ op ~ "= r._vector[i];");
        }
    }

    /// Returns: Non-Rooted $(D N) - Norm of $(D x).
    auto nrnNorm(uint N)() const
        if (isNumeric!E &&
            N >= 1)
    {
        static if (isFloatingPoint!E)
        {
            real y = 0;                 // TOREVIEW: Use maximum precision for now
        }
        else
        {
            E y = 0;                // TOREVIEW: Use other precision for now
        }
        foreach (i; iota!(0, D)) { y += _vector[i] ^^ N; }
        return y;
    }

    /// Returns: Squared Magnitude of x.
    @property real magnitudeSquared()() const
        if (isNumeric!E)
    {
        static if (normalizedFlag) // cannot use normalized() here (yet)
        {
            return 1;
        }
        else
        {
            return nrnNorm!2;
        }
    }
    /// Returns: Magnitude of x.
    @property real magnitude()() const
        if (isNumeric!E)
    {
        static if (normalizedFlag) // cannot use normalized() here (yet)
        {
            return 1;
        }
        else
        {
            return sqrt(magnitudeSquared);
        }
    }
    alias norm = magnitude;

    static if (isFloatingPoint!E)
    {
        /// Normalize $(D this).
        void normalize()
        {
            if (this != 0)         // zero vector have zero magnitude
            {
                immutable m = this.magnitude;
                foreach (i; iota!(0, D))
                {
                    _vector[i] /= m;
                }
            }
        }

        /// Returns: normalizedFlag Copy of this Vector.
        @property Vector normalized() const
        {
            Vector y = this;
            y.normalize();
            return y;
        }
        unittest
        {
            static if (D == 2 && !normalizedFlag)
            {
                assert(Vector(3, 4).magnitude == 5);
            }
        }
    }

    /// Returns: Vector Index at Character Coordinate $(D coord).
    private @property ref inout(E) get_(char coord)() inout {
        return _vector[coordToIndex!coord];
    }

    /// Coordinate Character c to Index
    template coordToIndex(char c)
    {
        static if ((c == 'x'))
        {
            enum coordToIndex = 0;
        }
        else static if ((c == 'y'))
        {
            enum coordToIndex = 1;
        }
        else static if ((c == 'z'))
        {
            static assert(D >= 3, "The " ~ c ~ " property is only available on vectors with a third dimension.");
            enum coordToIndex = 2;
        }
        else static if ((c == 'w'))
        {
            static assert(D >= 4, "The " ~ c ~ " property is only available on vectors with a fourth dimension.");
            enum coordToIndex = 3;
        }
        else
        {
            static assert(0, "Accepted coordinates are x, s, r, u, y, g, t, v, z, p, b, w, q and a not " ~ c ~ ".");
        }
    }

    /// Updates the vector with the values from other.
    void update(Vector!(E, D) other) { _vector = other._vector; }

    static if (D == 2) { void set(E x, E y) { _vector[0] = x; _vector[1] = y; } }
    else static if (D == 3) { void set(E x, E y, E z) { _vector[0] = x; _vector[1] = y; _vector[2] = z; } }
    else static if (D == 4) { void set(E x, E y, E z, E w) { _vector[0] = x; _vector[1] = y; _vector[2] = z; _vector[3] = w; } }

    static if (D >= 1) { alias x = get_!'x'; }
    static if (D >= 2) { alias y = get_!'y'; }
    static if (D >= 3) { alias z = get_!'z'; }
    static if (D >= 4) { alias w = get_!'w'; }

    static if (isNumeric!E)
    {
        /* Need these conversions when E is for instance ubyte.
           See this commit: https://github.com/Dav1dde/gl3n/commit/2504003df4f8a091e58a3d041831dc2522377f95 */
        enum E0 = 0.to!E;
        enum E1 = 1.to!E;
        static if (dimension == 2)
        {
            enum Vector e1 = Vector(E1, E0); /// canonical basis for Euclidian space
            enum Vector e2 = Vector(E0, E1); /// ditto
        }
        else static if (dimension == 3)
        {
            enum Vector e1 = Vector(E1, E0, E0); /// canonical basis for Euclidian space
            enum Vector e2 = Vector(E0, E1, E0); /// ditto
            enum Vector e3 = Vector(E0, E0, E1); /// ditto
        }
        else static if (dimension == 4)
        {
            enum Vector e1 = Vector(E1, E0, E0, E0); /// canonical basis for Euclidian space
            enum Vector e2 = Vector(E0, E1, E0, E0); /// ditto
            enum Vector e3 = Vector(E0, E0, E1, E0); /// ditto
            enum Vector e4 = Vector(E0, E0, E0, E1); /// ditto
        }
    }
    @safe pure nothrow @nogc unittest
    {
        static if (isNumeric!E)
        {
            assert(vec2.e1[] == [1, 0].s);
            assert(vec2.e2[] == [0, 1].s);

            assert(vec3.e1[] == [1, 0, 0].s);
            assert(vec3.e2[] == [0, 1, 0].s);
            assert(vec3.e3[] == [0, 0, 1].s);

            assert(vec4.e1[] == [1, 0, 0, 0].s);
            assert(vec4.e2[] == [0, 1, 0, 0].s);
            assert(vec4.e3[] == [0, 0, 1, 0].s);
            assert(vec4.e4[] == [0, 0, 0, 1].s);
        }
    }

    /** Element data. */
    E[D] _vector;

    unittest
    {
        // static if (isSigned!(E)) { assert(-Vector!(E,D)(+2),
        //                                   +Vector!(E,D)(-2)); }
    }

}
auto rowVector(Ts...)(Ts args) if (haveCommonType!Ts) { return Vector!(CommonType!Ts, args.length)(args); }
auto columnVector(Ts...)(Ts args) if (haveCommonType!Ts) { return Vector!(CommonType!Ts, args.length, false, Orient.column)(args); }
alias colVector = columnVector;

alias vector = rowVector; // TODO Should rowVector or columnVector be default?

mixin(makeInstanceAliases("Vector", "vec", 2,4, ["ubyte", "int", "float", "double", "real", "bool"]));

/* normalized vector aliases */
alias nvec2f = Vector!(float, 2, true);
alias nvec3f = Vector!(float, 3, true);
alias nvec4f = Vector!(float, 4, true);

@safe pure nothrow @nogc unittest
{
    assert(vec2f(2, 3)[] == [2, 3].s);
    assert(vec2f(2, 3)[0] == 2);
    assert(vec2f(2) == 2);
    assert(vec2f(true) == true);
    assert(vec2b(true) == true);
    assert(all!"a"(vec2b(true)[]));
    assert(any!"a"(vec2b(false, true)[]));
    assert(any!"a"(vec2b(true, false)[]));
    assert(!any!"a"(vec2b(false, false)[]));
    version(print)
    {
        dln(vec2f(2, 3));
        dln(transpose(vec2f(11, 22)));
        dln(vec2f(11, 22).toLaTeX);
        dln(vec2f(11, 22).T.toLaTeX);
    }
    assert((vec2(1, 3)*2.5f)[] == [2.5f, 7.5f].s);

    nvec2f v = vec2f(3, 4);
    assert(v[] == nvec2f(0.6, 0.8)[]);
}

auto transpose(E, uint D, bool normalizedFlag)(in Vector!(E, D,
                                                          normalizedFlag,
                                                          Orient.column) a)
{
    return Vector!(E, D, normalizedFlag, Orient.row)(a);
}
alias T = transpose; // C++ Armadillo naming convention.

auto elementwiseLessThanOrEqual(Ta, Tb, uint D)(Vector!(Ta, D) a,
                                                Vector!(Tb, D) b)
{
    Vector!(bool, D) c = void;
    foreach (i; iota!(0, D))
    {
        c[i] = a[i] <= b[i];
    }
    return c;
}
@safe pure nothrow @nogc unittest
{
    assert(elementwiseLessThanOrEqual(vec2f(1, 1),
                                      vec2f(2, 2)) == vec2b(true, true));
}

/// Returns: Scalar/Dot-Product of Two Vectors $(D a) and $(D b).
T dotProduct(T, U)(in T a, in U b)
    if (isVector!T &&
        isVector!U &&
        (T.dimension ==
         U.dimension))
{
    T c = void;
    foreach (i; iota!(0, T.dimension))
    {
        c[i] = a[i] * b[i];
    }
    return c;
}
alias dot = dotProduct;

/// Returns: Outer-Product of Two Vectors $(D a) and $(D b).
auto outerProduct(Ta, Tb, uint Da, uint Db)(in Vector!(Ta, Da) a,
                                            in Vector!(Tb, Db) b)
    if (Da >= 1 &&
        Db >= 1)
{
    Matrix!(CommonType!(Ta, Tb), Da, Db) y = void;
    foreach (r; iota!(0, Da))
    {
        foreach (c; iota!(0, Db))
        {
            y.at(r,c) = a[r] * b[c];
        }
    }
    return y;
}
alias outer = outerProduct;

/// Returns: Vector/Cross-Product of two 3-Dimensional Vectors.
T cross(T)(in T a, in T b)
    if (isVector!T &&
        T.dimension == 3) /// isVector!T &&
{
    return T(a.y * b.z - b.y * a.z,
             a.z * b.x - b.z * a.x,
             a.x * b.y - b.x * a.y);
}

/// Returns: (Euclidean) Distance between $(D a) and $(D b).
real distance(T, U)(in T a, in U b)
    if ((isVector!T && // either both vectors
         isVector!U) ||
        (isPoint!T && // or both points
         isPoint!U))
{
    return (a - b).magnitude;
}

@safe pure nothrow @nogc unittest
{
    auto v1 = vec3f(1, 2, -3);
    auto v2 = vec3f(1, 3, 2);
    assert(cross(v1, v2)[] == [13, -5, 1].s);
    assert(distance(vec2f(0, 0), vec2f(0, 10)) == 10);
    assert(distance(vec2f(0, 0), vec2d(0, 10)) == 10);
    assert(dot(v1, v2) == dot(v2, v1)); // commutative
}

// ==============================================================================================

enum Layout { columnMajor, rowMajor }; // Matrix Storage Major Dimension.

/// Base template for all matrix-types.
/// Params:
///  E = all values get stored as this type
///  rows_ = rows of the matrix
///  cols_ = columns of the matrix
///  layout = matrix layout
struct Matrix(E, uint rows_, uint cols_,
              Layout layout = Layout.rowMajor)
    if (rows_ >= 1 &&
        cols_ >= 1)
{
    alias mT = E; /// Internal type of the _matrix
    static const uint rows = rows_; /// Number of rows
    static const uint cols = cols_; /// Number of columns

    /** Matrix $(RED row-major) in memory. */
    static if (layout == Layout.rowMajor)
    {
        E[cols][rows] _matrix; // In C it would be mt[rows][cols], D does it like this: (mt[cols])[rows]
        ref inout(E) opCall(uint row, uint col) inout { return _matrix[row][col]; }
        ref inout(E)     at(uint row, uint col) inout { return _matrix[row][col]; }
    }
    else
    {
        E[rows][cols] _matrix; // In C it would be mt[cols][rows], D does it like this: (mt[rows])[cols]
        ref inout(E) opCall(uint row, uint col) inout { return _matrix[col][row]; }
        ref inout(E) at    (uint row, uint col) inout { return _matrix[col][row]; }
    }
    alias _matrix this;

    /// Returns: The pointer to the stored values as OpenGL requires it.
    /// Note this will return a pointer to a $(RED row-major) _matrix,
    /// $(RED this means you've to set the transpose argument to GL_TRUE when passing it to OpenGL).
    /// Examples:
    /// ---
    /// // 3rd argument = GL_TRUE
    /// glUniformMatrix4fv(programs.main.model, 1, GL_TRUE, mat4.translation(-0.5f, -0.5f, 1.0f).value_ptr);
    /// ---
    // @property auto value_ptr() { return _matrix[0].ptr; }

    /// Returns: The current _matrix formatted as flat string.

    @property void toString(scope void delegate(const(char)[]) sink) const
    {
        import std.format : formattedWrite;
        sink.formattedWrite("Matrix(%s)", _matrix);
    }

    @property string toLaTeX() const
    {
        string s;
        foreach (r; iota!(0, rows))
        {
            foreach (c; iota!(0, cols))
            {
                s ~= to!string(at(r, c)) ;
                if (c != cols - 1) { s ~= ` & `; } // if not last column
            }
            if (r != rows - 1) { s ~= ` \\ `; } // if not last row
        }
        return `\begin{pmatrix} ` ~ s ~ ` \end{pmatrix}`;
    }

    @property string toMathML() const
    {
        // opening
        string str = `<math><mrow>
  <mo>❲</mo>
  <mtable>`;

        foreach (r; iota!(0, rows))
        {
            str ~=  `
    <mtr>`;
            foreach (c; iota!(0, cols))
            {
                str ~= `
      <mtd>
        <mn>` ~ at(r, c).toMathML ~ `</mn>
      </mtd>`;
            }
            str ~=  `
    </mtr>`;
        }

        // closing
        str ~= `
  </mtable>
  <mo>❳</mo>
</mrow></math>
`;
        return str;
    }

    /// Returns: The current _matrix as pretty formatted string.
    @property string toPrettyString()
    {
        string fmtr = "%s";

        size_t rjust = max(format(fmtr, reduce!(max)(_matrix[])).length,
                           format(fmtr, reduce!(min)(_matrix[])).length) - 1;

        string[] outer_parts;
        foreach (E[] row; _matrix)
        {
            string[] inner_parts;
            foreach (E col; row)
            {
                inner_parts ~= rightJustify(format(fmtr, col), rjust);
            }
            outer_parts ~= " [" ~ join(inner_parts, ", ") ~ "]";
        }

        return "[" ~ join(outer_parts, "\n")[1..$] ~ "]";
    }

    static void isCompatibleMatrixImpl(uint r, uint c)(Matrix!(E, r, c) m) {}

    enum isCompatibleMatrix(T) = is(typeof(isCompatibleMatrixImpl(T.init)));

    static void isCompatibleVectorImpl(uint d)(Vector!(E, d) vec) {}

    enum isCompatibleVector(T) = is(typeof(isCompatibleVectorImpl(T.init)));

    private void construct(uint i, T, Tail...)(T head, Tail tail)
    {
        static if (i >= rows*cols)
        {
            static assert(0, "Too many arguments passed to constructor");
        }
        else static if (is(T : E))
        {
            _matrix[i / cols][i % cols] = head;
            construct!(i + 1)(tail);
        }
        else static if (is(T == Vector!(E, cols)))
        {
            static if (i % cols == 0)
            {
                _matrix[i / cols] = head._vector;
                construct!(i + T.dimension)(tail);
            }
            else
            {
                static assert(0, "Can't convert Vector into the matrix. Maybe it doesn't align to the columns correctly or dimension doesn't fit");
            }
        }
        else
        {
            static assert(0, "Matrix constructor argument must be of type " ~ E.stringof ~ " or Vector, not " ~ T.stringof);
        }
    }

    private void construct(uint i)()  // terminate
    {
        static assert(i == rows*cols, "Not enough arguments passed to constructor");
    }

    /// Constructs the matrix:
    /// If a single value is passed, the matrix will be cleared with this value (each column in each row will contain this value).
    /// If a matrix with more rows and columns is passed, the matrix will be the upper left nxm matrix.
    /// If a matrix with less rows and columns is passed, the passed matrix will be stored in the upper left of an identity matrix.
    /// It's also allowed to pass vectors and scalars at a time, but the vectors dimension must match the number of columns and align correctly.
    /// Examples:
    /// ---
    /// mat2 m2 = mat2(0.0f); // mat2 m2 = mat2(0.0f, 0.0f, 0.0f, 0.0f);
    /// mat3 m3 = mat3(m2); // mat3 m3 = mat3(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f);
    /// mat3 m3_2 = mat3(vec3(1.0f, 2.0f, 3.0f), 4.0f, 5.0f, 6.0f, vec3(7.0f, 8.0f, 9.0f));
    /// mat4 m4 = mat4.identity; // just an identity matrix
    /// mat3 m3_3 = mat3(m4); // mat3 m3_3 = mat3.identity
    /// ---
    this(Args...)(Args args)
    {
        construct!(0)(args);
    }

    this(T)(T mat)
        if (isMatrix!T &&
            (T.cols >= cols) &&
            (T.rows >= rows))
    {
        _matrix[] = mat._matrix[];
    }

    this(T)(T mat)
        if (isMatrix!T &&
            (T.cols < cols) &&
            (T.rows < rows))
    {
        makeIdentity();
        foreach (r; iota!(0, T.rows))
        {
            foreach (c; iota!(0, T.cols))
            {
                at(r, c) = mat.at(r, c);
            }
        }
    }

    this()(E value) { clear(value); }

    /// Returns: `true` if all values are not nan and finite, otherwise `false`.
    @property bool ok() const
    {
        static if (isFloatingPoint!E)
        {
            foreach (row; _matrix)
            {
                foreach (col; row)
                {
                    if (isNaN(col) ||
                        isInfinity(col))
                    {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    /// Sets all values of the matrix to value (each column in each row will contain this value).
    void clear(E value)
    {
        foreach (r; iota!(0, rows))
        {
            foreach (c; iota!(0, cols))
            {
                at(r,c) = value;
            }
        }
    }

    static if (rows == cols)
    {
        /// Makes the current matrix an identity matrix.
        void makeIdentity()
        {
            clear(0);
            foreach (r; iota!(0, rows))
            {
                at(r,r) = 1;
            }
        }

        /// Returns: Identity Matrix.
        static @property Matrix identity()
        {
            Matrix ret;
            ret.clear(0);

            foreach (r; iota!(0, rows))
            {
                ret.at(r,r) = 1;
            }

            return ret;
        }
        alias id = identity;    // shorthand

        /// Transpose Current Matrix.
        void transpose()
        {
            _matrix = transposed()._matrix;
        }
        alias T = transpose; // C++ Armadillo naming convention.

        unittest
        {
            mat2 m2 = mat2(1.0f);
            m2.transpose();
            assert(m2._matrix == mat2(1.0f)._matrix);
            m2.makeIdentity();
            assert(m2._matrix == [[1.0f, 0.0f],
                                  [0.0f, 1.0f]]);
            m2.transpose();
            assert(m2._matrix == [[1.0f, 0.0f],
                                  [0.0f, 1.0f]]);
            assert(m2._matrix == m2.identity._matrix);

            mat3 m3 = mat3(1.1f, 1.2f, 1.3f,
                           2.1f, 2.2f, 2.3f,
                           3.1f, 3.2f, 3.3f);
            m3.transpose();
            assert(m3._matrix == [[1.1f, 2.1f, 3.1f],
                                  [1.2f, 2.2f, 3.2f],
                                  [1.3f, 2.3f, 3.3f]]);

            mat4 m4 = mat4(2.0f);
            m4.transpose();
            assert(m4._matrix == mat4(2.0f)._matrix);
            m4.makeIdentity();
            assert(m4._matrix == [[1.0f, 0.0f, 0.0f, 0.0f],
                                  [0.0f, 1.0f, 0.0f, 0.0f],
                                  [0.0f, 0.0f, 1.0f, 0.0f],
                                  [0.0f, 0.0f, 0.0f, 1.0f]]);
            assert(m4._matrix == m4.identity._matrix);
        }

    }

    /// Returns: a transposed copy of the matrix.
    @property Matrix!(E, cols, rows) transposed() const
    {
        typeof(return) ret;
        foreach (r; iota!(0, rows))
        {
            foreach (c; iota!(0, cols))
            {
                ret.at(c,r) = at(r,c);
            }
        }
        return ret;
    }

}
alias mat2i = Matrix!(int, 2, 2);
alias mat2 = Matrix!(float, 2, 2);
alias mat2d = Matrix!(real, 2, 2);
alias mat2r = Matrix!(real, 2, 2);
alias mat3 = Matrix!(float, 3, 3);
alias mat34 = Matrix!(float, 3, 4);
alias mat4 = Matrix!(float, 4, 4);
alias mat2_cm = Matrix!(float, 2, 2, Layout.columnMajor);

@safe pure nothrow @nogc unittest
{
    auto m = mat2(1, 2,
                  3, 4);
    assert(m(0, 0) == 1);
    assert(m(0, 1) == 2);
    assert(m(1, 0) == 3);
    assert(m(1, 1) == 4);
}

@safe pure nothrow @nogc unittest
{
    auto m = mat2_cm(1, 3,
                     2, 4);
    assert(m(0, 0) == 1);
    assert(m(0, 1) == 2);
    assert(m(1, 0) == 3);
    assert(m(1, 1) == 4);
}

@safe pure nothrow @nogc unittest
{
    alias E = float;
    immutable a = Vector!(E, 2, false, Orient.column)(1, 2);
    immutable b = Vector!(E, 3, false, Orient.column)(3, 4, 5);
    immutable c = outerProduct(a, b);
    assert(c[] == [[3, 4, 5].s,
                   [6, 8, 10].s].s);
}

// ==============================================================================================

/// 3-Dimensional Spherical Point with Coordinate Type (Precision) $(D E).
struct SpherePoint3(E)
    if (isFloatingPoint!E)
{
    enum D = 3;                 // only in three dimensions
    alias ElementType = E;

    /** Construct from Components $(D args). */
    this(T...)(T args)
    {
        foreach (const ix, arg; args)
        {
            _spherePoint[ix] = arg;
        }
    }
    /** Element data. */
    E[D] _spherePoint;
    enum dimension = D;

    @property void toString(scope void delegate(const(char)[]) sink) const
    {
        import std.format : formattedWrite;
        sink.formattedWrite("SpherePoint3(%s)", _spherePoint);
    }

    @property string toMathML() const
    {
        // opening
        string str = `<math><mrow>
  <mo>(</mo>
  <mtable>`;

        foreach (i; iota!(0, D))
        {
            str ~= `
    <mtr>
      <mtd>
        <mn>` ~ _spherePoint[i].toMathML ~ `</mn>
      </mtd>
    </mtr>`;
        }

        // closing
        str ~= `
  </mtable>
  <mo>)</mo>
</mrow></math>
`;
        return str;
    }

    /** Returns: Area 0 */
    @property E area() const { return 0; }

    auto opSlice() { return _spherePoint[]; }
}
alias sphointf = SpherePoint3!float;
alias sphointd = SpherePoint3!double;
alias sphointr = SpherePoint3!real;

/** Instantiator for `SpherePoint3`. */
auto spherePoint(Ts...)(Ts args)
    if (haveCommonType!Ts)
{
    return SpherePoint3!(CommonType!Ts, args.length)(args);
}

// ==============================================================================================

/** $(D D)-Dimensional Particle with Cartesian Coordinate $(D position) and
    $(D velocity) of Element (Component) Type $(D E).
*/
struct Particle(E, uint D,
                bool normalizedVelocityFlag = false)
    if (D >= 1)
{
    Point!(E, D) position;
    Vector!(E, D, normalizedVelocityFlag) velocity;
    E mass;
}

mixin(makeInstanceAliases("Particle", "particle", 2,4, ["float", "double", "real"]));

/** $(D D)-Dimensional Particle with Coordinate Position and
    Direction/Velocity/Force Type (Precision) $(D E).
    F = m*a; where F is force, m is mass, a is acceleration.
*/
struct ForcedParticle(E, uint D,
                      bool normalizedVelocityFlag = false)
    if (D >= 1)
{
    Point!(E, D) position;
    Vector!(E, D, normalizedVelocityFlag) velocity;
    E mass;
    Vector!(E, D) force;

    /// Get acceleration.
    @property auto acceleration() const { return force/mass; }
}

// ==============================================================================================

/** $(D D)-Dimensional Axis-Aligned (Hyper) Cartesian $(D Box) with Element (Component) Type $(D E).

    Note: We must use inclusive compares betweeen boxes and points in inclusion
    functions such as inside() and includes() in order for the behaviour of
    bounding boxes (especially in integer space) to work as desired.
 */
struct Box(E, uint D)
    if (D >= 1)
{
    this(Vector!(E,D) lh) { min = lh; max = lh; }
    this(Vector!(E,D) l_,
         Vector!(E,D) h_) { min = l_; max = h_; }

    @property void toString(scope void delegate(const(char)[]) sink) const
    {
        import std.format : formattedWrite;
        sink.formattedWrite("Box(lower:%s, upper:%s)", min, max);
    }

    /// Get Box Center.
    // TODO @property Vector!(E,D) center() { return (min + max) / 2;}

    /// Constructs a Box enclosing $(D points).
    static Box fromPoints(in Vector!(E,D)[] points)
    {
        Box y;
        foreach (p; points)
        {
            y.expand(p);
        }
        return y;
    }

    /// Expands the Box, so that $(I v) is part of the Box.
    ref Box expand(Vector!(E,D) v)
    {
        foreach (i; iota!(0, D))
        {
            if (min[i] > v[i]) min[i] = v[i];
            if (max[i] < v[i]) max[i] = v[i];
        }
        return this;
    }

    /// Expands Box by another Box $(D b).
    ref Box expand(Box b)
    {
        return this.expand(b.min).expand(b.max);
    }

    unittest
    {
        immutable auto b = Box(Vector!(E,D)(1),
                               Vector!(E,D)(3));
        assert(b.sides == Vector!(E,D)(2));
        immutable auto c = Box(Vector!(E,D)(0),
                               Vector!(E,D)(4));
        assert(c.sides == Vector!(E,D)(4));
        assert(c.sidesProduct == 4^^D);
        assert(unite(b, c) == c);
    }

    /** Returns: Length of Sides */
    @property auto sides() const { return max - min; }

    /** Returns: Area */
    @property real sidesProduct() const
    {
        typeof(return) y = 1;
        foreach (const ref side; sides)
        {
            y *= side;
        }
        return y;
    }

    static if (D == 2)
    {
        alias area = sidesProduct;
    }
    else static if (D == 3)
    {
        alias volume = sidesProduct;
    }
    else static if (D >= 4)
    {
        alias hyperVolume = sidesProduct;
    }

    alias include = expand;

    Vector!(E,D) min;           /// Low.
    Vector!(E,D) max;           /// High.

    /** Either an element in min or max is nan or min <= max. */
    invariant()
    {
        // assert(any!"a==a.nan"(min),
        //                  all!"a || a == a.nan"(elementwiseLessThanOrEqual(min, max)[]));
    }
}

mixin(makeInstanceAliases("Box","box", 2,4, ["int", "float", "double", "real"]));

Box!(E,D) unite(E, uint D)(Box!(E,D) a,
                           Box!(E,D) b) { return a.expand(b); }
Box!(E,D) unite(E, uint D)(Box!(E,D) a,
                           Vector!(E,D) b) { return a.expand(b); }

// ==============================================================================================

/** $(D D)-Dimensional Infinite Cartesian (Hyper)-Plane with Element (Component) Type $(D E).
    See also: http://stackoverflow.com/questions/18600328/preferred-representation-of-a-3d-plane-in-c-c
 */
struct Plane(E, uint D)
    if (D >= 2 &&
        isFloatingPoint!E)
{
    enum dimension = D;

    alias ElementType = E;

    /// Normal type of plane.
    alias NormalType = Vector!(E, D, true);

    union
    {
        static if (D == 3)
        {
            struct
            {
                E a; /// normal.x
                E b; /// normal.y
                E c; /// normal.z
            }
        }
        NormalType normal;      /// Plane Normal.
    }
    E distance;                  /// Plane Constant (Offset from origo).

    @property void toString(scope void delegate(const(char)[]) sink) const
    {
        import std.format : formattedWrite;
        sink.formattedWrite("Plane(normal:%s, distance:%s)", normal, distance);
    }

    /// Constructs the plane, from either four scalars of type $(I E)
    /// or from a 3-dimensional vector (= normal) and a scalar.
    static if (D == 2)
    {
        this(E a, E b, E distance)
        {
            this.normal.x = a;
            this.normal.y = b;
            this.distance = distance;
        }
    }
    else static if (D == 3)
    {
        this(E a, E b, E c, E distance)
        {
            this.normal.x = a;
            this.normal.y = b;
            this.normal.z = c;
            this.distance = distance;
        }
    }

    this(NormalType normal, E distance)
    {
        this.normal = normal;
        this.distance = distance;
    }

    /* unittest
       { */
    /*     Plane p = Plane(0.0f, 1.0f, 2.0f, 3.0f); */
    /*     assert(p.normal == N(0.0f, 1.0f, 2.0f)); */
    /*     assert(p.distance == 3.0f); */

    /*     p.normal.x = 4.0f; */
    /*     assert(p.normal == N(4.0f, 1.0f, 2.0f)); */
    /*     assert(p.x == 4.0f); */
    /*     assert(p.y == 1.0f); */
    /*     assert(p.c == 2.0f); */
    /*     assert(p.distance == 3.0f); */
    /* } */

    /// Normalizes the plane inplace.
    void normalize()
    {
        immutable E det = cast(E)1 / normal.magnitude;
        normal *= det;
        distance *= det;
    }

    /// Returns: a normalized copy of the plane.
    /* @property Plane normalized() const { */
    /*     Plane y = Plane(a, b, c, distance); */
    /*     y.normalize(); */
    /*     return y; */
    /* } */

//     unittest {
//         Plane p = Plane(0.0f, 1.0f, 2.0f, 3.0f);
//         Plane pn = p.normalized();
//         assert(pn.normal == N(0.0f, 1.0f, 2.0f).normalized);
//         assert(almost_equal(pn.distance, 3.0f / N(0.0f, 1.0f, 2.0f).length));
//         p.normalize();
//         assert(p == pn);
//     }

    /// Returns: distance from a point to the plane.
    /// Note: the plane $(RED must) be normalized, the result can be negative.
    /* E distanceTo(N point) const { */
    /*     return dot(point, normal) + distance; */
    /* } */

    /// Returns: distanceTo from a point to the plane.
    /// Note: the plane does not have to be normalized, the result can be negative.
    /* E ndistance(N point) const { */
    /*     return (dot(point, normal) + distance) / normal.magnitude; */
    /* } */

    /* unittest
       { */
    /*     Plane p = Plane(-1.0f, 4.0f, 19.0f, -10.0f); */
    /*     assert(almost_equal(p.ndistance(N(5.0f, -2.0f, 0.0f)), -1.182992)); */
    /*     assert(almost_equal(p.ndistance(N(5.0f, -2.0f, 0.0f)), */
    /*                         p.normalized.distanceTo(N(5.0f, -2.0f, 0.0f)))); */
    /* } */

    /* bool opEquals(Plane other) const { */
    /*     return other.normal == normal && other.distance == distance; */
    /* } */

}

mixin(makeInstanceAliases("Plane","plane", 3,4, defaultElementTypes));

// ==============================================================================================

/** $(D D)-Dimensional Cartesian (Hyper)-Sphere with Element (Component) Type $(D E).
*/
struct Sphere(E, uint D)
    if (D >= 2 &&
        isNumeric!E)
{
    alias CenterType = Point!(E, D);

    CenterType center;
    E radius;

    void translate(Vector!(E, D) shift) { center = center + shift; } // point + vector => point
    alias shift = translate;

    @property:

    E diameter() const
    {
        return 2 * radius;
    }
    static if (D == 2)
    {
        auto area() const
        {
            return PI * radius ^^ 2;
        }
    }
    else static if (D == 3)
    {
        auto area() const
        {
            return 4 * PI * radius ^^ 2;
        }
        auto volume() const
        {
            return 4 * PI * radius ^^ 3 / 3;
        }
    }
    else static if (D >= 4)
    {
        // See also: https://en.wikipedia.org/wiki/Volume_of_an_n-ball
        real n = D;
        auto volume() const
        {
            import std.mathspecial: gamma;
            return PI ^^ (n / 2) / gamma(n / 2 + 1) * radius ^^ n;
        }
    }
}

auto sphere(C, R)(C center, R radius)
{
    return Sphere!(C.ElementType, C.dimension)(center, radius);
}
// TODO Use this instead:
// auto sphere(R, C...)(Point!(CommonType!C, C.length) center, R radius) {
// return Sphere!(CommonType!C, C.length)(center, radius);
// }

@safe pure nothrow @nogc unittest
{
    const x = sphere(point(1.0, 2, 3, 4), 10.0);
    version(print) dln(x, " has volume ", x.volume);
}

/**
   See also: http://stackoverflow.com/questions/401847/circle-rectangle-collision-detection-intersect
 */
bool intersect(T)(Circle!T circle, Rect!T rect)
{
    immutable hw = rect.w/2, hh = rect.h/2;

    immutable dist = Point!T(abs(circle.x - rect.x0 - hw),
                             abs(circle.y - rect.y0 - hh));

    if (dist.x > (hw + circle.r)) return false;
    if (dist.y > (hh + circle.r)) return false;

    if (dist.x <= hw) return true;
    if (dist.y <= hh) return true;

    immutable cornerDistance_sq = ((dist.x - hw)^^2 +
                                   (dist.y - hh)^^2);

    return (cornerDistance_sq <= circle.r^^2);
}

@safe pure nothrow @nogc unittest
{
    version(print)
    {
        dln(box2f(vec2f(1, 2),
                  vec2f(3, 3)));
        dln([12, 3, 3]);

        dln(sort(vec2f(2, 3)[]));
        dln(vec2f(2, 3));

        dln(vec2f(2, 3));
        dln(vec2f(2, 3));

        dln(vec3f(2, 3, 4));

        dln(box2f(vec2f(1, 2),
                  vec2f(3, 4)));

        dln(vec2i(2, 3));
        dln(vec3i(2, 3, 4));
        dln( + vec3i(2, 3, 4));
        dln("vec2i:\n", vec2i(2, 3).toMathML);

        auto m = mat2(1, 2, 3, 4);
        dln("LaTeX:\n", m.toLaTeX);
        dln("MathML:\n", m.toMathML);
    }
}

version(unittest)
{
    import array_help : s;
}

version(print)
{
    import dbgio;
}
