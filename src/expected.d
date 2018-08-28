/** Wrapper type for a sum-type (union) of an unexpected and expected value.
 *
 * See_Also: https://github.com/dlang/phobos/pull/6665
 *
 * TODO merge in https://run.dlang.io/?compiler=dmd&source=%2F%2Bdub.sdl:%0Adependency%20%22sumtype%22%20version%3D%22~%3E0.5.2%22%0A%2B%2F%0Aimport%20std.stdio;%0Aimport%20std.traits%20:%20isInstanceOf;%0A%0Astruct%20S%20%7B%0A%20%20%20%20@disable%20this();%0A%20%20%20%20~this()%20pure%20@safe%20%7B%20%7D%0A%20%20%20%20this(int%20i)%20pure%20@safe%20%7B%20this.i%20%3D%20i;%20%7D%0A%20%20%20%20void%20opAssign(S)%20%7B%20assert(false);%20%7D%0A%20%20%20%20immutable%20int%20i;%0A%20%20%20%20invariant%20%7B%20assert(i%20!%3D%200);%20%7D%0A%7D%0A%0Aauto%20toInt(string%20str)%20pure%20@safe%20%7B%0A%20%20%20%20try%20%7B%0A%20%20%20%20%20%20%20%20import%20std.conv:%20to;%0A%20%20%20%20%20%20%20%20%0A%20%20%20%20%20%20%20%20return%20expected!(S,%20string)(S(str.to!int));%0A%20%20%20%20%7D%20catch%20(Exception%20ex)%20%7B%0A%20%20%20%20%20%20%20%20return%20unexpected!(S,%20string)(ex.msg);%0A%20%20%20%20%7D%0A%7D%0A%0Avoid%20main()%20@safe%20%7B%0A%20%20%20%20toInt(%221%22).thenElse!((S%20val)%20%7B%0A%20%20%20%20%20%20%20%20writeln(val.i);%0A%20%20%20%20%7D,%20(string%20error)%20%7B%0A%20%20%20%20%20%20%20%20writeln(error);%0A%20%20%20%20%7D);%0A%20%20%20%20toInt(%221fff%22).thenElse!((S%20val)%20%7B%0A%20%20%20%20%20%20%20%20writeln(val.i);%0A%20%20%20%20%7D,%20(string%20error)%20%7B%0A%20%20%20%20%20%20%20%20writeln(error);%0A%20%20%20%20%7D);%0A%7D%0A%0Astruct%20Unexpected(T)%20%7B%0A%20%20%20%20T%20value;%0A%7D%0A%0Astruct%20Expected(T_,%20U_)%20%7B%0A%20%20%20%20import%20sumtype%20:%20SumType;%0A%20%20%20%20%0A%20%20%20%20alias%20T%20%3D%20T_,%20U%20%3D%20U_;%0A%20%20%20%20%0A%20%20%20%20SumType!(T,%20Unexpected!U)%20sum;%0A%20%20%20%20%0A%20%20%20%20this()(auto%20ref%20T%20value)%20%7B%0A%20%20%20%20%20%20%20%20sum%20%3D%20value;%0A%20%20%20%20%7D%0A%20%20%20%20%0A%20%20%20%20this()(auto%20ref%20Unexpected!U%20value)%20%7B%0A%20%20%20%20%20%20%20%20sum%20%3D%20value;%0A%20%20%20%20%7D%0A%7D%0A%0Atemplate%20thenElse(alias%20thenDo,%20alias%20elseDo)%0A%7B%0A%20%20%20%20auto%20thenElse(E)(E%20expected)%0A%20%20%20%20if%20(isInstanceOf!(Expected,%20E))%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%20%20import%20sumtype%20:%20match;%0A%20%20%20%20%20%20%20%20%0A%20%20%20%20%20%20%20%20return%20expected.sum.match!((E.T%20value)%20%3D%3E%20thenDo(value),%20(Unexpected!(E.U)%20error)%20%3D%3E%20elseDo(error.value));%0A%20%20%20%20%7D%0A%7D%0A%0Aauto%20expected(T,%20U)(auto%20ref%20T%20value)%20%7B%0A%20%20%20%20return%20Expected!(T,%20U)(value);%0A%7D%0A%0Aauto%20unexpected(T,%20U)(auto%20ref%20U%20value)%7B%0A%20%20%20%20return%20Expected!(T,%20U)(Unexpected!U(value));%0A%7D&args=-unittest%20-debug
 *
 * TODO https://dlang.org/phobos/std_typecons.html#.apply
 *
 * TODO I'm not convinced about the naming
 * - `Expected`: instead call it something that tells us that it can be either expected or unexpected?
 * - `Unexpected`: if so why shouldn't we have a similar value wrapper `Expected`?
 *
 * TODO we could get around the `Unexpected` wrapper logic by instead expressing
 * construction in static constructor functions, say,:
 * - static typeof(this) fromExpectedValue(T expectedValue)
 * - static typeof(this) fromUnexpectedValue(E unexpectedValue)
 *
 * TODO swap
 *
 * TODO which functions should be `nothrow`?
 *
 * TODO later on: remove _ok when `_expectedValue` and ` _unexpectedValue` can store this state
 * "collectively" for instance when both are pointers or classes (use trait
 * `isAddress`)
 *
 * TODO ok to default `E` to `Exception`?
 */
module expected;

import std.traits : isInstanceOf;

@safe pure:

/** Wrapper type for an unexpected value of type `E`.
 */
private struct Unexpected(E)
{
    E value;
    alias value this;
}

/** Union (sum) type of either an expected (most probable) value of type `T` or
 * an unexpected value of type `E` (being an instance of type `Unexpected!E`).
 *
 * `E` is typically an error code (for instance C/C++'s' `errno` int) or a
 * subclass of `Exception` (which is the default).
 *
 * See_Also: https://www.youtube.com/watch?v=nVzgkepAg5Y
 */
struct Expected(T, E = Exception)
if (!isInstanceOf!(Unexpected, T)) // an `Unexpected` cannot be `Expected` :)
{
    @safe:

    // TODO ok for default construction to initialize
    // - _expectedValue = T.init (zeros)
    // - _ok = true (better to have _isError so default is zero bits here aswell?)

    /// Construct from expected value `expectedValue.`
    this(T expectedValue) @trusted
    {
        // TODO reuse opAssign?
        _expectedValue = expectedValue;       // TODO use `moveEmplace` here aswell?
        _ok = true;
    }

    /// Construct from unexpected value `unexpectedValue.`
    this(Unexpected!E unexpectedValue) @trusted
    {
        // TODO reuse opAssign?
        _unexpectedValue = unexpectedValue; // TODO use `moveEmplace` here aswell?
        _ok = false;
    }

    /// Assign from expected value `expectedValue.`
    void opAssign(T expectedValue) @trusted
    {
        clear();
        import std.algorithm.mutation : moveEmplace;
        moveEmplace(expectedValue, _expectedValue);
        _ok = true;
    }

    /// Assign from unexpected value `unexpectedValue.`
    void opAssign(E unexpectedValue) @trusted
    {
        clear();
        import std.algorithm.mutation : moveEmplace;
        moveEmplace(unexpectedValue, _unexpectedValue);
        _ok = false;
    }

    /// Clear (empty) contents.
    private void clear() @trusted
    {
        release();
        static if (isAddress!T)
        {
            _expectedValue = null;
        }
    }

    /// Release any memory used to store contents.
    private void release() @trusted
    {
        import std.traits : hasElaborateDestructor;
        if (hasExpectedValue)
        {
            static if (!is(T == class))
            {
                static if (hasElaborateDestructor!T)
                {
                    destroy(_expectedValue);
                }
            }
            _ok = false;
        }
        else
        {
            static if (!is(E == class))
            {
                static if (hasElaborateDestructor!E)
                {
                    destroy(_unexpectedValue);
                }
            }
            destroy(_unexpectedValue);
            // TODO change _ok?
        }
    }

    /** Is `true` iff this has a expectedValue of type `T`. */
    bool hasExpectedValue() const
    {
        return _ok;
    }

    import std.traits : CommonType;

    /** Get current value if any or call function `elseWorkFun` with compatible return value.
     *
     * TODO is this anywhere near what we want?
     */
    CommonType!(T, typeof(elseWorkFun())) valueOr(alias elseWorkFun)() const
    if (is(CommonType!(T, typeof(elseWorkFun()))))
    {
        if (hasExpectedValue)
        {
            return expectedValue;
        }
        else
        {
            return elseWorkFun(); // TODO is this correct
        }
    }

    import std.functional : unaryFun;

    /** If `this` is an expected value (of type `T`) apply `fun` on it and
     * return result, otherwise return current unexpected value (of type `E`).
     *
     * See_Also: https://dlang.org/phobos/std_typecons.html#.apply
     */
    Expected!(typeof(unaryFun!fun(T.init)), E) apply(alias fun)() @trusted
    {
        alias fn = unaryFun!fun;
        if (hasExpectedValue)
        {
            return typeof(return)(fn(_expectedValue));
        }
        else
        {
            return typeof(return)(Unexpected!E(_unexpectedValue));
        }
    }

    // range interface:

    /// Check if empty.
    @property bool empty() const
    {
        return !_ok;
    }

    /// Get current value.
    @property inout(T) front() inout @trusted
    {
        assert(_ok);
        return _expectedValue;
    }

    /// Pop (clear) current value.
    void popFront()
    {
        assert(_ok);
        clear();
    }

private:
    union
    {
        T _expectedValue;         // TODO do we need to default-initialize this somehow?
        Unexpected!E _unexpectedValue;
    }

    /** Is true if `_expectedValue` is defined, otherwise `_unexpectedValue` is
     * defined.
     *
     * According to @andralex its ok to be opportunistic and default to
     * `T.init`, because of the naming `Expected`.
     */
    bool _ok = true;
}

/// Insantiator for `Expected` from an expected value `expectedValue.`
auto expected(T, E)(auto ref T expectedValue)
{
    return Expected!(T, E)(expectedValue);
}

/// Insantiator for `Expected` from an unexpected value `unexpectedValue.`
auto unexpected(T, E)(auto ref E unexpectedValue)
{
    return Expected!(T, E)(Unexpected!E(unexpectedValue));
}

///
@safe pure nothrow @nogc unittest
{
    alias T = string;           // expected type
    alias E = byte;

    alias Esi = Expected!(T, byte);

    // equality checks
    assert(Esi("abc") == Esi("abc"));
    assert(Esi("abcabc"[0 .. 3]) ==
           Esi("abcabc"[3 .. 6]));

    auto x = Esi("abc");
    assert(x.hasExpectedValue);
    assert(!x.empty);
    assert(x.apply!(threeUnderscores) == Esi("___"));

    x.popFront();
    assert(!x.hasExpectedValue);
    assert(x.empty);

    auto y = unexpected!(T, byte)(byte.init);
    assert(!y.hasExpectedValue);
    assert(x.empty);
    assert(y.apply!(threeUnderscores) == Esi(Unexpected!byte(byte.init)));
}

version(unittest)
inout(string) threeUnderscores(inout(string) x) @safe pure nothrow @nogc
{
    return "___";
}

import std.traits : isPointer;

private enum isAddress(T) = (is(T == class) || // a class is memory-wise
                             isPointer!T);     // just a pointer, consistent with opCmp
