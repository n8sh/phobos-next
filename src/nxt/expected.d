/** Wrapper type for a sum-type (union) of an unexpected and expected value.
 */
module nxt.expected;

import std.traits : isInstanceOf;

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
 * See_Also: https://github.com/dlang/phobos/pull/6665
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
struct Expected(T, E = Exception)
if (!isInstanceOf!(Unexpected, T) && // an `Unexpected` cannot be `Expected` :)
    !is(T == void)) // disallow void for now, for ref see https://forum.dlang.org/post/ncjhsxshttikzjqgiwev@forum.dlang.org
{
    import core.lifetime : moveEmplace;

    // TODO ok for default construction to initialize
    // - _expectedValue = T.init (zeros)
    // - _ok = true (better to have _isError so default is zero bits here aswell?)

    /// Construct from expected value `expectedValue.`
    this()(auto ref T expectedValue) @trusted
    {
        // TODO reuse opAssign?
        moveEmplace(expectedValue, _expectedValue);
        _ok = true;
    }

    /// Construct from unexpected value `unexpectedValue.`
    this(Unexpected!E unexpectedValue) @trusted
    {
        // TODO reuse opAssign?
        moveEmplace(unexpectedValue, _unexpectedValue);
        _ok = false;
    }

    /// Assign from expected value `expectedValue.`
    void opAssign()(auto ref T expectedValue) @trusted
    {
        // TODO is this ok?:
        clear();
        moveEmplace(expectedValue, _expectedValue);
        _ok = true;
    }

    /// Assign from unexpected value `unexpectedValue.`
    void opAssign(E unexpectedValue) @trusted
    {
        clear();
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
        import core.internal.traits : hasElaborateDestructor;
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

/** Is `true` iff `T` is a memory address. */
private template isAddress(T)
{
    import std.traits : isPointer;
    enum isAddress = (is(T == class) || // a class is memory-wise
                      isPointer!T);     // just a pointer, consistent with opCmp
}