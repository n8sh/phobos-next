/** Wrapper type for a sum-type (union) of an unexpected and expected value.
 *
 * See_Also: https://github.com/dlang/phobos/pull/6665
 *
 * TODO https://dlang.org/phobos/std_typecons.html#.apply
 *
 * TODO I'm not convinced about the naming
 * - `Expected`: instead call it something that tells us that it can be either expected or unexpected?
 * - `Unexpected`: if so why shouldn't we have a similar value wrapper `Expected`?
 *
 * TODO later on: remove _hasExpectedValue when `_expectedValue` and ` _unexpectedValue` can store this state
 * "collectively" for instance when both are pointers or classes (use trait
 * `isAddress`)
 */
module expected;

@safe pure:

private struct Unexpected(U)
{
    U value;
    alias value this;
}

auto unexpected(T, U)(auto ref U unexpectedValue)
{
    return Expected!(T, U)(Unexpected!U(unexpectedValue));
}

/** Sum type of either an expected value `T` or an `Unexpected` value `U`.
 *
 * See_Also: https://www.youtube.com/watch?v=nVzgkepAg5Y
 */
struct Expected(T, U)
{
    @safe:

    // TODO ok for default construction to initialize
    // - _expectedValue = T.init (zeros)
    // - _hasExpectedValue = true (better to have _isError so default is zero bits here aswell?)

    /// Construct from expected value `expectedValue.`
    this(T expectedValue) @trusted
    {
        // TODO reuse opAssign?
        _expectedValue = expectedValue;       // TODO use moveEmplace here aswell?
        _hasExpectedValue = true;
    }

    /// Construct from unexpected value `unexpectedValue.`
    this(Unexpected!U unexpectedValue) @trusted
    {
        // TODO reuse opAssign?
        _unexpectedValue = unexpectedValue; // TODO use moveEmplace here aswell?
        _hasExpectedValue = false;
    }

    /// Assign from expected value `expectedValue.`
    void opAssign(T expectedValue) @trusted
    {
        clear();
        import std.algorithm.mutation : moveEmplace;
        moveEmplace(expectedValue, _expectedValue);
        _hasExpectedValue = true;
    }

    /// Assign from unexpected value `unexpectedValue.`
    void opAssign(U unexpectedValue) @trusted
    {
        clear();
        import std.algorithm.mutation : moveEmplace;
        moveEmplace(unexpectedValue, _unexpectedValue);
        _hasExpectedValue = false;
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
            _hasExpectedValue = false;
        }
        else
        {
            static if (!is(U == class))
            {
                static if (hasElaborateDestructor!U)
                {
                    destroy(_unexpectedValue);
                }
            }
            destroy(_unexpectedValue);
            // TODO change _hasExpectedValue?
        }
    }

    /** Is `true` iff this has a expectedValue of type `T`. */
    bool hasExpectedValue() const { return _hasExpectedValue; }

    import std.traits : CommonType;

    /** Get current value if any or call function `elseWorkFun` with compatible return value.
     *
     * TODO is this anywhere near what we want?
     */
    CommonType!(T, typeof(elseWorkFun())) orElse(alias elseWorkFun)() const
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

    // range interface:

    /// Check if empty.
    @property bool empty() const
    {
        return !_hasExpectedValue;
    }

    /// Get current value.
    @property inout(T) front() inout @trusted
    {
        assert(_hasExpectedValue);
        return _expectedValue;
    }

    /// Pop (clear) current value.
    void popFront()
    {
        assert(_hasExpectedValue);
        clear();
    }

private:
    union
    {
        T _expectedValue;         // TODO do we need to default-initialize this somehow?
        Unexpected!U _unexpectedValue;
    }
    bool _hasExpectedValue = true;     // @andralex: ok to opportunistic and default to `T.init`
}

auto expected(T, U)(auto ref T value)
{
    return Expected!(T, U)(value);
}

@safe pure nothrow @nogc unittest
{
    alias T = string;           // expected type
    alias U = int;
    alias E = Expected!(T, int);

    auto x = E("alpha");
    assert(x.hasExpectedValue);
    assert(!x.empty);

    x.popFront();
    assert(!x.hasExpectedValue);
    assert(x.empty);

    import std.typecons : Nullable;

    auto e = E(Unexpected!int(int.init));
    assert(!e.hasExpectedValue);
    assert(x.empty);

    // TODO test x.orElse({ some_simple_code; })
}

import std.traits : isPointer;

private enum isAddress(T) = (is(T == class) || // a class is memory-wise
                             isPointer!T);     // just a pointer, consistent with opCmp
