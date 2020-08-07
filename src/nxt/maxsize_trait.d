/** Trait for getting maximum size of types `T`.
 *
 * Implementation in `std.variant` uses recursion.
 *
 * See_Also: https://forum.dlang.org/post/hzpuiyxrrfasfuktpgqn@forum.dlang.org
 */
module nxt.maxsize_trait;

/** Get maximum size of types `Ts`.
 *
 * Limitation: `Ts` cannot contain `void`.
 */
static template maxSizeOf(Ts...)
{
    align(1) union Impl { Ts t; }
 	enum maxSizeOf = Impl.sizeof;
}

///
@safe pure unittest
{
    static assert(maxSizeOf!(char) == 1);
    static assert(maxSizeOf!(byte) == 1);
    static assert(maxSizeOf!(byte, short) == 2);
    static assert(maxSizeOf!(short, byte) == 2);
    static assert(maxSizeOf!(byte, short, int) == 4);
    static assert(maxSizeOf!(byte, short, int, long) == 8);
    static assert(maxSizeOf!(byte, short, int, string) == 16);
    static assert(!__traits(compiles, { enum _ = maxSizeOf!(1, void); }));
}

// alternative implementation that supports `void`
static template maxSizeOf_1(Ts...)
{
	align(1) union Impl {
		static foreach (i, T; Ts) {
			static if (!is(T == void))
                mixin("T _field_" ~ i.stringof ~ ";");
		}
	}
	enum maxSizeOf_1 = Impl.sizeof;
}

///
@safe pure unittest
{
    static assert(maxSizeOf_1!(char) == 1);
    static assert(maxSizeOf_1!(byte) == 1);
    static assert(maxSizeOf_1!(byte, short) == 2);
    static assert(maxSizeOf_1!(short, byte) == 2);
    static assert(maxSizeOf_1!(byte, short, int) == 4);
    static assert(maxSizeOf_1!(byte, short, int, long) == 8);
    static assert(maxSizeOf_1!(byte, short, int, string) == 16);
}
