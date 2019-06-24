/** Algorithms for dynamic and static bitarrays based with integral blocks.
 *
 * Algorithms are qualified as `package` because they should only be used by
 * bit-array containers.
 */
module bitarray_algorithm;

@safe pure nothrow @nogc:

size_t countOnes(Blocks)(const scope auto ref Blocks blocks, size_t length) @trusted
if (isBlocks!Blocks)
{
    typeof(return) result = 0;

    alias Block = typeof(Blocks.init[0]);
    enum bitsPerBlock = 8*Block.sizeof;
    const fullBlockCount = length / bitsPerBlock;
    const restBitCount = length % bitsPerBlock;

    // dbg("bitsPerBlock:", bitsPerBlock,
    //     " length:", length,
    //     " fullBlockCount:", fullBlockCount,
    //     " restBitCount:", restBitCount);

    import core.bitop : popcnt;
    foreach (const block; blocks.ptr[0 .. fullBlockCount])
    {
        result += cast(typeof(result))popcnt(block);
    }
    if (restBitCount)
    {
        result += cast(typeof(result))popcnt(blocks.ptr[fullBlockCount] & ((1UL << restBitCount)-1));
    }

    return typeof(return)(result);
}

/// Test `countOnes` with only full blocks.
@safe pure nothrow @nogc unittest
{
    enum blockCount = 3;
    size_t[blockCount] x;
    const length = 8*x.sizeof;
    assert(countOnes(x, length) == 0);

    x[0] = 1;
    assert(countOnes(x, length) == 1);

    x[0] = 1+2;
    assert(countOnes(x, length) == 2);

    x[0] = 1+2;
    x[1] = 1+2;
    x[2] = 1+2;
    assert(countOnes(x, length) == 6);
}

/// Test `countOnes` with only parial blocks.
@safe pure nothrow @nogc unittest
{
    enum blockCount = 1;
    size_t[blockCount] x;
    foreach (const length; 1 .. 8*x.sizeof)
    {
        // dbg("=== length:", length);
        assert(countOnes(x, length) == 0);

        x[0] |= 1UL << (length-1);
        // dbg(x[0]);
        assert(countOnes(x, length) == 1);
        x[0] = 0;
    }
}

size_t indexOfFirstOne(Blocks)(const scope auto ref Blocks blocks, size_t length)
if (isBlocks!Blocks)
{
    foreach (const blockIndex, const block; blocks)
    {
        if (block != block.min) // optimize for ones-sparsity
        {
            import core.bitop : bsf;
            const hitIndex = blockIndex*8*block.sizeof + bsf(block);
            return hitIndex < length ? hitIndex : length; // if hit beyond end miss
        }
    }
    return length;              // miss
}

/** Find index of first cleared (zero) bit or `length` if no bit cleared.
 *
 * Optimized for zeros-sparsity.
 */
size_t indexOfFirstZero(Blocks)(const scope auto ref Blocks blocks, size_t length)
if (isBlocks!Blocks)
{
    foreach (const blockIndex, block; blocks)
    {
        if (block != block.max) // optimize for zeros-sparsity
        {
            import core.bitop : bsf;
            const hitIndex = blockIndex*8*block.sizeof + bsf(~block); // TODO is there a builtin for `bsf(~block)`?
            return hitIndex < length ? hitIndex : length; // if hit beyond end miss
        }
    }
    return length;              // miss
}

private enum isBlocks(Blocks) = (is(typeof(Blocks.init[0]) : uint) ||
                                 is(typeof(Blocks.init[0]) : ulong));

version(unittest) import dbgio;
