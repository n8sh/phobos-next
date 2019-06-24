/** Algorithms for dynamic and static bitarrays based with integral blocks.
 *
 * Algorithms are qualified as `package` because they should only be used by
 * bit-array containers.
 */
module bitarray_algorithm;

@safe pure nothrow @nogc:

size_t countOnes(Blocks)(const scope auto ref Blocks blocks)
if (isBlocks!Blocks)
{
    typeof(return) result = 0;
    foreach (const blockIndex, const block; blocks)
    {
        import core.bitop : popcnt;
        result += cast(typeof(result))popcnt(block);
    }
    return typeof(return)(result);
}

/// Test `countOnes`.
@safe pure nothrow @nogc unittest
{
    enum n = 3;
    size_t[n] x;
    assert(countOnes(x) == 0);

    x[0] = 1;
    assert(countOnes(x) == 1);

    x[0] = 1+2;
    assert(countOnes(x) == 2);

    x[0] = 1+2;
    x[1] = 1+2;
    x[2] = 1+2;
    assert(countOnes(x) == 6);
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

/** Find index of last set (one) bit or `length` if no bit set.
 *
 * Optimized for ones-sparsity.
 */
size_t indexOfLastOne(Blocks)(const scope auto ref Blocks blocks, size_t length)
if (isBlocks!Blocks)
{
    foreach_reverse (const blockIndex, const block; blocks)
    {
        if (block != block.min) // optimize for ones-sparsity
        {
            import core.bitop : bsr;
            return blockIndex*8*block.sizeof + bsr(block);
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
    foreach (const blockIndex, const block; blocks)
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

/** Find index of last cleared (zero) bit or `length` if no bit cleared.
 *
 * Optimized for zeros-sparsity.
 */
size_t indexOfLastZero(Blocks)(const scope auto ref Blocks blocks, size_t length)
if (isBlocks!Blocks)
{
    foreach_reverse (const blockIndex, const block; blocks)
    {
        if (block != block.max) // optimize for zeros-sparsity
        {
            import core.bitop : bsr;
            return blockIndex*8*block.sizeof + bsr(~block); // TODO is there a builtin for `bsr(~block)`?
        }
    }
    return length;              // miss
}

private enum isBlocks(Blocks) = (is(typeof(Blocks.init[0]) : uint) ||
                                 is(typeof(Blocks.init[0]) : ulong));
