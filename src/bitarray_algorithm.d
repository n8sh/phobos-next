/** Algorithms for dynamic and static bitarrays based with integral blocks.
 */
module bitarray_algorithm;

@safe pure nothrow @nogc:

size_t countOnes(Blocks)(const scope auto ref Blocks blocks)
if (isBlocks!Blocks)
{
    typeof(return) n = 0;
    foreach (const block; blocks)
    {
        import core.bitop : popcnt;
        n += cast(typeof(n))popcnt(block);
    }
    return typeof(return)(n);
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

size_t indexOfFirstOne(Blocks)(const scope auto ref Blocks blocks)
{
    foreach (const blockIndex, const block; blocks)
    {
        if (block != Block.min) // optimize for ones-sparsity
        {
            import core.bitop : bsf;
            return blockIndex*bitsPerBlock + bsf(block);
        }
    }
    return length;
}

/** Find index of last set (one) bit or `length` if no bit set.
 *
 * Optimized for ones-sparsity.
 */
size_t indexOfLastOne(Blocks)(const scope auto ref Blocks blocks)
{
    foreach_reverse (const blockIndex, const block; blocks)
    {
        if (block != Block.min) // optimize for ones-sparsity
        {
            import core.bitop : bsr;
            return blockIndex*bitsPerBlock + bsr(block);
        }
    }
    return length;
}

/** Find index of first cleared (zero) bit or `length` if no bit cleared.
 *
 * Optimized for zeros-sparsity.
 */
size_t indexOfFirstZero(Blocks)(const scope auto ref Blocks blocks)
{
    foreach (const blockIndex, const block; blocks)
    {
        if (block != Block.max) // optimize for zeros-sparsity
        {
            import core.bitop : bsf;
            return blockIndex*bitsPerBlock + bsf(~block); // TODO is there a builtin for `bsf(~block)`?
        }
    }
    return length;
}

/** Find index of last cleared (zero) bit or `length` if no bit cleared.
 *
 * Optimized for zeros-sparsity.
 */
size_t indexOfLastZero(Blocks)(const scope auto ref Blocks blocks)
{
    foreach_reverse (const blockIndex, const block; blocks)
    {
        if (block != Block.max) // optimize for zeros-sparsity
        {
            import core.bitop : bsr;
            return blockIndex*bitsPerBlock + bsr(~block); // TODO is there a builtin for `bsr(~block)`?
        }
    }
    return length;
}

private enum isBlocks(Blocks) = (is(typeof(Blocks.init[0]) : uint) ||
                                 is(typeof(Blocks.init[0]) : ulong));
