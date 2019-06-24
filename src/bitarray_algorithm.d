/** Algorithms for dynamic and static bitarrays based with integral blocks.
 *
 * Algorithms are qualified as `package` because they should only be used by
 * bit-array containers.
 */
module bitarray_algorithm;

@safe pure nothrow @nogc:

package size_t countOnes(Blocks)(const scope auto ref Blocks blocks)
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

package size_t indexOfFirstOne(Blocks)(const scope auto ref Blocks blocks, size_t length)
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
package size_t indexOfLastOne(Blocks)(const scope auto ref Blocks blocks, size_t length)
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
package size_t indexOfFirstZero(Blocks)(const scope auto ref Blocks blocks, size_t length)
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
package size_t indexOfLastZero(Blocks)(const scope auto ref Blocks blocks, size_t length)
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
