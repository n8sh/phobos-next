module splitter_ex;

/** Non-decoding ASCII-needle-only variant of Phobos' `splitter`. */
template splitterASCIIAmong(needles...)
if (needles.length != 0 &&
    isExpressions!needles)
{
}
