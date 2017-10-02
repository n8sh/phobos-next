module container_algorithm;

bool popFirst(C, E)(ref C haystack,
                    in E needle)
    if (__traits(hasMember, C, `popAt`))
    // TODO activate this restriction
    // if (isContainer!C &&
    //     is(ElementType!C == E.init))
{
    immutable offset = haystack[].countUntil(value);
    if (offset != -1)
    {
        haystack.popAt(offset);
        return true;
    }
    return false;
}
