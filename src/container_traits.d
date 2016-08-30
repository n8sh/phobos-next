module container_traits;

template ContainerElementType(ContainerType, ElementType)
{
    import std.traits : isMutable, hasIndirections, PointerTarget, isPointer, Unqual;

    template ET(bool isConst, T)
    {
        static if (isPointer!ElementType)
        {
            enum PointerIsConst = is(ElementType == const);
            enum PointerIsImmutable = is(ElementType == immutable);
            enum DataIsConst = is(PointerTarget!ElementType == const);
            enum DataIsImmutable = is(PointerTarget!ElementType == immutable);
            static if (isConst)
            {
                static if (PointerIsConst)
                    alias ET = ElementType;
                else static if (PointerIsImmutable)
                    alias ET = ElementType;
                else
                    alias ET = const(PointerTarget!ElementType)*;
            }
            else
            {
                static assert(DataIsImmutable, "An immutable container cannot reference const or mutable data");
                static if (PointerIsConst)
                    alias ET = immutable(PointerTarget!ElementType)*;
                else
                    alias ET = ElementType;
            }
        }
        else
        {
            static if (isConst)
            {
                static if (is(ElementType == immutable))
                    alias ET = ElementType;
                else
                    alias ET = const(Unqual!ElementType);
            }
            else
                alias ET = immutable(Unqual!ElementType);
        }
    }

    static if (isMutable!ContainerType)
        alias ContainerElementType = ElementType;
    else
    {
        static if (hasIndirections!ElementType)
            alias ContainerElementType = ET!(is(ContainerType == const), ElementType);
        else
            alias ContainerElementType = ElementType;
    }
}

template shouldAddGCRange(T)
{
    import std.traits : isPointer, hasIndirections;
    enum shouldAddGCRange = isPointer!T || hasIndirections!T || is (T == class);
}
