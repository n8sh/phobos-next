enum bool isIntegral(T) = IntegralTypeOf!T && !isAggregateType!T;
enum isAggregateType(T) = true;
alias IntegralTypeOf(T) = func!T;

int func(T)()
{
    static assert(0);
}

pragma(msg, isIntegral!int);
