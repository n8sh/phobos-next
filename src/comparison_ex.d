module comparison_ex;

import std.traits : isExpressionTuple;
import traits_ex : haveCommonType;

/** Similar to $(D among) but for set of replacements/substitutions $(D substs).
    Time-Complexity: O(1) thanks to D's $(D switch) guaranteeing hash-table matching.
 */
template substSwitch(substs...)
    if ((substs.length & 1) == 0 && // need even number of elements (>= 1)
        substs.length >= 2 && // and at least one replacement pair
        isExpressionTuple!substs &&
        haveCommonType!(substs))
{
    Value substSwitch(Value)(Value value)
        if (haveCommonType!(Value, substs)) // TODO need static map incorrect
    {
        switch (value)
        {
            import range_ex : iota;
            foreach (const i; iota!(0, substs.length / 2))
            {
            case substs[2*i]:
                return substs[2*i + 1];
            }
        default:
            return value;
        }
    }
}
alias replacementSwitch = substSwitch;

@safe pure nothrow unittest
{
    auto xyz_abc__(T)(T value)
    {
        immutable a = "a";
        const b = "b";
        auto c = "c";
        return value.substSwitch!("x", a,
                                  "y", b,
                                  "z", c);
    }
    assert(xyz_abc__("x") == "a");
    assert(xyz_abc__("y") == "b");
    assert(xyz_abc__("z") == "c");
    assert(xyz_abc__("w") == "w");
}
