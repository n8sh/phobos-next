#!/usr/bin/env rdmd-dev-module

/** ELF Symbol Name (De)Mangling.
    Copyright: Per Nordlöw 2017-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
    See also: https://mentorembedded.github.io/cxx-abi/abi.html

    TODO Only check for emptyness before any optionals.

    TODO Search for pattern "X> <Y" and assure that they all use
    return r.tryEvery(X, Y).

    TODO 1. Replace calls to decode ~ decode with separate decodes
    TODO 2 : Replace calls to decode ~ decode with a sequence call.

    TODO Detect recursion:
          See: http://forum.dlang.org/thread/edaduxaxmihvzkoudeqa@forum.dlang.org#post-edaduxaxmihvzkoudeqa:40forum.dlang.org
          See: http://code.dlang.org/packages/backtrace-d

    TODO What role does _ZL have? See localFlag for details.
 */
module mangling;

import std.range: empty, popFront, popFrontExactly, take, drop, front, takeOne, moveFront, repeat, replicate, isInputRange;
import std.algorithm: startsWith, findSplitAfter, skipOver, joiner, min;
import std.typecons: tuple, Tuple;
import std.conv: to;
import std.ascii: isDigit;
import std.array: array;
import std.stdio;
import std.functional : unaryFun, binaryFun;

import std.algorithm.comparison : either;
import algorithm_ex: tryEvery, split, splitBefore, findPopBefore, findPopAfter;
import lingua;
import languages : Lang;
import dbgio : dln;

/** C++ Demangler. */
class Demangler(R)
    if (isInputRange!R)
{
    this(R r,
         bool explicitVoidParameter = false,
         bool show = false)
    {
        this.r = r;
        this.explicitVoidParameter = explicitVoidParameter;
        this.show = show;
    }
    R r;
    bool show = false;
    bool explicitVoidParameter = false; // set to true make void parameters explicit
private:
    string[] sourceNames;
    CxxType[] ids; // ids demangled so far
    R scopeSeparator = "::";
}
auto demangler(T...)(T args)
    if (isInputRange!(T[0]))
{
    return new Demangler!(T[0])(args);
}

/** Like $(D skipOver) but return $(D string) instead of $(D bool).
    Bool-conversion of returned value gives same result as r.skipOver(lit).
*/
string skipLiteral(R, E)(Demangler!R x, E lit)
    if (isInputRange!R)
{
    return x.r.skipOver(lit) ? "" : null;
}

/** Decode Unqualified C++ Type at $(D r).
    See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangling-type
*/
R decodeCxxUnqualifiedType(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    return either(x.decodeCxxBuiltinType(),
                  x.decodeCxxSubstitution(),
                  x.decodeCxxFunctionType());
}

struct CxxType
{
    string typeName;
    bool isRef = false;      // & <ref-qualifier>
    bool isRvalueRef = false;    // && ref-qualifier (C++11)
    bool isComplexPair = false;    // complex pair (C 2000)
    bool isImaginary = false;    // imaginary (C 2000)
    byte pointyness = 0;           // pointer level
    CXXCVQualifiers cvQ;

    @property void toString(scope void delegate(const(char)[]) sink) const
    {
        if (cvQ.isVolatile) { sink("volatile "); }
        if (cvQ.isRestrict) { sink("restrict "); }

        sink(typeName);

        if (cvQ.isConst) { sink(" const"); }

        // suffix qualifiers
        foreach (immutable _; 0 .. pointyness)
        {
            sink(`*`);// str ~= "*".replicate(pointyness);
        }
        if (isRef) { sink(`&`); }
        if (isRvalueRef) { sink(`&&`); }
    }
}

/** Decode C++ Type at $(D r).
    See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangling-type
*/
R decodeCxxType(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);

    const packExpansion = x.r.skipOver(`Dp`); // (C++11)

    CxxType cxxType;

    while (!x.r.empty)
    {
        if (const cvQ_ = x.r.decodeCxxCVQualifiers()) // TODO Optimize
        {
            cxxType.cvQ.isRestrict |= cvQ_.isRestrict;
            cxxType.cvQ.isVolatile |= cvQ_.isVolatile;
            cxxType.cvQ.isConst |= cvQ_.isConst;
            continue;
        }
        else
        {
            auto miss = false;
            switch (x.r[0])
            {
                case 'P': x.r.popFront(); cxxType.pointyness++; break;
                    // <ref-qualifier>: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.ref-qualifier
                case 'R': x.r.popFront(); cxxType.isRef = true; break;
                case 'O': x.r.popFront(); cxxType.isRvalueRef = true; break;
                case 'C': x.r.popFront(); cxxType.isComplexPair = true; dln("TODO Handle complex pair (C 2000)"); break;
                case 'G': x.r.popFront(); cxxType.isImaginary = true; dln("TODO Handle imaginary (C 2000)"); break;
                case 'U': x.r.popFront();
                    const sourceName = x.decodeCxxSourceName();
                    cxxType.typeName = sourceName ~ x.decodeCxxType();
                    dln("TODO Handle vendor extended type qualifier <source-name>", x.r);
                    break;
                default: miss = true; break;
            }
            if (miss)
            {
                break;
            }
        }
    }
    assert(!(cxxType.isRef && cxxType.isRvalueRef));

    if (x.r.empty) { return cxxType.to!string; }

    cxxType.typeName = either(x.decodeCxxBuiltinType(),
                              x.decodeCxxFunctionType(),
                              x.decodeCxxClassEnumType(),
                              x.decodeCxxArrayType(),
                              x.decodeCxxPointerToMemberType(),
                              x.decodeCxxTemplateTemplateParamAndArgs(),
                              x.decodeCxxDecltype(),
                              x.decodeCxxSubstitution());

    x.ids ~= cxxType;

    return cxxType.to!string;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.class-enum-type */
R decodeCxxClassEnumType(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R type;
    R prefix;
    enum n = 2;
    if (x.r.length >= n)
    {
        switch (x.r[0..n])
        {
            case `Ts`: prefix = `struct `; break;
            case `Tu`: prefix = `union `; break;
            case `Te`: prefix = `enum `; break;
            default: break;
        }
        if (prefix)
        {
            x.r.popFrontExactly(n);
        }
    }
    const name = x.decodeCxxName();
    if (name)
    {
        type = prefix ~ name;
    }
    else
    {
        assert(!prefix); // if we failed to decode name prefix should not have existed either
    }
    return type;
}

R decodeCxxExpression(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R exp;
    assert(false, "TODO");
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.array-type */
R decodeCxxArrayType(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R type;
    if (x.r.skipOver('A'))
    {
        if (const num = x.decodeCxxNumber())
        {
            assert(x.r.skipOver('_'));
            type = x.decodeCxxType() ~ `[]` ~ num ~ `[]`;
        }
        else
        {
            const dimensionExpression = x.decodeCxxExpression();
            assert(x.r.skipOver('_'));
            type = x.decodeCxxType() ~ `[]` ~ dimensionExpression ~ `[]`;
        }
    }
    return type;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.pointer-to-member-type */
R decodeCxxPointerToMemberType(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R type;
    if (x.r.skipOver('M'))
    {
        const classType = x.decodeCxxType(); // <class type>
        const memberType = x.decodeCxxType(); // <mmeber type>
        type = classType ~ memberType;
    }
    return type;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.template-param */
R decodeCxxTemplateParam(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R param;
    if (x.r.skipOver('T'))
    {
        if (x.r.skipOver('_'))
        {
            param = `first template parameter`;
        }
        else
        {
            param = x.decodeCxxNumber();
            assert(x.r.skipOver('_'));
        }
    }
    return param;
}

R decodeCxxTemplateTemplateParamAndArgs(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R value;
    if (const param = either(x.decodeCxxTemplateParam(),
                             x.decodeCxxSubstitution()))
    {
        auto args = x.decodeCxxTemplateArgs();
        value = param ~ args.joiner(`, `).to!R;
    }
    return value;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.decltype */
R decodeCxxDecltype(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R type;
    if (x.r.skipOver(`Dt`) ||
        x.r.skipOver(`DT`))
    {
        type = x.decodeCxxExpression();
        assert(x.r.skipOver('E'));
    }
    return type;
}

R decodeCxxDigit(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    auto digit = x.r[0..1];
    x.r.popFront();
    return digit;
}

/** Try to Decode C++ Operator at $(D r).
    See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangling-operator
*/
R decodeCxxOperatorName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);

    if (x.r.skipOver('v'))     // vendor extended operator
    {
        const digit = x.decodeCxxDigit();
        const sourceName = x.decodeCxxSourceName();
        return digit ~ sourceName;
    }

    R op;
    enum n = 2;
    if (x.r.length < n) { return typeof(return).init; }
    const code = x.r[0..n];
    switch (code)
    {
        case `nw`: op = `operator new`; break;
        case `na`: op = `operator new[]`; break;
        case `dl`: op = `operator delete`; break;
        case `da`: op = `operator delete[]`; break;
        case `ps`: op = `operator+`; break; // unary plus
        case `ng`: op = `operator-`; break; // unary minus

        case `ad`: op = `operator&`; break; // address of
        case `de`: op = `operator*`; break; // dereference

        case `co`: op = `operator~`; break; // bitwise complement
        case `pl`: op = `operator+`; break; // plus
        case `mi`: op = `operator-`; break; // minus

        case `ml`: op = `operator*`; break; // multiplication
        case `dv`: op = `operator/`; break; // division
        case `rm`: op = `operator%`; break; // remainder

        case `an`: op = `operator&`; break; // bitwise and
        case `or`: op = `operator|`; break; // bitwise of

        case `eo`: op = `operator^`; break;
        case `aS`: op = `operator=`; break;

        case `pL`: op = `operator+=`; break;
        case `mI`: op = `operator-=`; break;
        case `mL`: op = `operator*=`; break;
        case `dV`: op = `operator/=`; break;
        case `rM`: op = `operator%=`; break;

        case `aN`: op = `operator&=`; break;
        case `oR`: op = `operator|=`; break;
        case `eO`: op = `operator^=`; break;

        case `ls`: op = `operator<<`; break;
        case `rs`: op = `operator>>`; break;
        case `lS`: op = `operator<<=`; break;
        case `rS`: op = `operator>>=`; break;

        case `eq`: op = `operator==`; break;
        case `ne`: op = `operator!=`; break;
        case `lt`: op = `operator<`; break;
        case `gt`: op = `operator>`; break;
        case `le`: op = `operator<=`; break;
        case `ge`: op = `operator>=`; break;

        case `nt`: op = `operator!`; break;
        case `aa`: op = `operator&&`; break;
        case `oo`: op = `operator||`; break;

        case `pp`: op = `operator++`; break; // (postfix in <expression> context)
        case `mm`: op = `operator--`; break; // (postfix in <expression> context)

        case `cm`: op = `operator,`; break;

        case `pm`: op = `operator->*`; break;
        case `pt`: op = `operator->`; break;

        case `cl`: op = `operator()`; break;
        case `ix`: op = `operator[]`; break;
        case `qu`: op = `operator?`; break;
        case `cv`: op = `(cast)`; break;
        case `li`: op = `operator""`; break;
        default: break;
    }

    if (op)
    {
        x.r.popFrontExactly(n); // digest it
    }

    switch (code)
    {
        case `cv`: op = '(' ~ x.decodeCxxType() ~ ')'; break;
        case `li`: op = (`operator ""` ~ x.decodeCxxSourceName()); break;
        default: break;
    }

    return op;
}

/** Try to Decode C++ Builtin Type at $(D r).
    See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.builtin-type
*/
R decodeCxxBuiltinType(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R type;
    enum n = 1;
    if (x.r.length < n) { return type; }
    switch (x.r[0])
    {
        case 'v': x.r.popFront(); type = `void`; break;
        case 'w': x.r.popFront(); type = `wchar_t`; break;

        case 'b': x.r.popFront(); type = `bool`; break;

        case 'c': x.r.popFront(); type = `char`; break;
        case 'a': x.r.popFront(); type = `signed char`; break;
        case 'h': x.r.popFront(); type = `unsigned char`; break;

        case 's': x.r.popFront(); type = `short`; break;
        case 't': x.r.popFront(); type = `unsigned short`; break;

        case 'i': x.r.popFront(); type = `int`; break;
        case 'j': x.r.popFront(); type = `unsigned int`; break;

        case 'l': x.r.popFront(); type = `long`; break;
        case 'm': x.r.popFront(); type = `unsigned long`; break;

        case 'x': x.r.popFront(); type = `long long`; break;  // __int64
        case 'y': x.r.popFront(); type = `unsigned long long`; break; // __int64

        case 'n': x.r.popFront(); type = `__int128`; break;
        case 'o': x.r.popFront(); type = `unsigned __int128`; break;

        case 'f': x.r.popFront(); type = `float`; break;
        case 'd': x.r.popFront(); type = `double`; break;
        case 'e': x.r.popFront(); type = `long double`; break; // __float80
        case 'g': x.r.popFront(); type = `__float128`; break;

        case 'z': x.r.popFront(); type = `...`; break; // ellipsis

        case 'D':
            x.r.popFront();
            assert(!x.r.empty); // need one more
            switch (x.r[0])
            {
                case 'd': x.r.popFront(); type = `IEEE 754r decimal floating point (64 bits)`; break;
                case 'e': x.r.popFront(); type = `IEEE 754r decimal floating point (128 bits)`; break;
                case 'f': x.r.popFront(); type = `IEEE 754r decimal floating point (32 bits)`; break;
                case 'h': x.r.popFront(); type = `IEEE 754r half-precision floating point (16 bits)`; break;
                case 'i': x.r.popFront(); type = `char32_t`; break;
                case 's': x.r.popFront(); type = `char16_t`; break;
                case 'a': x.r.popFront(); type = `auto`; break;
                case 'c': x.r.popFront(); type = `decltype(auto)`; break;
                case 'n': x.r.popFront(); type = `std::nullptr_t`; break; // (i.e., decltype(nullptr))
                default: dln(`TODO Handle `, x.r);
            }
            break;

            /* TODO */
            /* ::= u <source-name>	# vendor extended type */

        default:
            break;
    }

    return type;
}

/** Decode C++ Substitution Type at $(D r).
    See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.substitution
*/
R decodeCxxSubstitution(R)(Demangler!R x, R stdPrefix = `::std::`)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R type;
    if (x.r.skipOver('S'))
    {
        if (x.r.front == '_') // See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.seq-id
        {
            type = x.ids[0].to!R;
            x.r.popFront();
        }
        else if ('0' <= x.r.front && x.r.front <= '9') // See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.seq-id
        {
            const ix = (x.r.front - '0');
            auto ids_ = x.ids[min(x.ids.length - 1, ix + 1)]; // TODO Use of min here is hacky. Investigate.
            if (ix == 0)
            {
                /* NOTE: Undocumented: decrease pointyness.
                   See for example: parse_arch(size_t argc, const char** argv, const char* arch)
                   in dmd/src/mars.c
                */
                ids_.pointyness = ids_.pointyness >= 1 ? cast(byte)(ids_.pointyness - 1): 0;
            }
            type = ids_.to!R;
            x.r.popFront();
            x.r.skipOver('_'); // TODO Relaxed this to optional by removing surrounding assert. Investigate.
        }
        else if ('A' <= x.r.front && x.r.front <= 'Z') // See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.seq-id
        {
            const ix = (x.r.front - 'A' + 11);
            type = x.ids[ix].to!R;
            x.r.popFront();
            assert(x.r.skipOver('_'));
        }
        else
        {
            type = stdPrefix;
            switch (x.r.front)
            {
                case 't': x.r.popFront(); type ~= `ostream`; break;
                case 'a': x.r.popFront(); type ~= `allocator`; break;
                case 'b': x.r.popFront(); type ~= `basic_string`; break;
                case 's': x.r.popFront(); type ~= `basic_string<char, std::char_traits<char>, std::allocator<char> >`; break;
                case 'i': x.r.popFront(); type ~= `istream`; break;
                case 'o': x.r.popFront(); type ~= `ostream`; break;
                case 'd': x.r.popFront(); type ~= `iostream`; break;

                default:
                    dln(`Cannot handle C++ standard prefix character: '`, x.r.front, `'`);
                    x.r.popFront();
                    break;
            }
        }
    }
    return type;
}

/** Try to Decode C++ Function Type at $(D r).
    See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.function-type
*/
R decodeCxxFunctionType(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    auto restLookAhead = x.r; // needed for lookahead parsing of CV-qualifiers
    const cvQ = restLookAhead.decodeCxxCVQualifiers();
    R type;
    if (restLookAhead.skipOver('F'))
    {
        x.r = restLookAhead; // we have found it
        x.r.skipOver('Y'); // optional
        type = x.decodeCxxBareFunctionType().to!R;
        const refQ = x.decodeCxxRefQualifier();
        type ~= refQ.toCxxString;

    }
    return type;
}

struct CxxBareFunctionType(R)
    if (isInputRange!R)
{
    R[] types; // optional return and parameter types
    bool explicitVoidParameter = false; // set to true make void parameters explicit
    R toString()
        @safe pure
    {
        R value;
        if (!types.empty)
        {
            value ~= `(`;
            if (this.explicitVoidParameter ||
                !(types.length == 1 && types.front == "void"))
            {
                value ~= types.joiner(`, `).to!R;
            }
            value ~= `)`;
        }
        return value;
    }
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.bare-function-type */
CxxBareFunctionType!R decodeCxxBareFunctionType(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    typeof(return) bareFunctionType;
    bareFunctionType.explicitVoidParameter = x.explicitVoidParameter;

    /* TODO This behaviour may not follow grammar. */
    if (const firstType = x.decodeCxxType())
    {
        bareFunctionType.types ~= firstType;
    }

    while (!x.r.empty)
    {
        auto type = x.decodeCxxType();
        if (type)
        {
            bareFunctionType.types ~= type;
        }
        else
        {
            break;
        }
    }

    return bareFunctionType;
}

struct CXXCVQualifiers
{
    bool isRestrict; // (C99)
    bool isVolatile; // volatile
    bool isConst; // const

    auto opCast(T : bool)()
        @safe pure nothrow const
    {
        return (isRestrict ||
                isVolatile ||
                isConst);
    }

    string toString()
        @safe pure nothrow const
    {
        typeof(return) value;
        if (isRestrict) value ~= `restrict `;
        if (isVolatile) value ~= `volatile `;
        if (isConst)    value ~= `const `;
        return value;
    }
}

/** Decode <CV-qualifiers>
    See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.CV-qualifiers
*/
CXXCVQualifiers decodeCxxCVQualifiers(R)(ref R r)
    if (isInputRange!R)
{
    typeof(return) cvQ;
    if (r.skipOver('r')) { cvQ.isRestrict = true; }
    if (r.skipOver('V')) { cvQ.isVolatile = true; }
    if (r.skipOver('K')) { cvQ.isConst    = true; }
    return cvQ;
}

enum CxxRefQualifier
{
    none,
    normalRef,
    rvalueRef
}

/* See also: http://forum.dlang.org/thread/cvhapzsrhjdnpkdspavg@forum.dlang.org#post-cvhapzsrhjdnpkdspavg:40forum.dlang.org */
string toCxxString(CxxRefQualifier refQ)
    @safe pure nothrow
{
    final switch (refQ)
    {
        case CxxRefQualifier.none: return "";
        case CxxRefQualifier.normalRef: return "&";
        case CxxRefQualifier.rvalueRef: return "&&";
    }
}

/** Decode <ref-qualifier>
    See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.ref-qualifier
*/
CxxRefQualifier decodeCxxRefQualifier(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    if (x.r.skipOver('R'))
    {
        return CxxRefQualifier.normalRef;
    }
    else if (x.r.skipOver('O'))
    {
        return CxxRefQualifier.rvalueRef;
    }
    else
    {
        return CxxRefQualifier.none;
    }
}

/** Decode Identifier <source-name>.
    See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.source-name
*/
R decodeCxxSourceName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R id;
    const sign = x.r.skipOver('n'); // if negative number
    assert(!sign);
    const match = x.r.splitBefore!(a => !a.isDigit);
    const digits = match[0];
    x.r = match[1];
    if (!digits.empty)     // digit prefix
    {
        // TODO Functionize these three lines
        const num = digits.to!uint;
        id = x.r[0..num]; // identifier, x.r.take(num)
        x.r = x.r[num..$]; // x.r.drop(num);
        x.sourceNames ~= id;
    }
    return id;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.nested-name
   Note: Second alternative
   <template-prefix> <template-args>
   in
   <nested-name>
   is redundant as it is included in <prefix> and is skipped here.
 */
R decodeCxxNestedName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    if (x.r.skipOver('N')) // nested name: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.nested-name
    {
        const cvQ = x.r.decodeCxxCVQualifiers();
        const refQ = x.decodeCxxRefQualifier();
        const prefix = x.decodeCxxPrefix();
        const name = x.decodeCxxUnqualifiedName();
        assert(x.r.skipOver('E'));
        auto ret = (cvQ.to!R ~
                    prefix ~
                    name ~
                    refQ.toCxxString);
        return ret;
    }
    return null;
}

/** TODO Use this
    See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.ctor-dtor-name
 */
enum CtorDtorName
{
    completeObjectConstructor,
    baseObjectConstructor,
    completeObjectAllocatingConstructor,
    deletingDestructor,
    completeObjectDestructor,
    baseObjectDestructor
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.ctor-dtor-name */
R decodeCxxCtorDtorName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R name;
    enum n = 2;
    if (x.r.length < n) { return typeof(return).init; }
    import std.array: back;
    switch (x.r[0..n])
    {
        case `C1`: name = x.sourceNames.back; break; // complete object constructor
        case `C2`: name = x.sourceNames.back; break; // base object constructor
        case `C3`: name = x.sourceNames.back; break; // complete object allocating constructor
        case `D0`: name = '~' ~ x.sourceNames.back; break; // deleting destructor
        case `D1`: name = '~' ~ x.sourceNames.back; break; // complete object destructor
        case `D2`: name = '~' ~ x.sourceNames.back; break; // base object destructor
        default: break;
    }
    if (name)
    {
        x.r.popFrontExactly(n);
    }
    return name;
}

/** https://mentorembedded.github.io/cxx-abi/abi.html#mangle.unqualified-name */
R decodeCxxUnqualifiedName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    return either(x.decodeCxxOperatorName(),
                  x.decodeCxxSourceName(),
                  x.decodeCxxCtorDtorName(),
                  x.decodeCxxUnnamedTypeName());
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.unnamed-type-name */
R decodeCxxUnnamedTypeName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R type;
    if (x.r.skipOver(`Ut`))
    {
        type = x.decodeCxxNumber();
        assert(x.r.skipOver('_'));
    }
    return type;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.template-prefix
 */
R decodeCxxTemplatePrefix(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    // NOTE: Removed <prefix> because of recursion
    return either(x.decodeCxxUnqualifiedName(),
                  x.decodeCxxTemplateParam(),
                  x.decodeCxxSubstitution());
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.template-args */
R[] decodeCxxTemplateArgs(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    typeof(return) args;
    if (x.r.skipOver('I'))
    {
        args ~= x.decodeCxxTemplateArg();
        while (!x.r.empty)
        {
            auto arg = x.decodeCxxTemplateArg();
            if (arg)
            {
                args ~= arg;
            }
            else
            {
                break;
            }
        }
        assert(x.r.skipOver('E'));
    }
    return args;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.mangled-name */
R decodeCxxMangledName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R name;
    if (x.r.skipOver(`_Z`))
    {
        return x.decodeCxxEncoding();
    }
    return name;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.expr-primary */
R decodeCxxExprPrimary(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R expr;
    if (x.r.skipOver('L'))
    {
        expr = x.decodeCxxMangledName();
        if (!expr)
        {
            auto number = x.decodeCxxNumber();
            // TODO Howto demangle <float>?
            // TODO Howto demangle <float> _ <float> E
            expr = x.decodeCxxType(); // <R>, <nullptr>, <pointer> type
            bool pointerType = x.r.skipOver('0'); // null pointer template argument
        }
        assert(x.r.skipOver('E'));
    }
    return expr;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.template-arg */
R decodeCxxTemplateArg(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R arg;
    if (x.r.skipOver('X'))
    {
        arg = x.decodeCxxExpression();
        assert(x.r.skipOver('E'));
    }
    else if (x.r.skipOver('J'))
    {
        R[] args;
        while (!x.r.empty)
        {
            const subArg = x.decodeCxxTemplateArg();
            if (subArg)
            {
                args ~= subArg;
            }
            else
            {
                break;
            }
        }
        arg = args.joiner(`, `).to!R;
        assert(x.r.skipOver('E'));
    }
    else
    {
        arg = either(x.decodeCxxExprPrimary(),
                     x.decodeCxxType());
    }
    return arg;
}

R decodeCxxTemplatePrefixAndArgs(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    auto restBackup = x.r;
    if (const prefix = x.decodeCxxTemplatePrefix())
    {
        auto args = x.decodeCxxTemplateArgs();
        if (args)
        {
            return prefix ~ args.joiner(`, `).to!R;
        }
    }
    x.r = restBackup; // restore upon failure
    return typeof(return).init;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.prefix */
R decodeCxxPrefix(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    typeof(return) prefix;
    for (size_t i = 0; !x.r.empty; ++i) // NOTE: Turned self-recursion into iteration
    {
        if (const name = x.decodeCxxUnqualifiedName())
        {
            if (i >= 1)
            {
                prefix ~= x.scopeSeparator;
            }
            prefix ~= name;
            continue;
        }
        else if (const name = x.decodeCxxTemplatePrefixAndArgs())
        {
            prefix ~= name;
            continue;
        }
        else if (const templateParam = x.decodeCxxTemplateParam())
        {
            prefix ~= templateParam;
            continue;
        }
        else if (const decltype = x.decodeCxxDecltype())
        {
            prefix ~= decltype;
            continue;
        }
        else if (const subst = x.decodeCxxSubstitution())
        {
            prefix ~= subst;
            continue;
        }
        else
        {
            break;
        }
    }
    return prefix;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.unscoped-name */
R decodeCxxUnscopedName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    auto restBackup = x.r;
    const prefix = x.r.skipOver(`St`) ? "::std::" : null;
    if (const name = x.decodeCxxUnqualifiedName())
    {
        return prefix ~ name;
    }
    else
    {
        x.r = restBackup; // restore
        return typeof(return).init;
    }
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.unscoped-template-name */
R decodeCxxUnscopedTemplateName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    return either(x.decodeCxxSubstitution(), // faster backtracking with substitution
                  x.decodeCxxUnscopedName());
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.unscoped-template-name */
R decodeCxxUnscopedTemplateNameAndArgs(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R nameAndArgs;
    if (const name = x.decodeCxxUnscopedTemplateName())
    {
        nameAndArgs = name;
        if (auto args = x.decodeCxxTemplateArgs())
        {
            nameAndArgs ~= args.joiner(`, `).to!R;
        }
    }
    return nameAndArgs;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.number */
R decodeCxxNumber(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R number;
    const prefix = x.r.skipOver('n'); // optional prefix
    auto split = x.r.splitBefore!(a => !a.isDigit());
    if (prefix || !split[0].empty) // if complete match
    {
        x.r = split[1];
        number = split[0];
    }
    return number;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.discriminator */
R decodeCxxDescriminator(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    R descriminator;
    if (x.r.skipOver('_'))
    {
        if (x.r.skipOver('_'))            // number >= 10
        {
            descriminator = x.decodeCxxNumber();
            assert(x.r.skipOver('_')); // suffix
        }
        else                    // number < 10
        {
            x.r.skipOver('n'); // optional prefix
            /* TODO Merge these two into a variant of popFront() that returns
             the popped element. What is best out of:
             - General: x.r.takeOne().to!R
             - Arrays only: r[0..1]
             - Needs cast: x.r.front
             and are we in need of a combined variant of front() and popFront()
             say takeFront() that may fail and requires a cast.
             */
            /* descriminator = r[0..1]; // single digit */
            /* x.r.popFront(); */
            descriminator = x.r.moveFront().to!R;
        }
    }
    return descriminator;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.local-name */
R decodeCxxLocalName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    if (x.r.skipOver('Z'))
    {
        const functionEncoding = x.decodeCxxEncoding();
        x.r.skipOver('E');
        if (x.r.skipOver('D'))
        {
            assert(false, "TODO Decode C++0x Closure Type (lambda)"); // see https://mentorembedded.github.io/cxx-abi/abi.html#closure-types
        }
        else
        {
            const entityNameMaybe = either(x.skipLiteral('s'), // NOTE: Literal first to speed up
                                           x.decodeCxxName());
            const discriminator = x.decodeCxxDescriminator(); // optional
            return (functionEncoding ~
                    x.scopeSeparator ~
                    entityNameMaybe ~
                    discriminator.to!R); // TODO Optional
        }
    }
    return R.init;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.name */
R decodeCxxName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    return either(x.decodeCxxNestedName(),
                  x.decodeCxxUnscopedName(),
                  x.decodeCxxLocalName(), // TODO order flipped
                  x.decodeCxxUnscopedTemplateNameAndArgs()); // NOTE: order flipped
}

R decodeCxxNVOffset(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    return x.decodeCxxNumber();
}

R decodeCxxVOffset(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    auto offset = x.decodeCxxNumber();
    assert(x.r.skipOver('_'));
    return offset ~ x.decodeCxxNumber();
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.call-offset */
R decodeCxxCallOffset(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    typeof(return) offset;
    if (x.r.skipOver('h'))
    {
        offset = x.decodeCxxNVOffset();
        assert(x.r.skipOver('_'));
    }
    else if (x.r.skipOver('v'))
    {
        offset = x.decodeCxxVOffset();
        assert(x.r.skipOver('_'));
    }
    return offset;
}

/** See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.special-name */
R decodeCxxSpecialName(R)(Demangler!R x)
    if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    auto restBackup = x.r;
    typeof(return) name;
    if (x.r.skipOver('S'))
    {
        switch (x.r.moveFront)
        {
            case 'V': name = "virtual table: "; break;
            case 'T': name = "VTT structure: "; break;
            case 'I': name = "typeinfo structure: "; break;
            case 'S': name = "typeinfo name (null-terminated byte R): "; break;
            default:
                x.r = restBackup; // restore
                return name;
        }
        name ~= x.decodeCxxType();
    }
    else if (x.r.skipOver(`GV`))
    {
        name = x.decodeCxxName();
    }
    else if (x.r.skipOver('T'))
    {
        if (x.r.skipOver('c'))
        {
            name = x.r.tryEvery(x.decodeCxxCallOffset(),
                                x.decodeCxxCallOffset(),
                                x.decodeCxxEncoding()).joiner(` `).to!R;
        }
        else
        {
            name = x.r.tryEvery(x.decodeCxxCallOffset(),
                                x.decodeCxxEncoding()).joiner(` `).to!R;
        }
    }
    return name;
}

/* Decode C++ Symbol.
   See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.encoding
 */
R decodeCxxEncoding(R)(Demangler!R x) /* @safe pure nothrow @nogc */ if (isInputRange!R)
{
    if (x.show) dln("rest: ", x.r);
    const localFlag = x.r.skipOver('L'); // TODO What role does the L have in symbols starting with _ZL have?
    if (const name = x.decodeCxxSpecialName())
    {
        return name;
    }
    else
    {
        const name = x.decodeCxxName();
        auto type = x.decodeCxxBareFunctionType();
        return name ~ type.to!R;
    }
}

/** Demangled Expression. */
alias Expr = string;

struct Demangling
{
    Lang language;
    Expr unmangled;
    auto opCast(T : bool)()
        @safe pure nothrow const
    {
        return !expr.empty;
    }
}

/** Demangle Symbol $(D r) and Detect Language.
    See also: https://en.wikipedia.org/wiki/Name_mangling
    See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangling
    See also: https://gcc.gnu.org/onlinedocs/libstdc++/manual/ext_demangling.html
*/
Demangling decodeSymbol(R)(Demangler!R x) /* @safe pure nothrow @nogc */
    if (isInputRange!R)
{
    if (x.r.empty)
    {
        return Demangling(Lang.init, x.r);
    }

    if (!x.r.startsWith('_'))
    {
        return Demangling(Lang.c, x.r); // assume C
    }

    // See also: https://mentorembedded.github.io/cxx-abi/abi.html#mangle.mangled-name
    if (x.r.skipOver(`_Z`))
    {
        return Demangling(Lang.cxx,
                          x.decodeCxxEncoding());
    }
    else
    {
        import core.demangle: demangle;
        const symAsD = x.r.demangle;
        import std.conv: to;
        if (symAsD != x.r) // TODO Why doesn't (symAsD is r) work here?
            return Demangling(Lang.d, symAsD.to!R);
        else
            return Demangling(Lang.init, x.r);
    }
}

unittest
{
    import assert_ex;

    assertEqual(demangler(`memcpy`).decodeSymbol(),
                Demangling(Lang.c, `memcpy`));

    assertEqual(demangler(`_Z1hi`).decodeSymbol(),
                Demangling(Lang.cxx, `h(int)`));

    assertEqual(demangler(`_Z3foo3bar`).decodeSymbol(),
                Demangling(Lang.cxx, `foo(bar)`));

    assertEqual(demangler(`_ZN1N1fE`).decodeSymbol(),
                Demangling(Lang.cxx, `N::f`));

    assertEqual(demangler(`_ZN3Foo3BarEv`).decodeSymbol(),
                Demangling(Lang.cxx, `Foo::Bar()`));

    assertEqual(demangler(`_ZN3FooC1Ev`).decodeSymbol(),
                Demangling(Lang.cxx, `Foo::Foo()`));

    assertEqual(demangler(`_ZN9wikipedia7article6formatE`).decodeSymbol(),
                Demangling(Lang.cxx, `wikipedia::article::format`));

    assertEqual(demangler(`_ZSt5state`).decodeSymbol(),
                Demangling(Lang.cxx, `::std::state`));

    assertEqual(demangler(`_ZN9wikipedia7article8print_toERSo`).decodeSymbol(),
                Demangling(Lang.cxx, `wikipedia::article::print_to(::std::ostream&)`));

    assertEqual(demangler(`_ZN9wikipedia7article8print_toEOSo`).decodeSymbol(),
                Demangling(Lang.cxx, `wikipedia::article::print_to(::std::ostream&&)`));

    assertEqual(demangler(`_ZN9wikipedia7article6formatEv`, true).decodeSymbol(),
                Demangling(Lang.cxx, `wikipedia::article::format(void)`));

    assertEqual(demangler(`_ZN9wikipedia7article6formatEv`, false).decodeSymbol(),
                Demangling(Lang.cxx, `wikipedia::article::format()`));

    assertEqual(demangler(`_ZL8next_argRPPc`).decodeSymbol(),
                Demangling(Lang.cxx, `next_arg(char**&)`));

    assertEqual(demangler(`_ZL10parse_archmPPKcS0_`, true).decodeSymbol(),
                Demangling(Lang.cxx, `parse_arch(unsigned long, char const**, char const*)`));

    assertEqual(demangler(`_ZN5LexerC2EP6ModulePKhmmii`, true).decodeSymbol(),
                Demangling(Lang.cxx, `Lexer::Lexer(Module*, unsigned char const*, unsigned long, unsigned long, int, int)`));

    assertEqual(demangler(`_Zrm1XS_`).decodeSymbol(),
                Demangling(Lang.cxx, `operator%(X, X)`));

    assertEqual(demangler(`_ZZL8next_argRPPcE4keys`).decodeSymbol(),
                Demangling(Lang.cxx, `next_arg(char**&)::keys`));

    assertEqual(demangler(`_ZN12ExpStatement9scopeCodeEP5ScopePP9StatementS4_S4`).decodeSymbol(),
                Demangling(Lang.cxx, `ExpStatement::scopeCode(Scope*, Statement**, Statement**, Statement**)`));

    assertEqual(demangler(`_ZZ8genCmainP5ScopeE9cmaincode`).decodeSymbol(),
                Demangling(Lang.cxx, `genCmain(Scope*)::cmaincode`));
}
