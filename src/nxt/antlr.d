/** Lexer and parser for ANTLR (G,G2,G4) grammars.
 *
 * See_Also: https://theantlrguy.atlassian.net/wiki/spaces/ANTLR3/pages/2687036/ANTLR+Cheat+Sheet
 * See_Also: https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form
 * See_Also: https://github.com/antlr/grammars-v4
 * See_Also: https://github.com/antlr/grammars-v4/blob/master/bnf/bnf.g4
 * See_Also: https://stackoverflow.com/questions/53245751/convert-a-form-of-bnf-grammar-to-g4-grammar
 * See_Also: https://bnfc.digitalgrammars.com/
 *
 * TODO:
 *
 * - parse postfix operators *, +, ?
 * - add printing of set of rules
 * - create index of symbols and link them in second pass
 * - gorup things into a struct G4
 */
module nxt.antlr;

import std.array : Appender;
import std.stdio : writeln;

// `d-deps.el` requires these to be at the top:
import nxt.line_column : offsetLineColumn;
import nxt.file_ex : rawReadPath;

enum useKeywords = true;

alias Input = const(char)[];

///< Token kind. TODO: make this a string type like with std.experimental.lexer
enum TOK
{
    unknown,                    ///< Unknown.

    whitespace,

    // Keywords:
    SCOPE,          ///< Dynamically scoped attribute.
    FRAGMENT,       ///< Lexer rule is a helper rule, not real token for parser.
    LEXER,          ///< Grammar type.
    TREE,           ///< Grammar type.
    PARSER,         ///< Grammar type
    GRAMMAR,        ///< Grammar header.
    RETURNS,        ///< Rule return value(s).
    THROWS,         ///< Rule throws exceptions(s).
    CATCH,          ///< Catch rule exceptions(s).
    FINALLY,        ///< Do this no matter what.
    OPTIONS,        ///< Grammar or rule options.
    TOKENS,         ///< Can add tokens with this; usually imaginary tokens.
    IMPORT,         ///< Import grammar(s).
    CHANNELS,       ///< Channels.
    MODE,           ///< Mode.

    symbol,                     ///< Symbol.
    lexerRuleName,              ///< TODO: use instead of `symbol`
    parserRuleName,             ///< TODO: use instead of `symbol`
    attributeSymbol,            ///< Attribute Symbol (starting with `$`).
    actionSymbol,               ///< Action Symbol (starting with `@`).

    number,                     ///< Number.

    lineComment,                ///< Single line comment.
    blockComment,               ///< Multi-line (block) comment.

    leftParen,                  ///< Left parenthesis.
    rightParen,                 ///< Right parenthesis.

    action,                  ///< Code block.

    hooks,                       ///< Alternatives within '[' ... ']'

    textLiteralSingleQuoted,    ///< Text (string) literal, surrounded by single quotes.
    textLiteralDoubleQuoted,    ///< Text (string) literal, surrounded by double quotes.

    colon,                      ///< Colon `:`.
    semicolon,                  ///< Semicolon `;`.
    hash,                       ///< Hash `#`
    labelAssignment,            ///< Label assignment `=`
    listLabelAssignment,        ///< List label assignment `+=`

    optOrSemPred,               ///< Optional or semantic predicate (`?`)
    zeroOrMore,                 ///< Zero or more (`*`)
    oneOrMore,                  ///< One or more (`+`)
    alternative,                ///< Alternative (`|`)
    negation,                   ///< Match negation (`~`)
    lt,                         ///<  `<`
    gt,                         ///< `>`
    comma,                      ///< `.`
    exclude,                    ///< Exclude from AST (`!`)
    rootNode,                   ///< Root node (`^`)
    wildcard,                   ///< `.`
    dotdot,                     ///< `..`
    rewrite,                    ///< Rewrite rule (`->`)
    alwaysIncludePredicate,     ///< Rewrite rule (`=>`)

    _error,                     ///< Error token.
}

/// G4 rule.
struct Token
{
@safe pure nothrow @nogc:
    this(in TOK tok, in const(char)[] input = null)
    {
        this.tok = tok;
        this.input = input;
        // debug writeln("tok:", tok, " input:", input);
    }
    const(char)[] input;
    TOK tok;
}

static bool isSymbolStart(in dchar ch) pure nothrow @safe @nogc
{
    import std.uni : isAlpha;
    return (ch.isAlpha ||
            ch == '_' ||
            ch == '$' ||
            ch == '@');
}

/** G4 lexer.
 *
 * See_Also: `ANTLRv4Lexer.g4`
 */
struct G4Lexer
{
    import std.algorithm.comparison : among;

@safe pure:

    this(in Input input,
         in string path = null,
         in bool includeComments = false,
         in bool includeWhitespace = false) @trusted
    {
        _input = input;
        _path = path;

        import std.exception : enforce;
        import nxt.parsing : isNullTerminated;
        enforce(_input.isNullTerminated, "Input isn't null-terminated"); // input cannot be trusted

        _includeComments = includeComments;
        _includeWhitespace = includeWhitespace;

        nextFront();
    }

    @disable this(this);

    @property bool empty() const nothrow scope @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _endOfFile;
    }

    inout(Token) front() inout scope return nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        return _token;
    }

    void popFront() scope nothrow @trusted
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        nextFront();
        // debug writeln("after:", _token);
    }

    void popFrontEnforceTOK(in TOK tok, in string msg) nothrow
    {
        version(D_Coverage) {} else pragma(inline, true);
        const result = frontPop;
        if (result.tok != tok)
            errorAtFront(msg);  // TODO: print: expected token `tok`
    }

    Token frontPopEnforceTOK(in TOK tok, in string msg = "") nothrow
    {
        version(D_Coverage) {} else pragma(inline, true);
        auto result = frontPop;
        if (result.tok != tok)
            errorAtFront(msg);  // TODO: print: expected token `tok`
        return result;
    }

    Token frontPop() scope return nothrow
    {
        version(D_Coverage) {} else pragma(inline, true);
        auto result = front;
        popFront();
        return result;
    }

    import std.meta : AliasSeq;

    // from std.ascii.isWhite
    alias endOfLineChars = AliasSeq!('\n', // (0x0a)
                                     '\r', // (0x0c)
        );
    alias whiteChars = AliasSeq!(' ', // 0x20
                                 '\t', // (0x09)
                                 '\n', // (0x0a)
                                 '\v', // (0x0b)
                                 '\r', // (0x0c)
                                 '\f' // (0x0d)
        );

private:

    /// Peek next `char` in input.
    dchar peek0() const scope nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _input[_offset]; // TODO: decode `dchar`
    }

    /// Peek next next `char` in input.
    dchar peek1() const scope nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _input[_offset + 1]; // TODO: decode `dchar`
    }

    /// Peek `n`-th next `char` in input.
    dchar peekN(in size_t n) const scope nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _input[_offset + n]; // TODO: decode `dchar`
    }

    /// Drop next byte in input.
    void drop1() nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        _offset += 1;
    }

    /// Drop next `n` bytes in input.
    void dropN(in size_t n) nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        _offset += n;           // TODO: decode `dchar`
    }

    /// Skip over `n` bytes in input.
    Input skipOverN(in size_t n) return nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline);
        const part = _input[_offset .. _offset + n]; // TODO: decode `dchar`
        dropN(n);
        return part;
    }

    /// Skip over next `char`.
    Input skipOver1() return nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline);
        return _input[_offset .. ++_offset]; // TODO: decode `dchar`
    }

    /// Skip over next two `char`s.
    Input skipOver2() return nothrow @nogc
    {
        version(D_Coverage) {} else pragma(inline);
        return _input[_offset .. (_offset += 2)]; // TODO: decode `dchar`
    }

    /// Skip line comment.
    void skipLineComment() scope nothrow @nogc
    {
        while (!peek0().among!('\0', endOfLineChars))
            _offset += 1;       // TODO: decode `dchar`
    }

    /// Skip line comment.
    Input getLineComment() return nothrow @nogc
    {
        size_t i;
        while (!peekN(i).among!('\0', endOfLineChars))
            ++i;                // TODO: decode `dchar`
        return skipOverN(i);    // TODO: decode `dchar`
    }

    /// Skip block comment.
    void skipBlockComment() scope nothrow @nogc
    {
        while (!peek0().among!('\0'))
        {
            if (peek0() == '*' &&
                peek1() == '/')
            {
                _offset += 2;
                return;
            }
            _offset += 1;
        }
        errorAtFront("unterminated block comment");
    }

    /// Get symbol.
    Input getSymbol() return nothrow @nogc
    {
        import std.uni : isAlphaNum; // TODO: decode `dchar`
        size_t i;
        const bool attributeFlag = peek0() == '@';
        if (peek0().isSymbolStart)
            ++i;
        while (peekN(i).isAlphaNum ||
               peekN(i) == '_' ||
               (attributeFlag && // attribute name
                peekN(i) == ':')) // may include colon qualifier
        {
            ++i;
        }
        return skipOverN(i);
    }

    /// Get number.
    Input getNumber() return nothrow @nogc
    {
        import std.ascii : isDigit;
        size_t i;
        while (peekN(i).isDigit)
            ++i;
        return skipOverN(i);
    }

    Input getWhitespace() return nothrow @nogc
    {
        size_t i;
        while (peekN(i).among!(whiteChars)) // NOTE this is faster than `src[i].isWhite`
            ++i;
        return skipOverN(i);
    }

    bool skipOverEsc(ref size_t i) nothrow @nogc
    {
        if (peekN(i) == '\\')   // TODO: decode `dchar`
        {
            ++i;
            if (peekN(i) == 'n')
                ++i;            // TODO: convert to "\r"
            else if (peekN(i) == 't')
                ++i;            // TODO: convert to "\t"
            else if (peekN(i) == 'r')
                ++i;            // TODO: convert to ASCII "\r"
            else if (peekN(i) == ']')
                ++i;            // TODO: convert to ASCII "]"
            else if (peekN(i) == 'u')
            {
                ++i;
                import std.ascii : isDigit;
                while (peekN(i).isDigit)
                    ++i;
                // TODO: convert to `dchar`
            }
            else if (peekN(i) == '\0')
            {
                errorAtIndex("unterminated escape sequence at end of file");
                return false;
            }
            else
                i += 1;
            return true;
        }
        return false;
    }

    Input getTextLiteralDoubleQuoted() return nothrow @nogc
    {
        drop1();
        size_t i;
        while (!peekN(i).among!('\0', '"'))
        {
            if (!skipOverEsc(i))
                ++i;
        }
        const literal = skipOverN(i);
        if (peek0() == '"')
            drop1();        // pop ending double singlequote
        return literal;
    }

    Input getTextLiteralSingleQuoted() return nothrow @nogc
    {
        drop1();
        size_t i;
        while (!peekN(i).among!('\0', '\''))
        {
            if (!skipOverEsc(i))
                ++i;
        }
        const literal = skipOverN(i);
        if (peek0() == '\'')
            drop1();        // pop ending double singlequote
        return literal;
    }

    Input getHooks() return nothrow @nogc
    {
        size_t i;
        while (!peekN(i).among!('\0', ']')) // may contain whitespace
        {
            if (!skipOverEsc(i))
                ++i;
        }
        if (peekN(i) == ']') // skip ']'
            ++i;
        return skipOverN(i);
    }

    Input getAction() return nothrow @nogc
    {
        size_t i;

        import nxt.dynamic_array : DynamicArray;
        DynamicArray!char ds;   // delimiter stack

        bool inBlockComment;
        bool inLineComment;
        bool inChar;
        bool inString;

        const infoFlag = false;

        while (!peekN(i).among!('\0'))
        {
            // skip over all escape sequences in quoted
            if (inChar ||
                inString)
            {
                while (skipOverEsc(i)) {}
            }

            if (!inBlockComment &&
                !inLineComment &&
                !inChar &&
                !inString)
            {
                if (peekN(i) == '/' &&
                    peekN(i + 1) == '/')
                {
                    if (infoFlag) infoAtIndex("line comment start", i, ds[]);
                    inLineComment = true;
                    i += 2;
                    continue;
                }
                else if (peekN(i) == '/' &&
                         peekN(i + 1) == '*')
                {
                    if (infoFlag) infoAtIndex("block comment start", i, ds[]);
                    inBlockComment = true;
                    i += 2;
                    continue;
                }
                else if (peekN(i) == '{')
                {
                    if (infoFlag) infoAtIndex("brace open", i, ds[]);
                    ds.insertBack('{');
                }
                else if (peekN(i) == '}')
                {
                    if (infoFlag) infoAtIndex("brace close", i, ds[]);
                    if (!ds.empty &&
                        ds.back != '{')
                        errorAtIndex("unmatched", i);
                    ds.popBack();
                }
                else if (peekN(i) == '[')
                {
                    if (infoFlag) infoAtIndex("hook open", i, ds[]);
                    ds.insertBack('[');
                }
                else if (peekN(i) == ']')
                {
                    if (infoFlag) infoAtIndex("hook close", i, ds[]);
                    if (!ds.empty &&
                        ds.back != '[')
                        errorAtIndex("unmatched", i);
                    ds.popBack();
                }
                else if (peekN(i) == '(')
                {
                    if (infoFlag) infoAtIndex("paren open", i, ds[]);
                    ds.insertBack('(');
                }
                else if (peekN(i) == ')')
                {
                    if (infoFlag) infoAtIndex("paren close", i, ds[]);
                    if (!ds.empty &&
                        ds.back != '(')
                        errorAtIndex("unmatched", i);
                    ds.popBack();
                }
            }

            // block comment close
            if (inBlockComment &&
                peekN(i) == '*' &&
                peekN(i + 1) == '/')
            {
                if (infoFlag) infoAtIndex("block comment close", i, ds[]);
                inBlockComment = false;
                i += 2;
                continue;
            }

            // line comment close
            if (inLineComment &&
                (peekN(i) == '\n' ||
                 peekN(i) == '\r'))
            {
                if (infoFlag) infoAtIndex("line comment close", i, ds[]);
                inLineComment = false;
            }

            // single-quote open/close
            if (!inBlockComment &&
                !inLineComment &&
                !inString &&
                peekN(i) == '\'')
            {
                if (!ds.empty &&
                    ds.back == '\'')
                {
                    if (infoFlag) infoAtIndex("single-quote close", i, ds[]);
                    ds.popBack();
                    inChar = false;
                }
                else
                {
                    if (infoFlag) infoAtIndex("single-quote open", i, ds[]);
                    ds.insertBack('\'');
                    inChar = true;
                }
            }

            // double-quote open/close
            if (!inBlockComment &&
                !inLineComment &&
                !inChar &&
                peekN(i) == '"')
            {
                if (!ds.empty &&
                    ds.back == '"')
                {
                    if (infoFlag) infoAtIndex("double-quote close", i, ds[]);
                    ds.popBack();
                    inString = false;
                }
                else
                {
                    if (infoFlag) infoAtIndex("doubl-quote open", i, ds[]);
                    ds.insertBack('"');
                    inString = true;
                }
            }

            i += 1;

            if (ds.length == 0)
                break;
        }

        if (inBlockComment)
            errorAtIndex("unterminated block comment", i);
        if (ds.length != 0)
            errorAtIndex("unbalanced code block", i);

        return skipOverN(i);
    }

    void nextFront() scope nothrow @nogc @trusted
    {
        switch (peek0())
        {
        case '/':
            if (peek1() == '/') // `//`
            {
                _offset += 2;
                skipLineComment();
                if (_includeComments)
                    _token = Token(TOK.lineComment);
                else
                    nextFront();
            }
            else if (peek1() == '*') // `/*`
            {
                _offset += 2;
                skipBlockComment();
                if (_includeComments)
                    _token = Token(TOK.blockComment);
                else
                    return nextFront();
            }
            else
                errorAtIndex("unexpected character");
            break;
        case '(':
            _token = Token(TOK.leftParen, skipOver1());
            break;
        case ')':
            _token = Token(TOK.rightParen, skipOver1());
            break;
        case '{':
            _token = Token(TOK.action, getAction());
            break;
        case '[':
            _token = Token(TOK.hooks, getHooks());
            break;
        case '"':
            _token = Token(TOK.textLiteralDoubleQuoted,
                           getTextLiteralDoubleQuoted());
            break;
        case '\'':
            _token = Token(TOK.textLiteralSingleQuoted,
                           getTextLiteralSingleQuoted());
            break;
        case ':':
            _token = Token(TOK.colon, skipOver1());
            break;
        case ';':
            _token = Token(TOK.semicolon, skipOver1());
            break;
        case '#':
            _token = Token(TOK.hash, skipOver1());
            break;
        case '=':
            if (peek1() == '>')
                _token = Token(TOK.alwaysIncludePredicate, skipOver2());
            else
                _token = Token(TOK.labelAssignment, skipOver1());
            break;
        case '*':
            _token = Token(TOK.zeroOrMore, skipOver1());
            break;
        case '+':
            if (peek1() == '=')
                _token = Token(TOK.listLabelAssignment, skipOver2());
            else
                _token = Token(TOK.oneOrMore, skipOver1());
            break;
        case '|':
            _token = Token(TOK.alternative, skipOver1());
            break;
        case '~':
            _token = Token(TOK.negation, skipOver1());
            break;
        case '?':
            _token = Token(TOK.optOrSemPred, skipOver1());
            break;
        case '<':
            _token = Token(TOK.lt, skipOver1());
            break;
        case '>':
            _token = Token(TOK.gt, skipOver1());
            break;
        case ',':
            _token = Token(TOK.comma, skipOver1());
            break;
        case '!':
            _token = Token(TOK.exclude, skipOver1());
            break;
        case '^':
            _token = Token(TOK.rootNode, skipOver1());
            break;
        case '.':
            if (peek1() == '.') // `..`
                _token = Token(TOK.dotdot, skipOver2());
            else
                _token = Token(TOK.wildcard, skipOver1());
            break;
        case '-':
            if (peek1() == '>') // `->`
                _token = Token(TOK.rewrite, skipOver2());
            else
                errorAtIndex("unexpected character");
            break;
        case '0':
            ..
        case '9':
            _token = Token(TOK.number, getNumber());
            break;
        case ' ':
        case '\t':
        case '\n':
        case '\v':
        case '\r':
        case '\f':
            // TODO: extend to std.uni
            // import std.uni : isWhite;
            // assert(peek0().isWhite);
            const ws = getWhitespace();
            if (_includeWhitespace)
                _token = Token(TOK.whitespace, ws);
            else
                return nextFront();
            break;
        case '\0':
            _token = Token.init;
            _endOfFile = true;
            return;
        default:
            if (peek0().isSymbolStart)
            {
                const symbol = getSymbol();
                switch (symbol[0])
                {
                case '$':
                    _token = Token(TOK.attributeSymbol, symbol);
                    break;
                case '@':
                    _token = Token(TOK.actionSymbol, symbol);
                    break;
                default:
                    static if (useKeywords)
                    {
                        switch (symbol)
                        {
                        case "scope": _token = Token(TOK.SCOPE, symbol); break;
                        case "fragment": _token = Token(TOK.FRAGMENT, symbol); break;
                        case "lexer": _token = Token(TOK.LEXER, symbol); break;
                        case "tree": _token = Token(TOK.TREE, symbol); break;
                        case "parser": _token = Token(TOK.PARSER, symbol); break;
                        case "grammar": _token = Token(TOK.GRAMMAR, symbol); break;
                        case "returns": _token = Token(TOK.RETURNS, symbol); break;
                        case "throws": _token = Token(TOK.THROWS, symbol); break;
                        case "catch": _token = Token(TOK.CATCH, symbol); break;
                        case "finally": _token = Token(TOK.FINALLY, symbol); break;
                        case "options": _token = Token(TOK.OPTIONS, symbol); break;
                        case "tokens": _token = Token(TOK.TOKENS, symbol); break;
                        case "import": _token = Token(TOK.IMPORT, symbol); break;
                        case "channels": _token = Token(TOK.CHANNELS, symbol); break;
                        case "mode": _token = Token(TOK.MODE, symbol); break;
                        default: _token = Token(TOK.symbol, symbol); break;
                        }
                    }
                    else
                    {
                        _token = Token(TOK.symbol, symbol);
                    }
                    break;
                }
            }
            else
            {
                _token = Token(TOK._error);
                errorAtIndex("unexpected character");
            }
        }
    }

    void warningAtFront(in string msg) const @trusted nothrow @nogc scope
    {
        messageAtToken(front, "Warning", msg);
    }

    void errorAtFront(in string msg) const @trusted nothrow @nogc scope
    {
        messageAtToken(front, "Error", msg);
        assert(false);          ///< TODO: propagate error instead of assert
    }

    private void messageAtToken(in Token token,
                                in string tag,
                                in string msg) const @trusted nothrow @nogc scope
    {
        import core.stdc.stdio : printf;
        const offset = token.input.ptr - _input.ptr; // unsafe
        // debug writeln("offset:", offset);
        const lc = offsetLineColumn(_input, offset);
        // debug writeln("lc:", lc);
        // TODO: remove printf
        debug printf("%.*s(%u,%u): %s: %.*s, token `%.*s` at offset %llu\n",
                     cast(int)_path.length, _path.ptr,
                     lc.line + 1, lc.column + 1,
                     tag.ptr,
                     cast(int)msg.length, msg.ptr,
                     cast(int)token.input.length, token.input.ptr,
                     offset);
    }

    // TODO: into warning(const char* format...) like in `dmd` and put in `nxt.parsing` and reuse here and in lispy.d
    void errorAtIndex(const string msg, in size_t i = 0) const @trusted nothrow @nogc scope
    {
        messageAtIndex("Error", msg, i);
        assert(false);          ///< TODO: propagate error instead of assert
    }

    void warningAtIndex(const string msg, in size_t i = 0) const @trusted nothrow @nogc scope
    {
        messageAtIndex("Warning", msg, i);
    }

    void infoAtIndex(const string msg, in size_t i = 0, in const(char)[] ds = null) const @trusted nothrow @nogc scope
    {
        messageAtIndex("Info", msg, i, ds);
    }

    void messageAtIndex(in string tag,
                        in string msg,
                        in size_t i = 0,
                        in const(char)[] ds = null) const @trusted nothrow @nogc scope
    {
        import core.stdc.stdio : printf;
        const lc = offsetLineColumn(_input, _offset + i);
        // TODO: remove printf
        debug printf("%.*s(%u,%u): %s: %.*s at offset %llu being char `%c` ds:`%.*s`\n",
                     cast(int)_path.length, _path.ptr,
                     lc.line + 1, lc.column + 1,
                     tag.ptr,
                     cast(int)msg.length, msg.ptr,
                     _offset + i,
                     peekN(i),
                     cast(int)ds.length, ds.ptr);
    }

private:
    size_t _offset;             // current offset in `_input`
    const Input _input;         ///< Input data.
    const string _path;         ///< Input file (or null if in-memory).

    Token _token;
    bool _endOfFile;            // signals null terminator found
    bool _includeComments = false;
    bool _includeWhitespace = false;
}

/// Node.
enum NODE
{
    grammar,                    ///< Grammar defintion (name).
    rule                        ///< Grammar rule.
}

/// AST node.
abstract class Node
{
@safe pure nothrow @nogc:
    this(in Token token)
    {
        this.token = token;
    }
    Token token;
}

abstract class BranchN(uint n) : Node
{
@safe pure nothrow @nogc:
    this(in Token token, Node[n] sub)
    {
        super(token);
        this.sub = sub;
    }
    Node[n] sub;
}

abstract class BranchM : Node
{
@safe pure nothrow @nogc:
    this(in Token token, Node[] subs = null)
    {
        super(token);
        this.subs = subs;
    }
    Node[] subs;
}

/// Sequence.
final class SeqM : BranchM
{
@safe pure nothrow @nogc:
    this(in Token token, Node[] subs = null)
    {
        super(token);
        this.subs = subs;
    }
    Node[] subs;
}

/// Rule of alternatives.
class RuleAltM : BranchM
{
@safe pure nothrow @nogc:
    this(in Token token, Node[] subs = null)
    {
        super(token);
        this.subs = subs;
    }
    Node[] subs;
}

/// Fragment rule of alternatives.
final class FragmentRuleAltM : RuleAltM
{
@safe pure nothrow @nogc:
    this(in Token token, Node[] subs = null)
    {
        super(token);
        this.subs = subs;
    }
    Node[] subs;
}

class Leaf : Node
{
@safe pure nothrow @nogc:
    this(in Token token)
    {
        super(token);
    }
}

class Symbol : Node
{
@safe pure nothrow @nogc:
    this(in Token token)
    {
        super(token);
    }
}

final class Grammar : Leaf
{
@safe pure nothrow @nogc:
    this(in Token token, Input name)
    {
        super(token);
        this.name = name;
    }
    Input name;
}

final class LexerGrammar : Leaf
{
@safe pure nothrow @nogc:
    this(in Token token, Input name)
    {
        super(token);
        this.name = name;
    }
    Input name;
}

/// See_Also: https://theantlrguy.atlassian.net/wiki/spaces/ANTLR3/pages/2687210/Quick+Starter+on+Parser+Grammars+-+No+Past+Experience+Required
final class ParserGrammar : Leaf
{
@safe pure nothrow @nogc:
    this(in Token token, Input name)
    {
        super(token);
        this.name = name;
    }
    Input name;
}

final class Import : Leaf
{
@safe pure nothrow @nogc:
    this(in Token token, const Input[] modules)
    {
        super(token);
        this.modules = modules;
    }
    const Input[] modules;
}

final class Mode : Leaf
{
@safe pure nothrow @nogc:
    this(in Token token, Input name)
    {
        super(token);
        this.name = name;
    }
    Input name;
}

final class Options : Leaf
{
@safe pure nothrow @nogc:
    this(in Token token, in Token code)
    {
        super(token);
        this.code = code;
    }
    Input name;
    Token code;
}

final class Scope : Leaf
{
@safe pure nothrow @nogc:
    this(in Token token, in Token code)
    {
        super(token);
        this.code = code;
    }
    Input name;
    Token code;
}

final class AttributeSymbol : Leaf
{
@safe pure nothrow @nogc:
    this(in Token token, in Token code)
    {
        super(token);
        this.code = code;
    }
    Input name;
    Token code;
}

final class ActionSymbol : Leaf
{
@safe pure nothrow @nogc:
    this(in Token token, in Token code)
    {
        super(token);
        this.code = code;
    }
    Input name;
    Token code;
}

final class Channels : Leaf
{
@safe pure nothrow @nogc:
    this(in Token token, in Token code)
    {
        super(token);
        this.code = code;
    }
    Input name;
    Token code;
}

final class Tokens : Leaf
{
@safe pure nothrow @nogc:
    this(in Token token, in Token code)
    {
        super(token);
        this.code = code;
    }
    Input name;
    Token code;
}

/** G4 parser.
 *
 * See: `ANTLRv4Parser.g4`
 */
struct G4Parser
{
@safe pure:
    this(in Input input,
         in string path = null,
         in bool includeComments = false) @trusted
    {
        _lexer = G4Lexer(input, path, includeComments);
        nextFront();
    }

    @property bool empty() const nothrow scope @nogc
    {
        version(D_Coverage) {} else pragma(inline, true);
        return _lexer.empty;
    }

    inout(Node) front() inout scope return @trusted
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        return _front;
    }

    void popFront()
    {
        version(D_Coverage) {} else pragma(inline, true);
        assert(!empty);
        nextFront();
    }

    private void handleRule(in Token name,
                            in bool isFragment,
                            ActionSymbol actionSymbol = null) @trusted
    {
        _lexer.popFrontEnforceTOK(TOK.colon, "no colon");
        Appender!(Node[]) alts; // TODO: use stack for small arrays
        while (_lexer.front.tok != TOK.semicolon)
        {
            Appender!(Node[]) seq; // TODO: use stack for small arrays
            while (_lexer.front.tok != TOK.alternative &&
                   _lexer.front.tok != TOK.semicolon)
            {
                seq.put(new Symbol(_lexer.frontPop));
            }
            if (!seq.data.length)
            {
                _lexer.warningAtFront("empty sequence");
                _lexer.popFront();
                continue;
            }
            alts.put(new SeqM(name, seq.data));
            if (_lexer.front.tok == TOK.alternative)
                _lexer.popFront(); // skip terminator
        }

        _lexer.popFrontEnforceTOK(TOK.semicolon, "no terminating semicolon");

        if (isFragment)
            _front = new FragmentRuleAltM(name, alts.data);
        else
            _front = new RuleAltM(name, alts.data);
    }

    Input[] getArgs(in TOK separator,
                    in TOK terminator)
    {
        Appender!(Input[]) result;
        while (true)
        {
            result.put(_lexer.frontPopEnforceTOK(TOK.symbol).input);
            if (_lexer.front.tok != separator)
                break;
            _lexer.popFront();
        }
        _lexer.popFrontEnforceTOK(terminator, "no terminating semicolon");
        return result.data;
    }

    AttributeSymbol getAttributeSymbol()
    {
        return new AttributeSymbol(_lexer.frontPop,
                                   _lexer.frontPopEnforceTOK(TOK.action,
                                                             "missing action"));
    }

    ActionSymbol getActionSymbol()
    {
        return new ActionSymbol(_lexer.frontPop,
                                _lexer.frontPopEnforceTOK(TOK.action,
                                                          "missing action"));
    }

    void nextFront() @trusted
    {
        switch (_lexer.front.tok)
        {
        case TOK.LEXER:
        case TOK.PARSER:
        case TOK.GRAMMAR:
            const head = _lexer.frontPop;
            bool lexerFlag;
            bool parserFlag;
            if (head.tok == TOK.LEXER)
            {
                lexerFlag = true;
                _lexer.popFrontEnforceTOK(TOK.GRAMMAR, "expected `grammar` after `lexer`");
            }
            else if (head.tok == TOK.PARSER)
            {
                parserFlag = true;
                _lexer.popFrontEnforceTOK(TOK.GRAMMAR, "expected `grammar` after `parser`");
            }

            if (lexerFlag)
                _front = new LexerGrammar(head, _lexer.frontPop.input);
            else if (parserFlag)
                _front = new ParserGrammar(head, _lexer.frontPop.input);
            else
                _front = new Grammar(head, _lexer.frontPop.input);

            _lexer.popFrontEnforceTOK(TOK.semicolon, "no terminating semicolon");
            break;
        case TOK.IMPORT:
            _front = new Import(_lexer.frontPop, getArgs(TOK.comma, TOK.semicolon));
            break;
        case TOK.MODE:
            _front = new Mode(_lexer.frontPop, _lexer.frontPop.input);
            _lexer.popFrontEnforceTOK(TOK.semicolon, "no terminating semicolon");
            break;
        case TOK.SCOPE:
            const head = _lexer.frontPop;
            if (_lexer.front.tok == TOK.colon)
                handleRule(head, false); // normal rule
            else
                _front = new Scope(head,
                                   _lexer.frontPopEnforceTOK(TOK.action,
                                                             "missing action"));
            break;
        case TOK.FRAGMENT:
            _lexer.popFront();  // skip keyword
            handleRule(_lexer.frontPop, true);
            break;
        case TOK.OPTIONS:
            _front = new Options(_lexer.frontPop,
                                 _lexer.frontPopEnforceTOK(TOK.action,
                                                           "missing action"));
            break;
        case TOK.CHANNELS:
            _front = new Channels(_lexer.frontPop,
                                  _lexer.frontPopEnforceTOK(TOK.action,
                                                            "missing action"));
            break;
        case TOK.TOKENS:
            _front = new Tokens(_lexer.frontPop,
                                _lexer.frontPopEnforceTOK(TOK.action,
                                                            "missing action"));
            break;
        case TOK.symbol:
            handleRule(_lexer.frontPop,
                       false,
                       _lexer.front.tok == TOK.actionSymbol ? getActionSymbol() : null);
            break;
        case TOK.attributeSymbol:
            _front = getAttributeSymbol();
            break;
        case TOK.actionSymbol:
            _front = getActionSymbol();
            break;
        case TOK.blockComment:
        case TOK.lineComment:
            return _lexer.popFront();   // ignore
        default:
            return _lexer.errorAtFront("TODO: handle");
        }
    }

private:
    G4Lexer _lexer;
    Node _front;
}

/// G4 filer parser.
struct G4FileParser           // TODO: convert to `class`
{
@safe:
    this(in string filePath)
    {
        import std.path : expandTilde;
        const path = filePath.expandTilde;
        const data = cast(Input)rawReadPath(path); // cast to Input because we don't want to keep all file around:
        parser = G4Parser(data, filePath, false);
    }
    ~this() @nogc {}
    G4Parser parser;
    alias parser this;
}

///
unittest
{
    import nxt.array_algorithm : endsWith;
    import std.file : dirEntries, SpanMode;
    import std.path : expandTilde;

    const root = "~/Work/grammars-v4/".expandTilde;
    const testLexer = true;
    const testParser = true;

    if (testLexer)
        foreach (e; dirEntries(root, SpanMode.breadth))
        {
            const fn = e.name;
            if (fn.endsWith(`.g`) ||
                fn.endsWith(`.g2`) ||
                fn.endsWith(`.g4`))
            {
                debug writeln("Lexing ", fn, " ...");
                const data = cast(Input)rawReadPath(fn);
                auto lexer = G4Lexer(data, fn, false);
                while (!lexer.empty)
                    lexer.popFront();
            }
        }

    if (testParser)
        foreach (e; dirEntries(root, SpanMode.breadth))
        {
            const fn = e.name;
            if (fn.endsWith(`.g`) ||
                fn.endsWith(`.g2`) ||
                fn.endsWith(`.g4`))
            {
                // if (!fn.endsWith(`pascal.g4`)) // only pascal
                //     continue;
                debug writeln("Parsing ", fn, " ...");
                auto parser = G4FileParser(fn);
                while (!parser.empty)
                    parser.popFront();
            }
        }
}
