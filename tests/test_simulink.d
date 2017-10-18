// #!/usr/bin/env rdmd

import std.array : array;
import std.range : empty, iota;
import std.algorithm;
import std.conv : to;
import std.file : read, dirEntries, DirEntry, SpanMode;
import std.path : expandTilde;
import std.digest.crc, std.stdio;
import std.string : strip, indexOf, lastIndexOf;
import std.concurrency : spawn, send, receive;
import std.random : uniform;

import arsd.simpledisplay;      // graphics

/// True if we should warn about holes in enumerations.
immutable warnEnumHoles = true;

/// Model execution frequency.
enum Frequency
{
    _7_5,
    _15,
    _30,
    _60,
}

/// Type category.
enum TypeCategory
{
    unknown,

    enumeration,
    bus,

    boolean,

    // signed integers
    int8,
    int16,
    int32,
    int64,

    // unsigned integers
    uint8,
    uint16,
    uint32,
    uint64,

    // floating point
    singleFloat,
    doubleFloat,
}

/// Simulink Base Type.
class BaseType
{
    string name;                ///< Type name.
    uint dimensions = 1;        ///< Number of dimensions.
    TypeCategory typeCategory;
    string headerFile;          ///< getHeaderFile()
    string description;         ///< getDescription()
}

/// Matlab enumeration members.
alias Enumerators = int[string];

/// Simulink enumeration basetype definition
class EnumType : BaseType
{
    this(string name, Enumerators enumerators, string defaultEnumerator, string headerFile)
    {
        this.name = name;
        this.enumerators = enumerators;
        this.defaultEnumerator = defaultEnumerator;
        this.headerFile = headerFile;
    }

    auto isContinuous()
    {
        return sort(enumerators.byValue.array).findAdjacent!((a, b) => a + 1 != b).empty;
    }
    auto hasHoles() { return !isContinuous; }

    auto enumeratorsSortedByValue() const
    {
        return enumerators.byKeyValue.array.sort!((a, b) => a.value < b.value);
    }

    Enumerators enumerators;
    string defaultEnumerator;
    string headerFile;
}

/// Simulink Bus Element/Member (Instance).
struct Sl_BusElement
{
    string name;
    uint dimensions;

    TypeCategory typeCategory;
    string dataType;
    string min;
    string max;
    string docUnits; // (physical) unit
    string description;
    alias doc = description;
}

enum Sl_Option : bool { off, on }

/// Simulink Block (Instance).
class Sl_Block
{
    /// these two together given by XML-tag `Location`
    Point position;
    Size size;

    uint zOrder;                // visual layer ordering
    Sl_Option showName;

    uint SID;                   // identity code
}

/// Simulink BusCreator (Instance) Block.
class Sl_BusCreator : Sl_Block
{
    string block;
    uint inputCount; // number of inports (XML-tag `Inputs`)
    string[] outputSignals;
    string blockType;
    Sl_Option nonVirtualBus;
    string rndMeth; // can be: zero, floow
    Sl_Option saturateOnIntegerOverflow;
}

/// Simulink Gain Block.
class Sl_Gain : Sl_Block
{
    string gainFactor; // (XML-tag `gain`)
    string[] paramMin;
    string[] paramMax;
    string[] outMin;
    string[] outMax;
    string rndMeth; // can be: zero, floow
    Sl_Option saturateOnIntegerOverflow;
}

enum Sl_LogicOperator { and_ }

/// Simulink Logic Block.
class Sl_Logic : Sl_Block
{
    Sl_LogicOperator op;
    uint inputCount; // number of inports (XML-tag `Input`)
    Sl_Option alLPortsSameDatatype;
}

/// Simulink Data type Conversion.
class Sl_DataTypeConversion : Sl_Block
{
    string[] outMin;
    string[] outMax;
    Sl_Option lockScale;
}

/// Data complexity.
enum Complexity
{
    real_,                      ///< Real number.
    complex_                    ///< Complex number.
}

/// Simulink Port.
class Sl_Port
{
    uint portIndex; // starting at 1
    string name;
    string[] outMin;
    string[] outMax;
    Complexity complexity; // XML-tag `SignalType`
}

/// Simulink Inport.
class Sl_Inport : Sl_Port
{
    Sl_Option interpolate;
}

/// Simulink Outport.
class Sl_Outport : Sl_Port
{
}

/// Simulink Constant.
class Sl_Constant
{
    string value;
    string[] outMin;
    string[] outMax;
}

/// Simulink Bus Basetype Definition.
class Sl_BusType : BaseType
{
    this(string name, Sl_BusElement[] elements)
    {
        this.name = name;
        this.elements = elements;
    }
    Sl_BusElement[] elements;
}

/// Simulink Alias Type Definition.
class Sl_AliasType : BaseType
{
    this(string name, string baseType)
    {
        this.name = name;
        this.baseType = baseType; // TODO convert to enum
    }
    string baseType;
}

/// Rea Matlab bus definitions in `path`.
void readMatlabBusDefinition(string path)
{
    writeln("Reading MATLAB Bus Definitions from ", path);
    const data = cast(const char[])read(path);
}

alias BusIndex = uint;

/// Parse Simulink bus definitions using `lines`.
Sl_BusType parseMatlabBusDefinitions(R)(R lines, string path)
{
    BusIndex index = BusIndex.max;
    string busName;

    Sl_BusElement[] elements;
    BusIndex currentIndex = 0;

    enum State
    {
        initial,
        elementStarted,
        elementEnded,
    }

    size_t loff = 0; // line offset
    foreach (sline; lines.map!(l => l.strip)
                         .filter!(l => !l.empty))
    {
        if (sline.startsWith("function "))
        {
        }
        else if (sline == "clear elems;") { /* ignore */ }
        else if (sline.skipOver("% Bus object: "))
        {
            busName = sline.to!(typeof(busName));
        }
        else if (sline.startsWith("%"))
        {
            // ignore other comment
        }
        else if (sline.skipOver("elems(")) // elements found
        {
            if (auto hit = sline.findSplit(")"))
            {
                const length = hit[0].to!BusIndex; // get current index

                index = length - 1; // minus one because MATLAB indexes start at 1
                assert(index == currentIndex || // index should either be same
                       index == currentIndex + 1); // or next
                currentIndex = index;

                elements.length = length; // reserve elements

                auto rest = hit[2];
                if (rest.strip == "= Simulink.Sl_BusElement;") { continue; /* next line*/ }
                assert(rest.skipOver("."));
                if (auto rhit = rest.findSplit("="))
                {
                    auto propertyName = rhit[0].strip;
                    auto propertyValue = rhit[2][0 .. $ - 1].strip;
                    assert(index < elements.length);

                    // strip single quotes from propertyValue
                    if (propertyValue.length >= 2 &&
                        propertyValue[0] == '\'' &&
                        propertyValue[$ - 1] == '\'')
                    {
                        propertyValue = propertyValue[1 .. $ - 1]; // strip
                    }

                    switch (propertyName)
                    {
                    case "Name":
                        elements[index].name = propertyValue.to!string;
                        break;
                    case "Dimensions":
                        elements[index].dimensions = propertyValue.to!(typeof(elements[index].dimensions)); // TODO functionize to decode
                        break;
                    case "DataType":
                        TypeCategory typeCategory;

                        // TODO Add and use variadic skipOver as: propertyValue.skipOver("Bus: ", "Enum: ")
                        if (propertyValue.skipOver("Bus: "))
                        {
                            typeCategory = TypeCategory.bus;
                        }
                        else if (propertyValue.skipOver("Enum: "))
                        {
                            typeCategory = TypeCategory.enumeration;
                        }
                        else
                        {
                            switch (propertyValue)
                            {
                            case "boolean": typeCategory = TypeCategory.boolean; break;

                            case "single": typeCategory = TypeCategory.singleFloat; break;
                            case "double": typeCategory = TypeCategory.doubleFloat; break;

                            case "int8":  typeCategory = TypeCategory.int8; break;
                            case "int16": typeCategory = TypeCategory.int16; break;
                            case "int32": typeCategory = TypeCategory.int32; break;
                            case "int64": typeCategory = TypeCategory.int64; break;

                            case "uint8":  typeCategory = TypeCategory.uint8; break;
                            case "uint16": typeCategory = TypeCategory.uint16; break;
                            case "uint32": typeCategory = TypeCategory.uint32; break;
                            case "uint64": typeCategory = TypeCategory.uint64; break;

                            // TODO how do these differ from boolean, single and double?
                            case "Bool": typeCategory = TypeCategory.boolean; break;
                            case "Real32_Type": typeCategory = TypeCategory.singleFloat; break;
                            case "Real64_Type": typeCategory = TypeCategory.doubleFloat; break;

                            default:
                                writeln("Handle TypeCategory for ", propertyValue);
                                break;
                            }
                        }
                        elements[index].typeCategory = typeCategory;
                        elements[index].dataType = propertyValue.to!string;
                        break;
                    case "Min":
                        if (propertyValue != "[]")
                            elements[index].min = propertyValue.to!string;
                        break;
                    case "Max":
                        if (propertyValue != "[]")
                            elements[index].max = propertyValue.to!string;
                        break;
                    default:
                        break;
                    }
                }
            }
            else
            {
                throw new Exception("MATLAB bus definition parse error");
            }
        }
        ++loff;
    }

    const show = true;
    if (show)
    {
        writeln(" * ", busName);
        foreach (element; elements)
        {
            writeln("   | ", element);
        }
    }

    if (!busName.empty)
    {
        foreach (const i, const ref element; elements)
        {
            if (element.name.empty)
            {
                writeln(path, ":0:warning: Element at ", i, " of bus ", busName, " in ", path, " has no name");
            }
        }
    }
    else
    {
        writeln("warning: Bus defined in ", path, " has no name");
    }

    return new Sl_BusType(busName, elements);
}

/// Parse MATLAB enumeration definition from `lines`.
EnumType parseMatlabEnum(R)(R lines)
{
    size_t i = 0;
    string name;
    string superType;
    Enumerators enumerators;
    string defaultEnumerator;
    string headerFile;

    enum State
    {
        initial,

        membersStarted,
        membersEnded,

        defaultValueStarted,
        defaultValueEnded,

        headerFileStarted,
        headerFileEnded,

    }
    State state = State.initial;

    foreach (sline; lines.map!(l => l.strip)
                         .filter!(l => !l.empty))
    {
        if (sline.skipOver("classdef (Enumeration) "))
        {
            if (auto hit = sline.findSplit("<"))
            {
                name = hit[0].strip.to!(typeof(name));
                superType = hit[2].strip.to!(typeof(superType));
            }
            else
            {
                throw new Exception("MATLAB enum definition parse error");
            }
        }
        else if (sline.skipOver("function retVal = getDefaultValue()"))
        {
            state = State.defaultValueStarted;
        }
        else if (sline.skipOver("function retVal = getHeaderFile()"))
        {
            state = State.headerFileStarted;
        }
        else
        {
            final switch (state)
            {
            case State.initial:
                if (sline == "enumeration")
                {
                    state = State.membersStarted;
                }
                break;

            case State.membersStarted:
                if (sline == "end")
                {
                    state = State.membersEnded;
                }
                else
                {
                    assert(sline[$ - 1] == ')');
                    if (auto vat = sline[0 .. $ - 1].findSplit("(")) // value and type
                    {
                        const ename = vat[0].to!string;
                        const evalue = vat[2].to!int;
                        enumerators[ename] = evalue;
                    }
                    else
                    {
                        throw new Exception("MATLAB enum enumerators parse error");
                    }
                }
                break;

            case State.membersEnded:
                break;

            case State.defaultValueStarted:
                if (sline == "end")
                {
                    state = State.defaultValueEnded;
                }
                else
                {
                    assert(sline[$ - 1] == ';');
                    defaultEnumerator = sline[sline.lastIndexOf('.') + 1 .. $ - 1].to!string;
                }
                break;

            case State.defaultValueEnded:
                break;

            case State.headerFileStarted:
                if (sline == "end")
                {
                    state = State.headerFileEnded;
                }
                else
                {
                    assert(sline.endsWith("';"));
                    assert(sline.skipOver("retVal = '"));
                    headerFile = sline[0 .. $ - 2].to!string;
                }
                break;

            case State.headerFileEnded:
                break;
            }
        }
        ++i;
    }

    // writeln(enumerators);
    // writeln(defaultEnumerator);
    // writeln(headerFile);

    assert(defaultEnumerator in enumerators);

    auto enum_ = new EnumType(name, enumerators, defaultEnumerator, headerFile);

    if (warnEnumHoles)
    {
        if (!enum_.isContinuous)
        {
            writeln(" - warning: MATLAB enumeration type ", enum_.name, " values are discontinous (has holes): ", enum_.enumerators);
        }
    }

    return enum_;
}

/// Read MATLAB type definition from `path`.
void readMTypeDefinition(DirEntry de)
{
    const path = de.name;
    writeln("Reading MATLAB BaseType Definition from ", path);

    auto lines = File(path).byLine;

    BaseType[string] types;

    if (lines.empty) { return; }

    BaseType type;
    if (lines.front.startsWith("function "))
    {
        type = parseMatlabBusDefinitions(lines, path);
    }
    else if (lines.front.startsWith("classdef (Enumeration)"))
    {
        type = parseMatlabEnum(lines);
    }
    if (type !is null)
    {
        types[type.name] = type;
    }
}

/** Read Matlab Simulink model blockdiagram from in `path`.
    Parameter `path` must be a path to a ZIP-archive with extension `.slx`.
 */
void readMatlabSimulinkModelBlockdiagram(string path)
{
    enum show = false;
    import std.xml;

    writeln("Reading Simulink model ", path);
    import std.zip : ZipArchive;
    auto zip = new ZipArchive(read(path));
    foreach (zipFileName, am; zip.directory)
    {
        // print
        if (show)
        {
            writef("%10s  %08x  %s", am.expandedSize, am.crc32, zipFileName);
            if (!am.comment.empty) { writef(" comment:", am.comment); }
            writeln;
        }

        // assert(am.expandedData.length == 0);
        zip.expand(am);
        // assert(am.expandedData.length == am.expandedSize);
        if (zipFileName == "simulink/blockdiagram.xml")
        {
            const sam = cast(string)am.expandedData;
            check(sam); // check that XML is well-formed
            auto doc = new Document(sam);
            // writeln(doc);
            // write(cast(const char[])am.expandedData);
        }
    }
}

/// Read all things.
void readAll(immutable string [] args)
{
    import std.path : isFile, extension;

    foreach (dirEntry; args[1].dirEntries(SpanMode.breadth)
                              .filter!(p => (p.isFile))) // which are files
    {
        const path = dirEntry.name;
        if (path.endsWith("_Type.m"))
        {
            readMTypeDefinition(dirEntry);
        }
        else if (path.endsWith("_types.m"))
        {
            readMatlabBusDefinition(path);
        }
        else if (path.extension == ".slx")
        {
            readMatlabSimulinkModelBlockdiagram(path);
        }
    }
}

void main(string[] args)
{
    if (args.length < 1 + 1)
    {
        writeln("Usage: sleck [ROOT_DIR]");
        return;
    }

    auto readTid = spawn(&readAll, args.idup);

    const screenWidth = 1920;
    const screenHeight = 1200;
    const windowSize = Size(screenWidth - 100,
                            screenHeight - 100);

    auto window = new SimpleWindow(windowSize,
                                   "sleck (Simulink Checker)",
                                   OpenGlOptions.no,
                                   Resizablity.automaticallyScaleIfPossible);
    const wW = window.width;
    const wH = window.height;

    const eventLatency = 10;    // in milliseconds

    window.eventLoop(eventLatency, {
        auto painter = window.draw();
        foreach (i; 0.iota(1))
        {
            painter.outlineColor = Color.black;
            painter.fillColor = Color.red;

            // TODO use my randomize
            painter.drawLine(Point(uniform(0, wW),
                                   uniform(0, wH)),
                             Point(uniform(0, wW),
                                   uniform(0, wH)));

            auto r = Rectangle(0, 0, 200, 200);
            painter.drawRectangle(Point(uniform(0, wW/2),
                                        uniform(0, wH/2)),
                                  uniform(1, wW/2),
                                  uniform(1, wH/2));
        }
    });
}
