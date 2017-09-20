#!/usr/bin/env rdmd-dev-module

/** Operations for Creating Temporary Files and Directories.
    Copyright: Per Nordlöw 2017-.
    License: $(WEB boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors: $(WEB Per Nordlöw)
 */
module tempfs;

version(linux)                  // TODO wrap in OS-independent interface
{
/** Create a New Temporary File starting with ($D namePrefix) and ending with 6
    randomly defined characters.
    Returns: File Descriptor to opened file.
*/
    int tempfile(string namePrefix = null) @trusted
    {
        import core.sys.posix.stdlib: mkstemp;

        char[4096] buf;
        buf[0 .. namePrefix.length] = namePrefix[]; // copy the name into the mutable buffer
        buf[namePrefix.length .. namePrefix.length + 6] = "XXXXXX"[];
        buf[namePrefix.length + 6] = 0; // make sure it is zero terminated yourself

        auto tmp = mkstemp(buf.ptr);

        // dln(buf[0 .. namePrefix.length + 6]);
        return tmp;
    }

/** TODO Scoped variant of tempfile.
    Search http://forum.dlang.org/thread/mailman.262.1386205638.3242.digitalmars-d-learn@puremagic.com
*/

/** Create a New Temporary Directory Tree.
    Returns: Path to root of tree.
*/
    char* temptree(char* name_x,
                   char* template_ = null) @trusted
    {
        return null;
    }

    unittest
    {
    }
}
