/** Git operations.
 *
 * Original version https://gist.github.com/PetarKirov/b4c8b64e7fc9bb7391901bcb541ddf3a
 *
 * See_Also: https://forum.dlang.org/post/ziqgqpkdjolplyfztulp@forum.dlang.org
 */
module nxt.git;

import std.exception : enforce;
import std.format : format;
import std.file : exists, getcwd, isDir, isFile;
import std.path : absolutePath, buildNormalizedPath, dirName, relativePath;
import std.stdio : writeln, writefln;

@safe:

/// Based on: https://git-scm.com/docs/git-status#_output
enum GitStatusSingleSide : char
{
    @("unmodified")           unmodified =  ' ',
    @("ignored")              ignored = '!',
    @("untracked")            untracked = '?',
    @("modified")             modified = 'M',
    @("added")                added = 'A',
    @("deleted")              deleted = 'D',
    @("renamed")              renamed = 'R',
    @("copied")               copied = 'C',
    @("updated but unmerged") updatedNotMerged = 'U'
}

struct GitStatus
{
    GitStatusSingleSide x, y;
    char[2] xy() { return [x, y]; }
    alias xy this;
}

string interpretGitStatus(GitStatus s)
{
    if (s.x == 'U' ||
        s.y == 'U' ||
        s == "DD" ||
        s == "AA")
    {
        // Unmerged
        if (s.x == s.y)
            return "both " ~ s.x.enumUdaToString;
        else if (s.x == 'U')
            return s.y.enumUdaToString ~ " by them";
        else if (s.y == 'U')
            return s.x.enumUdaToString ~ " by us";
        else
            assert(0, "Unhandled `git status` case: " ~ s);
    }
    else if (s == "??" ||
             s == "!!")
    {
        // ignored or untracked
        return s.x.enumUdaToString;
    }
    else
    {
        // No merge conflict
        return "in index: <%s> | <%s> in work tree".format(s.tupleof);
    }
}

GitStatus gitStatus(scope string filePath)
{
    //enforce(filePath.isFile, "'" ~ filePath ~ "' is not a file.");

    const gitRoot = getGitRootPathOfFileOrDir(filePath);

    const gitStatusResult = ["git", "status", "--porcelain=v1", filePath]
    .executeInDir(gitRoot);

    enforce(gitStatusResult.length >= "XY f".length);

    GitStatus result =
    {
    x: gitStatusResult[0].assertMemberOfEnum!GitStatusSingleSide,
    y: gitStatusResult[1].assertMemberOfEnum!GitStatusSingleSide
    };

    return result;
}


string toGitRelativePath(scope string path)
{
    const cwd = getcwd();
    const gitRoot = path.getGitRootPathOfFileOrDir;
    return path
    .relativePath(/* base: */ gitRoot).buildNormalizedPath;
}

string getGitRootPathOfFileOrDir(scope string path_)
{
    auto path = path_.absolutePath;

    // Find the closest directory to the path that exists
    while (!path.exists ||
           !path.isDir)
        path = path.dirName;

    const gitRootPathResult = "git rev-parse --show-toplevel".executeInDir(path);
    return gitRootPathResult;
}

/++
 Execute `Cmd` in the given `workDir` and return the output.

 If `Cmd` is a `string` it does so via `executeShell`, otherwise via `execute`,
 the latter of which doesn't go through the shell.

 Returns:
 The captured output of the execution of the command with all trailing
 whitespace removed.
 +/
string executeInDir(Cmd)(scope Cmd cmd,
                         string workDir,
                         string messageIfCommandFails = null)
if (is(Cmd : const char[]) || is(Cmd : const char[][]))
in (workDir.isDir)
{
    import std.process : Config, execute, executeShell;
    import std.string : stripRight;

    static if (is(Cmd : const char[]))
        const result = executeShell(
            cmd,
            /* env: */ null,
            /* config: */ Config.none,
            /* maxOutput: */ size_t.max,
            /* workDir: */ workDir
            ); // comments written in anticipation of DIP1030 ;)
    else
        const result = execute(
            cmd,
            /* env: */ null,
            /* config: */ Config.none,
            /* maxOutput: */ size_t.max,
            /* workDir: */ workDir
            ); // comments written in anticipation of DIP1030 ;)

    enforce(result.status == 0,
            "command: '%-s' failed.%s".format(cmd, "\n" ~ messageIfCommandFails));

    return result.output.stripRight;
}

string enumUdaToString(E)(E value)
if (is(E == enum))
{
    final switch (value)
    {
        static foreach (memberName; __traits(allMembers, E))
        case mixin(E, '.', memberName):
            return __traits(getAttributes, mixin(E, '.', memberName))[0];
    }
}

E assertMemberOfEnum(E)(BaseEnumType!E value)
if (is(E == enum))
{
    final switch (value)
    {
        static foreach (memberName; __traits(allMembers, E))
        {
            {
                enum member = mixin(E, '.', memberName);
                case member:
                    return member;
            }
        }
    }
}

template BaseEnumType(E)
{
    static if (is(E Base == enum))
        alias BaseEnumType = Base;
    else
        static assert (0, "`E` is not an enum type");
}

void main(string[] args)
{
    enforce(args.length == 2, "Usage:\n\tabs_to_rel_git_path <path>");
    const path = args[1];
    //enforce(path.isFile, "'" ~ path ~ "' is not a file.");

    static void hLine()
    {
        import std.range : repeat;
        "%-(%s%)".writefln("-".repeat(20));
    }

    const gitRoot = path.getGitRootPathOfFileOrDir;
    const relativePath = path.toGitRelativePath;
    const status = path.gitStatus;

    hLine();
    writeln("Git repo: ", gitRoot);
    hLine();
    writefln("File: %s\nStatus: %s", relativePath, status.interpretGitStatus);
    hLine();
}
