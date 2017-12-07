void main(string[] args)
{
    import core.memory: GC;
    GC.disable;

    import std.stdio: writeln;
    import std.process: thisProcessID, Pid;
    import core.sys.posix.signal: SIGSTOP;

    const thisPID = thisProcessID;
    writeln(`child (pid=`, thisPID, `): was started`);

    // stop child
    import std.process: ProcessException;
    import core.sys.posix.signal: kill;
    if (true)
    {
        if (kill(thisPID, SIGSTOP) == -1)
            throw ProcessException.newFromErrno;
    }

    import core.time: MonoTime;
    const wakeupTime = MonoTime.currTime;
    writeln(`child (pid=`, thisPID, `): awoke (continued) at `, wakeupTime - MonoTime.zero);

    // See also: http://forum.dlang.org/post/pziiwlhoyyolgvvljgcg@forum.dlang.org
    // TODO Use kill instead when thisProcessPid has been added
    // kill(new Pid(thisPID), SIGTSTP);
    writeln(`child (pid=`, thisPID, `): is exiting`);

    GC.enable;
}
