import std.stdio: writeln, stdin, stdout, stderr;
import std.process: spawnProcess, Pid, kill, wait, Config, thisProcessID;
import core.sys.posix.signal: SIGSTOP, SIGABRT, SIGTERM, SIGCONT;
import core.memory: GC;
import core.thread: sleep;
import core.time: MonoTime;

void main(string[] args)
{
    const sourcePath = `aard_process.d`;

    import std.path: expandTilde;
    const workDir = expandTilde(`~/Work/knet`);     // std.file.getcwd doesn't work here because it
    writeln(`sched (pid=`, thisProcessID, `): was started`);

    GC.disable;

    import std.datetime: StopWatch;
    StopWatch sw;

    enum n = 1;
    Pid[n] childs;
    const dmdArgs = [`rdmd`, `-O`, `-release`, `-inline`, sourcePath];
    const lsArgs = [`ls`, `/dev/null`];
    const childArgs = dmdArgs;

    enum ms = `milliseconds`;

    // spawn childs
    sw.start;
    foreach (ref child; childs)
    {
        child = spawnProcess(childArgs,
                             stdin,
                             stdout,
                             stderr,
                             null,
                             Config.none,
                             workDir);
        writeln(`sched (pid=`, thisProcessID, `): spawned child (pid=`, child.processID, `)`);
    }
    sw.stop;
    writeln(`sched (pid=`, thisProcessID, `): spawned `, n, ` processes in `, sw.peek.msecs, ` `, ms);

    // sleep
    const sleepSecs = 2;
    writeln(`sched (pid=`, thisProcessID, `): sleeping for `, sleepSecs, ` seconds until all childs have stopped themselves`);
    sleep(sleepSecs);

    // start childs
    sw.reset; sw.start;
    MonoTime[n] spawnTimes;
    foreach (i, child; childs)
    {
        spawnTimes[i] = MonoTime.currTime;
        child.kill(SIGCONT);
        writeln(`sched (pid=`, thisProcessID, `): continued (SIGCONT) child (pid=`, child.processID, `) at `, spawnTimes[i] - MonoTime.zero);
    }
    sw.stop;
    writeln(`sched (pid=`, thisProcessID, `): continued `, n, ` processes in `, sw.peek.msecs, ` `, ms);

    // wait childs
    if (false)
    {
        sw.reset; sw.start;
        foreach (child; childs)
        {
            child.wait;
        }
        sw.stop;
        writeln(`sched (pid=`, thisProcessID, `): waited for `, n, ` processes in `, sw.peek.msecs, ` `, ms);
    }

    // kill childs
    sw.reset; sw.start;
    foreach (child; childs)
    {
        child.kill(SIGTERM);
        writeln(`sched (pid=`, thisProcessID, `): killed (SIGTERM) child (pid=`, child.processID, `)`);
    }
    sw.stop;
    auto killallPid = spawnProcess([`killall`, `-9`, `aard_process`]); // TODO remove the need for this
    killallPid.wait;
    writeln(`sched (pid=`, thisProcessID, `): killed `, n, ` processes in `, sw.peek.msecs, ` `, ms);

    writeln(`sched (pid=`, thisProcessID, `): is exiting`);
    GC.enable;
}
