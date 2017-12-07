#!/usr/bin/env rdmd-dev

import fs;
import backtrace.backtrace;

void main(string[] args)
{
    import std.stdio: stderr;
    backtrace.backtrace.install(stderr);

    // term.setTitle("Basic I/O");
    // auto input = RealTimeConsoleInput(viz, &term,
    //                                   ConsoleInputFlags.raw |
    //                                   ConsoleInputFlags.mouse |
    //                                   ConsoleInputFlags.paste);
    // term.write("test some long string to see if it wraps or what because i dont really know what it is going to do so i just want to test i think it will wrap but gotta be sure lolololololololol");
    // term.writefln("%d %d", term.cursorX, term.cursorY);
    // term.writeln("fdsfdfsfsfdf");

    scanner(args);
}
