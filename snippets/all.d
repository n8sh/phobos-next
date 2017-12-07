#!/usr/bin/env rdmd-dev-module

/** Helper to import useful Phobos packages.
    */
module all;

/* std */
public import std.algorithm;
public import std.array;
public import std.ascii;
public import std.base64;
public import std.bigint;
public import std.bitmanip;
public import std.compiler;
public import std.complex;
public import std.concurrency;
public import std.container;
public import std.conv;
public import std.csv;
public import std.datetime.datetime;
public import std.encoding;
public import std.exception;
public import std.file;
public import std.format;
public import std.functional;
public import std.getopt;
public import std.json;
public import std.math;
public import std.mathspecial;
public import std.mmfile;
public import std.numeric;
public import std.outbuffer;
public import std.parallelism;
public import std.path;
public import std.process;
public import std.random;
public import std.range;
public import std.regex;
public import std.signals;
public import std.socket;
// Deprecated: public import std.socketstream;
public import std.stdio;
// Deprecated: public import std.cstream;
// Deprecated: public import std.stream;
public import std.string;
public import std.system;
public import std.traits;
public import std.typecons;
public import std.meta;
public import std.uni;
public import std.uri;
public import std.utf;
public import std.uuid;
public import std.variant;
public import std.xml;
public import std.zip;
public import std.zlib;
public import std.net.isemail;
public import std.digest.crc;
public import std.digest.digest;
public import std.digest.md;
public import std.digest.ripemd;
public import std.digest.sha;
public import std.windows.charset;
//public import std.net.curl;

/* core */
public import core.checkedint;
public import core.atomic;
public import core.bitop;
public import core.cpuid;
public import core.demangle;
public import core.exception;
public import core.memory;
public import core.runtime;
public import core.simd;
public import core.thread;
public import core.time;
public import core.vararg;
public import core.sync.barrier;
public import core.sync.condition;
public import core.sync.config;
public import core.sync.exception;
public import core.sync.mutex;
public import core.sync.rwmutex;
public import core.sync.semaphore;

/* experimental */
public import std.experimental.logger;
public import std.experimental.allocator;
public import std.experimental.ndslice;
public import std.experimental.typecons;
