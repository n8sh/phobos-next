#!/usr/bin/env rdmd-dev-module

module module1;

void f1(string[] args) {
    import module2: f2;
}

unittest {
}
