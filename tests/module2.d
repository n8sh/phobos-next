#!/usr/bin/env rdmd-dev-module

module module2;

void f2(string[] args)
{
    import module1: f1;
    import dbgio;
}
