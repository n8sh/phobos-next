## TODO

- Compilation works with ldmd2 but fails with compiler `dmd` as

    Error: unrecognized switch '-d-version=segregated_gc'

- Check why finalizers are being called for classes and structs without destructors
- Check ti to check if we should use value or ref pool
- Use `slotUsages` during allocation
- Use `slotMarks` during sweep
- Figure out if we need medium and large sized slots as outlined in reference [1].

## References

1. Inside D's GC:
https://olshansky.me/gc/runtime/dlang/2017/06/14/inside-d-gc.html
