# Virtual Machine approach for Regular Expression Matching

It is a stack project, with test cases specified. After cloning this repo, do:

```
stack setup
stack test
```

## Dependencies
Stack will manage them, but they are just HUnit and TimeIt, used by the tests.

## Benchmark

Backtrack implementation

String                     | Regex    | Time 
---------------------------|----------|-------
45 millions of `a` and `b` | aa\*bb\* | 89.26s
