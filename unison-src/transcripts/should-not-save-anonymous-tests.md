This transcript tests that UCM does not allow for anonymous tests and provides a helpful error message.

Fix for [2520](https://github.com/unisonweb/unison/issues/2520)

```ucm
.> builtins.merge
.> cd builtin
```

```unison
test> [Ok "Good"]
```

```ucm
.> add
```

The test should not be added and a helpful error should be provided to explain why.
