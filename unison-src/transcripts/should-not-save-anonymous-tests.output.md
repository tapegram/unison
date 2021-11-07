This transcript tests that UCM does not allow for anonymous tests and provides a helpful error message.

Fix for [2520](https://github.com/unisonweb/unison/issues/2520)

```ucm
.> builtins.merge

  Done.

```
```unison
use .base
```

```ucm

  I loaded scratch.u and didn't find anything.

```
```ucm
.> add

  

```
The test should not be added and a helpful error should be provided to explain why.
