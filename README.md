# daytripper

Helpers for round-trip tests

You can control some parameters through Tasty:

`--daydripper-write-missing` or `TASTY_DAYTRIPPER_WRITE_MISSING=True` will write golden files if not found.

`--falsify-tests=N` or `TASTY_FALSIFY_TESTS=N` will set the number of tests per property.
