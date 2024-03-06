# Revision history for ttadb

## 0.1.1.0 -- 2024-03-06

* Fix for Scotty not passing Exceptions though: wrap HTTP verbs
* Thx to John for the solution

```bash
$ cabal run -- ttadb --db ttadb.db 
Setting phasers to stun... (port 4242) (ctrl-c to quit)
"I'm processing a GET!"
ConversionFailed {errSQLType = "NULL", errHaskellType = "Bool", errMessage = "expecting an SQLInteger column type"}
```

## 0.1.0.0 -- 2023-10-03 

* First version. Released on an unsuspecting world.
