# TTADB

```bash
$ cabal --version
cabal-install version 3.10.2.0
compiled using version 3.10.2.1 of the Cabal library 

$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.2.7
```

```bash
$ cabal run
Usage: ttadb [--port INT] --db PATH --static-dir PATH [--req-logger LOGGER]

  To-Do List that talks to a Database

Available options:
  -h,--help                Show this help text
  --port INT               Port to listen on
  --db PATH                Path to database (ex: ttadb.db)
  --static-dir PATH        Path to static directory (js,css,etc)
  --req-logger LOGGER      Logging style: [dev|apache]
```

```bash
$ cabal run -- ttadb --db ../VAULT/ttadb.db --static-dir static/
Resolving dependencies...
...
Setting phasers to stun... (port 4242) (ctrl-c to quit)
```

open browser to, `http://localhost:4242/`
