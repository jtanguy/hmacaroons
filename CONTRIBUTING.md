# How to contribute

First and most important: Submit issues !

If you want to contribute, please fork the repo, and send us a pull request with
your changes.

## Tests and cabal flags

For this project we use the standard hunit/quickcheck tests.
If you want to submit a new feature, please try to integrate at least
*some* tests.

## Code style

We do not impose any specific style of code, but you can improve your code
by running it through `hlint`.

## Crypto analysis

This library has **not** been audited by security experts !
It relies on an existing implementation and crypto libraries.

Any analysis is welcome, and I will gladly merge it.

## Nix support

It is a stack project and we use [haskell.nix] to bring nix support derived from
the cabal and stack files.

If you want to build the project via nix you can call:

```bash
nix-build -A hmacaroons.checks.test
```

### Update the stack resolver

If you're updating the stack resolver, you'll have to update some nix files
meant to track the dependencies avoid rebuilding everything from the sources at
each build.

For instance, updating the resolver from 16.31 to 17.0, you'll get an error:

```
--- /nix/store/4pkfzi5fnkrw8qax9hfygbidx9x4x653-materialized/default.nix        1970-01-01 00:00:01.000000000 +0000
+++ /nix/store/vxy1gb3nz9ml7sbszbyg4d2nmz8v9r15-hmacaroons-stack-to-nix-pkgs/default.nix        1970-01-01 00:00:01.000000000 +0000
@@ -6,6 +6,6 @@
         hex = ./.stack-to-nix.cache.0;
         };
       };
-  resolver = "lts-16.31";
+  resolver = "lts-17.0";
   modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
   }
\ No newline at end of file
Materialized nix used for hmacaroons-stack-to-nix-pkgs incorrect. To fix run: /nix/store/pn7q30j4l0q1b5cigxp2s23bqc03b6ar-updateMaterialized
builder for '/nix/store/4hwyw7yvijjbxzf2cgqjz7gywr4g7h4m-hmacaroons-stack-to-nix-pkgs.drv' failed with exit code 1
error: build of '/nix/store/4hwyw7yvijjbxzf2cgqjz7gywr4g7h4m-hmacaroons-stack-to-nix-pkgs.drv' failed
```

The simple way to resolve this is to run the script advised in the output:

```bash
$ /nix/store/pn7q30j4l0q1b5cigxp2s23bqc03b6ar-updateMaterialized
```

It updates the materialized/ this way:

```diff
diff --git a/materialized/default.nix b/materialized/default.nix
index c36089d..ccbbeee 100644
--- a/materialized/default.nix
+++ b/materialized/default.nix
@@ -6,6 +6,6 @@
         hex = ./.stack-to-nix.cache.0;
         };
       };
-  resolver = "lts-16.31";
+  resolver = "lts-17.0";
   modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
   }
\ No newline at end of file
diff --git a/stack.yaml b/stack.yaml
index 989e1bb..75c82b2 100644
--- a/stack.yaml
+++ b/stack.yaml
@@ -18,7 +18,7 @@
 #
 # resolver: ./custom-snapshot.yaml
 # resolver: https://example.com/snapshots/2018-01-01.yaml
-resolver: lts-16.31
+resolver: lts-17.0

 # User packages to be built.
 # Various formats can be used as shown in the example below.
```

### Unknown stack resolver

If you update to a recent stack resolver, you might have such error of [haskell.nix]
not being aware of it:

```
error: This version of stackage.nix does not know about the Stackage resolver lts-16.31.
You may need to update haskell.nix to one that includes a newer stackage.nix.
```

[haskell.nix] known version is tracked in the `nix/` directory via [niv]. You can simply update this dependency with:

```bash
$ nix-shell nix/shell.nix --command "niv update haskell.nix"
Update haskell.nix
Done: Update haskell.nix
```

This will udpate the `nix/sources.json` file:

```diff
diff --git a/nix/sources.json b/nix/sources.json
index a1cfb41..d53cd52 100644
--- a/nix/sources.json
+++ b/nix/sources.json
@@ -5,10 +5,10 @@
         "homepage": "https://input-output-hk.github.io/haskell.nix",
         "owner": "input-output-hk",
         "repo": "haskell.nix",
-        "rev": "7e49dd7f126210d8bfbde2dda93cf6d72d7b3e2e",
-        "sha256": "076jk9x5y1g37bn2afxrcijf1xf1yinljh8dbzv553i43l989k5n",
+        "rev": "ef4aef4ce2060dc1a41b2690df1f54f986e0f9ab",
+        "sha256": "0537fbjh4mcnywa33h4hl135kw7i8c0j8qndyzv5i82j7mc8wjvs",
         "type": "tarball",
-        "url": "https://github.com/input-output-hk/haskell.nix/archive/7e49dd7f126210d8bfbde2dda93cf6d72d7b3e2e.tar.gz",
+        "url": "https://github.com/input-output-hk/haskell.nix/archive/ef4aef4ce2060dc1a41b2690df1f54f986e0f9ab.tar.gz",
         "url_template": "https://github.com/<owner>/<repo>/archive/<rev>.tar.gz"
     }
 }
```

[haskell.nix]: https://input-output-hk.github.io/haskell.nix/
[niv]: https://github.com/nmattia/niv#niv
