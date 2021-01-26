let pkg = import ./. {};
in pkg.hmacaroons.checks.test.buildInputs ++ pkg.hmacaroons.components.library.buildInputs
