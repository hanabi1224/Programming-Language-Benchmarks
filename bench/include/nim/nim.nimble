# Package

version       = "0.1.0"
author        = "hanabi1224"
description   = "bencher"
license       = "MIT"
srcDir        = "."
bin           = @["app"]

# Dependencies

requires "nim >= 1.6.0", "nimbitarray", "https://github.com/nim-lang/nim-bigints"
