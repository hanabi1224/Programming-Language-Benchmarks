using Pkg
Pkg.add("PackageCompiler")
using PackageCompiler
create_app("out/App", "compiled", incremental=true, force=true)
