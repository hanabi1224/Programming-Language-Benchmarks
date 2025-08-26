// Based on the OCaml version from "The Computer Language Benchmarks Game"
// https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

use "collections"

actor Main
  new create(env: Env) =>
    let n: USize = try env.args(1)?.usize()? else 7 end
    Fannkuch(env, n)

class Fannkuch
  let _env: Env
  var _perm: Array[USize]
  var _copy: Array[USize]
  var _max_flips: USize = 0
  var _checksum: ISize = 0
  var _perm_count: USize = 0
  let _n: USize

  new create(env: Env, n: USize) =>
    _env = env
    _n = n
    _perm = recover Array[USize](n) end
    _copy = recover Array[USize](n) end

    for i in Range[USize](0, n) do
      _perm.push(i)
    end

    run()
    print_results()

  fun ref run() =>
    // This is a direct translation of the recursive permutation
    // generation from the OCaml version.
    do_iter(_n)

  fun ref do_iter(ht: USize) =>
    if ht == 1 then
      // Process the permutation
      _copy.clear()
      for i in Range[USize](0, _perm.size()) do
        try _copy.push(_perm(i)?) end
      end
      
      let flips = count_flips(_copy)
      
      // Update checksum: add for even permutations, subtract for odd
      if (_perm_count % 2) == 0 then
        _checksum = _checksum + flips.isize()
      else
        _checksum = _checksum - flips.isize()
      end

      if flips > _max_flips then
        _max_flips = flips
      end
      
      _perm_count = _perm_count + 1
    else
      for i in Range[USize](0, ht) do
        do_iter(ht - 1)
        // Rotate the first `ht` elements
        if ht > 1 then
          try
            let t = _perm(0)?
            for j in Range[USize](1, ht) do
              _perm(j-1)? = _perm(j)?
            end
            _perm(ht-1)? = t
          end
        end
      end
    end

  fun ref count_flips(p: Array[USize]): USize =>
    var flips: USize = 0
    try
      var first = p(0)?
      while first != 0 do
        // Flip the first `first + 1` elements
        for i in Range[USize](0, (first / 2) + 1) do
          let k = first - i
          let t = p(i)?
          p(i)? = p(k)?
          p(k)? = t
        end
        flips = flips + 1
        first = p(0)?
      end
    end
    flips

  fun ref print_results() =>
    _env.out.print(_checksum.string())
    _env.out.print("Pfannkuchen(" + _n.string() + ") = " + _max_flips.string())
