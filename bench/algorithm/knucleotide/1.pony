// Based on the OCaml (3.ml) and C implementations in this repo; single-threaded;
use "collections"
use "files"

primitive Nuc
  fun code(u: U8): U8 =>
    // map both cases to 2-bit codes: a=0, t=1, c=2, g=3; others -> 255
    match u
    | U8('A') | U8('a') => 0
    | U8('T') | U8('t') => 1
    | U8('C') | U8('c') => 2
    | U8('G') | U8('g') => 3
    else 255 end

  fun chr(c: U8): U8 =>
    // reverse map for printing (uppercase)
    match c
    | 0 => U8('A')
    | 1 => U8('T')
    | 2 => U8('C')
    else U8('G') end

primitive IO
  fun read_all(env: Env, file_name: String val): String val =>
    let auth = env.root
    let file_auth = FileAuth(auth)
    let path = FilePath(file_auth, file_name)
    let f = File(path)
    if not f.valid() then
      ""
    else
      var buf = recover iso String end
      while f.valid() do
        let chunk = f.read_string(1 << 16)
        if chunk.size() == 0 then break end
        buf = (consume buf) + consume chunk
      end
      consume buf
    end

  fun three_sequence(input: String val): Array[U8] val ? =>
    // Extract only the sequence after a header line starting with ">THREE"
    let a = input.array()
    let n = input.size()
    var i: USize = 0
    var found: Bool = false
    // Seek to a line starting with '>THREE'
    while i < n do
      if a(i)? == U8('>') then
        // read line start and check for "THREE"
        var j = i + 1
        if (j + 4) < n then
          if (a(j)? == U8('T')) and (a(j+1)? == U8('H')) and (a(j+2)? == U8('R')) and (a(j+3)? == U8('E')) and (a(j+4)? == U8('E')) then
            // skip to end of line
            j = j + 5
            while (j < n) and (a(j)? != U8('\n')) do j = j + 1 end
            if j < n then j = j + 1 end
            i = j
            found = true
            break
          end
        end
      end
      // skip to next line
      while (i < n) and (a(i)? != U8('\n')) do i = i + 1 end
      if i < n then i = i + 1 end
    end
    let out = recover iso Array[U8] end
    if not found then
      consume out
    else
      // read sequence until next '>' or EOF; ignore non-ATCG
      while i < n do
        let b = a(i)?
        if b == U8('>') then break end
        let c = Nuc.code(b)
        if c != 255 then out.push(c) end
        i = i + 1
      end
      consume out
    end

primitive Pack
  fun pack_seq(seq: Array[U8] val, off: USize, k: USize): U64 ? =>
    var key: U64 = 0
    var i: USize = 0
    while i < k do
      key = (key << 2) or seq(off + i)?.u64()
      i = i + 1
    end
    key

  fun pack_str(s: String val): (U64, USize) ? =>
    let a = s.array()
    let k = s.size()
    var key: U64 = 0
    var i: USize = 0
    while i < k do
      let c = Nuc.code(a(i)?)
      key = (key << 2) or c.u64()
      i = i + 1
    end
    (key, k)

  fun unpack(key: U64, k: USize): String val ? =>
    let out = recover iso Array[U8](k) end
    var tmp: U64 = key
    var i: USize = 0
    // produce reversed then reverse into correct order
    let rev = recover iso Array[U8](k) end
    while i < k do
      let c = (tmp and 3).u8()
      rev.push(Nuc.chr(c))
      tmp = tmp >> 2
      i = i + 1
    end
    // reverse rev into out
    var j: USize = 0
    while j < k do
      out.push(rev(k - 1 - j)?)
      j = j + 1
    end
    String.from_array(consume out)

primitive KMers
  fun counts_array(seq: Array[U8] val, k: USize): Array[USize] val ? =>
    let len = if k >= 32 then USize.max_value() else (USize(1) << (k << 1)) end
    let out = recover iso Array[USize](len) end
    if (k == 0) or (seq.size() < k) then
      // fill zeros to length
      var i: USize = 0
      while i < len do out.push(0); i = i + 1 end
      consume out
    else
      // initialize counts with zeros
      var i: USize = 0
      while i < len do out.push(0); i = i + 1 end
      let mask: U64 = if k >= 32 then U64.max_value() else ((U64(1) << (k.u64() * 2)) - U64(1)) end
  var key: U64 = Pack.pack_seq(seq, 0, k)?
      out(key.usize())? = out(key.usize())? + 1
      i = k
      while i < seq.size() do
        key = ((key << 2) or seq(i)?.u64()) and mask
        let idx = key.usize()
        out(idx)? = out(idx)? + 1
        i = i + 1
      end
      consume out
    end

  fun count_pattern(seq: Array[U8] val, key: U64, k: USize): USize ? =>
    if (k == 0) or (seq.size() < k) then return 0 end
    let mask: U64 = if k >= 32 then U64.max_value() else ((U64(1) << (k.u64() * 2)) - U64(1)) end
  var rolling: U64 = Pack.pack_seq(seq, 0, k)?
    var cnt: USize = if rolling == key then 1 else 0 end
    var i: USize = k
    while i < seq.size() do
      rolling = ((rolling << 2) or seq(i)?.u64()) and mask
      if rolling == key then cnt = cnt + 1 end
      i = i + 1
    end
    cnt

primitive Sorter
  fun sort_freq(a: Array[(String val, F64)] ref) ? =>
    // insertion sort: small arrays (4 or 16 elements)
    var i: USize = 1
    while i < a.size() do
      let key = a(i)?
  var j: ISize = i.isize() - 1
  while j >= ISize(0) do
        let cur = a(j.usize())?
        if (cur._2 < key._2) or ((cur._2 == key._2) and (cur._1 > key._1)) then
          a(j.usize() + 1)? = cur
          j = j - 1
        else
          break
        end
      end
      a((j + 1).usize())? = key
      i = i + 1
    end

primitive Format
  fun fixed3(x: F64): String val =>
    var y = x * 1000.0
    if y < 0 then y = 0 end
    let n = (y + 0.5).floor().u64()
    let ip = (n / 1000).u64()
    let fp = (n % 1000).u64()
  let out = recover iso String end
  out.append(ip.string())
  out.push(U8('.'))
  out.push(((fp / 100).u8()) + U8('0'))
  out.push((((fp / 10) % 10).u8()) + U8('0'))
  out.push(((fp % 10).u8()) + U8('0'))
  recover val consume out end

actor Main
  new create(env: Env) =>
    try
      let file_name = try env.args(1)? else "25000_in" end
      let input = IO.read_all(env, file_name)
      let seq = IO.three_sequence(input)?
      let seqlen = seq.size()

      // Frequencies for k=1, k=2
      let ks: Array[USize] val = [USize(1); USize(2)]
      for k in ks.values() do
        let counts = KMers.counts_array(seq, k)?
        let total: F64 = ((seqlen - k) + 1).f64()
        let items = Array[(String val, F64)]
        var idx: USize = 0
        while idx < counts.size() do
          let c = counts(idx)?
          if c > 0 then
            let s = Pack.unpack(idx.u64(), k)?
            items.push((s, (c.f64() * 100.0) / total))
          end
          idx = idx + 1
        end
        Sorter.sort_freq(items)?
        for it in items.values() do
          let line = recover iso String end
          line.append(it._1)
          line.push(U8(' '))
          line.append(Format.fixed3(it._2))
          env.out.print(consume line)
        end
        env.out.print("")
      end

      // Specific pattern counts
      let patterns: Array[String] val =
        [ "GGT"; "GGTA"; "GGTATT"; "GGTATTTTAATT"; "GGTATTTTAATTTATAGT" ]
      for p in patterns.values() do
        let packed = Pack.pack_str(p)?
        let key: U64 = packed._1
        let k: USize = packed._2
        let c = KMers.count_pattern(seq, key, k)?
        let line2 = recover iso String end
        line2.append(c.string())
        line2.push(U8('\t'))
        line2.append(p)
        env.out.print(consume line2)
      end
    end
