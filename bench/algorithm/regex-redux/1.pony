// Based on Python code
use "files"

primitive StripFasta
  fun apply(s: String val): String val ? =>
    let a = s.array()
    let out = recover iso Array[U8] end
    var i: USize = 0
    while i < s.size() do
      let c = a(i)?
      if c == U8('>') then
        // skip until end of line (inclusive)
        i = i + 1
        while (i < s.size()) and (a(i)? != U8('\n')) do i = i + 1 end
        if i < s.size() then i = i + 1 end
      elseif c == U8('\n') then
        i = i + 1
      else
        out.push(c)
        i = i + 1
      end
    end
    recover val String.from_array(consume out) end// Very small matcher for the benchmark patterns:
// supports concatenation, character classes like [acg], and alternation with '|'.
primitive SimplePattern
  fun count_in(s: String val, pat: String val): USize ? =>
    var total: USize = 0
    var i: USize = 0
    while i < s.size() do
      let len = _match_len(s, i, pat)?
      if len > 0 then
        total = total + 1
        i = i + len
      else
        i = i + 1
      end
    end
    total

  fun _match_len(s: String val, off: USize, pat: String val): USize ? =>
    for alt in pat.split_by("|").values() do
      let l = _match_seq_len(s, off, alt)?
      if l > 0 then return l end
    end
    0

  fun _match_seq_len(s: String val, off: USize, p: String val): USize ? =>
    var i: USize = off
    var j: USize = 0
    let sa = s.array()
    let pa = p.array()
    while j < p.size() do
  let pc = pa(j)?
  if pc == U8('[') then
        j = j + 1
        var matched: Bool = false
  while (j < p.size()) and (pa(j)? != U8(']')) do
          if (i < s.size()) and (sa(i)? == pa(j)?) then matched = true end
          j = j + 1
        end
  if (j >= p.size()) or (pa(j)? != U8(']')) then return 0 end
        if not matched then return 0 end
        i = i + 1
        j = j + 1
      else
        if (i >= s.size()) or (sa(i)? != pc) then return 0 end
        i = i + 1
        j = j + 1
      end
    end
    i - off

// Replace tHa[Nt] -> <4>
primitive Replace1
  fun apply(s: String val): String val ? =>
    let a = s.array()
    let out = recover iso Array[U8] end
    var i: USize = 0
    while i < s.size() do
      if (i + 3) < s.size() then
        if (a(i)? == U8('t')) and (a(i+1)? == U8('H')) and (a(i+2)? == U8('a')) then
          let c3 = a(i+3)?
          if (c3 == U8('N')) or (c3 == U8('t')) then
            out.push(U8('<')); out.push(U8('4')); out.push(U8('>'))
            i = i + 4
            continue
          end
        end
      end
      out.push(a(i)?)
      i = i + 1
    end
    recover val String.from_array(consume out) end

// Replace aND|caN|Ha[DS]|WaS -> <3>
primitive Replace2
  fun apply(s: String val): String val ? =>
    let a = s.array()
    let out = recover iso Array[U8] end
    var i: USize = 0
    while i < s.size() do
      var matched = false
      // aND
      if (i + 2) < s.size() then
        if (a(i)? == U8('a')) and (a(i+1)? == U8('N')) and (a(i+2)? == U8('D')) then
          out.push(U8('<')); out.push(U8('3')); out.push(U8('>'))
          i = i + 3
          matched = true
        end
      end
      // caN
      if (not matched) and ((i + 2) < s.size()) then
        if (a(i)? == U8('c')) and (a(i+1)? == U8('a')) and (a(i+2)? == U8('N')) then
          out.push(U8('<')); out.push(U8('3')); out.push(U8('>'))
          i = i + 3
          matched = true
        end
      end
      // Ha[DS]
      if (not matched) and ((i + 2) < s.size()) then
        if (a(i)? == U8('H')) and (a(i+1)? == U8('a')) then
          let c2 = a(i+2)?
          if (c2 == U8('D')) or (c2 == U8('S')) then
            out.push(U8('<')); out.push(U8('3')); out.push(U8('>'))
            i = i + 3
            matched = true
          end
        end
      end
      // WaS
      if (not matched) and ((i + 2) < s.size()) then
        if (a(i)? == U8('W')) and (a(i+1)? == U8('a')) and (a(i+2)? == U8('S')) then
          out.push(U8('<')); out.push(U8('3')); out.push(U8('>'))
          i = i + 3
          matched = true
        end
      end
      if not matched then
        out.push(a(i)?)
        i = i + 1
      end
    end
    recover val String.from_array(consume out) end

// Replace a[NSt]|BY => <2>
primitive Replace3
  fun apply(s: String val): String val ? =>
    let a = s.array()
    let out = recover iso Array[U8] end
    var i: USize = 0
    while i < s.size() do
      var matched = false
      // a[NSt]
      if (i + 1) < s.size() then
        if a(i)? == U8('a') then
          let c1 = a(i+1)?
          if (c1 == U8('N')) or (c1 == U8('S')) or (c1 == U8('t')) then
            out.push(U8('<')); out.push(U8('2')); out.push(U8('>'))
            i = i + 2
            matched = true
          end
        end
      end
      // BY
      if (not matched) and ((i + 1) < s.size()) then
        if (a(i)? == U8('B')) and (a(i+1)? == U8('Y')) then
          out.push(U8('<')); out.push(U8('2')); out.push(U8('>'))
          i = i + 2
          matched = true
        end
      end
      if not matched then
        out.push(a(i)?)
        i = i + 1
      end
    end
    recover val String.from_array(consume out) end

// Replace <[^>]*> => |
primitive Replace4
  fun apply(s: String val): String val ? =>
    let a = s.array()
    let out = recover iso Array[U8] end
    var i: USize = 0
    while i < s.size() do
      if a(i)? == U8('<') then
        // replace until next '>' (if any)
        out.push(U8('|'))
        i = i + 1
        while (i < s.size()) and (a(i)? != U8('>')) do i = i + 1 end
        if i < s.size() then i = i + 1 end
      else
        out.push(a(i)?)
        i = i + 1
      end
    end
    recover val String.from_array(consume out) end

// Replace \|[^|][^|]*\| => -
primitive Replace5
  fun apply(s: String val): String val ? =>
    let a = s.array()
    let out = recover iso Array[U8] end
    var i: USize = 0
    while i < s.size() do
      if a(i)? == U8('|') then
        // Look for pattern: |<at least one non-pipe><zero or more non-pipes>|
        if (i + 2) < s.size() then  // Need at least |x|
          let n1 = a(i+1)?
          if n1 != U8('|') then  // First char after | must not be |
            // Find the closing |
            var k = i + 2
            while (k < s.size()) and (a(k)? != U8('|')) do k = k + 1 end
            if (k < s.size()) and (a(k)? == U8('|')) then
              // Found complete pattern |...| with at least one non-pipe char
              out.push(U8('-'))
              i = k + 1
              continue
            end
          end
        end
      end
      out.push(a(i)?)
      i = i + 1
    end
    recover val String.from_array(consume out) end

actor Main
  new create(env: Env) =>
    // Read input file arg or default to 25000_in as in bench.
    let file_name = try env.args(1)? else "25000_in" end

    // Load file contents
    let auth = env.root
    let file_auth = FileAuth(auth)
    let path = FilePath(file_auth, file_name)
  let f = File(path)
  if not f.valid() then
      env.out.print("")
      return
    end
  var buf = recover iso String end
    while f.valid() do
      let chunk = f.read_string(1 << 16)
      if chunk.size() == 0 then break end
      buf = (consume buf) + consume chunk
    end
  let input_str: String val = consume buf
    let input_len = input_str.size()

    // Strip headers and newlines
    let sequence: String val = try StripFasta(input_str)? else return end
    let sequence_len = sequence.size()

  // Count variants
  var variants = Array[String]
  variants.push("agggtaaa|tttaccct")
  variants.push("[cgt]gggtaaa|tttaccc[acg]")
  variants.push("a[act]ggtaaa|tttacc[agt]t")
  variants.push("ag[act]gtaaa|tttac[agt]ct")
  variants.push("agg[act]taaa|ttta[agt]cct")
  variants.push("aggg[acg]aaa|ttt[cgt]ccct")
  variants.push("agggt[cgt]aa|tt[acg]accct")
  variants.push("agggta[cgt]a|t[acg]taccct")
  variants.push("agggtaa[cgt]|[acg]ttaccct")

  for pat in variants.values() do
      let c = try SimplePattern.count_in(sequence, pat)? else 0 end
      env.out.print(pat + " " + c.string())
    end

    // Sequential substitutions
    var cur: String val = sequence
    cur = try Replace1(cur)? else return end
    cur = try Replace2(cur)? else return end
    cur = try Replace3(cur)? else return end
    cur = try Replace4(cur)? else return end
    cur = try Replace5(cur)? else return end

    env.out.print("")
    env.out.print(input_len.string())
    env.out.print(sequence_len.string())
    env.out.print(cur.size().string())
