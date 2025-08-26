// based on 1.go implementation
primitive _NoneI64

class Node
  var _hash: I64 = 0
  var _has_hash: Bool = false
  var _left: (Node | None)
  var _right: (Node | None)

  new create(depth: USize) =>
    if depth > 0 then
      let d = depth - 1
      _left = Node(d)
      _right = Node(d)
    else
      _left = None
      _right = None
    end

  fun ref cal_hash() =>
    if _has_hash then return end
    match _left
    | None =>
      // leaf
      _hash = 1
      _has_hash = true
      return
    | let l: Node => l.cal_hash()
    end
    match _right
    | let r: Node => r.cal_hash()
    | None => None
    end
    let hl: I64 = match _left | let l': Node => l'.get_hash() | None => 0 end
    let hr: I64 = match _right | let r': Node => r'.get_hash() | None => 0 end
    _hash = hl + hr
    _has_hash = true

  fun box get_hash(): I64 =>
    if _has_hash then _hash else 0 end

  fun box check(): Bool =>
    if not _has_hash then
      false
    elseif (_left is None) and (_right is None) then
      true
    else
      var ok: Bool = true
      match _left
      | let l: Node box =>
        if not l.check() then ok = false end
      | None => None
      end
      match _right
      | let r: Node box =>
        if not r.check() then ok = false end
      | None => None
      end
      ok
    end

actor Main
  new create(env: Env) =>
    let min_depth: USize = 4
    let n: USize = try env.args(1)?.usize()? else 10 end
    let max_depth = if (min_depth + 2) > n then min_depth + 2 else n end

    let stretch_depth = max_depth + 1
  let stretch_tree = recover iso Node(stretch_depth) end
    stretch_tree.cal_hash()
    env.out.print("stretch tree of depth " + stretch_depth.string()
      + "\t root hash: " + stretch_tree.get_hash().string()
      + " check: " + (if stretch_tree.check() then "true" else "false" end))

    let long_lived_tree = Node(max_depth)

    var depth = min_depth
    while depth <= max_depth do
      let iterations: USize = USize(1).shl((max_depth - depth) + min_depth)
      var sum: I64 = 0
      var i: USize = 0
      while i < iterations do
        let t = recover iso Node(depth) end
        t.cal_hash()
        sum = sum + t.get_hash()
        i = i + 1
      end
      env.out.print(iterations.string() + "\t trees of depth " + depth.string()
        + "\t root hash sum: " + sum.string())
      depth = depth + 2
    end

    long_lived_tree.cal_hash()
    env.out.print("long lived tree of depth " + max_depth.string()
      + "\t root hash: " + long_lived_tree.get_hash().string()
      + " check: " + (if long_lived_tree.check() then "true" else "false" end))
