use "collections"

class LCG
  let a: U32 = 1103515245
  let c: U32 = 12345
  let m: U32 = 1 << 31

  var seed: U32

  new create(seed': U32) =>
    seed = seed'

  fun ref next(): U32 => 
    seed = ((a * seed) + c) % m
    seed

class LRU
  let env: Env
  let size: USize
  let keys: Map[U32, ListNode[(U32, U32)]]
  let entries: List[(U32, U32)]

  let pool: List[(U32, U32)] = List[(U32, U32)]()
  let empty_node: ListNode[(U32, U32)] = ListNode[(U32, U32)]((0, 0))

  new create(env': Env, size': U32) =>
    env = env'
    size = size'.usize()
    keys = Map[U32, ListNode[(U32, U32)]](size)
    entries = List[(U32, U32)]()    

  fun ref get(key: U32): (U32 | None) =>
    if keys.contains(key) then
      let node = keys.get_or_else(key, empty_node)
      try
        let pair = node.apply()?
        node.remove()
        entries.append_node(node)
        pair._2
      else
        None
      end
    else
      None
    end
  
  fun ref put(key: U32, value: U32) =>    
    if keys.contains(key) then
      let node = keys.get_or_else(key, empty_node)
      node.remove()
      try 
        node.update((key, value))?
      end
      entries.append_node(node)
    else
      var new_node: ListNode[(U32, U32)]
      if pool.size() > 0 then
        try
          new_node = pool.remove(0)?
          new_node.update((key, value))?
        else
          // env.out.print("new node 1")
          new_node = ListNode[(U32, U32)]((key, value))
        end
      else
        // env.out.print("new node 2")
        new_node = ListNode[(U32, U32)]((key, value))
      end
      if entries.size() >= size then
        try
          let head = entries.remove(0)?
          pool.append_node(head)
          let pair = head.apply()?
          keys.remove(pair._1)?
        end
        // env.out.print("*k:" + key.string() + ", v:" + value.string() + ", keys:" + keys.size().string() + ", entries:" + entries.size().string())
      end
      entries.append_node(new_node)
      keys.insert(key, new_node)
    end
    // env.out.print("k:" + key.string() + ", v:" + value.string() + ", keys:" + keys.size().string() + ", entries:" + entries.size().string())

actor Main
  let env: Env

  new create(env': Env) =>
    env = env'

    let size = try
      env.args(1)?.u32()?
    else
      100
    end

    let n = try
      env.args(2)?.usize()?
    else
      100
    end

    let mod = size * 10

    let rng0 = LCG(0)
    let rng1 = LCG(1)
    let lru = LRU(env, size)

    var hit: USize = 0
    var missed: USize = 0
    var i: USize = 0
    while i < n do
      let n0 = rng0.next() % mod
      lru.put(n0, n0)
      let n1 = rng1.next() % mod
      if lru.get(n1) is None then
        missed = missed + 1
      else
        hit = hit + 1
      end
      // print("i:" + i.string() + ", hit:" + hit.string() + ", missed:" + missed.string() + ", n0:" + n0.string() + ", n1:" + n1.string())
      i = i + 1
    end

    print(hit.string())
    print(missed.string())

  fun print(s: String) =>
    env.out.print(s)
