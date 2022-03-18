use "format"

actor Main
  new create(env: Env) =>
    let n = try env.args(1)? else "" end
    env.out.print("Hello world " + n + "!")
