function real_main()
    n = length(ARGS) > 0 ? ARGS[1] : ""
    str = "Hello world $(n)!"
    println(str)
end

if abspath(PROGRAM_FILE) == @__FILE__
    real_main()
end
