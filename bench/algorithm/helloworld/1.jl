
n = length(ARGS) > 0 ? getindex(ARGS, 1) : ""
str = "Hello world $(n)!"
println(str)
