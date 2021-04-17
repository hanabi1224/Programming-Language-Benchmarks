local function generate() 
    return coroutine.create(function()
        local i = 2
        while true do
            coroutine.yield(i)
            i = i + 1
        end
    end)
end

local function filter(co, prime)
    return coroutine.create(function()
        while true do 
            local ok, i = coroutine.resume(co)
            if (not ok) then
                print(i)
                return
            end
            if (i % prime ~= 0) then
                coroutine.yield(i)
            end
        end
    end)
end

local n = tonumber(arg and arg[1]) or 1000
co = generate()
for i=1,n do
    local ok, prime = coroutine.resume(co)
    print(prime)
    co = filter(co, prime)
end
