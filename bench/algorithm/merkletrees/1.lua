local function MakeTree(depth)
    if depth > 0 then
        depth = depth - 1
        return {
            left = MakeTree(depth),
            right = MakeTree(depth)
        }
    else
        return {
            value = 1
        }
    end
end

local function Check(tree)
    if tree.hash then
        if tree.value then
            return true
        else
            return Check(tree.left) and Check(tree.right)
        end
    else
        return false
    end
end

local function CalHash(tree)
    if not tree.hash then
        if tree.value then
            tree.hash = tree.value
        else
            CalHash(tree.left)
            CalHash(tree.right)
            tree.hash = tree.left.hash + tree.right.hash
        end
    end
end

local N = tonumber(arg and arg[1]) or 6
local mindepth = 4
local maxdepth = mindepth + 2
if maxdepth < N then
    maxdepth = N
end

do
    local stretchdepth = maxdepth + 1
    local stretchtree = MakeTree(stretchdepth)
    CalHash(stretchtree)
    io.write(string.format("stretch tree of depth %d\t root hash: %d check: %s\n", stretchdepth, stretchtree.hash,
        Check(stretchtree)))
end

local longlivedtree = MakeTree(maxdepth)

for depth = mindepth, maxdepth, 2 do
    local iterations = 2 ^ (maxdepth - depth + mindepth)
    local sum = 0
    for i = 1, iterations do
        local t = MakeTree(depth)
        CalHash(t)
        sum = sum + t.hash
    end
    io.write(string.format("%d\t trees of depth %d\t root hash sum: %d\n", iterations, depth, sum))
end

CalHash(longlivedtree)
io.write(string.format("long lived tree of depth %d\t root hash: %d check: %s\n", maxdepth, longlivedtree.hash,
    Check(longlivedtree)))
