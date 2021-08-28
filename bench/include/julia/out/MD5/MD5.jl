module MD5


using SHA
using SHA: lrot, SHA_CTX
import SHA: update!, digest!, transform!, pad_remainder!, buffer_pointer
import SHA: blocklen, state_type, digestlen, short_blocklen
export md5

include("constants.jl")
include("types.jl")
include("core.jl")


# Our basic function is to process arrays of bytes
function md5(data::T) where T<:Union{AbstractVector{UInt8}, NTuple{N,UInt8} where N}
    ctx = MD5_CTX()
    update!(ctx, data)
    return digest!(ctx)
end


# AbstractStrings are a pretty handy thing to be able to crunch through
md5(str::AbstractString) = md5(codeunits(str))

# Convenience function for IO devices, allows for things like:
# open("test.txt") do f
#     sha256(f)
# done
function md5(io::IO, chunk_size=4*1024)
    ctx = MD5_CTX()
    buff = Vector{UInt8}(undef, chunk_size)
    while !eof(io)
        num_read = readbytes!(io, buff)
        update!(ctx, buff[1:num_read])
    end
    return digest!(ctx)
end





end # module