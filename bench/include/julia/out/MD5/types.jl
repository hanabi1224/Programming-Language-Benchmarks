mutable struct MD5_CTX <: SHA_CTX
    state::Array{UInt32,1}
    bytecount::UInt64
    buffer::Array{UInt8,1}
end

digestlen(::Type{MD5_CTX}) = 16
state_type(::Type{MD5_CTX}) = UInt32
# blocklen is the number of bytes of data processed by the transform!() function at once
blocklen(::Type{MD5_CTX}) = UInt64(64)




MD5_CTX() = MD5_CTX(copy(MD5_initial_hash_value), 0, zeros(UInt8, blocklen(MD5_CTX)))
Base.copy(ctx::T) where {T<:MD5_CTX} = T(copy(ctx.state), ctx.bytecount, copy(ctx.buffer))
Base.show(io::IO, ::MD5_CTX) = write(io, "MD5 hash state")