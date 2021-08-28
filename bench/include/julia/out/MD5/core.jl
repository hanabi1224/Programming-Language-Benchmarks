let body = quote end
    ex  = quote
        pbuf = buffer_pointer(context)
        @inbounds A = context.state[1]
        @inbounds B = context.state[2]
        @inbounds C = context.state[3]
        @inbounds D = context.state[4]
    end
    push!(body.args, ex)
    for i in 0:63
        if 0 ≤ i ≤ 15
            ex = :(F = (B & C) | ((~B) & D))
            g = i
        elseif 16 ≤ i ≤ 31
            ex = :(F = (D & B) | ((~D) & C))
            g = 5i + 1
        elseif 32 ≤ i ≤ 47
            ex = :(F = B ⊻ C ⊻ D)
            g = 3i + 5
        elseif 48 ≤ i ≤ 63
            ex = :(F = C ⊻ (B | (~D)))
            g = 7i
        end
        push!(body.args, ex)
        g = (g % 16) + 1
        ex = quote
            temp = D
            D = C
            C = B
            inner = A + F + $(kk[i+1]) + unsafe_load(pbuf, $g)
            rot_inner = lrot($(ss[i+1]), inner, 32)
            B = B + rot_inner
            A = temp
        end
        push!(body.args, ex)
    end

    ex = quote
        @inbounds context.state[1] += A
        @inbounds context.state[2] += B
        @inbounds context.state[3] += C
        @inbounds context.state[4] += D
    end
    push!(body.args, ex)

    @eval function transform!(context::MD5_CTX)
        $body
    end
end

function digest!(context::T) where {T<:MD5_CTX}
    pad_remainder!(context)

    bitcount_idx = div(short_blocklen(T), sizeof(context.bytecount)) + 1
    pbuf = Ptr{typeof(context.bytecount)}(pointer(context.buffer))
    unsafe_store!(pbuf, 8context.bytecount, bitcount_idx)

    # Final transform:
    transform!(context)

    # ctx has been mutated
    reinterpret(UInt8, context.state)
end