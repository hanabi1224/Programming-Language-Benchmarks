# Note: this code more or less comes directly from https://en.wikipedia.org/wiki/MD5
# I believe this is fair use. and does not have license implications

const kk =  floor.(UInt32, Int64(2)^32 * abs.(sin.(1:64)))

const ss = UInt64[
7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,
]

const MD5_initial_hash_value = UInt32[0x67452301, 0xefcdab89, 0x98badcfe,  0x10325476] # A,B,C,D