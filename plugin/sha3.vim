" This is a port of Keccak-simple32BI.c.
"
" INSTALL:
"   Copy this file to ~/.vim/autoload/ directory.
"
" Keccak-simple32BI.c
" <ORIGINAL-HEADER>
" The Keccak sponge function, designed by Guido Bertoni, Joan Daemen,
" Michaël Peeters and Gilles Van Assche. For more information, feedback or
" questions, please refer to our website: http://keccak.noekeon.org/
" 
" Implementation by Ronny Van Keer,
" hereby denoted as "the implementer".
" 
" To the extent possible under law, the implementer has waived all copyright
" and related or neighboring rights to the source code in this file.
" http://creativecommons.org/publicdomain/zero/1.0/

" WARNING: This implementation assumes a little-endian platform. Support for big-endinanness is not yet implemented.
" </ORIGINAL-HEADER>

function! sha3#sha3_0(seq)
  call s:setup_0()
  return s:bytestohex(s:crypto_hash(s:bytes(a:seq)))
endfunction

function! sha3#sha3_224(seq)
  call s:setup_224()
  return s:bytestohex(s:crypto_hash(s:bytes(a:seq)))
endfunction

function! sha3#sha3_256(seq)
  call s:setup_256()
  return s:bytestohex(s:crypto_hash(s:bytes(a:seq)))
endfunction

function! sha3#sha3_384(seq)
  call s:setup_384()
  return s:bytestohex(s:crypto_hash(s:bytes(a:seq)))
endfunction

function! sha3#sha3_512(seq)
  call s:setup_512()
  return s:bytestohex(s:crypto_hash(s:bytes(a:seq)))
endfunction

function! sha3#sha3_0_test()
  call s:setup_0()
  call s:test()
endfunction

function! sha3#sha3_224_test()
  call s:setup_224()
  call s:test()
endfunction

function! sha3#sha3_256_test()
  call s:setup_256()
  call s:test()
endfunction

function! sha3#sha3_384_test()
  call s:setup_384()
  call s:test()
endfunction

function! sha3#sha3_512_test()
  call s:setup_512()
  call s:test()
endfunction

function! s:setup_0()
  let s:cKeccakB = 1600
  let s:cKeccakR = 1024
  unlet! s:cKeccakFixedOutputLengthInBytes
  let s:cKeccakR_SizeInBytes = s:cKeccakR / 8
  let s:crypto_hash_BYTES = s:cKeccakR_SizeInBytes
  let s:cKeccakNumberOfRounds = 24
  let s:cKeccakLaneSizeInBytes = 8
  let s:cKeccakLaneSizeInBits = s:cKeccakLaneSizeInBytes * 8
  let s:testVectorFile = 'ShortMsgKAT_0.txt'
endfunction

function! s:setup_224()
  let s:cKeccakB = 1600
  let s:cKeccakR = 1152
  let s:cKeccakFixedOutputLengthInBytes = 28
  let s:cKeccakR_SizeInBytes = s:cKeccakR / 8
  let s:crypto_hash_BYTES = s:cKeccakR_SizeInBytes
  let s:cKeccakNumberOfRounds = 24
  let s:cKeccakLaneSizeInBytes = 8
  let s:cKeccakLaneSizeInBits = s:cKeccakLaneSizeInBytes * 8
  let s:testVectorFile = 'ShortMsgKAT_224.txt'
endfunction

function! s:setup_256()
  let s:cKeccakB = 1600
  let s:cKeccakR = 1088
  let s:cKeccakFixedOutputLengthInBytes = 32
  let s:cKeccakR_SizeInBytes = s:cKeccakR / 8
  let s:crypto_hash_BYTES = s:cKeccakFixedOutputLengthInBytes
  let s:cKeccakNumberOfRounds = 24
  let s:cKeccakLaneSizeInBytes = 8
  let s:cKeccakLaneSizeInBits = s:cKeccakLaneSizeInBytes * 8
  let s:testVectorFile = 'ShortMsgKAT_256.txt'
endfunction

function! s:setup_384()
  let s:cKeccakB = 1600
  let s:cKeccakR = 832
  let s:cKeccakFixedOutputLengthInBytes = 48
  let s:cKeccakR_SizeInBytes = s:cKeccakR / 8
  let s:crypto_hash_BYTES = s:cKeccakR_SizeInBytes
  let s:cKeccakNumberOfRounds = 24
  let s:cKeccakLaneSizeInBytes = 8
  let s:cKeccakLaneSizeInBits = s:cKeccakLaneSizeInBytes * 8
  let s:testVectorFile = 'ShortMsgKAT_384.txt'
endfunction

function! s:setup_512()
  let s:cKeccakB = 1600
  let s:cKeccakR = 576
  let s:cKeccakFixedOutputLengthInBytes = 64
  let s:cKeccakR_SizeInBytes = s:cKeccakR / 8
  let s:crypto_hash_BYTES = s:cKeccakFixedOutputLengthInBytes
  let s:cKeccakNumberOfRounds = 24
  let s:cKeccakLaneSizeInBytes = 8
  let s:cKeccakLaneSizeInBits = s:cKeccakLaneSizeInBytes * 8
  let s:testVectorFile = 'ShortMsgKAT_512.txt'
endfunction

"" for reload
"unlet! s:cKeccakB s:cKeccakR s:cKeccakFixedOutputLengthInBytes s:cKeccakR_SizeInBytes s:crypto_hash_BYTES
"
"" Keccak-simple-settings.h
"let s:cKeccakB = 1600
"let s:cKeccakR = 1024
""let s:cKeccakFixedOutputLengthInBytes = 64
"
"let s:cKeccakR_SizeInBytes = s:cKeccakR / 8
"
"if !exists('s:crypto_hash_BYTES')
"  if exists('s:cKeccakFixedOutputLengthInBytes')
"    let s:crypto_hash_BYTES = s:cKeccakFixedOutputLengthInBytes
"  else
"    let s:crypto_hash_BYTES = s:cKeccakR_SizeInBytes
"  endif
"endif
"if s:crypto_hash_BYTES > s:cKeccakR_SizeInBytes
"  throw "Full squeezing not yet implemented"
"endif
"
"if s:cKeccakB == 1600
"  "typedef unsigned int        UINT32;
"  " WARNING: on 8-bit and 16-bit platforms, this should be replaced by:
"  "typedef unsigned long       UINT32;
"  let s:cKeccakNumberOfRounds = 24
"  let s:cKeccakLaneSizeInBytes = 8
"else
"  throw "Unsupported Keccak-f width"
"endif
"
"let s:cKeccakLaneSizeInBits = s:cKeccakLaneSizeInBytes * 8

let s:pow2 = [
      \ 0x1,        0x2,        0x4,        0x8,
      \ 0x10,       0x20,       0x40,       0x80,
      \ 0x100,      0x200,      0x400,      0x800,
      \ 0x1000,     0x2000,     0x4000,     0x8000,
      \ 0x10000,    0x20000,    0x40000,    0x80000,
      \ 0x100000,   0x200000,   0x400000,   0x800000,
      \ 0x1000000,  0x2000000,  0x4000000,  0x8000000,
      \ 0x10000000, 0x20000000, 0x40000000, 0x80000000,
      \ ]

function! s:leftshift(a, n)
  return a:n == 0 ? a:a : a:n > 31 ? 0 : a:a * s:pow2[a:n]
endfunction

function! s:rightshift(a, n)
  return a:n == 0 ? a:a : a:n > 31 ? 0 :
        \ a:a < 0
        \   ? (a:a - 0x80000000) / s:pow2[a:n] + 0x40000000 / s:pow2[a:n - 1]
        \   : a:a / s:pow2[a:n]
endfunction

function! s:UINT32(a)
  return a:a
endfunction

function! s:cast_uint8p_to_uint32p(a)
  let out = []
  for i in range(0, len(a:a) - 1, 4)
    call add(out, a:a[i + 3] * 0x1000000 + a:a[i + 2] * 0x10000 + a:a[i + 1] * 0x100 + a:a[i])
  endfor
  return out
endfunction

function! s:cast_uint32p_to_uint8p(a)
  let out = []
  for x in a:a
    call add(out, and(x, 0xFF))
    call add(out, and(s:rightshift(x, 8), 0xFF))
    call add(out, and(s:rightshift(x, 16), 0xFF))
    call add(out, and(s:rightshift(x, 24), 0xFF))
  endfor
  return out
endfunction

function! s:bytes(seq)
  if type(a:seq)
    return s:strtobytes(a:seq)
  elseif type(a:seq) == type([])
    return a:seq
  else
    throw 'type error'
  endif
endfunction

function! s:hextobytes(hex)
  return map(split(a:hex, '..\zs'), 'str2nr(v:val, 16)')
endfunction

function! s:bytestohex(bytes)
  return join(map(copy(a:bytes), 'printf("%02x", v:val)'), '')
endfunction

function! s:strtobytes(str)
  return map(range(len(a:str)), 'char2nr(a:str[v:val])')
endfunction

function! s:ROL32(a, offset)
  return xor(s:leftshift(s:UINT32(a:a), a:offset), s:rightshift(s:UINT32(a:a), 32 - a:offset))
endfunction

function! s:crypto_hash(in)
  let state = repeat([0], 5 * 5 * 2)

  let inlen = len(a:in)
  let inI = 0
  while inlen >= s:cKeccakR_SizeInBytes
    call s:KeccakF(state, s:cast_uint8p_to_uint32p(a:in[inI : inI + s:cKeccakR_SizeInBytes - 1]), s:cKeccakR_SizeInBytes / s:cKeccakLaneSizeInBytes)
    let inlen -= s:cKeccakR_SizeInBytes
    let inI += s:cKeccakR_SizeInBytes
  endwhile

  " padding
  let temp = repeat([0], s:cKeccakR_SizeInBytes)
  if inlen != 0
    let temp[0 : inlen - 1] = a:in[inI : inI + inlen - 1]
  endif
  let [temp[inlen], inlen] = [1, inlen + 1]
  if inlen != len(temp)
    let temp[inlen : ] = repeat([0], s:cKeccakR_SizeInBytes - inlen)
  endif
  let temp[s:cKeccakR_SizeInBytes - 1] = or(temp[s:cKeccakR_SizeInBytes - 1], 0x80)
  call s:KeccakF(state, s:cast_uint8p_to_uint32p(temp), s:cKeccakR_SizeInBytes / s:cKeccakLaneSizeInBytes)

  if s:crypto_hash_BYTES % s:cKeccakLaneSizeInBytes == 0
    let out = s:extractFromState(state, s:crypto_hash_BYTES / s:cKeccakLaneSizeInBytes)
  else
    throw "The output size must be a multiple of the lane size in this simple implementation."
  endif

  return out
endfunction

function! s:extractFromState(state, laneCount)
  " Credit: Henry S. Warren, Hacker's Delight, Addison-Wesley, 2002
  let out = repeat([0], s:crypto_hash_BYTES / 4)   " XXX: allocate uint32[]
  let pI = 0
  let pS = 0
  for i in range(a:laneCount - 1, 0, -1)
    let [x0, pS] = [a:state[pS], pS + 1]
    let [x1, pS] = [a:state[pS], pS + 1]
    let t = or(and(x0, 0x0000FFFF), s:leftshift(x1, 16))
    let x1 = or(s:rightshift(x0, 16), and(x1, 0xFFFF0000))
    let x0 = t
    let t = and(xor(x0, s:rightshift(x0, 8)), 0x0000FF00)
    let x0 = xor(xor(x0, t), s:leftshift(t, 8))
    let t = and(xor(x0, s:rightshift(x0, 4)), 0x00F000F0)
    let x0 = xor(xor(x0, t), s:leftshift(t, 4))
    let t = and(xor(x0, s:rightshift(x0, 2)), 0x0C0C0C0C)
    let x0 = xor(xor(x0, t), s:leftshift(t, 2))
    let t = and(xor(x0, s:rightshift(x0, 1)), 0x22222222)
    let x0 = xor(xor(x0, t), s:leftshift(t, 1))
    let t = and(xor(x1, s:rightshift(x1, 8)), 0x0000FF00)
    let x1 = xor(xor(x1, t), s:leftshift(t, 8))
    let t = and(xor(x1, s:rightshift(x1, 4)), 0x00F000F0)
    let x1 = xor(xor(x1, t), s:leftshift(t, 4))
    let t = and(xor(x1, s:rightshift(x1, 2)), 0x0C0C0C0C)
    let x1 = xor(xor(x1, t), s:leftshift(t, 2))
    let t = and(xor(x1, s:rightshift(x1, 1)), 0x22222222)
    let x1 = xor(xor(x1, t), s:leftshift(t, 1))
    let [out[pI], pI] = [x0, pI + 1]
    let [out[pI], pI] = [x1, pI + 1]
  endfor
  return s:cast_uint32p_to_uint8p(out)
endfunction

let s:KeccakF1600RoundConstants_int2 = [
      \ 0x00000001,    0x00000000,
      \ 0x00000000,    0x00000089,
      \ 0x00000000,    0x8000008b,
      \ 0x00000000,    0x80008080,
      \ 0x00000001,    0x0000008b,
      \ 0x00000001,    0x00008000,
      \ 0x00000001,    0x80008088,
      \ 0x00000001,    0x80000082,
      \ 0x00000000,    0x0000000b,
      \ 0x00000000,    0x0000000a,
      \ 0x00000001,    0x00008082,
      \ 0x00000000,    0x00008003,
      \ 0x00000001,    0x0000808b,
      \ 0x00000001,    0x8000000b,
      \ 0x00000001,    0x8000008a,
      \ 0x00000001,    0x80000081,
      \ 0x00000000,    0x80000081,
      \ 0x00000000,    0x80000008,
      \ 0x00000000,    0x00000083,
      \ 0x00000000,    0x80008003,
      \ 0x00000001,    0x80008088,
      \ 0x00000000,    0x80000088,
      \ 0x00000001,    0x00008000,
      \ 0x00000000,    0x80008082
      \ ]

function! s:KeccakF(state, in, laneCount)
  " Credit: Henry S. Warren, Hacker's Delight, Addison-Wesley, 2002
  let pI = 0
  let pS = 0
  for i in range(a:laneCount - 1, 0, -1)
    let [x0, pI] = [a:in[pI], pI + 1]
    let t = and(xor(x0, s:rightshift(x0, 1)), 0x22222222)
    let x0 = xor(xor(x0, t), s:leftshift(t, 1))
    let t = and(xor(x0, s:rightshift(x0, 2)), 0x0C0C0C0C)
    let x0 = xor(xor(x0, t), s:leftshift(t, 2))
    let t = and(xor(x0, s:rightshift(x0, 4)), 0x00F000F0)
    let x0 = xor(xor(x0, t), s:leftshift(t, 4))
    let t = and(xor(x0, s:rightshift(x0, 8)), 0x0000FF00)
    let x0 = xor(xor(x0, t), s:leftshift(t, 8))
    let [x1, pI] = [a:in[pI], pI + 1]
    let t = and(xor(x1, s:rightshift(x1, 1)), 0x22222222)
    let x1 = xor(xor(x1, t), s:leftshift(t, 1))
    let t = and(xor(x1, s:rightshift(x1, 2)), 0x0C0C0C0C)
    let x1 = xor(xor(x1, t), s:leftshift(t, 2))
    let t = and(xor(x1, s:rightshift(x1, 4)), 0x00F000F0)
    let x1 = xor(xor(x1, t), s:leftshift(t, 4))
    let t = and(xor(x1, s:rightshift(x1, 8)), 0x0000FF00)
    let x1 = xor(xor(x1, t), s:leftshift(t, 8))
    let [a:state[pS], pS] = [xor(a:state[pS], or(and(x0, 0x0000FFFF), s:leftshift(x1, 16))), pS + 1]
    let [a:state[pS], pS] = [xor(a:state[pS], or(s:rightshift(x0, 16), and(x1, 0xFFFF0000))), pS + 1]
  endfor

  " copyFromState(A, state)
  let Aba0 = a:state[ 0]
  let Aba1 = a:state[ 1]
  let Abe0 = a:state[ 2]
  let Abe1 = a:state[ 3]
  let Abi0 = a:state[ 4]
  let Abi1 = a:state[ 5]
  let Abo0 = a:state[ 6]
  let Abo1 = a:state[ 7]
  let Abu0 = a:state[ 8]
  let Abu1 = a:state[ 9]
  let Aga0 = a:state[10]
  let Aga1 = a:state[11]
  let Age0 = a:state[12]
  let Age1 = a:state[13]
  let Agi0 = a:state[14]
  let Agi1 = a:state[15]
  let Ago0 = a:state[16]
  let Ago1 = a:state[17]
  let Agu0 = a:state[18]
  let Agu1 = a:state[19]
  let Aka0 = a:state[20]
  let Aka1 = a:state[21]
  let Ake0 = a:state[22]
  let Ake1 = a:state[23]
  let Aki0 = a:state[24]
  let Aki1 = a:state[25]
  let Ako0 = a:state[26]
  let Ako1 = a:state[27]
  let Aku0 = a:state[28]
  let Aku1 = a:state[29]
  let Ama0 = a:state[30]
  let Ama1 = a:state[31]
  let Ame0 = a:state[32]
  let Ame1 = a:state[33]
  let Ami0 = a:state[34]
  let Ami1 = a:state[35]
  let Amo0 = a:state[36]
  let Amo1 = a:state[37]
  let Amu0 = a:state[38]
  let Amu1 = a:state[39]
  let Asa0 = a:state[40]
  let Asa1 = a:state[41]
  let Ase0 = a:state[42]
  let Ase1 = a:state[43]
  let Asi0 = a:state[44]
  let Asi1 = a:state[45]
  let Aso0 = a:state[46]
  let Aso1 = a:state[47]
  let Asu0 = a:state[48]
  let Asu1 = a:state[49]

  for round in range(0, s:cKeccakNumberOfRounds - 1, 2)
    " prepareTheta
    let BCa0 = xor(xor(xor(xor(Aba0, Aga0), Aka0), Ama0), Asa0)
    let BCa1 = xor(xor(xor(xor(Aba1, Aga1), Aka1), Ama1), Asa1)
    let BCe0 = xor(xor(xor(xor(Abe0, Age0), Ake0), Ame0), Ase0)
    let BCe1 = xor(xor(xor(xor(Abe1, Age1), Ake1), Ame1), Ase1)
    let BCi0 = xor(xor(xor(xor(Abi0, Agi0), Aki0), Ami0), Asi0)
    let BCi1 = xor(xor(xor(xor(Abi1, Agi1), Aki1), Ami1), Asi1)
    let BCo0 = xor(xor(xor(xor(Abo0, Ago0), Ako0), Amo0), Aso0)
    let BCo1 = xor(xor(xor(xor(Abo1, Ago1), Ako1), Amo1), Aso1)
    let BCu0 = xor(xor(xor(xor(Abu0, Agu0), Aku0), Amu0), Asu0)
    let BCu1 = xor(xor(xor(xor(Abu1, Agu1), Aku1), Amu1), Asu1)

    " thetaRhoPiChiIota(round  , A, E)
    let Da0 = xor(BCu0, s:ROL32(BCe1, 1))
    let Da1 = xor(BCu1, BCe0)
    let De0 = xor(BCa0, s:ROL32(BCi1, 1))
    let De1 = xor(BCa1, BCi0)
    let Di0 = xor(BCe0, s:ROL32(BCo1, 1))
    let Di1 = xor(BCe1, BCo0)
    let Do0 = xor(BCi0, s:ROL32(BCu1, 1))
    let Do1 = xor(BCi1, BCu0)
    let Du0 = xor(BCo0, s:ROL32(BCa1, 1))
    let Du1 = xor(BCo1, BCa0)

    let Aba0 = xor(Aba0, Da0)
    let BCa0 = Aba0
    let Age0 = xor(Age0, De0)
    let BCe0 = s:ROL32(Age0, 22)
    let Aki1 = xor(Aki1, Di1)
    let BCi0 = s:ROL32(Aki1, 22)
    let Amo1 = xor(Amo1, Do1)
    let BCo0 = s:ROL32(Amo1, 11)
    let Asu0 = xor(Asu0, Du0)
    let BCu0 = s:ROL32(Asu0, 7)
    let Eba0 = xor(BCa0, and(invert(BCe0), BCi0))
    let Eba0 = xor(Eba0, s:KeccakF1600RoundConstants_int2[round*2+0])
    let Ebe0 = xor(BCe0, and(invert(BCi0), BCo0))
    let Ebi0 = xor(BCi0, and(invert(BCo0), BCu0))
    let Ebo0 = xor(BCo0, and(invert(BCu0), BCa0))
    let Ebu0 = xor(BCu0, and(invert(BCa0), BCe0))

    let Aba1 = xor(Aba1, Da1)
    let BCa1 = Aba1
    let Age1 = xor(Age1, De1)
    let BCe1 = s:ROL32(Age1, 22)
    let Aki0 = xor(Aki0, Di0)
    let BCi1 = s:ROL32(Aki0, 21)
    let Amo0 = xor(Amo0, Do0)
    let BCo1 = s:ROL32(Amo0, 10)
    let Asu1 = xor(Asu1, Du1)
    let BCu1 = s:ROL32(Asu1, 7)
    let Eba1 = xor(BCa1, and(invert(BCe1), BCi1))
    let Eba1 = xor(Eba1, s:KeccakF1600RoundConstants_int2[round*2+1])
    let Ebe1 = xor(BCe1, and(invert(BCi1), BCo1))
    let Ebi1 = xor(BCi1, and(invert(BCo1), BCu1))
    let Ebo1 = xor(BCo1, and(invert(BCu1), BCa1))
    let Ebu1 = xor(BCu1, and(invert(BCa1), BCe1))

    let Abo0 = xor(Abo0, Do0)
    let BCa0 = s:ROL32(Abo0, 14)
    let Agu0 = xor(Agu0, Du0)
    let BCe0 = s:ROL32(Agu0, 10)
    let Aka1 = xor(Aka1, Da1)
    let BCi0 = s:ROL32(Aka1, 2)
    let Ame1 = xor(Ame1, De1)
    let BCo0 = s:ROL32(Ame1, 23)
    let Asi1 = xor(Asi1, Di1)
    let BCu0 = s:ROL32(Asi1, 31)
    let Ega0 = xor(BCa0, and(invert(BCe0), BCi0))
    let Ege0 = xor(BCe0, and(invert(BCi0), BCo0))
    let Egi0 = xor(BCi0, and(invert(BCo0), BCu0))
    let Ego0 = xor(BCo0, and(invert(BCu0), BCa0))
    let Egu0 = xor(BCu0, and(invert(BCa0), BCe0))

    let Abo1 = xor(Abo1, Do1)
    let BCa1 = s:ROL32(Abo1, 14)
    let Agu1 = xor(Agu1, Du1)
    let BCe1 = s:ROL32(Agu1, 10)
    let Aka0 = xor(Aka0, Da0)
    let BCi1 = s:ROL32(Aka0, 1)
    let Ame0 = xor(Ame0, De0)
    let BCo1 = s:ROL32(Ame0, 22)
    let Asi0 = xor(Asi0, Di0)
    let BCu1 = s:ROL32(Asi0, 30)
    let Ega1 = xor(BCa1, and(invert(BCe1), BCi1))
    let Ege1 = xor(BCe1, and(invert(BCi1), BCo1))
    let Egi1 = xor(BCi1, and(invert(BCo1), BCu1))
    let Ego1 = xor(BCo1, and(invert(BCu1), BCa1))
    let Egu1 = xor(BCu1, and(invert(BCa1), BCe1))

    let Abe1 = xor(Abe1, De1)
    let BCa0 = s:ROL32(Abe1, 1)
    let Agi0 = xor(Agi0, Di0)
    let BCe0 = s:ROL32(Agi0, 3)
    let Ako1 = xor(Ako1, Do1)
    let BCi0 = s:ROL32(Ako1, 13)
    let Amu0 = xor(Amu0, Du0)
    let BCo0 = s:ROL32(Amu0, 4)
    let Asa0 = xor(Asa0, Da0)
    let BCu0 = s:ROL32(Asa0, 9)
    let Eka0 = xor(BCa0, and(invert(BCe0), BCi0))
    let Eke0 = xor(BCe0, and(invert(BCi0), BCo0))
    let Eki0 = xor(BCi0, and(invert(BCo0), BCu0))
    let Eko0 = xor(BCo0, and(invert(BCu0), BCa0))
    let Eku0 = xor(BCu0, and(invert(BCa0), BCe0))

    let Abe0 = xor(Abe0, De0)
    let BCa1 = Abe0
    let Agi1 = xor(Agi1, Di1)
    let BCe1 = s:ROL32(Agi1, 3)
    let Ako0 = xor(Ako0, Do0)
    let BCi1 = s:ROL32(Ako0, 12)
    let Amu1 = xor(Amu1, Du1)
    let BCo1 = s:ROL32(Amu1, 4)
    let Asa1 = xor(Asa1, Da1)
    let BCu1 = s:ROL32(Asa1, 9)
    let Eka1 = xor(BCa1, and(invert(BCe1), BCi1))
    let Eke1 = xor(BCe1, and(invert(BCi1), BCo1))
    let Eki1 = xor(BCi1, and(invert(BCo1), BCu1))
    let Eko1 = xor(BCo1, and(invert(BCu1), BCa1))
    let Eku1 = xor(BCu1, and(invert(BCa1), BCe1))

    let Abu1 = xor(Abu1, Du1)
    let BCa0 = s:ROL32(Abu1, 14)
    let Aga0 = xor(Aga0, Da0)
    let BCe0 = s:ROL32(Aga0, 18)
    let Ake0 = xor(Ake0, De0)
    let BCi0 = s:ROL32(Ake0, 5)
    let Ami1 = xor(Ami1, Di1)
    let BCo0 = s:ROL32(Ami1, 8)
    let Aso0 = xor(Aso0, Do0)
    let BCu0 = s:ROL32(Aso0, 28)
    let Ema0 = xor(BCa0, and(invert(BCe0), BCi0))
    let Eme0 = xor(BCe0, and(invert(BCi0), BCo0))
    let Emi0 = xor(BCi0, and(invert(BCo0), BCu0))
    let Emo0 = xor(BCo0, and(invert(BCu0), BCa0))
    let Emu0 = xor(BCu0, and(invert(BCa0), BCe0))

    let Abu0 = xor(Abu0, Du0)
    let BCa1 = s:ROL32(Abu0, 13)
    let Aga1 = xor(Aga1, Da1)
    let BCe1 = s:ROL32(Aga1, 18)
    let Ake1 = xor(Ake1, De1)
    let BCi1 = s:ROL32(Ake1, 5)
    let Ami0 = xor(Ami0, Di0)
    let BCo1 = s:ROL32(Ami0, 7)
    let Aso1 = xor(Aso1, Do1)
    let BCu1 = s:ROL32(Aso1, 28)
    let Ema1 = xor(BCa1, and(invert(BCe1), BCi1))
    let Eme1 = xor(BCe1, and(invert(BCi1), BCo1))
    let Emi1 = xor(BCi1, and(invert(BCo1), BCu1))
    let Emo1 = xor(BCo1, and(invert(BCu1), BCa1))
    let Emu1 = xor(BCu1, and(invert(BCa1), BCe1))

    let Abi0 = xor(Abi0, Di0)
    let BCa0 = s:ROL32(Abi0, 31)
    let Ago1 = xor(Ago1, Do1)
    let BCe0 = s:ROL32(Ago1, 28)
    let Aku1 = xor(Aku1, Du1)
    let BCi0 = s:ROL32(Aku1, 20)
    let Ama1 = xor(Ama1, Da1)
    let BCo0 = s:ROL32(Ama1, 21)
    let Ase0 = xor(Ase0, De0)
    let BCu0 = s:ROL32(Ase0, 1)
    let Esa0 = xor(BCa0, and(invert(BCe0), BCi0))
    let Ese0 = xor(BCe0, and(invert(BCi0), BCo0))
    let Esi0 = xor(BCi0, and(invert(BCo0), BCu0))
    let Eso0 = xor(BCo0, and(invert(BCu0), BCa0))
    let Esu0 = xor(BCu0, and(invert(BCa0), BCe0))

    let Abi1 = xor(Abi1, Di1)
    let BCa1 = s:ROL32(Abi1, 31)
    let Ago0 = xor(Ago0, Do0)
    let BCe1 = s:ROL32(Ago0, 27)
    let Aku0 = xor(Aku0, Du0)
    let BCi1 = s:ROL32(Aku0, 19)
    let Ama0 = xor(Ama0, Da0)
    let BCo1 = s:ROL32(Ama0, 20)
    let Ase1 = xor(Ase1, De1)
    let BCu1 = s:ROL32(Ase1, 1)
    let Esa1 = xor(BCa1, and(invert(BCe1), BCi1))
    let Ese1 = xor(BCe1, and(invert(BCi1), BCo1))
    let Esi1 = xor(BCi1, and(invert(BCo1), BCu1))
    let Eso1 = xor(BCo1, and(invert(BCu1), BCa1))
    let Esu1 = xor(BCu1, and(invert(BCa1), BCe1))

    "    prepareTheta
    let BCa0 = xor(xor(xor(xor(Eba0, Ega0), Eka0), Ema0), Esa0)
    let BCa1 = xor(xor(xor(xor(Eba1, Ega1), Eka1), Ema1), Esa1)
    let BCe0 = xor(xor(xor(xor(Ebe0, Ege0), Eke0), Eme0), Ese0)
    let BCe1 = xor(xor(xor(xor(Ebe1, Ege1), Eke1), Eme1), Ese1)
    let BCi0 = xor(xor(xor(xor(Ebi0, Egi0), Eki0), Emi0), Esi0)
    let BCi1 = xor(xor(xor(xor(Ebi1, Egi1), Eki1), Emi1), Esi1)
    let BCo0 = xor(xor(xor(xor(Ebo0, Ego0), Eko0), Emo0), Eso0)
    let BCo1 = xor(xor(xor(xor(Ebo1, Ego1), Eko1), Emo1), Eso1)
    let BCu0 = xor(xor(xor(xor(Ebu0, Egu0), Eku0), Emu0), Esu0)
    let BCu1 = xor(xor(xor(xor(Ebu1, Egu1), Eku1), Emu1), Esu1)

    "thetaRhoPiChiIota(round+1, E, A)
    let Da0 = xor(BCu0, s:ROL32(BCe1, 1))
    let Da1 = xor(BCu1, BCe0)
    let De0 = xor(BCa0, s:ROL32(BCi1, 1))
    let De1 = xor(BCa1, BCi0)
    let Di0 = xor(BCe0, s:ROL32(BCo1, 1))
    let Di1 = xor(BCe1, BCo0)
    let Do0 = xor(BCi0, s:ROL32(BCu1, 1))
    let Do1 = xor(BCi1, BCu0)
    let Du0 = xor(BCo0, s:ROL32(BCa1, 1))
    let Du1 = xor(BCo1, BCa0)

    let Eba0 = xor(Eba0, Da0)
    let BCa0 = Eba0
    let Ege0 = xor(Ege0, De0)
    let BCe0 = s:ROL32(Ege0, 22)
    let Eki1 = xor(Eki1, Di1)
    let BCi0 = s:ROL32(Eki1, 22)
    let Emo1 = xor(Emo1, Do1)
    let BCo0 = s:ROL32(Emo1, 11)
    let Esu0 = xor(Esu0, Du0)
    let BCu0 = s:ROL32(Esu0, 7)
    let Aba0 = xor(BCa0, and(invert(BCe0), BCi0))
    let Aba0 = xor(Aba0, s:KeccakF1600RoundConstants_int2[round*2+2])
    let Abe0 = xor(BCe0, and(invert(BCi0), BCo0))
    let Abi0 = xor(BCi0, and(invert(BCo0), BCu0))
    let Abo0 = xor(BCo0, and(invert(BCu0), BCa0))
    let Abu0 = xor(BCu0, and(invert(BCa0), BCe0))

    let Eba1 = xor(Eba1, Da1)
    let BCa1 = Eba1
    let Ege1 = xor(Ege1, De1)
    let BCe1 = s:ROL32(Ege1, 22)
    let Eki0 = xor(Eki0, Di0)
    let BCi1 = s:ROL32(Eki0, 21)
    let Emo0 = xor(Emo0, Do0)
    let BCo1 = s:ROL32(Emo0, 10)
    let Esu1 = xor(Esu1, Du1)
    let BCu1 = s:ROL32(Esu1, 7)
    let Aba1 = xor(BCa1, and(invert(BCe1), BCi1))
    let Aba1 = xor(Aba1, s:KeccakF1600RoundConstants_int2[round*2+3])
    let Abe1 = xor(BCe1, and(invert(BCi1), BCo1))
    let Abi1 = xor(BCi1, and(invert(BCo1), BCu1))
    let Abo1 = xor(BCo1, and(invert(BCu1), BCa1))
    let Abu1 = xor(BCu1, and(invert(BCa1), BCe1))

    let Ebo0 = xor(Ebo0, Do0)
    let BCa0 = s:ROL32(Ebo0, 14)
    let Egu0 = xor(Egu0, Du0)
    let BCe0 = s:ROL32(Egu0, 10)
    let Eka1 = xor(Eka1, Da1)
    let BCi0 = s:ROL32(Eka1, 2)
    let Eme1 = xor(Eme1, De1)
    let BCo0 = s:ROL32(Eme1, 23)
    let Esi1 = xor(Esi1, Di1)
    let BCu0 = s:ROL32(Esi1, 31)
    let Aga0 = xor(BCa0, and(invert(BCe0), BCi0))
    let Age0 = xor(BCe0, and(invert(BCi0), BCo0))
    let Agi0 = xor(BCi0, and(invert(BCo0), BCu0))
    let Ago0 = xor(BCo0, and(invert(BCu0), BCa0))
    let Agu0 = xor(BCu0, and(invert(BCa0), BCe0))

    let Ebo1 = xor(Ebo1, Do1)
    let BCa1 = s:ROL32(Ebo1, 14)
    let Egu1 = xor(Egu1, Du1)
    let BCe1 = s:ROL32(Egu1, 10)
    let Eka0 = xor(Eka0, Da0)
    let BCi1 = s:ROL32(Eka0, 1)
    let Eme0 = xor(Eme0, De0)
    let BCo1 = s:ROL32(Eme0, 22)
    let Esi0 = xor(Esi0, Di0)
    let BCu1 = s:ROL32(Esi0, 30)
    let Aga1 = xor(BCa1, and(invert(BCe1), BCi1))
    let Age1 = xor(BCe1, and(invert(BCi1), BCo1))
    let Agi1 = xor(BCi1, and(invert(BCo1), BCu1))
    let Ago1 = xor(BCo1, and(invert(BCu1), BCa1))
    let Agu1 = xor(BCu1, and(invert(BCa1), BCe1))

    let Ebe1 = xor(Ebe1, De1)
    let BCa0 = s:ROL32(Ebe1, 1)
    let Egi0 = xor(Egi0, Di0)
    let BCe0 = s:ROL32(Egi0, 3)
    let Eko1 = xor(Eko1, Do1)
    let BCi0 = s:ROL32(Eko1, 13)
    let Emu0 = xor(Emu0, Du0)
    let BCo0 = s:ROL32(Emu0, 4)
    let Esa0 = xor(Esa0, Da0)
    let BCu0 = s:ROL32(Esa0, 9)
    let Aka0 = xor(BCa0, and(invert(BCe0), BCi0))
    let Ake0 = xor(BCe0, and(invert(BCi0), BCo0))
    let Aki0 = xor(BCi0, and(invert(BCo0), BCu0))
    let Ako0 = xor(BCo0, and(invert(BCu0), BCa0))
    let Aku0 = xor(BCu0, and(invert(BCa0), BCe0))

    let Ebe0 = xor(Ebe0, De0)
    let BCa1 = Ebe0
    let Egi1 = xor(Egi1, Di1)
    let BCe1 = s:ROL32(Egi1, 3)
    let Eko0 = xor(Eko0, Do0)
    let BCi1 = s:ROL32(Eko0, 12)
    let Emu1 = xor(Emu1, Du1)
    let BCo1 = s:ROL32(Emu1, 4)
    let Esa1 = xor(Esa1, Da1)
    let BCu1 = s:ROL32(Esa1, 9)
    let Aka1 = xor(BCa1, and(invert(BCe1), BCi1))
    let Ake1 = xor(BCe1, and(invert(BCi1), BCo1))
    let Aki1 = xor(BCi1, and(invert(BCo1), BCu1))
    let Ako1 = xor(BCo1, and(invert(BCu1), BCa1))
    let Aku1 = xor(BCu1, and(invert(BCa1), BCe1))

    let Ebu1 = xor(Ebu1, Du1)
    let BCa0 = s:ROL32(Ebu1, 14)
    let Ega0 = xor(Ega0, Da0)
    let BCe0 = s:ROL32(Ega0, 18)
    let Eke0 = xor(Eke0, De0)
    let BCi0 = s:ROL32(Eke0, 5)
    let Emi1 = xor(Emi1, Di1)
    let BCo0 = s:ROL32(Emi1, 8)
    let Eso0 = xor(Eso0, Do0)
    let BCu0 = s:ROL32(Eso0, 28)
    let Ama0 = xor(BCa0, and(invert(BCe0), BCi0))
    let Ame0 = xor(BCe0, and(invert(BCi0), BCo0))
    let Ami0 = xor(BCi0, and(invert(BCo0), BCu0))
    let Amo0 = xor(BCo0, and(invert(BCu0), BCa0))
    let Amu0 = xor(BCu0, and(invert(BCa0), BCe0))

    let Ebu0 = xor(Ebu0, Du0)
    let BCa1 = s:ROL32(Ebu0, 13)
    let Ega1 = xor(Ega1, Da1)
    let BCe1 = s:ROL32(Ega1, 18)
    let Eke1 = xor(Eke1, De1)
    let BCi1 = s:ROL32(Eke1, 5)
    let Emi0 = xor(Emi0, Di0)
    let BCo1 = s:ROL32(Emi0, 7)
    let Eso1 = xor(Eso1, Do1)
    let BCu1 = s:ROL32(Eso1, 28)
    let Ama1 = xor(BCa1, and(invert(BCe1), BCi1))
    let Ame1 = xor(BCe1, and(invert(BCi1), BCo1))
    let Ami1 = xor(BCi1, and(invert(BCo1), BCu1))
    let Amo1 = xor(BCo1, and(invert(BCu1), BCa1))
    let Amu1 = xor(BCu1, and(invert(BCa1), BCe1))

    let Ebi0 = xor(Ebi0, Di0)
    let BCa0 = s:ROL32(Ebi0, 31)
    let Ego1 = xor(Ego1, Do1)
    let BCe0 = s:ROL32(Ego1, 28)
    let Eku1 = xor(Eku1, Du1)
    let BCi0 = s:ROL32(Eku1, 20)
    let Ema1 = xor(Ema1, Da1)
    let BCo0 = s:ROL32(Ema1, 21)
    let Ese0 = xor(Ese0, De0)
    let BCu0 = s:ROL32(Ese0, 1)
    let Asa0 = xor(BCa0, and(invert(BCe0), BCi0))
    let Ase0 = xor(BCe0, and(invert(BCi0), BCo0))
    let Asi0 = xor(BCi0, and(invert(BCo0), BCu0))
    let Aso0 = xor(BCo0, and(invert(BCu0), BCa0))
    let Asu0 = xor(BCu0, and(invert(BCa0), BCe0))

    let Ebi1 = xor(Ebi1, Di1)
    let BCa1 = s:ROL32(Ebi1, 31)
    let Ego0 = xor(Ego0, Do0)
    let BCe1 = s:ROL32(Ego0, 27)
    let Eku0 = xor(Eku0, Du0)
    let BCi1 = s:ROL32(Eku0, 19)
    let Ema0 = xor(Ema0, Da0)
    let BCo1 = s:ROL32(Ema0, 20)
    let Ese1 = xor(Ese1, De1)
    let BCu1 = s:ROL32(Ese1, 1)
    let Asa1 = xor(BCa1, and(invert(BCe1), BCi1))
    let Ase1 = xor(BCe1, and(invert(BCi1), BCo1))
    let Asi1 = xor(BCi1, and(invert(BCo1), BCu1))
    let Aso1 = xor(BCo1, and(invert(BCu1), BCa1))
    let Asu1 = xor(BCu1, and(invert(BCa1), BCe1))
  endfor

  " copyToState(state, A)
  let a:state[ 0] = Aba0
  let a:state[ 1] = Aba1
  let a:state[ 2] = Abe0
  let a:state[ 3] = Abe1
  let a:state[ 4] = Abi0
  let a:state[ 5] = Abi1
  let a:state[ 6] = Abo0
  let a:state[ 7] = Abo1
  let a:state[ 8] = Abu0
  let a:state[ 9] = Abu1
  let a:state[10] = Aga0
  let a:state[11] = Aga1
  let a:state[12] = Age0
  let a:state[13] = Age1
  let a:state[14] = Agi0
  let a:state[15] = Agi1
  let a:state[16] = Ago0
  let a:state[17] = Ago1
  let a:state[18] = Agu0
  let a:state[19] = Agu1
  let a:state[20] = Aka0
  let a:state[21] = Aka1
  let a:state[22] = Ake0
  let a:state[23] = Ake1
  let a:state[24] = Aki0
  let a:state[25] = Aki1
  let a:state[26] = Ako0
  let a:state[27] = Ako1
  let a:state[28] = Aku0
  let a:state[29] = Aku1
  let a:state[30] = Ama0
  let a:state[31] = Ama1
  let a:state[32] = Ame0
  let a:state[33] = Ame1
  let a:state[34] = Ami0
  let a:state[35] = Ami1
  let a:state[36] = Amo0
  let a:state[37] = Amo1
  let a:state[38] = Amu0
  let a:state[39] = Amu1
  let a:state[40] = Asa0
  let a:state[41] = Asa1
  let a:state[42] = Ase0
  let a:state[43] = Ase1
  let a:state[44] = Asi0
  let a:state[45] = Asi1
  let a:state[46] = Aso0
  let a:state[47] = Aso1
  let a:state[48] = Asu0
  let a:state[49] = Asu1
endfunction

let s:cKeccakMaxMessageSizeInBytes = 2047 / 8

function! s:test() abort
  if exists('s:cKeccakFixedOutputLengthInBytes')
    let refLen = s:cKeccakFixedOutputLengthInBytes
  else
    let refLen = s:cKeccakR_SizeInBytes
  endif
  echo printf( "Testing Keccak[r=%u, c=%u] against %s over %d squeezed bytes\n", s:cKeccakR, s:cKeccakB - s:cKeccakR, s:testVectorFile, refLen)
  let testfile = readfile(s:testVectorFile)
  for inlen in range(0, s:cKeccakMaxMessageSizeInBytes)
    let marker = printf("Len = %u", inlen * 8)
    let idx = index(testfile, marker)
    if idx == -1
      throw printf("ERROR: no test vector found (%u bytes)\n", inlen)
    endif
    let in = repeat([0], inlen)
    let msg = s:hextobytes(matchstr(testfile[idx + 1], 'Msg = \zs\x\+'))
    if inlen != 0
      let in[0 : len(msg) - 1] = msg
    endif
    if exists('s:cKeccakFixedOutputLengthInBytes')
      let squeezed = s:hextobytes(matchstr(testfile[idx + 2], 'MD = \zs\x\+'))
    else
      let squeezed = s:hextobytes(matchstr(testfile[idx + 2], 'Squeezed = \zs\x\+'))
    endif
    let out = s:crypto_hash(in)
    if out[0 : refLen - 1] != squeezed[0 : refLen - 1]
      throw printf("ERROR: hash verification (%u bytes)", inlen)
    endif
  endfor
  echo "Success!"
endfunction

