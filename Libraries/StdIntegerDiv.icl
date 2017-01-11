implementation module StdIntegerDiv

import StdEnv
from StdInteger import ::Integer{..}
from StdIntegerAdd import add_integer,sub_integer

MAX_INT    :==IF_INT_64_OR_32 0x7fffffffffffffff 0x7fffffff
MAX_NEG_INT:==IF_INT_64_OR_32 0x8000000000000000 0x80000000
N_BITS_IN_INT:== IF_INT_64_OR_32 64 32

/* Start from Internal module */
n_leading_zeros n :== IF_INT_64_OR_32 (n_leading_zeros_64 n) (n_leading_zeros_32 n)
n_leading_zeros_nibble n :== ((0x000055af+>>(n+n)) bitand 3)

n_leading_zeros_64 :: !Int -> Int
n_leading_zeros_64 n // n<>0
    | n+<0x100000000
    | n+<0x10000
    | n+<0x100
    | n+<0x10
    = 60 + n_leading_zeros_nibble n
    = 56 + n_leading_zeros_nibble (n+>>4)
    | n+<0x1000
    = 52 + n_leading_zeros_nibble (n+>>8)
    = 48 + n_leading_zeros_nibble (n+>>12)
    | n+<0x1000000
    | n+<0x100000
    = 44 + n_leading_zeros_nibble (n+>>16)
    = 40 + n_leading_zeros_nibble (n+>>20)
    | n+<0x10000000
    = 36 + n_leading_zeros_nibble (n+>>24)
    = 32 + n_leading_zeros_nibble (n+>>28)
    | n+<0x1000000000000
    | n+<0x10000000000
    | n+<0x1000000000
    = 28 + n_leading_zeros_nibble (n+>>32)
    = 24 + n_leading_zeros_nibble (n+>>36)
    | n+<0x100000000000
    = 20 + n_leading_zeros_nibble (n+>>40)
    = 16 + n_leading_zeros_nibble (n+>>44)
    | n+<0x100000000000000
    | n+<0x10000000000000
    = 12 + n_leading_zeros_nibble (n+>>48)
    = 8 + n_leading_zeros_nibble (n+>>52)
    | n+<0x1000000000000000
    = 4 + n_leading_zeros_nibble (n+>>56)
    = n_leading_zeros_nibble (n+>>60)

n_leading_zeros_32 :: !Int -> Int
n_leading_zeros_32 n // n<>0
    | n+<0x10000
    | n+<0x100
    | n+<0x10
    = 28 + n_leading_zeros_nibble n
    = 24 + n_leading_zeros_nibble (n+>>4)
    | n+<0x1000
    = 20 + n_leading_zeros_nibble (n+>>8)
    = 16 + n_leading_zeros_nibble (n+>>12)
    | n+<0x1000000
    | n+<0x100000
    = 12 + n_leading_zeros_nibble (n+>>16)
    = 8 + n_leading_zeros_nibble (n+>>20)
    | n+<0x10000000
    = 4 + n_leading_zeros_nibble (n+>>24)
    = n_leading_zeros_nibble (n+>>28)

int_array_to_unsigned_integer :: !*{#Int} -> *Integer
int_array_to_unsigned_integer a
    #! s = size a
    # n=a.[s-1]
    | n<>0
    | s<>1 || n<0
    = {integer_s=0, integer_a=a}
    = {integer_s=n, integer_a={}}
    = remove_zeros_and_resize_p (s-1) a
  where
    remove_zeros_and_resize_p :: !Int !*{#Int} -> *Integer
    remove_zeros_and_resize_p i a
    | i==0
    = {integer_s=0,integer_a={}}
    #! n=a.[i-1]
    | n==0
    = remove_zeros_and_resize_p (i-1) a
    | i<>1 || n<0
    = {integer_s=0, integer_a=resize_int_array i a}
    = {integer_s=n, integer_a={}}
/* End from Internal module */

ltU :: !Int !Int -> Bool
ltU a b = code inline {
    ltU
}

divdu :: !Int !Int !Int -> (!Int,!Int)
divdu a b c = code inline {
    divLU
}

ushiftr :: !Int !Int -> Int
ushiftr a b = code inline {
    shiftrU
}

mulud :: !Int !Int -> (!Int,!Int)
mulud a b = code inline {
    mulUUL
}

addLU :: !Int !Int !Int -> (!Int,!Int)
addLU _ _ _ = code inline {
    addLU
}

subLU :: !Int !Int !Int -> (!Int,!Int)
subLU _ _ _ = code inline {
    subLU
}

create_unintialized_int_array :: !Int -> *{#Int}
create_unintialized_int_array size = code {
    create_array_ INT 0 1
}

resize_int_array :: !Int !*{#Int} -> *{#Int}
resize_int_array s a
    | size a>=s
    = unsafe_resize_int_array s a

unsafe_resize_int_array :: !Int !*{#Int} -> *{#Int}
unsafe_resize_int_array s a = code {
    fill1_r _ARAY_ 0 1 0 01
}

(+<) infix 4
(+<) a b :== ltU a b

(+>) infix 4;
(+>) a b :== ltU b a;

(+>>) infix 7;
(+>>) a b :== ushiftr a b;

div_integer :: !Integer !Integer -> *Integer
div_integer {integer_a=a1,integer_s=s1} {integer_a=a2,integer_s=s2}
    | size a1==0
    | size a2==0
    | s1<>MAX_NEG_INT || s2<> -1
    = {integer_s=s1/s2,integer_a={}}
    = {integer_s=0, integer_a={MAX_NEG_INT}}
    | size a2==1 && a2.[0]==MAX_NEG_INT && s1==MAX_NEG_INT
    | s2>=0
    = {integer_s= -1,integer_a={}}
    = {integer_s= 1,integer_a={}}
    = {integer_s=0, integer_a={}}
    // size a1<>0
    | size a2==0
    | size a1<=2
    | size a1==1
    = div_integer_size_1_integer s1 a1 s2
    = div_integer_size_2_integer s1 a1 s2
    | s2>=0
    = {integer_s=s1, integer_a=div_integer_unsigned_int a1 s2}
    = {integer_s= bitnot s1, integer_a=div_integer_unsigned_int a1 (~s2)}
    | size a2==1
    #! s = s1 bitxor s2
    = {integer_s=s, integer_a=div_integer_unsigned_int a1 a2.[0]}
    | s1 bitxor s2>=0
    = div_integer_integer a1 a2
    = make_positive_integer_negative (div_integer_integer a1 a2)
  where
    div_integer_size_1_integer s1 a1 i2
    # n=a1.[0]
    | i2>=0
    # (r,q) = divdu 0 n i2
    = sign_and_unsigned_int_to_integer s1 q
    # (r,q) = divdu 0 n (~i2)
    = neg_sign_and_unsigned_int_to_integer s1 q

    div_integer_size_2_integer s1 a1 i2
    # n0=a1.[0]
    # n1=a1.[1]
    | i2>=0
    | n1+<i2
    # (r,q) = divdu n1 n0 i2
    = sign_and_unsigned_int_to_integer s1 q
    # (r1,q1) = divdu 0 n1 i2
    # (r0,q0) = divdu r1 n0 i2
    | q1==0
    = sign_and_unsigned_int_to_integer s1 q0
    = {integer_s=s1, integer_a={q0,q1}}
    # i2 = ~i2
    | n1+<i2
    # (r,q) = divdu n1 n0 i2
    = neg_sign_and_unsigned_int_to_integer s1 q
    # (r1,q1) = divdu 0 n1 i2
    # (r0,q0) = divdu r1 n0 i2
    | q1==0
    = neg_sign_and_unsigned_int_to_integer s1 q0
    = {integer_s=bitnot s1, integer_a={q0,q1}}

    sign_and_unsigned_int_to_integer s i
    | s>=0
    = unsigned_int_to_integer i
    = neg_unsigned_int_to_integer i

    neg_sign_and_unsigned_int_to_integer s i
    | s>=0
    = neg_unsigned_int_to_integer i
    = unsigned_int_to_integer i

    neg_unsigned_int_to_integer i
    | MAX_NEG_INT+<i
    = {integer_s= -1,integer_a={i}}
    = {integer_s= ~i,integer_a={}}

    div_integer_unsigned_int :: !{#Int} !Int -> *{#Int}
    div_integer_unsigned_int a1 s2
    # (r,da) = div_rem_integer_unsigned_int a1 s2
    = da

make_positive_integer_negative :: !u:Integer -> u:Integer
make_positive_integer_negative {integer_s,integer_a}
    | size integer_a==0
    | integer_s<>MAX_NEG_INT
    = {integer_s = ~integer_s, integer_a = integer_a}
    = {integer_s = -1, integer_a = {integer_s}}
    = {integer_s = -1, integer_a = integer_a}

import StdDebug

div_integer_a :: !*{#Int} !{#Int} -> *{#Int}
div_integer_a a1 a2
    // highest bit of a2 must be set
    | size a1>0
    | size a2>1
    | size a1>=size a2
    # d_size = size a2
    | extra_int a1 a2
    | size a1==size a2
    = div_integer_1_extra_int a1 a2
    # (a1,a2) = shift_left_nd_extra_int 0 a1 a2
    #! s = size a1
    # a1 = div_loop (s-1) a1 a2
    = remove_begin_elements (size a2) a1
    #! n=size a1-size a2
    | n<=1
    | n==0
    = {}
    = div_integer_1_no_extra_int a1 a2
    #! s = size a1
    # a1 = div_loop (s-1) a1 a2
    = remove_begin_elements (size a2) a1
    = {}
    | size a2==1
    # (r,q) = div_rem_integer_unsigned_int a1 a2.[0]
    = q
    = abort "div_integer_a 2"
  where
    div_integer_1_extra_int :: !{#Int} !{#Int} -> *{#Int}
    div_integer_1_extra_int a1 a2
    # (d1,d0) = shift_left_2_0 a2
    # (n2,n1,n0) = shift_left_3x_0 a1
    # (ok,q) = try_fast_div_1 n2 n1 n0 d1 d0
    | ok
    = {q}
    # (a1,a2) = shift_left_nd_extra_int_0 a1 a2
    #! s = size a1
    #! n = size a2
    # a1 = div_last (s-1) n a1 a2
    = remove_begin_elements n a1

    div_integer_1_no_extra_int :: !*{#Int} !{#Int} -> *{#Int}
    div_integer_1_no_extra_int a1 a2
    # (d1,d0) = shift_left_2_0 a2
    #! s = size a1
    #! n2=a1.[s-1]
    #! n1=a1.[s-2]
    #! n0=a1.[s-3]
    # (ok,q) = try_fast_div_1 n2 n1 n0 d1 d0
    | ok
    = {q}
    #! s = size a1
    #! n = size a2
    # a1 = div_last (s-1) n a1 a2
    = remove_begin_elements n a1

    shift_left_2_0 a
    #! s = size a
    #! i1=a.[s-1]
    #! i0=a.[s-2]
    = (i1,i0)

    shift_left_3x_0 a
    #! s = size a
    #! i2=a.[s-1]
    #! i1=a.[s-2]
    = (0,i2,i1)

unsigned_int_to_integer i
    | i>=0
    = {integer_s=i,integer_a={}}
    = {integer_s=0,integer_a={i}}

div_integer_integer :: !{#Int} !{#Int} -> *Integer
div_integer_integer a1 a2
    | size a1>=size a2
    # d_size = size a2
    # n_leading_zeros = n_leading_zeros a2.[d_size-1]
    | extra_int a1 a2
    | size a1==size a2
    = div_integer_1_extra_int n_leading_zeros a1 a2
    # (a1,a2) = shift_left_nd_extra_int n_leading_zeros a1 a2
    #! s = size a1
    # a1 = div_loop (s-1) a1 a2
    # a1 = remove_begin_elements (size a2) a1
    = {integer_s=0, integer_a=a1}
    # n=size a1-size a2
    | n<=1
    | n==0
    = {integer_s=0,integer_a={}}
    = div_integer_1_no_extra_int n_leading_zeros a1 a2
    # (a1,a2) = shift_left_nd_no_extra_int n_leading_zeros a1 a2
    #! s = size a1
    # a1 = div_loop (s-1) a1 a2
    # a1 = remove_begin_elements (size a2) a1
    = {integer_s=0, integer_a=a1}
    = {integer_s=0,integer_a={}}
  where
    div_integer_1_extra_int :: !Int !{#Int} !{#Int} -> *Integer
    div_integer_1_extra_int n_leading_zeros a1 a2
    # (d1,d0) = shift_left_2 n_leading_zeros a2
    # (n2,n1,n0) = shift_left_3x n_leading_zeros a1
    # (ok,q) = try_fast_div_1 n2 n1 n0 d1 d0
    | ok
    | q>=0
    = {integer_s=q,integer_a={}}
    = {integer_s=0, integer_a={q}}
    # (a1,a2) = shift_left_nd_extra_int n_leading_zeros a1 a2
    #! s = size a1
    #! n = size a2
    # a1 = div_last (s-1) n a1 a2
    # a1 = remove_begin_elements n a1
    | a1.[0]>=0
    = {integer_s=a1.[0],integer_a={}}
    = {integer_s=0, integer_a=a1}

    div_integer_1_no_extra_int :: !Int !{#Int} !{#Int} -> *Integer
    div_integer_1_no_extra_int n_leading_zeros a1 a2
    # (d1,d0) = shift_left_2 n_leading_zeros a2
    # (n2,n1,n0) = shift_left_3 n_leading_zeros a1
    # (ok,q) = try_fast_div_1 n2 n1 n0 d1 d0
    | ok
    | q>=0
    = {integer_s=q,integer_a={}}
    = {integer_s=0, integer_a={q}}
    # (a1,a2) = shift_left_nd_no_extra_int n_leading_zeros a1 a2
    #! s = size a1
    #! n = size a2
    # a1 = div_last (s-1) n a1 a2
    # a1 = remove_begin_elements n a1
    | a1.[0]>=0
    = {integer_s=a1.[0],integer_a={}}
    = {integer_s=0, integer_a=a1}

    shift_left_2 0 a
    #! s = size a
    #! i1=a.[s-1]
    #! i0=a.[s-2]
    = (i1,i0)
    shift_left_2 n a
    #! s = size a2
    #! i1=a2.[s-1]
    #! i0=a2.[s-2]
    #! ix=if (s>2) a2.[s-3] 0
    # nr=N_BITS_IN_INT-n
    # i1=(i1<<n) bitor (ushiftr i0 nr)
    # i0=(i0<<n) bitor (ushiftr ix nr)
    = (i1,i0)

    shift_left_3x 0 a
    #! s = size a
    #! i2=a.[s-1]
    #! i1=a.[s-2]
    = (0,i2,i1)
    shift_left_3x n a
    #! s = size a
    | s>=3
    #! i2=a1.[s-1]
    #! i1=a1.[s-2]
    #! i0=a1.[s-3]
    # nr=N_BITS_IN_INT-n
    # i3=ushiftr i2 nr
    # i2=(i2<<n) bitor (ushiftr i1 nr)
    # i1=(i1<<n) bitor (ushiftr i0 nr)
    = (i3,i2,i1)
    #! i2=a1.[1]
    #! i1=a1.[0]
    # nr=N_BITS_IN_INT-n
    # i3=ushiftr i2 nr
    # i2=(i2<<n) bitor (ushiftr i1 nr)
    # i1=(i1<<n)
    = (i3,i2,i1)

    shift_left_3 0 a
    #! s = size a1
    #! i2=a.[s-1]
    #! i1=a.[s-2]
    #! i0=a.[s-3]
    = (i2,i1,i0)
    shift_left_3 n a
    #! s = size a
    #! i2=a.[s-1]
    #! i1=a.[s-2]
    #! i0=a.[s-3]
    #! ix=if (s>3) a.[s-4] 0
    # nr=N_BITS_IN_INT-n
    # i2=(i2<<n) bitor (ushiftr i1 nr)
    # i1=(i1<<n) bitor (ushiftr i0 nr)
    # i0=(i0<<n) bitor (ushiftr ix nr)
    = (i2,i1,i0)

try_fast_div_1 :: !Int !Int !Int !Int !Int -> (!Bool,!Int)
try_fast_div_1 n2 n1 n0 d1 d0
    | n2==d1
    = (False,0) // to do
    # (r,q) = divdu n2 n1 d1
    # (ph,pl) = mulud d0 q
    | r+<ph || (r==ph && n0+<pl)
    # (nnh,nnl) = subLU (ph-r) pl n0
    | nnh+<d1 || (nnh==d1 && nnl+<d0)
    # q=q-1
    # (nh,n0) = subLU (d1-nnh) d0 nnl
    | nh<>0 || not (n0+<q)
    = (True,q)
    = (False,0)
    = (True,q-2)
    # (nh,n0) = subLU (r-ph) n0 pl
    | nh<>0 || not (n0+<q)
    = (True,q)
    = (False,0)

div_rem_integer :: !Integer !Integer -> (!*Integer,!*Integer)
div_rem_integer {integer_a=a1,integer_s=s1} {integer_a=a2,integer_s=s2}
    | size a1==0
    | size a2==0
    | s1<>MAX_NEG_INT || s2<> -1
    # q=s1/s2
    # r=s1-q*s2
    = ({integer_s=q,integer_a={}}, {integer_s=r,integer_a={}})
    = ({integer_s=0, integer_a={MAX_NEG_INT}}, {integer_s=0,integer_a={}})
    | size a2==1 && a2.[0]==MAX_NEG_INT && s1==MAX_NEG_INT
    | s2>=0
    = ({integer_s= -1,integer_a={}}, {integer_s=0,integer_a={}})
    = ({integer_s= 1,integer_a={}}, {integer_s=0,integer_a={}})
    = ({integer_s=0, integer_a={}}, {integer_s=s1,integer_a={}})
    // size a1<>0
    | size a2==0
    | s2>=0
    # (r,q) = div_rem_integer_unsigned_int a1 s2
    = (make_integer_s_a s1 q,make_integer_s_uint s1 r)
    # s = bitnot s1
      (r,q) = div_rem_integer_unsigned_int a1 (~s2)
    = (make_integer_s_a s q,make_integer_s_uint s1 r)
    | size a2==1
    #! s = s1 bitxor s2
    # (r,q) = div_rem_integer_unsigned_int a1 a2.[0]
    = (make_integer_s_a s q,make_integer_s_uint s1 r)
    | s1>=0
    | s2>=0
    = div_rem_integer_integer a1 a2
    # (q,r) = div_rem_integer_integer a1 a2
    = (make_positive_integer_negative q,r)
    | s2>=0
    # (q,r) = div_rem_integer_integer a1 a2
    = (q,make_positive_integer_negative r)
    # (q,r) = div_rem_integer_integer a1 a2
    = (make_positive_integer_negative q,make_positive_integer_negative r)

rem_integer :: !Integer !Integer -> *Integer
rem_integer {integer_a=a1,integer_s=s1} {integer_a=a2,integer_s=s2}
    | size a1==0
    | size a2==0
    | s1<>MAX_NEG_INT || s2<> -1
    # r=s1 rem s2
    = {integer_s=r,integer_a={}}
    = {integer_s=0,integer_a={}}
    | size a2==1 && a2.[0]==MAX_NEG_INT && s1==MAX_NEG_INT
    | s2>=0
    = {integer_s=0,integer_a={}}
    = {integer_s=0,integer_a={}}
    = {integer_s=s1,integer_a={}}
    // size a1<>0
    | size a2==0
    | s2>=0
    # (r,q) = div_rem_integer_unsigned_int a1 s2
    = make_integer_s_uint s1 r
    = abort "rem_integer 1"
    | size a2==1
    # (r,q) = div_rem_integer_unsigned_int a1 a2.[0]
    = make_integer_s_uint s1 r
    | s1>=0
    # (q,r) = div_rem_integer_integer a1 a2
    = r
    # (q,r) = div_rem_integer_integer a1 a2
    = make_positive_integer_negative r

div_loop :: !Int !*{#Int} !{#Int} -> *{#Int}
div_loop i a1 a2
    #! s=size a2
    | i<=s
    | i<>s
    = a1
    // last element, remainder not needed
    = div_last i s a1 a2
    #! h = a1.[i]
    #! d = a2.[s-1]
    | h==d
    # q = -1
    # b = i-s
    # (c,a1) = submul 0 0 b q a2 a1
    # (q,a1) = adjust (c-h) b i q a2 a1
    # a1 = {a1 & [i] = q}
    = div_loop (i-1) a1 a2
    #! l = a1.[i-1]
    # (r,q) = divdu h l d
    #! m = a2.[s-2]
    # (ph,pl) = mulud m q
    #! nl = a1.[i-2]
    | r+<ph || (r==ph && nl+<pl)
    # (nnh,nnl) = subLU (ph-r) pl nl
    | nnh+<d || (nnh==d && nnl+<m)
    # q=q-1
    # (nh,nl) = subLU (d-nnh) m nnl
    # (c,a1) = submul2 0 0 (i-s) q (s-2) a2 a1
    | nh==0 && nl+<c // && trace_tn "div_loop 1"
    # (nh,nl) = subLU nh nl c
    # a1 = {a1 & [i-2]=nl,[i-1]=nh}
    # (c2,a1) = add_integer_array 0 0 (i-s) a2 a1
    # a1 = {a1 & [i] = q-1}
    = div_loop (i-1) a1 a2
    # (nh,nl) = subLU nh nl c
    # a1 = {a1 & [i-2]=nl,[i-1]=nh,[i]=q}
    = div_loop (i-1) a1 a2
    # q=q-2
    # (nh,nl) = subLU (d-nnh) m nnl
    # (nh,nl) = addLU (nh+d) nl m
    # (c,a1) = submul2 0 0 (i-s) q (s-2) a2 a1
    | nh==0 && nl+<c
    = abort "div_loop 2" // impossible
    # (nh,nl) = subLU nh nl c
    # a1 = {a1 & [i-2]=nl,[i-1]=nh,[i]=q}
    = div_loop (i-1) a1 a2
    // | ph+<r || (ph==r && pl+<nl)
    # (c,a1) = submul2 0 0 (i-s) q (s-2) a2 a1
    # (nh,nl) = subLU (r-ph) nl pl
    | nh==0 && nl+<c // && trace_tn "\ndiv_loop 3" // for {0,0,0,1} / {1,0,2}
    # (nh,nl) = subLU nh nl c
    # a1 = {a1 & [i-2]=nl,[i-1]=nh}
    # (c2,a1) = add_integer_array 0 0 (i-s) a2 a1
    # a1 = {a1 & [i] = q-1}
    = div_loop (i-1) a1 a2
    # (nh,nl) = subLU nh nl c
    # a1 = {a1 & [i-2]=nl,[i-1]=nh,[i]=q}
    = div_loop (i-1) a1 a2

div_last :: !Int !Int !*{#Int} !{#Int} -> *{#Int}
div_last i s a1 a2
    #! h = a1.[i]
    #! d = a2.[s-1]
    | h==d
    # q = -1
    # b = i-s
    # (c,a1) = submul 0 0 b q a2 a1
    # (q,a1) = adjust (c-h) b i q a2 a1
    = {a1 & [i] = q}
    #! l = a1.[i-1]
    # (r,q) = divdu h l d
    #! m = a2.[s-2]
    # (ph,pl) = mulud m q
    #! nl = a1.[i-2]
    | r+<ph || (r==ph && nl+<pl)
    # (nnh,nnl) = subLU (ph-r) pl nl
    | nnh+<d || (nnh==d && nnl+<m)
    # q=q-1
    # (nh,nl) = subLU (d-nnh) m nnl
    | nh<>0 || not (nl+<q)
    = {a1 & [i] = q}
    # (c,a1) = submul2 0 0 (i-s) q (s-2) a2 a1
    | nl+<c // && trace_tn "div_loop 1"
    = {a1 & [i] = q-1}
    = {a1 & [i] = q}
    = {a1 & [i] = q-2}
    # (nh,nl) = subLU (r-ph) nl pl
    | nh<>0 || not (nl+<q)
    = {a1 & [i] = q}
    # (c,a1) = submul2 0 0 (i-s) q (s-2) a2 a1
    | nl+<c // && trace_tn "\ndiv_loop 3" // for {0,0,0,1} / {1,0,2}
    = {a1 & [i] = q-1}
    = {a1 & [i] = q}

div_rem_integer_a :: !*{#Int} !{#Int} -> (!*{#Int},!*Integer)
div_rem_integer_a a1 a2
    // highest bit of a2 must be set
    | size a1>0
    | size a2>1
/*
    # (q,r) = div_rem_integer_integer a1 a2
    | size q.a==0
    = ({q.s},r)
    = (q.a,r)
*/
    | size a1>=size a2
    # d_size = size a2
    | extra_int a1 a2
    | size a1==size a2
    = abort ("div_rem_integer_a 1") // div_integer_1_extra_int n_leading_zeros a1 a2

    # (a1,a2) = shift_left_nd_extra_int_0 a1 a2
    #! s = size a1
    # a1 = div_rem_loop (s-1) a1 a2
    # n = size a2
    # r = create_unintialized_int_array n
    # (a1,r) = copy_array_slice_u 0 n a1 r
    # a1 = remove_begin_elements n a1
    = (a1,int_array_to_unsigned_integer r)
    # n=size a1-size a2
    | n<=1
    | n==0
    = ({},{integer_s=0,integer_a=copy_array a1})
    = abort "div_rem_integer_a 1" // div_integer_1_no_extra_int n_leading_zeros a1 a2
    #! s = size a1
    # a1 = div_rem_loop (s-1) a1 a2
    # n = size a2
    # r = create_unintialized_int_array n
    # (a1,r) = copy_array_slice_u 0 n a1 r
    # a1 = remove_begin_elements n a1
    = (a1,int_array_to_unsigned_integer r)
    = ({},{integer_s=0,integer_a=copy_array a1})
    | size a2==1
    # (r,q) = div_rem_integer_unsigned_int a1 a2.[0]
    = (q,unsigned_int_to_integer r)
    = abort "div_rem_integer_a 1"
    = abort "div_rem_integer_a 2"

div_rem_integer_integer :: !{#Int} !{#Int} -> (!*Integer,!*Integer)
div_rem_integer_integer a1 a2
    | size a1>=size a2
    # d_size = size a2
    # n_leading_zeros = n_leading_zeros a2.[d_size-1]
    | extra_int a1 a2
    # (a1,a2) = shift_left_nd_extra_int n_leading_zeros a1 a2
    #! s = size a1
    # a1 = div_rem_loop (s-1) a1 a2
    # n = size a2
    # r = create_unintialized_int_array n
    # (a1,r) = copy_array_slice_u 0 n a1 r
    # r = shift_right n_leading_zeros r
    # a1 = remove_begin_elements n a1
    # r = int_array_to_unsigned_integer r
    | size a1==1
    #! q=a1.[0]
    | q>=0
    = ({integer_s=q, integer_a={}},r)
    = ({integer_s=0, integer_a=a1},r)
    = ({integer_s=0, integer_a=a1},r)
    | size a1==size a2
    = ({integer_s=0,integer_a={}} ,{integer_s=0,integer_a=copy_array a1})
    # (a1,a2) = shift_left_nd_no_extra_int n_leading_zeros a1 a2
    #! s = size a1
    # a1 = div_rem_loop (s-1) a1 a2
    # n = size a2
    # r = create_unintialized_int_array n
    # (a1,r) = copy_array_slice_u 0 n a1 r
    # r = shift_right n_leading_zeros r
    # a1 = remove_begin_elements n a1
    # r = int_array_to_unsigned_integer r
    | size a1==1
    #! q=a1.[0]
    | q>=0
    = ({integer_s=q, integer_a={}},r)
    = ({integer_s=0, integer_a=a1},r)
    = ({integer_s=0, integer_a=a1},r)
    = ({integer_s=0,integer_a={}},{integer_s=0,integer_a=copy_array a1})

div_rem_loop :: !Int !*{#Int} !{#Int} -> *{#Int}
div_rem_loop i a1 a2
    #! s=size a2
    | i<s
    = a1
    #! h = a1.[i]
    #! d = a2.[s-1]
    | h==d
    # q = -1
    # b = i-s
    # (c,a1) = submul 0 0 b q a2 a1
    # (q,a1) = adjust (c-h) b i q a2 a1
    # a1 = {a1 & [i] = q}
    = div_rem_loop (i-1) a1 a2
    #! l = a1.[i-1]
    # (r,q) = divdu h l d
    #! m = a2.[s-2]
    # (ph,pl) = mulud m q
    #! nl = a1.[i-2]
    | r+<ph || (r==ph && nl+<pl)
    # (nnh,nnl) = subLU (ph-r) pl nl
    | nnh+<d || (nnh==d && nnl+<m)
    # q=q-1
    # (nh,nl) = subLU (d-nnh) m nnl
    # (c,a1) = submul2 0 0 (i-s) q (s-2) a2 a1
    | nh==0 && nl+<c // && trace_tn "div_rem_loop 1"
    # (nh,nl) = subLU nh nl c
    # a1 = {a1 & [i-2]=nl,[i-1]=nh}
    # (c2,a1) = add_integer_array 0 0 (i-s) a2 a1
    # a1 = {a1 & [i] = q-1}
    = div_rem_loop (i-1) a1 a2
    # (nh,nl) = subLU nh nl c
    # a1 = {a1 & [i-2]=nl,[i-1]=nh,[i]=q}
    = div_rem_loop (i-1) a1 a2
    # q=q-2
    # (nh,nl) = subLU (d-nnh) m nnl
    # (nh,nl) = addLU (nh+d) nl m
    # (c,a1) = submul2 0 0 (i-s) q (s-2) a2 a1
    | nh==0 && nl+<c
    = abort "div_rem_loop 2" // impossible
    # (nh,nl) = subLU nh nl c
    # a1 = {a1 & [i-2]=nl,[i-1]=nh,[i]=q}
    = div_rem_loop (i-1) a1 a2
    // | ph+<r || (ph==r && pl+<nl)
    # (c,a1) = submul2 0 0 (i-s) q (s-2) a2 a1
    # (nh,nl) = subLU (r-ph) nl pl
    | nh==0 && nl+<c // && trace_tn "\ndiv_loop 3" // for {0,0,0,1} / {1,0,2}
    # (nh,nl) = subLU nh nl c
    # a1 = {a1 & [i-2]=nl,[i-1]=nh}
    # (c2,a1) = add_integer_array 0 0 (i-s) a2 a1
    # a1 = {a1 & [i] = q-1}
    = div_rem_loop (i-1) a1 a2
    # (nh,nl) = subLU nh nl c
    # a1 = {a1 & [i-2]=nl,[i-1]=nh,[i]=q}
    = div_rem_loop (i-1) a1 a2

div_rem_integer_unsigned_int :: !{#Int} !Int -> (!Int,!*{#Int})
div_rem_integer_unsigned_int a1 s2
    # s=size a1-1
    # l=a1.[s]
    | l+<s2
    # da = create_unintialized_int_array s
    = div_uint_a_unsigned_int (s-1) l s2 a1 da
    # da = create_unintialized_int_array (s+1)
    # (r,q) = divdu 0 l s2
    # da = {da & [s]=q}
    = div_uint_a_unsigned_int (s-1) r s2 a1 da

extra_int :: !{#Int} !{#Int} -> Bool
extra_int a1 a2
    // size a1>=size a2
    = extra_int (size a1-1) (size a2-1) a1 a2
  where
    extra_int :: !Int !Int !{#Int} !{#.Int} -> Bool
    extra_int a1_i a2_i a1 a2
    | a1_i<0
    = True
    | a1.[a1_i]==a2.[a2_i]
    = extra_int (a1_i-1) (a2_i-1) a1 a2
    = a2.[a2_i]+<a1.[a1_i]

shift_left_nd_extra_int 0 a1 a2
    = shift_left_nd_extra_int_0 a1 a2
shift_left_nd_extra_int n_leading_zeros a1 a2
    # a2 = shift_left n_leading_zeros a2
    # s=size a1
    # da1 = create_unintialized_int_array (s+1)
    # (c,a1) = shift_left_bits_a 0 0 n_leading_zeros (N_BITS_IN_INT-n_leading_zeros) a1 da1
    # a1 = {a1 & [s]=c}
    = (a1,a2)

shift_left_nd_extra_int_0 a1 a2
    # s = size a1
    # da1 = {create_unintialized_int_array (s+1) & [s]=0}
    = (copy_array_slice 0 s a1 da1, a2)

shift_left_nd_no_extra_int 0 a1 a2
    = (copy_array a1,a2)
shift_left_nd_no_extra_int n_leading_zeros a1 a2
    # a2 = shift_left n_leading_zeros a2
    # a1 = shift_left n_leading_zeros a1
    = (a1,a2)

shift_left n_bits a
    # (_,a) = shift_left_bits_a 0 0 n_bits (N_BITS_IN_INT-n_bits) a (create_unintialized_int_array (size a))
    = a

submul :: !Int !Int !Int !Int !{#Int} !*{#Int} -> (!Int,!*{#Int})
submul i c d m sa da
    | i<size sa
    #! sai=sa.[i]
    # (c0,p0) = mulud sai m
    # (c1,p1) = addLU c0 p0 c
    # j=i+d
    #! daj=da.[j]
    # (c2,p) = subLU 0 daj p1
    # c = c1-c2
    # da = {da & [j]=p}
    = submul (i+1) c d m sa da
    = (c,da)

submul2 :: !Int !Int !Int !Int !Int !{#Int} !*{#Int} -> (!Int,!*{#Int})
submul2 i c d m e_i sa da
    | i<e_i
    #! sai=sa.[i]
    # (c0,p0) = mulud sai m
    # (c1,p1) = addLU c0 p0 c
    # j=i+d
    #! daj=da.[j]
    # (c2,p) = subLU 0 daj p1
    # c = c1-c2
    # da = {da & [j]=p}
    = submul2 (i+1) c d m e_i sa da
    = (c,da)

adjust :: !Int !Int !Int !Int !{#Int} !*{#Int} -> (!Int,!*{#Int})
adjust cmh b i q a2 a1
    | cmh==0
    | less (size a2-1) b a1 a2
    = (q,a1)
    = abort "adjust 1"
    | cmh>0
    # (c2,a1) = add_integer_array 0 0 b a2 a1
    | c2==cmh && q<>0
    | less (size a2-1) b a1 a2
    = (q-1,a1)
    = abort "adjust 2"
    = abort "second add_integer_array"
/*
    | c2==0
    # (c2,a1) = add_integer_array 0 0 b a2 a1
    | c2==cmh && q<>0
    | less (size a2-1) b a1 a2
    = (q-2,a1)
    = abort "adjust 3"
    = abort ("adjust estimate too high "+++toString cmh+++" "+++toString c2)
*/
    = abort ("adjust estimate too low "+++toString cmh)

less :: !Int !Int !{#Int} !{#Int} -> Bool
less i b a1 a2
    | i>=0
    | a1.[i+b]+<a2.[i]
    = True
    | a1.[i+b]==a2.[i]
    = less (i-1) b a1 a2
    = False
    = False

shift_left_bits_a :: !Int !Int !Int !Int !{#Int} !*{#Int} -> (!Int,!*{#Int})
shift_left_bits_a i c n n32 sa da
    | i<size sa
    #! sai=sa.[i]
    # da = {da & [i]=c+(sai<<n)}
    # c = ushiftr sai n32
    = shift_left_bits_a (i+1) c n n32 sa da
    = (c,da)

shift_right :: !Int !*{#Int} -> *{#Int}
shift_right n a
    | n==0
    = a
    #! s=size a
    = shift_right_bits_a (s-1) 0 n (N_BITS_IN_INT-n) a
  where
    shift_right_bits_a :: !Int !Int !Int !Int !*{#Int} -> *{#Int}
    shift_right_bits_a i c n n32 a
    | i>=0
    #! ai=a.[i]
    # a = {a & [i]=c+(ushiftr ai n)}
    # c = ai<<n32
    = shift_right_bits_a (i-1) c n n32 a
    = a

shift_left_array_elements :: !Int !Int !*{#Int} -> *{#Int}
shift_left_array_elements s_i d_i a
    | s_i<size a
    #! e=a.[s_i]
    # a = {a & [d_i]=e}
    = shift_left_array_elements (s_i+1) (d_i+1) a
    = a

remove_begin_elements :: !Int !*{#Int} -> *{#Int}
remove_begin_elements n a
    # a = shift_left_array_elements n 0 a
    #! sa = size a
    = resize_int_array (sa-n) a

add_integer_array :: !Int !Int !Int !{#Int} !*{#Int} -> (!Int,!*{#Int})
add_integer_array i carry d sa da
    | i<size sa
    # i1=sa.[i]
    # (carry,n) = addLU 0 i1 carry
//		# (carry,n) = addLU 0 carry i1
    # j=i+d
    #! i2=da.[j]
    # (carry,n) = addLU carry n i2
    # da = {da & [j]=n}
    = add_integer_array (i+1) carry d sa da
    = (carry,da)

/*
sub_integer_arrays :: !Int !Int !Int !{#Int} !{#Int} !*{#Int} -> (!Int,!*{#Int})
sub_integer_arrays i borrow s a1 a2 da
    | i<s
    # i1=a1.[i]
    # (borrow,n) = subLU 0 i1 borrow
    # i2=a2.[i]
    # (borrow,n) = subLU borrow n i2
    # da = {da & [i]=n}
    = sub_integer_arrays (i+1) (~borrow) s a1 a2 da
    = (borrow,da)

test_count_leading_zeros n
    | n<>0
    | count_leading_zeros n==count_leading_zeros_0 n 0
    = test_count_leading_zeros (n+1)
    = abort ("test_count_leading_zeros failed for "+++toString n)
    = n
  where
    count_leading_zeros_0 n i
    | n+<MAX_NEG_INT
    = count_leading_zeros_0 (n+n) (i+1)
    = i
}
*/

/*
count_leading_zeros_nibble n :== ((0x000055af>>(n+n)) bitand 3)

count_leading_zeros n :== IF_INT_64_OR_32 (count_leading_zeros_64 n) (count_leading_zeros_32 n)

count_leading_zeros_64 n // n<>0
/*
    | ushiftr n 32==0
    = 32+count_leading_zeros_32 n
    = count_leading_zeros_32 (ushiftr n 32)
*/
    | n+<0x100000000
    | n+<0x10000
    | n+<0x100
    | n+<0x10
    = 60 + count_leading_zeros_nibble n
    = 56 + count_leading_zeros_nibble (n>>4)
    | n+<0x1000
    = 52 + count_leading_zeros_nibble (n>>8)
    = 48 + count_leading_zeros_nibble (n>>12)
    | n+<0x1000000
    | n+<0x100000
    = 44 + count_leading_zeros_nibble (n>>16)
    = 40 + count_leading_zeros_nibble (n>>20)
    | n+<0x10000000
    = 36 + count_leading_zeros_nibble (n>>24)
    = 32 + count_leading_zeros_nibble (n>>28)
    | n+<0x1000000000000
    | n+<0x10000000000
    | n+<0x1000000000
    = 28 + count_leading_zeros_nibble (n>>32)
    = 24 + count_leading_zeros_nibble (n>>36)
    | n+<0x100000000000
    = 20 + count_leading_zeros_nibble (n>>40)
    = 16 + count_leading_zeros_nibble (n>>44)
    | n+<0x100000000000000
    | n+<0x10000000000000
    = 12 + count_leading_zeros_nibble (n>>48)
    = 8 + count_leading_zeros_nibble (n>>52)
    | n+<0x1000000000000000
    = 4 + count_leading_zeros_nibble (n>>56)
    = count_leading_zeros_nibble (ushiftr n 60)

count_leading_zeros_32 n // n<>0
    | n+<0x10000
    | n+<0x100
    | n+<0x10
    = 28 + count_leading_zeros_nibble n
    = 24 + count_leading_zeros_nibble (n>>4)
    | n+<0x1000
    = 20 + count_leading_zeros_nibble (n>>8)
    = 16 + count_leading_zeros_nibble (n>>12)
    | n+<0x1000000
    | n+<0x100000
    = 12 + count_leading_zeros_nibble (n>>16)
    = 8 + count_leading_zeros_nibble (n>>20)
    | n+<0x10000000
    = 4 + count_leading_zeros_nibble (n>>24)
    = count_leading_zeros_nibble (ushiftr n 28)
*/

div_uint_a_unsigned_int :: !Int !Int !Int {#Int} !*{#Int} -> (!Int,!*{#Int})
div_uint_a_unsigned_int i r d sa da
    | i>=0
    #! sai=sa.[i]
    # (r,q) = divdu r sai d
    # da = {da & [i]=q}
    = div_uint_a_unsigned_int (i-1) r d sa da
    = (r,da)

copy_array :: !{#Int} -> *{#Int}
copy_array a
    = {e\\e<-:a}

copy_array_slice :: !Int !Int !{#Int} !*{#Int} -> *{#Int}
copy_array_slice i s sa da
    | i<s
    # da = {da & [i]=sa.[i]}
    = copy_array_slice (i+1) s sa da
    = da

copy_array_slice_u :: !Int !Int !*{#Int} !*{#Int} -> (!*{#Int},!*{#Int})
copy_array_slice_u i s sa da
    | i<s
    #! sai=sa.[i]
    # da = {da & [i]=sai}
    = copy_array_slice_u (i+1) s sa da
    = (sa,da)

make_integer_s_uint s n
    | n>=0
    | s>=0
    = {integer_s=n,integer_a={}}
    = {integer_s= ~n,integer_a={}}
    | n==MAX_NEG_INT && s<0
    = {integer_s=n,integer_a={}}
    = {integer_s=s,integer_a={n}}

// size a>0
make_integer_s_a s a
    #! sa = size a
    # n=a.[sa-1]
    | n<>0
    | sa<>1
    = {integer_s=s,integer_a=a}
    | n>=0
    | s>=0
    = {integer_s=n,integer_a={}}
    = {integer_s= ~n,integer_a={}}
    | n==MAX_NEG_INT && s<0
    = {integer_s=n,integer_a={}}
    = {integer_s=s,integer_a=a}

floordiv_integer :: !Integer !Integer -> *Integer
floordiv_integer a b
    | a.integer_s bitxor b.integer_s>=0
    = div_integer a b
    # (q,r) = div_rem_integer a b
    | r.integer_s==0 && size r.integer_a==0
    = q
    = sub_integer q {integer_s=1,integer_a={}}

mod_integer :: !Integer !Integer -> *Integer
mod_integer a b
    | a.integer_s bitxor b.integer_s>=0
    = rem_integer a b
    # r = rem_integer a b
    | r.integer_s==0 && size r.integer_a==0
    = r
    = add_integer r b

floordiv_mod_integer :: !Integer !Integer -> (!*Integer,!*Integer)
floordiv_mod_integer a b
    | a.integer_s bitxor b.integer_s>=0
    = div_rem_integer a b
    # (q,r) = div_rem_integer a b
    | r.integer_s==0 && size r.integer_a==0
    = (q,r)
    = (sub_integer q {integer_s=1,integer_a={}},add_integer r b)
