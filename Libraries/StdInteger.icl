implementation module StdInteger

import StdEnv
import StdIntegerToString, StdStringToInteger, StdIntegerAdd, StdIntegerMul,StdIntegerDiv

instance toChar Integer where
    toChar i = toChar (toInt i)

instance toInt Integer where
    toInt {integer_s,integer_a}
    	| size integer_a == 0 = integer_s
        | otherwise = (integer_a.[0] bitxor integer_s) - integer_s

// instance toReal Integer where
//     toReal {integer_s,integer_a} = integer_a_to_double integer_s integer_a

instance toString Integer where
    toString i = integer_to_string i

instance toInteger Char where
    toInteger c = toInteger (toInt c)

instance toInteger Int where
    toInteger i = code inline {
    	pushI 0
    	create_array_ INT 0 1
    }

// instance toInteger Real where
//     toInteger r = toInteger (toInt c)

instance toInteger String where
    toInteger s = string_to_integer s

instance +				Integer
where
	(+) a b = add_integer a b

instance *  			Integer
where
	(*) a b = mul_integer a b

instance zero			Integer
where
    zero = {integer_s = 0, integer_a = {}}
instance one			Integer
where
    one = {integer_s = 1, integer_a = {}}

instance ~				Integer
where
	~ {integer_s,integer_a}
		| size integer_a==0
			| integer_s <> IF_INT_64_OR_32 0x8000000000000000 0x80000000
				= {integer_s = ~integer_s, integer_a = integer_a}
				= {integer_s = 0, integer_a = {integer_s}}
			= {integer_s = bitnot integer_s, integer_a = integer_a}
instance -  			Integer
where
	(-) a b = sub_integer a b

instance abs			Integer
where
	abs {integer_s,integer_a}
		| size integer_a==0
			| integer_s>=0
				= {integer_s = integer_s, integer_a = integer_a}
			| integer_s <> IF_INT_64_OR_32 0x8000000000000000 0x80000000
				= {integer_s = ~integer_s, integer_a = integer_a}
				= {integer_s = 0, integer_a = {integer_s}}
			= {integer_s = 0, integer_a = integer_a}

instance sign			Integer
where
	sign {integer_s,integer_a}
		| size integer_a==0
			= sign integer_s
			= 1+integer_s+integer_s//FIXME

instance /				Integer
where
	(/) a b = floordiv_integer a b
instance rem            Integer
where
	(rem) a b = rem_integer a b

//instance ^				Integer

instance ==				Integer
where
	(==) {integer_s=s1,integer_a=a1} {integer_s=s2,integer_a=a2}
		| s1<>s2
			= False
		# sa1=size a1
		  sa2=size a2
		| sa1==0 && sa2==0
			= True
		| sa1<>sa2
			= False
			= equal_arrays 0 sa1 a1 a2
		where
			equal_arrays :: !Int !Int !{#Int} !{#Int} -> Bool
			equal_arrays i s a1 a2
				| i<s
					| a1.[i]==a2.[i]
						= equal_arrays (i+1) s a1 a2
						= False
					= True

instance <  			Integer
where
	(<) {integer_s=s1,integer_a=a1} {integer_s=s2,integer_a=a2}
		| size a1==0
			| size a2==0
				= s1<s2
				= 0<=s2
			| size a2==0
				= s1<0
				| s1<>s2
					= s1<s2
					# sa1=size a1
					# sa2=size a2
					= compare_same_sign_large_integers sa1 sa2 s1 a1 a2 == LT

compare_same_sign_large_integers :: !Int !Int !Int !{#Int} !{#Int} -> Ordering
compare_same_sign_large_integers sa1 sa2 s1 a1 a2
	| s1>=0
		| sa1<sa2
			= LT
		| sa1>sa2
			= GT
			= compare_p_arrays (sa1-1) a1 a2
		| sa1<sa2
			= GT
		| sa1>sa2
			= LT
			= compare_n_arrays (sa1-1) a1 a2
where
	compare_p_arrays :: !Int !{#Int} !{#Int} -> Ordering
	compare_p_arrays i a1 a2
		| i>=0
			| a1.[i]==a2.[i]
				= compare_p_arrays (i-1) a1 a2
			| a1.[i]+<a2.[i]
				= LT
				= GT
			= EQ
	compare_n_arrays :: !Int !{#Int} !{#Int} -> Ordering
	compare_n_arrays i a1 a2
		| i>=0
			| a1.[i]==a2.[i]
				= compare_n_arrays (i-1) a1 a2
			| a1.[i]+<a2.[i]
				= GT
				= LT
			= EQ

:: Ordering :== Int

GT :== 1
EQ :== 0
LT :== -1

ushiftr :: !Int !Int -> Int;
ushiftr a b = code inline {
	shiftrU
}

ltU :: !Int !Int -> Bool;
ltU a b = code inline {
	ltU
}

divdu :: !Int !Int !Int -> (!Int,!Int);
divdu a b c = code inline {
	divLU
}

mulud :: !Int !Int -> (!Int,!Int);
mulud a b = code inline {
	mulUUL
}

addLU :: !Int !Int !Int -> (!Int,!Int);
addLU _ _ _ = code inline {
	addLU
}

subLU :: !Int !Int !Int -> (!Int,!Int);
subLU _ _ _ = code inline {
	subLU
}

(+<) infix 4;
(+<) a b :== ltU a b;

(+>) infix 4;
(+>) a b :== ltU b a;

(+>>) infix 7;
(+>>) a b :== ushiftr a b;

/*
IntMaxUIntD2 :== IF_INT_64_OR_32 9223372036854775808 2147483648
RealMaxUIntD2 :== IF_INT_64_OR_32 9223372036854775808.0 2147483648.0
RealNegMaxUIntD2 :== IF_INT_64_OR_32 (-9223372036854775808.0) (-2147483648.0)

integer_a_to_double :: !Int -> !{#Int} -> Double
integer_a_to_double s a
	#! n=size a;
	| n==1
		# r = toReal (a~[0]-IntMaxUIntD2);
		| s>=0
			= r+RealMaxUIntD2;
			= RealNegMaxUIntD2-r;
		/*
		| s>=0
			= (int2_to_double a~[0] 0x43300000) - 4503599627370496.0 /* 2^52 */;
			= 4503599627370496.0 /* 2^52 */ - (int2_to_double a~[0] 0x43300000);
		*/
		// to do: 64 bits
		# h=a~[n-1];
		# l=a~[n-2];
		| ltU h 0x100000
			# shift_l = n_leading_zeros h-11;
			| n==2
				# h = (h<<shift_l)+(ushiftr l (32-shift_l));
				# l = l<<shift_l;
				# exp = 1075 /*0x3ff+52*/-shift_l
				= make_double_no_denormal_or_zero s exp h l
				# m = a~[n-3];
				# shift_r = 32-shift_l
				# f = m<<shift_l
				# h = (h<<shift_l)+(ushiftr l shift_r)
				# l = (l<<shift_l)+(ushiftr m shift_r)
				# exp = 1011-shift_l+(n<<5) // (0x3ff+52)-shift_l+((n-2)<<5)
				| f>=0 || (l bitand 1==0 && f==0x80000000 && rest_all_zeros (n-4) a)
					= make_double_no_denormal_or_zero s exp h l
					= make_next_double s exp h l
			# shift_r = 11-n_leading_zeros h
			| shift_r==0
				# exp = 1011+(n<<5) // 0x3ff+52+((n-2)<<5)
				| n==2
					= int2_to_double l ((s bitand 0x80000000)+(exp<<20)+(h bitand 0x0fffff))
				# f=a~[n-3]
				| f>=0 || (l bitand 1==0 && f==0x80000000 && rest_all_zeros (n-4) a)
					= make_double_no_denormal_or_zero s exp h l
					= make_next_double s exp h l
				# shift_l = 32-shift_r
				# f = l<<shift_l
				# l = (ushiftr l shift_r)+(h<<shift_l)
				# h = (ushiftr h shift_r);
				# exp = 1011+shift_r+(n<<5) // (0x3ff+52)+shift_r+((n-2)<<5)
				| f>=0 || (l bitand 1==0 && f==0x80000000 && rest_all_zeros (n-3) a)
					= make_double_no_denormal_or_zero s exp h l
					= make_next_double s exp h l
where
	make_next_double -: !Int !Int !Int !Int -> Double
	make_next_double s exp h l
		# l=l+1
		| not (l==0)
			= make_double_no_denormal_or_zero s exp h l
		# h=h+1
		| ltU h 0x200000
			= make_double_no_denormal_or_zero s exp h l
			# h=0x100000
			# exp=exp+1
			= make_double_no_denormal_or_zero s exp h l

integer_a_shifted_to_int53 -: !{#Int} !Int -> (!Int,!Int,!Int)
integer_a_shifted_to_int53 a sub_exp
	#! n=size a; // >= 2
	// to do: 64 bits
	# h=a~[n-1];
	# l=a~[n-2];
	| ltU h 0x100000
		# shift_l = n_leading_zeros h-11;
		| n==2
			# h = (h<<shift_l)+(ushiftr l (32-shift_l));
			# l = l<<shift_l;
			# exp = 1075 /*0x3ff+52*/-shift_l-sub_exp
			| exp>0
				= (exp,h,l)
				= round_exact_denormal exp h l
			# m = a~[n-3];
			# shift_r = 32-shift_l
			# f = m<<shift_l
			# h = (h<<shift_l)+(ushiftr l shift_r)
			# l = (l<<shift_l)+(ushiftr m shift_r)
			# exp = 1011-shift_l+(n<<5)-sub_exp // (0x3ff+52)-shift_l+((n-2)<<5)
			| exp>0
				| f>=0 || (l bitand 1==0 && f==0x80000000 && rest_all_zeros (n-4) a)
					= (exp,h,l)
					= next_int53_no_denormal_or_zero exp h l
				| f==0 && rest_all_zeros (n-4) a
					= round_exact_denormal exp h l
					= round_inexact_denormal exp h l
		# shift_r = 11-n_leading_zeros h
		| shift_r==0
			# exp = 1011+(n<<5)-sub_exp // 0x3ff+52+((n-2)<<5)
			| n==2
				| exp>0
					= (exp,h,l);
					= round_exact_denormal exp h l
			# f=a~[n-3]
			| exp>0
				| f>=0 || (l bitand 1==0 && f==0x80000000 && rest_all_zeros (n-4) a)
					= (exp,h,l)
					= next_int53_no_denormal_or_zero exp h l
				| f==0 && rest_all_zeros (n-4) a
					= round_exact_denormal exp h l
					= round_inexact_denormal exp h l
			# shift_l = 32-shift_r
			# f = l<<shift_l
			# l = (ushiftr l shift_r)+(h<<shift_l)
			# h = (ushiftr h shift_r);
			# exp = 1011+shift_r+(n<<5)-sub_exp // (0x3ff+52)+shift_r+((n-2)<<5)
			| exp>0
				| f>=0 || (l bitand 1==0 && f==0x80000000 && rest_all_zeros (n-3) a)
					= (exp,h,l)
					= next_int53_no_denormal_or_zero exp h l
				| f==0 && rest_all_zeros (n-3) a
					= round_exact_denormal exp h l
					= round_inexact_denormal exp h l

round_exact_denormal -: !Int !Int !Int -> (!Int,!Int,!Int)
round_exact_denormal exp h l
	| exp >= -30
		| exp==0
			| l bitand 3<>3
				# l = (l+>>1)+(h<<31)
				# h = h+>>1
				= (0,h,l);
				# (h,l) = addLU h l 1
				| h+<0x200000
					# l = (l+>>1)+(h<<31)
					# h = h+>>1
					= (0,h,l);
					= (1,0x100000,0)
			# shift_r = 1-exp
			# shift_l = 31+exp // 32-shift_r
			# f = l<<shift_l
			# l = (l+>>shift_r)+(h<<shift_l)
			# h = h+>>shift_r
			| f>=0 || (l bitand 1==0 && f==0x80000000)
				= (0,h,l)
				# (h,l) = addLU h l 1
				= (0,h,l)
	| exp >= -51
		| exp == -31
			| l>=0 || (h bitand 1==0 && l==0x80000000)
				= (0,0,h)
				= (0,0,h+1)
			# shift_r = -31-exp
			# shift_l = 63+exp // 32-shift_r
			# fl = l<<shift_l
			# fh = (l+>>shift_r)+(h<<shift_l)
			# l = h+>>shift_r
			| fh>=0 || (l bitand 1==0 && fh==0x80000000 && fl==0)
				= (0,0,l)
				# (h,l) = addLU 0 l 1
				= (0,h,l)
	| exp == -52 && h bitand 0x180000==0x180000
		= (0,0,1)
		= (0,0,0)

round_inexact_denormal -: !Int !Int !Int -> (!Int,!Int,!Int)
round_inexact_denormal exp h l
	| exp >= -30
		| exp==0
			| l bitand 1==0
				# l = (l+>>1)+(h<<31)
				# h = h+>>1
				= (0,h,l);
				# (h,l) = addLU h l 1
				| h+<0x200000
					# l = (l+>>1)+(h<<31)
					# h = h+>>1
					= (0,h,l);
					= (1,0x100000,0)
			# shift_r = 1-exp
			# shift_l = 31+exp // 32-shift_r
			# f = l<<shift_l
			# l = (l+>>shift_r)+(h<<shift_l)
			# h = h+>>shift_r
			| f>=0
				= (0,h,l)
				# (h,l) = addLU h l 1
				= (0,h,l)
	| exp >= -51
		| exp == -31
			| l>=0 || (h bitand 1==0 && l==0x80000000)
				= (0,0,h)
				= (0,0,h+1)
			# shift_r = -31-exp
			# shift_l = 63+exp // 32-shift_r
			# l = h+>>shift_r
			| h<<shift_l>=0
				= (0,0,l)
				# (h,l) = addLU 0 l 1
				= (0,h,l)
	| exp == -52
		= (0,0,1)
		= (0,0,0)

rational_to_double :: !Rational -> Double
rational_to_double (n :% d)
	| size d~integer_a==0
		# di=d~integer_s
		# sna = size n~integer_a
		| sna==0
			= toReal n~integer_s / toReal di
		| sna==1
			# r = toReal (n~integer_a~[0]-IntMaxUIntD2);
			| n~integer_s>=0
				= (r+RealMaxUIntD2) / toReal di
				= (RealNegMaxUIntD2-r) / toReal di
		// sna>1
			| di==1
				= integer_a_to_double n~integer_s n~integer_a
			| di bitand (di-1)==0
				| di==0
					= infinity n~integer_s
				# (exp,h,l) = integer_a_shifted_to_int53 n~integer_a (31-n_leading_zeros di)
				= make_double_no_denormal_or_zero n~integer_s exp h l
				# (exp,h,l) = integer_div_n_to_int53 n~integer_a di
				= make_double_no_denormal_or_zero n~integer_s exp h l
	| size d~integer_a==1
		# sna = size n~integer_a
		# di = d~integer_a~[0]
		| sna==0
			# dr = (toReal (di-IntMaxUIntD2))+RealMaxUIntD2
			= toReal n~integer_s / dr
		| sna==1
			# dr = (toReal (di-IntMaxUIntD2))+RealMaxUIntD2
			# r = toReal (n~integer_a~[0]-IntMaxUIntD2)
			| n~integer_s>=0
				= (r+RealMaxUIntD2) / dr
				= (RealNegMaxUIntD2-r) / dr
		| di==IntMaxUIntD2
			# (exp,h,l) = integer_a_shifted_to_int53 n~integer_a 31
			= make_double_no_denormal_or_zero n~integer_s exp h l
			# (exp,h,l) = integer_div_n_to_int53 n~integer_a di
			= make_double_no_denormal_or_zero n~integer_s exp h l
		# da=d~integer_a
		# sda=size da
		# d0=da~[sda-1]
		| (d0 bitand (d0-1))==0 && skip_zeros (sda-2) da<0
			# shift = ((sda-1)<<5)+31-n_leading_zeros da~[sda-1]
			= shifted_integer_to_double n shift
		# sna = size n~integer_a
		| sda==2
			# d0 = da~[1]
			# d1 = da~[0]
			| d0<0
				| sna==0
					# ni=n~integer_s
					| ni>=0
						= int_div_n2_shift_to_double ni d0 d1 0 0
						= int_div_n2_shift_to_double (-ni) d0 d1 0 (-1)
					| sna==1
						= int_div_n2_shift_to_double n~integer_a~[0] d0 d1 0 n~integer_s
						= integer_div_n2_shift_to_double n~integer_a d0 d1 0 n~integer_s
				# shift_d = n_leading_zeros d0
				# d0=(d0<<shift_d)+(ushiftr d1 (32-shift_d))
				# d1=d1<<shift_d
				| sna==0
					# ni=n~integer_s
					| ni>=0
						= int_div_n2_shift_to_double ni d0 d1 shift_d 0
						= int_div_n2_shift_to_double (-ni) d0 d1 shift_d (-1)
					| sna==1
						= int_div_n2_shift_to_double n~integer_a~[0] d0 d1 shift_d n~integer_s
						= integer_div_n2_shift_to_double n~integer_a d0 d1 shift_d n~integer_s
			# sda = size da
			# d0 = da~[sda-1]
			# d1 = da~[sda-2]
			| d0<0
				| sna==0
					# ni=n~integer_s
					| ni>=0
						= integer_div_nm_shift_to_double1 ni d0 d1 da 0 0
						= integer_div_nm_shift_to_double1 (-ni) d0 d1 da 0 (-1)
					= integer_div_nm_shift_to_double n~integer_a d0 d1 da 0 n~integer_s
				# d2 = da~[sda-3]
				# shift_d = n_leading_zeros d0
				# shift_d_c = 32-shift_d
				# d0=(d0<<shift_d)+(ushiftr d1 shift_d_c)
				# d1=(d1<<shift_d)+(ushiftr d2 shift_d_c)
				| sna==0
					# ni=n~integer_s
					| ni>=0
						= integer_div_nm_shift_to_double1 ni d0 d1 da shift_d 0
						= integer_div_nm_shift_to_double1 (-ni) d0 d1 da shift_d (-1)
					= integer_div_nm_shift_to_double n~integer_a d0 d1 da shift_d n~integer_s
where
	skip_zeros i a
		| i>=0 && a~[i]==0
			= skip_zeros (i-1) a
			= i

int_div_n2_shift_to_double n0 d0 d1 shift_d sign
	| n0<0
		= n4_div_n2_shift_to_double n0 0 0 0 d0 d1 0 shift_d (-1) sign
		# shift_n = n_leading_zeros n0
		# n0=n0<<shift_n
		= n4_div_n2_shift_to_double n0 0 0 0 d0 d1 shift_n shift_d (-1) sign

integer_div_n2_shift_to_double na d0 d1 shift_d sign
	# sna = size na
	# (n0,n1,n2,n3,shift_n) = get_n4 na
	= n4_div_n2_shift_to_double n0 n1 n2 n3 d0 d1 shift_n shift_d (sna-2) sign

integer_div_nm_shift_to_double1 n0 d0 d1 da shift_d sign
	#! shift_n_elements = 1-size da
	| n0<0
		= integer_div_nm_shift_to_double_4 n0 0 0 0 0 shift_n_elements {} d0 d1 da shift_d sign
		# shift_n = n_leading_zeros n0
		# n0=n0<<shift_n
		= integer_div_nm_shift_to_double_4 n0 0 0 0 shift_n shift_n_elements {} d0 d1 da shift_d sign

integer_div_nm_shift_to_double na d0 d1 da shift_d sign
	# (n0,n1,n2,n3,shift_n) = get_n4 na
	#! shift_n_elements = size na-size da
	= integer_div_nm_shift_to_double_4 n0 n1 n2 n3 shift_n shift_n_elements na d0 d1 da shift_d sign

integer_div_nm_shift_to_double_4 n0 n1 n2 n3 shift_n shift_n_elements na d0 d1 da shift_d sign
	| n0+<d0 || (n0==d0 && n1+<d1)
		# (exp,h,l) = integer_div_nm_shift_to_int53 n0 n1 n2 n3 na d0 d1 da shift_n shift_d (shift_n_elements<<5)
		= make_double sign exp h l
		# n3=(n3+>>1)+(n2<<31)
		# n2=(n2+>>1)+(n1<<31)
		# n1=(n1+>>1)+(n0<<31)
		# n0=n0+>>1
		# shift_n=shift_n-1
		# (exp,h,l) = integer_div_nm_shift_to_int53 n0 n1 n2 n3 na d0 d1 da shift_n shift_d (shift_n_elements<<5)
		= make_double sign exp h l
where
	integer_div_nm_shift_to_int53 -: !Int !Int !Int !Int !{#Int} !Int !Int !{#Int} !Int !Int !Int -> (!Int,!Int,!Int)
	integer_div_nm_shift_to_int53 n0 n1 n2 n3 na d0 d1 da shift_n shift_d add_exp
		# (q0,q1,r0,r1) = n4_div_rem_n2 n0 n1 n2 n3 d0 d1
		| not (r0+<d0 || (r0==d0 && r1+<d1))
			= abort `"integer_div_nm_shift_to_int53 1"
		| q0<0
			# exp = 0x3fe-shift_n+shift_d+add_exp
			| exp>0
				| q1 bitand 0x400==0
					# q1=(q1+>>11)+(q0<<21)
					# q0=q0+>>11
					= (exp,q0,q1)
				| q1 bitand 0x3ff+>1
					# q1=(q1+>>11)+(q0<<21)
					# q0=q0+>>11
					= next_int53_no_denormal_or_zero exp q0 q1
					= integer_div_nm_shift_to_int53_2 r0 r1 q0 q1 na da shift_n shift_d exp
				| q1 bitand 0x3ff+>1
					# q1=(q1+>>11)+(q0<<21)
					# q0=q0+>>11
					= round_inexact_denormal exp q0 q1
					= integer_div_nm_shift_to_int53_2 r0 r1 q0 q1 na da shift_n shift_d exp
			= abort `"integer_div_nm_shift_to_int53 2"

	integer_div_nm_shift_to_int53_2 -: !Int !Int !Int !Int !{#Int} !{#Int} !Int !Int !Int -> (!Int,!Int,!Int)
	integer_div_nm_shift_to_int53_2 r0 r1 q0 q1 na da shift_n shift_d exp
		// q1 bitand 0x4ff == 0x400 or 0x401
		# d2 = da~[size da-3]
		# d2 = if (shift_d==0) d2
			   (if (shift_d>0) ((d2<<shift_n)+(if (size da>=4) (da~[size da-4]+>>(32-shift_d)) 0))
			    (abort `"integer_div_nm_shift_to_int53_2 1"))
		# r2 = if (size na>=5) na~[size na-5] 0
		#! r2 = if (shift_n==0) r2
			   (if (shift_n>0) ((r2<<shift_n)+(if (size na>=6) (na~[size na-6]+>>(32-shift_n)) 0))
			    (abort `"integer_div_nm_shift_to_int53_2 2"))
		# (ph,pl) = mulud d2 q0
		| ph+<r0 || (ph==r0 && pl+<r1)
			# (r0,r1) = subLU (r0-ph) r1 pl
			| r0+>1
				= shift_to_next_int53_and_round_if_denormal exp q0 q1
			# (ph,pl) = mulud d2 q1
			| ph+<r1 || (ph==r1 && pl+<r2)
				| r0<>0
					= shift_to_next_int53_and_round_if_denormal exp q0 q1
					# (r1,r2) = subLU (r1-ph) r2 pl
					= integer_div_nm_shift_to_int53_3 r1 r2 q0 q1 4 na da shift_n shift_d exp
				| r0<>0
					# (r1,r2) = subLU (r1-ph) r2 pl
					= integer_div_nm_shift_to_int53_3 r1 r2 q0 q1 4 na da shift_n shift_d exp
					| q1 bitand 0x3ff==0
						| q1<>0
							# q1=q1-1
							= shift_to_int53_and_round_if_denormal exp q0 q1
							# q0=q0-1
							= shift_to_int53_and_round_if_denormal exp q0 q1 // denormal
						# q1=q1-1
						# (r1,r2) = subLU (r1-ph) r2 pl // needs borrow
						# (r0,r1) = addLU (d0-1) d1 r1
						| r0+>1
							= shift_to_next_int53_and_round_if_denormal exp q0 q1
							= abort `"integer_div_nm_shift_to_int53_2 3" // not possible because msb d0==1
		| q1 bitand 0x3ff==0
			| q1<>0
				# q1=q1-1
				= shift_to_int53_and_round_if_denormal exp q0 q1
				# q1=q1-1
				# q0=q0-1
				= shift_to_int53_and_round_if_denormal exp q0 q1 // denormal
			# q1=q1-1
			# (x0,x1) = subLU (ph-r0) pl r1
			| x0+<d0 || (x0==d0 && x1+<d1)
				# (r0,r1) = subLU (d0-x0) d1 x1
				| r0+>1
					= shift_to_next_int53_and_round_if_denormal exp q0 q1
				# (ph,pl) = mulud d2 q1
				| ph+<r1 || (ph==r1 && pl+<r2)
					| r0<>0
						= shift_to_next_int53_and_round_if_denormal exp q0 q1
						# (r1,r2) = subLU (r1-ph) r2 pl
						= integer_div_nm_shift_to_int53_3 r1 r2 q0 q1 4 na da shift_n shift_d exp
					| r0<>0
						# (r1,r2) = subLU (r1-ph) r2 pl
						= integer_div_nm_shift_to_int53_3 r1 r2 q0 q1 4 na da shift_n shift_d exp
						| q1<>0
							# q1=q1-1 // ..0x3ff
							= shift_to_int53_and_round_if_denormal exp q0 q1
							# q0=q0-1
							# q1=q1-1 // ..0x3ff
							= shift_to_int53_and_round_if_denormal exp q0 q1
				| q1<>0
					# q1=q1-1 // ..0x3ff
					= shift_to_int53_and_round_if_denormal exp q0 q1
					# q1=q1-1
					# q0=q0-1
					= shift_to_int53_and_round_if_denormal exp q0 q1 // denormal
	where
		integer_div_nm_shift_to_int53_3 -: !Int !Int !Int !Int !Int !{#Int} !{#Int} !Int !Int !Int -> (!Int,!Int,!Int)
		integer_div_nm_shift_to_int53_3 r1 r2 q0 q1 i na da shift_n shift_d exp
			| r1+>q0 || (q0==r1 && r2+>q1)
				= shift_to_next_int53_and_round_if_denormal exp q0 q1
			| negative_rest r1 r2 q0 q1 i na da shift_n shift_d
				| q1<>0
					# q1=q1-1
					| q1 bitand 0x400==0
						= shift_to_int53_and_round_if_denormal exp q0 q1
						= shift_to_next_int53_and_round_if_denormal exp q0 q1
					# q1=q1-1
					# q0=q0-1
					= shift_to_int53_and_round_if_denormal exp q0 q1 // denormal
				= shift_to_next_int53_and_round_if_denormal exp q0 q1

		negative_rest -: !Int !Int !Int !Int !Int !{#Int} !{#Int} !Int !Int -> Bool
		negative_rest r0 r1 q0 q1 i na da shift_n shift_d
			| i>size da
				= False
				# di = size da-i
				# d2 = da~[di]
				# d2 = if (shift_d==0) d2
					   (if (shift_d>0) ((d2<<shift_n)+(if (di>0) (da~[di-1]+>>(32-shift_d)) 0))
			   			 (abort `"negative_rest 1"))
				# ni = size na-2-i
				# r2 = if (ni>=0) na~[ni] 0
				#! r2 = if (shift_n==0) r2
					   (if (shift_n>0) ((r2<<shift_n)+(if (ni>0) (na~[ni-1]+>>(32-shift_n)) 0))
					    (abort `"negative_rest 2"))
				# (ph,pl) = mulud d2 q0
				| ph+>r0 || (ph==r0 && pl+>r1)
					= True
					# (r0,r1) = subLU (r0-ph) r1 pl
					| r0+>1
						= False
					# (ph,pl) = mulud d2 q1
					| ph+<1 || (ph==r1 && pl+<r2)
						| r0<>0
							= False
							# (r1,r2) = subLU (r1-ph) r2 pl
							| r1+>q0 || (q0==r1 && r2+>q1)
								= False
								= negative_rest r1 r2 q0 q1 (i+1) na da shift_n shift_d
						| r0<>0
							# (r1,r2) = subLU (r1-ph) r2 pl
							| r1+>q0 || (q0==r1 && r2+>q1)
								= False
								= negative_rest r1 r2 q0 q1 (i+1) na da shift_n shift_d
							= True

		shift_to_int53_and_round_if_denormal -: !Int !Int !Int -> (!Int,!Int,!Int)
		shift_to_int53_and_round_if_denormal exp q0 q1
			# q1=(q1+>>11)+(q0<<21)
			# q0=q0+>>11
			| exp>0
				= (exp,q0,q1)
				= round_inexact_denormal exp q0 q1

		shift_to_next_int53_and_round_if_denormal -: !Int !Int !Int -> (!Int,!Int,!Int)
		shift_to_next_int53_and_round_if_denormal exp q0 q1
			# q1=(q1+>>11)+(q0<<21)
			# q0=q0+>>11
			| exp>0
				= next_int53_no_denormal_or_zero exp q0 q1
				= round_inexact_denormal exp q0 q1

get_n4 :: !{#Int} -> (!Int,!Int,!Int,!Int,!Int)
get_n4 na
	# sna = size na
	# n0 = na~[sna-1]
	| n0<0
		| sna==1
			= (n0,0,0,0,0)
		# n1 = na~[sna-2]
		| sna==2
			= (n0,n1,0,0,0)
		# n2 = na~[sna-3]
		| sna==3
			= (n0,n1,n2,0,0)
			# n3 = na~[sna-4]
			= (n0,n1,n2,n3,0)
		# shift_n = n_leading_zeros n0
		| sna==1
			# n0=n0<<shift_n
			= (n0,0,0,0,shift_n)
		# n1 = na~[sna-2]
		| sna==2
			# n0=(n0<<shift_n)+(n1+>>(32-shift_n))
			# n1=n1<<shift_n
			= (n0,n1,0,0,shift_n)
		# n2 = na~[sna-3]
		# shift_n_c = 32-shift_n
		| sna==3
			# n0=(n0<<shift_n)+(n1+>>shift_n_c)
			# n1=(n1<<shift_n)+(n2+>>shift_n_c)
			# n2=n2<<shift_n
			= (n0,n1,n2,0,shift_n)
			# n3 = na~[sna-4]
			# n0=(n0<<shift_n)+(n1+>>shift_n_c)
			# n1=(n1<<shift_n)+(n2+>>shift_n_c)
			# n2=(n2<<shift_n)+(n3+>>shift_n_c)
			# n3=n3<<shift_n
			= (n0,n1,n2,n3,shift_n)

n4_div_n2_shift_to_double n0 n1 n2 n3 d0 d1 shift_n shift_d shift_n_elements sign
	# add_exp = shift_n_elements<<5
	| n0+<d0 || (n0==d0 && n1+<d1)
		# (exp,h,l) = n4_div_n2_shift_to_int53 n0 n1 n2 n3 d0 d1 shift_n shift_d add_exp
		= make_double_no_denormal_or_zero sign exp h l
		# n3=(n3+>>1)+(n2<<31)
		# n2=(n2+>>1)+(n1<<31)
		# n1=(n1+>>1)+(n0<<31)
		# n0=n0+>>1
		# shift_n=shift_n-1
		# (exp,h,l) = n4_div_n2_shift_to_int53 n0 n1 n2 n3 d0 d1 shift_n shift_d add_exp
		= make_double_no_denormal_or_zero sign exp h l
where
	n4_div_n2_shift_to_int53 n0 n1 n2 n3 d0 d1 shift_n shift_d add_exp
		# (q0,q1) = n4_div_n2 n0 n1 n2 n3 d0 d1
		| q0<0
			# exp = 0x3fe-shift_n+shift_d+add_exp
			| q1 bitand 0x400==0
				# q1=(q1+>>11)+(q0<<21)
				# q0=q0+>>11
				= (exp,q0,q1)
				# q1=(q1+>>11)+(q0<<21)
				# q0=q0+>>11
				= next_int53_no_denormal_or_zero exp q0 q1
			= abort `"n4_div_n2_shift_to_int53"

n4_div_n2 -: !Int !Int !Int !Int !Int !Int -> (!Int,!Int)
n4_div_n2 n0 n1 n2 n3 d0 d1
	| n0+<d0
		# (r0,q0) = divdu n0 n1 d0
		# (ph,pl) = mulud d1 q0
		| ph+<r0 || (ph==r0 && not (n2+<pl))
			# (n1,n2) = subLU (r0-ph) n2 pl
			= n4_div_n2_2 q0 n1 n2 n3 d0 d1
			# q0=q0-1
			# r0=r0+d0
			# (ph,pl) = subLU ph pl d1
			| r0+<d0
				# (n1,n2) = subLU (r0-ph) n2 pl
				= n4_div_n2_2 q0 n1 n2 n3 d0 d1
				| ph+<r0 || (ph==r0 && not (n2+<pl))
					# (n1,n2) = subLU (r0-ph) n2 pl
					= n4_div_n2_2 q0 n1 n2 n3 d0 d1
					# (n1,n2) = subLU (r0-ph) n2 pl
					# q0=q0-1
					# (n1,n2) = addLU (n1+d0) n2 d1
					= n4_div_n2_2 q0 n1 n2 n3 d0 d1
	| n0==d0 && d1<>0
		# r0 = n0+n1
		# ph = d1-1
		# pl = -d1
		# q0 = -1
		| r0+<n0
			# (n1,n2) = subLU (r0-ph) n2 pl
			= n4_div_n2_2 q0 n1 n2 n3 d0 d1
			| ph+<r0 || (ph==r0 && not (n2+<pl))
				# (n1,n2) = subLU (r0-ph) n2 pl
				= n4_div_n2_2 q0 n1 n2 n3 d0 d1
				# r0=r0+d0
				# (ph,pl) = subLU ph pl d1
				# q0 = -2
				| r0+<d0
					# (n1,n2) = subLU (r0-ph) n2 pl
					= n4_div_n2_2 q0 n1 n2 n3 d0 d1
					| ph+<r0 || (ph==r0 && not (n2+<pl))
						# (n1,n2) = subLU (r0-ph) n2 pl
						= n4_div_n2_2 q0 n1 n2 n3 d0 d1
						= abort `"n4_div_n2 1"
		= abort `"n4_div_n2 2"
where
	n4_div_n2_2 -: !Int !Int !Int !Int !Int !Int -> (!Int,!Int)
	n4_div_n2_2 q0 n1 n2 n3 d0 d1
		| n1+<d0
			# (r1,q1) = divdu n1 n2 d0
			# (ph,pl) = mulud d1 q1
			| ph+<r1 || (ph==r1 && not (n3+<pl))
				= (q0,q1)
				# q1=q1-1
				# r1=r1+d0
				# (ph,pl) = subLU ph pl d1
				| r1+<d0
					= (q0,q1)
					| ph+<r1 || (ph==r1 && not (n3+<pl))
						= (q0,q1)
						# q1=q1-1
						= (q0,q1);
		| n1==d0 && d1<>0
			# r1 = n1+n2
			# ph = d1-1
			# pl = -d1
			# q1 = -1
			| r1+<n1
				= (q0,q1)
				| ph+<r1 || (ph==r1 && not (n3+<pl))
					= (q0,q1)
					# r1=r1+d0
					# q1 = -2
					| r1+<d0
						= (q0,q1)
						# (ph,pl) = subLU ph pl d1
						| ph+<r1 || (ph==r1 && not (n3+<pl))
							= (q0,q1)
							= abort `"n4_div_n2_2 1"
			= abort `"n4_div_n2_2 2"

n4_div_rem_n2 -: !Int !Int !Int !Int !Int !Int -> (!Int,!Int,!Int,!Int)
n4_div_rem_n2 n0 n1 n2 n3 d0 d1
	| n0+<d0
		# (r0,q0) = divdu n0 n1 d0
		# (ph,pl) = mulud d1 q0
		| ph+<r0 || (ph==r0 && not (n2+<pl))
			# (n1,n2) = subLU (r0-ph) n2 pl
			= n4_div_rem_n2_2 q0 n1 n2 n3 d0 d1
			# q0=q0-1
			# r0=r0+d0
			# (ph,pl) = subLU ph pl d1
			| r0+<d0
				# (n1,n2) = subLU (r0-ph) n2 pl
				= n4_div_rem_n2_2 q0 n1 n2 n3 d0 d1
				| ph+<r0 || (ph==r0 && not (n2+<pl))
					# (n1,n2) = subLU (r0-ph) n2 pl
					= n4_div_rem_n2_2 q0 n1 n2 n3 d0 d1
					# (n1,n2) = subLU (r0-ph) n2 pl
					# q0=q0-1
					# (n1,n2) = addLU (n1+d0) n2 d1
					= n4_div_rem_n2_2 q0 n1 n2 n3 d0 d1
	| n0==d0 && d1<>0
		# r0 = n0+n1
		# ph = d1-1
		# pl = -d1
		# q0 = -1
		| r0+<n0
			# (n1,n2) = subLU (r0-ph) n2 pl
			= n4_div_rem_n2_2 q0 n1 n2 n3 d0 d1
			| ph+<r0 || (ph==r0 && not (n2+<pl))
				# (n1,n2) = subLU (r0-ph) n2 pl
				= n4_div_rem_n2_2 q0 n1 n2 n3 d0 d1
				# r0=r0+d0
				# (ph,pl) = subLU ph pl d1
				# q0 = -2
				| r0+<d0
					# (n1,n2) = subLU (r0-ph) n2 pl
					= n4_div_rem_n2_2 q0 n1 n2 n3 d0 d1
					| ph+<r0 || (ph==r0 && not (n2+<pl))
						# (n1,n2) = subLU (r0-ph) n2 pl
						= n4_div_rem_n2_2 q0 n1 n2 n3 d0 d1
						= abort `"n4_div_rem_n2 1"
		= abort `"n4_div_rem_n2 2"
where
	n4_div_rem_n2_2 -: !Int !Int !Int !Int !Int !Int -> (!Int,!Int,!Int,!Int)
	n4_div_rem_n2_2 q0 n1 n2 n3 d0 d1
		| n1+<d0
			# (r1,q1) = divdu n1 n2 d0
			# (ph,pl) = mulud d1 q1
			| ph+<r1 || (ph==r1 && not (n3+<pl))
				# (n2,n3) = subLU (r1-ph) n3 pl
				= (q0,q1,n2,n3)
				# q1=q1-1
				# r1=r1+d0
				# (ph,pl) = subLU ph pl d1
				| r1+<d0
					# (n2,n3) = subLU (r1-ph) n3 pl
					= (q0,q1,n2,n3)
					| ph+<r1 || (ph==r1 && not (n3+<pl))
						# (n2,n3) = subLU (r1-ph) n3 pl
						= (q0,q1,n2,n3)
						# (n2,n3) = subLU (r1-ph) n3 pl
						# q1=q1-1
						# (n2,n3) = addLU (n2+d0) n3 d1
						= (q0,q1,n2,n3);
		| n1==d0 && d1<>0
			# r1 = n1+n2
			# ph = d1-1
			# pl = -d1
			# q1 = -1
			| r1+<n1
				# (n2,n3) = subLU (r1-ph) n3 pl
				= (q0,q1,n2,n3)
				| ph+<r1 || (ph==r1 && not (n3+<pl))
					# (n2,n3) = subLU (r1-ph) n3 pl
					= (q0,q1,n2,n3)
					# r1=r1+d0
					# (ph,pl) = subLU ph pl d1
					# q1 = -2
					| r1+<d0
						# (n2,n3) = subLU (r1-ph) n3 pl
						= (q0,q1,n2,n3)
						| ph+<r1 || (ph==r1 && not (n3+<pl))
							# (n2,n3) = subLU (r1-ph) n3 pl
							= (q0,q1,n2,n3)
							= abort `"n4_div_rem_n2_2 1"
			= abort `"n4_div_rem_n2_2 2"

shifted_integer_to_double -: !Integer !Int -> Double
shifted_integer_to_double n shift
	# sna = size n~integer_a
	| sna==0
		# ni = n~integer_s
		| ni==0
			= 0.0;
		| ni>0
			= pos_shifted_int_to_double ni shift
			= neg_shifted_int_to_double (-ni) shift
	| sna==1
		# ni = n~integer_a~[0]
		| n~integer_s>=0
			= pos_shifted_int_to_double ni shift
			= neg_shifted_int_to_double ni shift
	# (exp,h,l) = integer_a_shifted_to_int53 n~integer_a shift
	= make_double n~integer_s exp h l
where
	pos_shifted_int_to_double ni shift
		# (exp,h,l) = int_to_int53 ni
		# exp=exp-shift;
		| exp>0
			= make_double 0 exp h l
			# (exp,h,l) = round_exact_denormal exp h l
			= make_double 0 exp h l

	neg_shifted_int_to_double ni shift
		# (exp,h,l) = int_to_int53 ni
		# exp=exp-shift;
		| exp>0
			= make_double (-1) exp h l
			# (exp,h,l) = round_exact_denormal exp h l
			= make_double (-1) exp h l

	int_to_int53 -: !Int -> (!Int,!Int,!Int)
	int_to_int53 i
		# nlz = n_leading_zeros i
		  exp = 0x3ff+31-nlz
		  shift = 21+nlz
		| shift<32
			# l=i<<shift
			  h=i+>>(32-shift)
			= (exp,h,l)
			# l=0
			  h=i<<(shift-32)
			= (exp,h,l)

integer_div_n_to_int53 -: !{#Int} !Int -> (!Int,!Int,!Int)
integer_div_n_to_int53 a d
	# sna=size a
	# n0=a~[sna-1]
	| n0+<d
		# n1=a~[sna-2]
		# (r,q0) = divdu n0 n1 d;
		| sna==2
			# (r,q1) = divdu r 0 d;
			| not (q0+<0x200000)
				# exp = 0x3ff+31
				= shift_and_round1 exp q0 q1
				# (r,q2) = divdu r 0 d;
				# exp = 0x3ff+31
				= shift_and_round2 exp q0 q1 q2
			# n2=a~[sna-3]
			# (r,q1) = divdu r n2 d;
			| not (q0+<0x200000)
				# exp = 0x3ff+31+((sna-2)<<5)
				= shift_and_round1 exp q0 q1
				| sna==3
					# (r,q2) = divdu r 0 d;
					# exp = 0x3ff+63
					= shift_and_round2 exp q0 q1 q2
					# n3=a~[sna-4]
					# (r,q2) = divdu r n3 d;
					# exp = 0x3ff+63+((sna-3)<<5)
					= shift_and_round2 exp q0 q1 q2
		# (r,q0) = divdu 0 n0 d;
		# n1=a~[sna-2]
		# (r,q1) = divdu r n1 d;
		| not (q0+<0x200000)
			# exp = 0x3ff+63+((sna-2)<<5)
			= shift_and_round1 exp q0 q1
			| sna==2
				# (r,q2) = divdu r 0 d;
				# exp = 0x3ff+63
				= shift_and_round2 exp q0 q1 q2
				# n2=a~[sna-3]
				# (r,q2) = divdu r n2 d;
				# exp = 0x3ff+63+((sna-2)<<5)
				= shift_and_round2 exp q0 q1 q2
where
	shift_and_round1 exp q0 q1 // q0>=0x200000
		# n_bits = n_leading_zeros q0
		# exp = exp-n_bits
		# shift_r = 11-n_bits
		| q1 bitand (1<<(shift_r-1))==0
			# q1 = (ushiftr q1 shift_r)+(q0<<(32-shift_r))
			# q0 = ushiftr q0 shift_r
			= (exp,q0,q1)
			# q1 = (ushiftr q1 shift_r)+(q0<<(32-shift_r))
			# q0 = ushiftr q0 shift_r
			= next_int53_no_denormal_or_zero exp q0 q1

	shift_and_round2 exp q0 q1 q2 // q0<0x200000
		# n_bits = n_leading_zeros q0
		# exp = exp-n_bits
		# shift_l = n_bits-11
		| shift_l==0
			| q2>=0
				= (exp,q0,q1)
				= next_int53_no_denormal_or_zero exp q0 q1
		# shift_r = 32-shift_l
		| (q2<<shift_l)>=0
			# q0 = (q0<<shift_l)+(ushiftr q1 shift_r)
			# q1 = (q1<<shift_l)+(ushiftr q2 shift_r)
			= (exp,q0,q1)
			# q0 = (q0<<shift_l)+(ushiftr q1 shift_r)
			# q1 = (q1<<shift_l)+(ushiftr q2 shift_r)
			= next_int53_no_denormal_or_zero exp q0 q1

next_int53_no_denormal_or_zero -: !Int !Int !Int -> (!Int,!Int,!Int)
next_int53_no_denormal_or_zero exp h l
	# l=l+1
	| not (l==0)
		= (exp,h,l)
	# h=h+1
	| h+<0x200000
		= (exp,h,l)
		# h=0x100000
		# exp=exp+1
		= (exp,h,l)

make_double -: !Int !Int !Int !Int -> Double
make_double s exp h l
	| exp>=0
		= make_double_no_denormal_or_zero s exp h l

make_double_no_denormal_or_zero -: !Int !Int !Int !Int -> Double
make_double_no_denormal_or_zero s exp h l
	| exp<0x7ff
		= int2_to_double l ((s bitand 0x80000000)+(exp<<20)+(h bitand 0x0fffff))
		= infinity s

int2_to_double -: !Int !Int -> Double
int2_to_double l h = `code inline {
	update_b 1 1
	update_b 0 0
}

infinity s
	| s>=0
		= 1.0/0.0;
		= -1.0/0.0;

rest_all_zeros -: !Int !{#Int} -> Bool
rest_all_zeros i a
	| i>=0
		| a~[i]==0
			= rest_all_zeros (i-1) a;
			= False
		= True

*/
