implementation module StdIntegerMul;

import StdEnv;

from StdInteger import ::Integer{..};

addLU :: !Int !Int !Int -> (!Int,!Int);
addLU _ _ _ = code inline {
	addLU
}

mulIo :: !Int !Int -> (!Bool,!Int);
mulIo a b = code inline {
	mulIo
}

mulud :: !Int !Int -> (!Int,!Int);
mulud a b = code inline {
	mulUUL
}

create_unintialized_int_array :: !Int -> *{#Int};
create_unintialized_int_array size = code {
	create_array_ INT 0 1
}

resize_int_array :: !Int !*{#Int} -> *{#Int};
resize_int_array s a
	| size a>=s
		= unsafe_resize_int_array s a;

unsafe_resize_int_array :: !Int !*{#Int} -> *{#Int};
unsafe_resize_int_array s a = code {
	fill1_r _ARAY_ 0 1 0 01
}

mul_integer_a :: !{#Int} !{#Int} -> *{#Int};
mul_integer_a a1 a2
	| size a1<size a2
		= mul_ints_ints_smallest_first a1 a2;
		= mul_ints_ints_smallest_first a2 a1;
{}{
	mul_ints_ints_smallest_first a1 a2
		# s1=size a1;
		# s2=size a2;
		# da = create_unintialized_int_array (s1+s2);
		# m=a1.[0];
		# (c,da) = mul_uint_uint_a 0 0 m a2 da;
		# da={da & [s2]=c};
		= repeat_add_muls 1 a1 a2 da;

	repeat_add_muls i a1 a2 da
		| i<size a1
			# m=a1.[i];
/*
			| m==0
				# da = {da & [i+size a2]=0};
				= repeat_add_muls (i+1) a1 a2 da;
*/
			# da = add_mul_uint_uint_a 0 0 i m a2 da;
			= repeat_add_muls (i+1) a1 a2 da;
			= remove_zeros da;

	add_mul_uint_uint_a :: !Int !Int !Int !Int !{#Int} !*{#Int} -> *{#Int};
	add_mul_uint_uint_a i c d m sa da
		| i<size sa
			#! sai=sa.[i];
			# (c0,p0) = mulud sai m;
			# id=i+d;
			# (dai,da) = da![id];
			# (c,daipc) = addLU c0 dai c;
			# (c,p) = addLU c p0 daipc;
			# da = {da & [id]=p};
			= add_mul_uint_uint_a (i+1) c d m sa da;
			= {da & [i+d]=c};

	remove_zeros a
		#! n=size a-1;
		| a.[n]<>0
			= a;
		#! n=skip_zeros n a;
		= resize_int_array (n+1) a;
	{
		skip_zeros i a
			| a.[i]==0
				= skip_zeros (i-1) a;
				= i;
	}
}

mul_uint_uint_a :: !Int !Int !Int !{#Int} !*{#Int} -> (!Int,!*{#Int});
mul_uint_uint_a i c m sa da
	| i<size sa
		#! sai=sa.[i];
		# (c2,p) = mulud sai m;
		# (c2,p) = addLU c2 p c;
		# da = {da & [i]=p};
		= mul_uint_uint_a (i+1) c2 m sa da;
		= (c,da);

mul_integer :: !Integer !Integer -> *Integer;
mul_integer {integer_a=a1,integer_s=s1} {integer_a=a2,integer_s=s2}
	| size a1==0
		| size a2==0
			# (overflow,s) = mulIo s1 s2;
			| overflow
				= mul_int_int_o s1 s2;
				= {integer_s=s,integer_a={}};
			= mul_int_integer s1 s2 a2;
		| size a2==0
			= mul_int_integer s2 s1 a1;
			# a = mul_integer_a a1 a2;
			= {integer_s=s1 bitxor s2, integer_a=a};
{}{
	mul_int_int_o s1 s2
		# m1 = s1>>IF_INT_64_OR_32 63 31;
		# abs_s1 = (s1 bitxor m1) - m1;
		# m2 = s2>>IF_INT_64_OR_32 63 31;
		# abs_s2 = (s2 bitxor m2) - m2;
		# (h,l) = mulud abs_s1 abs_s2;
		# s = m1 bitxor m2;
		| h==0
			= {integer_s=s,integer_a={l}};
			= {integer_s=s,integer_a={l,h}};

	mul_int_integer s1 s2 a2
		| s1>0
			# a = mul_unsigned_int_ints s1 a2;
			= {integer_s=s2,integer_a=a};
		| s1==0
			= {integer_s=0,integer_a={}};
			# a = mul_unsigned_int_ints (~s1) a2;
			= {integer_s= bitnot s2,integer_a=a};

	mul_unsigned_int_ints :: !Int !{#Int} -> *{#Int};
	mul_unsigned_int_ints m a
		#! s=size a;
		# da = create_unintialized_int_array (s+1);
		# (c,da) = mul_uint_uint_a 0 0 m a da;
		| c==0
			= resize_int_array s da;
			= {da & [s]=c};
}
