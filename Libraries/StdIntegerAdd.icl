implementation module StdIntegerAdd;

import StdEnv;

from StdInteger import ::Integer(..);

MAX_INT :== IF_INT_64_OR_32 0x7fffffffffffffff 0x7fffffff;

addIo :: !Int !Int -> (!Bool,!Int);
addIo a b = code inline {
	addIo
}

subIo :: !Int !Int -> (!Bool,!Int);
subIo a b = code inline {
	subIo
}

addLU :: !Int !Int !Int -> (!Int,!Int);
addLU _ _ _ = code inline {
	addLU
}

subLU :: !Int !Int !Int -> (!Int,!Int);
subLU _ _ _ = code inline {
	subLU
}

ltU :: !Int !Int -> Bool;
ltU a b = code inline {
	ltU
}

create_unintialized_int_array :: !Int -> *{#Int};
create_unintialized_int_array size = code inline {
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

(+<) infix 4;
(+<) a b :== ltU a b;

add_integer :: !Integer !Integer -> *Integer;
add_integer {integer_s=s1,integer_a=a1} {integer_s=s2,integer_a=a2}
	| size a1==0
		| size a2==0
			# (overflow,s) = addIo s1 s2;
			| overflow
				= add_integer_o s s1;
				= {integer_s=s,integer_a={}};
			= add_signed_int_integer s1 s2 a2;
		| size a2==0
			= add_signed_int_integer s2 s1 a1;
			= add_integer_integer s1 a1 s2 a2;
{}{
	add_integer_o :: !Int !Int -> *Integer;
	add_integer_o s s1
		| s1>=0 // and s2>=0
			= {integer_s=0,integer_a={s}};
		// s1<0 && s2<0
		| s<>0
			= {integer_s= -1,integer_a={~s}};
			// s1==MaxNegInt && s2==MaxNegInt
			= {integer_s= -1,integer_a={0,1}};

	add_signed_int_integer i s a
		| i>=0
			| s>=0
				= {integer_s=0, integer_a=add_unsigned_int_integer i a};
				= neg_sub_uint_a_unsigned_int a i;
			| s>=0
				= sub_uint_a_unsigned_int a (~i);
				= {integer_s= -1, integer_a=add_unsigned_int_integer (~i) a};

	add_integer_integer s1 a1 s2 a2
		| s1>=0
			| s2>=0
				= {integer_s=0, integer_a=add_uint_a_uint_a a1 a2};
				= sub_uint_a_uint_a a1 a2;
			| s2>=0
				= neg_sub_uint_a_uint_a a1 a2;
				= {integer_s= -1, integer_a=add_uint_a_uint_a a1 a2};
}

sub_integer :: !Integer !Integer -> *Integer;
sub_integer {integer_s=s1,integer_a=a1} {integer_s=s2,integer_a=a2}
	| size a1==0
		| size a2==0
			# (overflow,s) = subIo s1 s2;
			| overflow
				= sub_integer_o s s1;
				= {integer_s=s,integer_a={}};
			= sub_signed_int_integer s1 s2 a2;
		| size a2==0
			= sub_integer_signed_int s1 a1 s2;
			= sub_integer_integer s1 a1 s2 a2;
{}{
	sub_integer_o s s1
		| s1>=0 // && s2<0
			= {integer_s=0,integer_a={s}};
		// s1<0 && s2>=0
			= {integer_s= -1,integer_a={~s}};

	sub_signed_int_integer i1 s2 a2
		| i1>=0
			| s2>=0
				= neg_sub_uint_a_unsigned_int a2 i1;
				= {integer_s=0, integer_a=add_unsigned_int_integer i1 a2};
			| s2>=0
				= {integer_s= -1, integer_a=add_unsigned_int_integer (~i1) a2};
				= sub_uint_a_unsigned_int a2 (~i1);

	sub_integer_signed_int s1 a1 i2
		| s1>=0
			| i2>=0
				= sub_uint_a_unsigned_int a1 i2;
				= {integer_s=0, integer_a=add_unsigned_int_integer (~i2) a1};
			| i2>=0
				# a = add_unsigned_int_integer i2 a1;
				= {integer_s= -1,integer_a=a};
				= neg_sub_uint_a_unsigned_int a1 (~i2);

	sub_integer_integer s1 a1 s2 a2
		| s1>=0
			| s2>=0
				= sub_uint_a_uint_a a1 a2;
				# a = add_uint_a_uint_a a1 a2;
				= {integer_s=0, integer_a=a};
			| s2>=0
				# a = add_uint_a_uint_a a1 a2;
				= {integer_s= -1, integer_a=a};
				= neg_sub_uint_a_uint_a a1 a2;
}

neg_sub_uint_a_unsigned_int a i
	# a0=a.[0]
	# da = {copy_array a & [0]=a0-i};
	| a0+<i
		= remove_zeros_n (subtract_borrow 1 da);
		= remove_zeros_n da;

import StdDebug;

sub_uint_a_unsigned_int a i
	# a0=a.[0];
	# da={copy_array a & [0]=a0-i};
	| a0+<i
		= remove_zeros_p (subtract_borrow 1 da);
		= remove_zeros_p da;

sub_uint_a_uint_a a1 a2
	| size a1==size a2
		= sub_integer_integer_same_length (size a1-1) a1 a2;
	| size a1>size a2
		= sub_integer_integer_larger_first_p a1 a2;
		= sub_integer_integer_larger_first_n a2 a1;

neg_sub_uint_a_uint_a a1 a2
	| size a1==size a2
		= sub_integer_integer_same_length (size a1-1) a2 a1;
	| size a1>size a2
		= sub_integer_integer_larger_first_n a1 a2;
		= sub_integer_integer_larger_first_p a2 a1;

sub_integer_integer_larger_first_p a1 a2
	# s = size a1;
	# da = create_unintialized_int_array s;
	# (b,da) = sub_integer_arrays 0 0 (size a2) a1 a2 da;
	| b<>0
		| b<> 1 // impossible
			= abort "sub_integer_integer_larger_first_p"
		# n = size a2;
		| a1.[n]==1 && n+1==s
			= remove_zeros_and_resize_p n da;
			# da = subtract_borrow_and_copy_array_slice n s a1 da;
			= {integer_s=0,integer_a=da};
		# da = copy_array_slice (size a2) s a1 da;
		= {integer_s=0,integer_a=da};

sub_integer_integer_larger_first_n a1 a2
	# s = size a1;
	# da = create_unintialized_int_array s;
	# (b,da) = sub_integer_arrays 0 0 (size a2) a1 a2 da;
	| b<>0
		| b<> 1 // impossible
			= abort "sub_integer_integer_larger_first_n";
		# n = size a2;
		| a1.[n]==1 && n+1==s
			= remove_zeros_and_resize_n n da;
			# da = subtract_borrow_and_copy_array_slice n s a1 da;
			= {integer_s= -1,integer_a=da};
		# da = copy_array_slice (size a2) s a1 da;
		= {integer_s= -1,integer_a=da};

sub_integer_integer_same_length :: !Int !{#Int} !{#Int} -> *Integer;
sub_integer_integer_same_length i a1 a2
	| i<0
		= {integer_s=0,integer_a={}};
	| a1.[i]==a2.[i]
		= sub_integer_integer_same_length (i-1) a1 a2;
	# s = i+1;
	# da = create_unintialized_int_array s;
	| a1.[i]+<a2.[i]
		# (b,da) = sub_integer_arrays 0 0 s a2 a1 da;
		| b<>0 // impossible
			= abort "sub_integer_integer_same_length 1";
		= remove_zeros_n da;
		# (b,da) = sub_integer_arrays 0 0 s a1 a2 da;
		| b<>0 // impossible
			= abort "sub_integer_integer_same_length 2";
		= remove_zeros_p da;

remove_zeros_p :: !*{#Int} -> *Integer;
remove_zeros_p a
	#! s = size a;
	# n=a.[s-1];
	| n<>0
		| s<>1 || n<0
			= {integer_s=0, integer_a=a};
			= {integer_s=n, integer_a={}};
		= remove_zeros_and_resize_p (s-1) a;

remove_zeros_n :: !*{#Int} -> *Integer;
remove_zeros_n a
	#! s=size a;
	# n=a.[s-1];
	| n<>0
		| s<>1 || MAX_INT+<n
			= {integer_s= -1, integer_a=a};
			= {integer_s= ~n, integer_a={}};
	= remove_zeros_and_resize_n (s-1) a;

remove_zeros_and_resize_p :: !Int !*{#Int} -> *Integer;
remove_zeros_and_resize_p i a
	| i==0
		= {integer_s=0,integer_a={}};
	#! n=a.[i-1];
	| n==0
		= remove_zeros_and_resize_p (i-1) a;
	| i<>1 || n<0
		= {integer_s=0, integer_a=resize_int_array i a};
		= {integer_s=n, integer_a={}};

remove_zeros_and_resize_n :: !Int !*{#Int} -> *Integer;
remove_zeros_and_resize_n i a
	| i==0
		= {integer_s=0,integer_a={}};
	#! n=a.[i-1];
	| n==0
		= remove_zeros_and_resize_n (i-1) a;
	| i<>1 || MAX_INT+<n
		= {integer_s= -1, integer_a=resize_int_array i a};
		= {integer_s= ~n, integer_a={}};

add_unsigned_int_integer :: !Int !{#Int} -> *{#Int};
add_unsigned_int_integer i a
	# n=i+a.[0];
	| n+<i
		# s=size a;
		| s>1 && a.[s-1]<> -1
			# da = create_unintialized_int_array s;
			  da = {da & [0]=n};
			= add_carry_and_copy_array_slice 1 s a da;
			# da = create_unintialized_int_array (s+1);
			  da = {da & [0]=n};
			  (carry,da) = add_carry_and_copy_array_slice2 1 s a da;
			| carry
				= {da & [s]=1};
				= resize_int_array s da;
		= {copy_array a & [0]=n};

add_uint_a_uint_a :: !{#Int} !{#Int} -> *{#Int};
add_uint_a_uint_a a1 a2
	| size a1==size a2
		# s = size a1;
		  da = create_unintialized_int_array (s+1);
		  (carry,da) = add_integer_arrays 0 0 s a1 a2 da;
		| carry==0
			= resize_int_array s da;
			= {da & [s]=carry};
		| size a1<size a2
			= add_integer_integer_smaller_first a1 a2;
			= add_integer_integer_smaller_first a2 a1;
{}{
	add_integer_integer_smaller_first :: !{#Int} !{#Int} -> *{#Int};
	add_integer_integer_smaller_first a1 a2
		# s=size a2;
		| a2.[s-1]<> -1 // no carry
			# da = create_unintialized_int_array s;
			# d = size a1;
			# (carry,da) = add_integer_arrays 0 0 d a1 a2 da;
			| carry==0
				= copy_array_slice d s a2 da;
			| carry<>1
				= abort "add_integer_integer_smaller_first";
				= add_carry_and_copy_array_slice d s a2 da;
			# da = create_unintialized_int_array (s+1);
			# (carry,da) = add_integer_arrays 0 0 (size a1) a1 a2 da;
			# d = size a1;
			| carry==0
				# da = resize_int_array s da;
				= copy_array_slice d s a2 da;
			| carry<>1
				= abort "add_integer_integer_smaller_first";
				# (carry,da) = add_carry_and_copy_array_slice2 d s a2 da;
				| carry
					= {da & [s]=1};
					= resize_int_array s da;
}

copy_array :: !{#Int} -> *{#Int};
copy_array a
	= {e\\e<-:a};

copy_array_slice :: !Int !Int !{#Int} !*{#Int} -> *{#Int};
copy_array_slice i s sa da
	| i<s
		# da = {da & [i]=sa.[i]};
		= copy_array_slice (i+1) s sa da;
		= da;

/* unroll loop 4 times
copy_array :: !{#Int} -> *{#Int};
copy_array a
	#! s=size a;
	# da = create_unintialized_int_array s;
	= copy_array_slice 0 s a da;

copy_array_slice :: !Int !Int !{#Int} !*{#Int} -> *{#Int};
copy_array_slice i s sa da
	= copy_array_slice4 i (s-3) sa da;
{
	copy_array_slice4 :: !Int !Int !{#Int} !*{#Int} -> *{#Int};
	copy_array_slice4 i s sa da
		| i<s
			# da = {da & [i]=sa.[i]};
			# da = {da & [i+1]=sa.[i+1]};
			# da = {da & [i+2]=sa.[i+2]};
			# da = {da & [i+3]=sa.[i+3]};
			= copy_array_slice4 (i+4) s sa da;
			= copy_array_slice i (s+3) sa da;

	copy_array_slice :: !Int !Int !{#Int} !*{#Int} -> *{#Int};
	copy_array_slice i s sa da
		| i<s
			# da = {da & [i]=sa.[i]};
			= copy_array_slice (i+1) s sa da;
			= da;
}
*/

add_carry_and_copy_array_slice :: !Int !Int !{#Int} !*{#Int} -> *{#Int};
add_carry_and_copy_array_slice i s sa da
	# n = sa.[i]+1;
	# da = {da & [i]=n};
	| n==0
		= add_carry_and_copy_array_slice (i+1) s sa da;
		= copy_array_slice (i+1) s sa da;

add_carry_and_copy_array_slice2 :: !Int !Int !{#Int} !*{#Int} -> (!Bool,!*{#Int});
add_carry_and_copy_array_slice2 i s sa da
	| i<s
		# n = sa.[i]+1;
		# da = {da & [i]=n};
		| n==0
			= add_carry_and_copy_array_slice2 (i+1) s sa da;
			= (False,copy_array_slice (i+1) s sa da);
		= (True,da);

subtract_borrow i a
	#! n = a.[i]-1;
	# a = {a & [i]=n};
	| n== -1
		= subtract_borrow (i+1) a;
		= a;

subtract_borrow_and_copy_array_slice :: !Int !Int !{#Int} !*{#Int} -> *{#Int};
subtract_borrow_and_copy_array_slice i s sa da
	# n = sa.[i]-1;
	# da = {da & [i]=n};
	| n== -1
		= subtract_borrow_and_copy_array_slice (i+1) s sa da;
		= copy_array_slice (i+1) s sa da;

add_integer_arrays :: !Int !Int !Int !{#Int} !{#Int} !*{#Int} -> (!Int,!*{#Int});
add_integer_arrays i carry s a1 a2 da
	| i<s
		# i1=a1.[i];
		# (carry,n) = addLU 0 i1 carry;
//		# (carry,n) = addLU 0 carry i1;
		# i2=a2.[i];
		# (carry,n) = addLU carry n i2;
		# da = {da & [i]=n};
		= add_integer_arrays (i+1) carry s a1 a2 da;
		= (carry,da);
/*

add_integer_arrays :: !Int !Int !Int !{#Int} !{#Int} !*{#Int} -> (!Int,!*{#Int});
add_integer_arrays i carry s a1 a2 da
	= add_integer_arrays2 i carry (s-1) a1 a2 da;
{
	add_integer_arrays2 :: !Int !Int !Int !{#Int} !{#Int} !*{#Int} -> (!Int,!*{#Int});
	add_integer_arrays2 i carry s a1 a2 da
		| i<s

			# (carry,n) = addLU 0 a1.[i] carry;
			# (carry,n) = addLU carry n a2.[i];
			# da = {da & [i]=n};

			# (carry,n) = addLU 0 a1.[i+1] carry;
			# (carry,n) = addLU carry n a2.[i+1];
			# da = {da & [i+1]=n};

			= add_integer_arrays2 (i+2) carry s a1 a2 da;
			= add_integer_arrays i carry (s+1) a1 a2 da;

	add_integer_arrays :: !Int !Int !Int !{#Int} !{#Int} !*{#Int} -> (!Int,!*{#Int});
	add_integer_arrays i carry s a1 a2 da
		| i<s
			# (carry,n) = addLU 0 a1.[i] carry;
			# (carry,n) = addLU carry n a2.[i];
			# da = {da & [i]=n};
			= add_integer_arrays (i+1) carry s a1 a2 da;
			= (carry,da);
}
*/

sub_integer_arrays :: !Int !Int !Int !{#Int} !{#Int} !*{#Int} -> (!Int,!*{#Int});
sub_integer_arrays i borrow s a1 a2 da
	| i<s
		# i1=a1.[i];
		# (borrow,n) = subLU 0 i1 borrow;
		# i2=a2.[i];
		# (borrow,n) = subLU borrow n i2;
		# da = {da & [i]=n};
		= sub_integer_arrays (i+1) (~borrow) s a1 a2 da;
		= (borrow,da);
