implementation module StdStringToInteger;

import StdEnv;

from StdInteger import ::Integer(..);

N_bits_in_int    :== IF_INT_64_OR_32 64 32;
Max_bit_n_in_int :== IF_INT_64_OR_32 63 31;

N_digits_in_part :== IF_INT_64_OR_32 27 13;
N_digits_in_part_p1 :== IF_INT_64_OR_32 28 14;
N_digits_in_part_m1 :== IF_INT_64_OR_32 26 12;

I5PD   :== IF_INT_64_OR_32  7450580596923828125 1220703125; // 5^27 5^13
I2T5PD :== IF_INT_64_OR_32 14901161193847656250 2441406250; // 2*5^27 2*5^13

ltU :: !Int !Int -> Bool;
ltU a b = code inline {
	ltU
}

mulud :: !Int !Int -> (!Int,!Int);
mulud a b = code inline {
	mulUUL
}

addLU :: !Int !Int !Int -> (!Int,!Int);
addLU _ _ _ = code inline {
	addLU
}

divdu :: !Int !Int !Int -> (!Int,!Int);
divdu a b c = code inline {
	divLU
}

ushiftr :: !Int !Int -> Int;
ushiftr a b = code inline {
	shiftrU
}

(+<) infix 4;
(+<) a b :== ltU a b;

(+>>) infix 7;
(+>>) a b :== ushiftr a b;

Udivd_by_mul_2t5pd nh nl
 :== let {
	m = IF_INT_64_OR_32 4389219009585806480 -1034148220;
	dn = IF_INT_64_OR_32 -3545582879861895366 -1853561046;
	i2t5pd = dn;
	n2 = nh;
	n10 = nl;
	mn1 = n10>>Max_bit_n_in_int;
	nadj = n10+(mn1 bitand dn);
	(q1h,q1l) = mulud m (n2-mn1);
	(q1,_) = addLU (q1h+n2) q1l nadj;
//	nq1i = -1-q1;
	nq1i = bitnot q1;
	(ph,pl) = mulud nq1i (IF_INT_64_OR_32 i2t5pd I2T5PD);
	nhsd = nh-(IF_INT_64_OR_32 i2t5pd I2T5PD);
	(drh,drl) = addLU (nhsd+ph) nl pl;
	q = drh - nq1i;
	r = drl + ((IF_INT_64_OR_32 i2t5pd I2T5PD) bitand drh)
 } in (r,q);

I1E9:==1000000000;

Udivd_1e9_32 nh nl
  :== let {
    di = 316718722;
	dn = -294967296;
	nh4 = nh<<2;
	n2 = nh4 + (ushiftr nl 30);
	n10 = nl<<2;
	mn1 = n10>>31;
	na = n10 + (mn1 bitand dn);
	(ph,pl) = mulud di (n2-mn1);
	(q1,_) = addLU (ph+(n2+1)) pl na;
	rl = nl + q1 * -1000000000;
	rh = rl>>31;
	r = rl+(I1E9 bitand rh);
	q = q1+rh;
  } in (r,q);

Udivd_1e18_64 nh nl
  :== let {
    di = 2820903858849102350;
	dn = -2446744073709551616;
	nhs = nh<<4;
	n2 = nhs + (nl+>>60);
	n10 = nl<<4;
	mn1 = n10>>63;
	na = n10 + (mn1 bitand dn);
	(ph,pl) = mulud di (n2-mn1);
	(q1,_) = addLU (ph+(n2+1)) pl na;
	rl = nl + q1 * IM1E18;
	rh = rl>>63;
	r = rl+(I1E18 bitand rh);
	q = q1+rh;
  } in (r,q);

convert9c :: !Int !{#Char} -> Int;
convert9c i s
  #	n=(toInt s.[i])-48;
	n=n*10+((toInt s.[i+1])-48);
	n=n*10+((toInt s.[i+2])-48);
	n=n*10+((toInt s.[i+3])-48);
	n=n*10+((toInt s.[i+4])-48);
	n=n*10+((toInt s.[i+5])-48);
	n=n*10+((toInt s.[i+6])-48);
	n=n*10+((toInt s.[i+7])-48);
	n=n*10+((toInt s.[i+8])-48);
  =	n;

convert18c_64 :: !Int !{#Char} -> Int;
convert18c_64 i s
  #	t48 = 0x3000000030;
  #	n=((toInt s.[i])<<32)+(toInt s.[i+9])-t48;
	n=n*10+(((toInt s.[i+1])<<32)+(toInt s.[i+10])-t48);
	n=n*10+(((toInt s.[i+2])<<32)+(toInt s.[i+11])-t48);
	n=n*10+(((toInt s.[i+3])<<32)+(toInt s.[i+12])-t48);
	n=n*10+(((toInt s.[i+4])<<32)+(toInt s.[i+13])-t48);
	n=n*10+(((toInt s.[i+5])<<32)+(toInt s.[i+14])-t48);
	n=n*10+(((toInt s.[i+6])<<32)+(toInt s.[i+15])-t48);
	n=n*10+(((toInt s.[i+7])<<32)+(toInt s.[i+16])-t48);
	n=n*10+(((toInt s.[i+8])<<32)+(toInt s.[i+17])-t48);
  =	(n+>>32) * I1E9 + (n bitand 0xffffffff);

convert13c_32 :: !Int !{#Char} -> (!Int,!Int);
convert13c_32 i s
  #	nl=convert9c (i+4) s;
	nh=(toInt s.[i])-48;
	nh=nh*10+((toInt s.[i+1])-48);
	nh=nh*10+((toInt s.[i+2])-48);
	nh=nh*10+((toInt s.[i+3])-48);
	(h,l) = mulud nh I1E9;
  =	addLU h l nl;

convert27c_64 :: !Int !{#Char} -> (!Int,!Int);
convert27c_64 i s
  #	nl=convert18c_64 (i+9) s;
   	nh=convert9c i s;
	(h,l) = mulud nh 1000000000000000000;
  =	addLU h l nl;

convert10to13c :: !Int !Int !{#Char} -> (!Int,!Int);
convert10to13c i_f_s i_l_s s
  #	i = i_l_s-9;
	nl = convert9c i s;
	nh = convertchars i_f_s i 0 s;
	(h,l) = mulud nh I1E9;
  =	addLU h l nl;

I1E18 :== 1000000000000000000;
IM1E18 :== -1000000000000000000;

convert19to27c_64 :: !Int !Int !{#Char} -> (!Int,!Int);
convert19to27c_64 i_f_s i_l_s s
  #	i = i_l_s-18;
	nl = convert18c_64 i s;
	nh = convertchars i_f_s i 0 s;
	(h,l) = mulud nh I1E18;
  =	addLU h l nl;

convertchars :: !Int !Int !Int !{#Char} -> Int;
convertchars i end_i n s
	| i<end_i
		# n=n*10+((toInt s.[i])-48);
		= convertchars (i+1) end_i n s;
		= n;

string_to_integer :: !{#Char} -> *Integer;
string_to_integer s
	| s.[0]=='-'
		| size s<=N_digits_in_part_p1
			| size s<(IF_INT_64_OR_32 20 11)
				= {integer_s=toInt s,integer_a={}};
				# (h,l) = (IF_INT_64_OR_32 convert19to27c_64 convert10to13c) 1 (size s) s;
				# ml = ~l;
				| h==0
					| ml<0
						= {integer_s= ml,integer_a={}};
						= {integer_s= -1,integer_a={l}};
					= {integer_s= -1,integer_a={l,h}};
				# n = string_to_integer2 1 s;
				= {n & integer_s= -1};
		| size s<=N_digits_in_part
			| size s<(IF_INT_64_OR_32 19 10)
				= {integer_s=toInt s,integer_a={}};
				# (h,l) = (IF_INT_64_OR_32 convert19to27c_64 convert10to13c) 0 (size s) s;
				| h==0
					| l>=0
						= {integer_s=l,integer_a={}};
						= {integer_s=0,integer_a={l}};
					= {integer_s=0,integer_a={l,h}};
				= string_to_integer2 0 s;

string_to_integer2 :: !Int !{#Char} -> *Integer;
string_to_integer2 i_s s
	# n_chars = size s - i_s;
	  n_parts = n_chars/N_digits_in_part;
	  n_chars_in_parts = n_parts * N_digits_in_part;
	  n_chars_in_first_part = n_chars-n_chars_in_parts;
	  n_shifts = n_chars_in_parts>>(IF_INT_64_OR_32 6 5);
	  size_a = n_parts + 3 + n_shifts;
	  a = createArray size_a 0;
	  i_a_f = n_shifts;
	  (i_a_e,a) = convert_first_chars n_chars_in_first_part i_a_f a i_s s;
	  a=stoi_next_part_no_shift (i_s+n_chars_in_first_part) s i_a_f i_a_e 0 0 a;
	= remove_trailing_zeros0 (size_a-1) a;
{
	convert_first_chars :: !Int !Int !*{#Int} !Int !{#Char} -> (!Int,!*{#Int});
	convert_first_chars n_chars_in_first_part i_a_f a i_s s
		| n_chars_in_first_part==0
			= (i_a_f,a);
		| n_chars_in_first_part<IF_INT_64_OR_32 19 10
			# i = toInt (s % (i_s,i_s+n_chars_in_first_part-1));
			  a = {a & [i_a_f]=i};
			= (i_a_f+1,a);
			# (h,l) = (IF_INT_64_OR_32 convert19to27c_64 convert10to13c) i_s (i_s+n_chars_in_first_part) s;
			| h==0
				# a = {a & [i_a_f]=l};
				= (i_a_f+1,a);
				# a = {a & [i_a_f]=l,[i_a_f+1]=h};
				= (i_a_f+2,a);
}

stoi_next_part_no_shift :: !Int !{#Char} !Int !Int !Int !Int !*{#Int} -> *{#Int};
stoi_next_part_no_shift i_s s i_a_f i_a_e shift r a
	| i_s<size s
//		&& trace_tn (to_string i_a_f i_a_e a)

		# (h,l) = (IF_INT_64_OR_32 convert27c_64 convert13c_32) i_s s;
		# (yh,yl) = mulud r I5PD;
//			trace_tn ("h = "+++toString h+++" l "+++toString l) &&
//			trace_tn ("yh = "+++toString yh+++" yl "+++toString yl) &&
		| shift<IF_INT_64_OR_32 37 19 // 64-27 32-13
			# x = l bitand (IF_INT_64_OR_32 0x7ffffff 0x1fff);
			  l = (ushiftr l N_digits_in_part) + (h<<IF_INT_64_OR_32 37 19);
			  (rh,rl) = addLU yh yl l;
			  new_r = x bitor ((rl bitand ((1<<shift)-1))<<N_digits_in_part);
			  n = (ushiftr rl shift)+(rh<<(N_bits_in_int-shift));
			  (i_a_e,a) = mul_5pd_add i_a_f i_a_e a 0;
			  (i_a_e,a) = addi n i_a_f i_a_e a;
			= stoi_next_part_no_shift (i_s+N_digits_in_part) s i_a_f i_a_e (shift+N_digits_in_part) new_r a;
		| shift==IF_INT_64_OR_32 37 19
			# yh =(yh<<N_digits_in_part) + (ushiftr yl (IF_INT_64_OR_32 37 19));
			  yl = yl<<N_digits_in_part;
			  (h,l) = addLU (yh+h) yl l;
			  (i_a_e,a) = mul_5pd_add i_a_f i_a_e a 0;
			  (i_a_e,a) = addi h i_a_f i_a_e a;
			  (i_a_f,a) = insert_at_begin l i_a_f a;
			= stoi_next_part_no_shift (i_s+N_digits_in_part) s i_a_f i_a_e 0 0 a;

			# n = shift-(IF_INT_64_OR_32 37 19);
			  new_r = l bitand ((1<<n)-1);
			  l =(ushiftr l n) + (h<<(N_bits_in_int-n));
			  h = ushiftr h n;
			  ys = N_digits_in_part-n;
			  yh =(yh<<ys) + (ushiftr yl (N_bits_in_int-ys));
			  yl = yl<<ys;
			  (h,l) = addLU (yh+h) yl l;
			  (i_a_e,a) = mul_5pd_add i_a_f i_a_e a 0;
			  (i_a_e,a) = addi h i_a_f i_a_e a;
			  (i_a_f,a) = insert_at_begin l i_a_f a;
			= stoi_next_part_no_shift (i_s+N_digits_in_part) s i_a_f i_a_e n new_r a;
	| i_s==size s && i_a_f==0 && i_a_e<=size a
		| shift==0
			= a;
			# (i_a,a) = shift_left 0 r shift i_a_e a;
			= a;
where {
	insert_at_begin :: !Int !Int !*{#Int} -> (!Int,!*{#Int});
	insert_at_begin n i_a_f a
		| i_a_f<=0
			= abort "insert_at_begin";
		# i_a_f = i_a_f-1;
		# a = {a & [i_a_f]=n};
		= (i_a_f,a);
}

addi :: !Int !Int !Int !*{#Int} -> (!Int,!*{#Int});
addi n i_a_f i_a_e a
	| i_a_f==i_a_e
		= (i_a_f+1,{a & [i_a_f]=n});
	# (ai,a)=a![i_a_f];
	# s=ai+n;
	# a={a & [i_a_f]=s};
	| s+<n
		= add_carry (i_a_f+1) i_a_e a;
		= (i_a_e,a);

add_carry :: !Int !Int !*{#Int} -> (!Int,!*{#Int});
add_carry i i_a a
	| i<i_a
		# (ai,a)=a![i];
		# ai=ai+1;
		# a = {a & [i]=ai};
		| ai<>0
			= (i_a,a);
			= add_carry (i+1) i_a a;
		= (i_a+1,{a & [i]=1});

remove_trailing_zeros0 :: !Int !*{#Int} -> *Integer;
remove_trailing_zeros0 i a
	| i==0 && a.[0]>=0
		= {integer_s=a.[0],integer_a={}};
	| a.[i]==0
		= remove_trailing_zeros0 (i-1) a;
	| i+1==size a
		= {integer_s=0,integer_a=a};
		= {integer_s=0,integer_a={a.[n]\\n<-[0..i]}};

remove_trailing_zeros :: !Int !Int !*{#Int} -> (!Int,!*{#Int});
remove_trailing_zeros b i a
	| i>=b && a.[i]==0
		= remove_trailing_zeros b (i-1) a;
		= (i,a);

mul_5pd_add :: !Int !Int !*{#Int} !Int -> (!Int,!*{#Int});
mul_5pd_add i i_a a n
	| i==i_a
		| n<>0
			= (i+1,{a & [i]=n});
			= (i,a);
		# (ai,a) = a![i];
		# (h,l) = mulud ai I5PD;
		# (h,l) = addLU h l n;
		# a = {a & [i]=l};
		= mul_5pd_add (i+1) i_a a h;

shift_left :: !Int !Int !Int !Int !*{#Int} -> (!Int,!*{#Int});
shift_left i n shift i_a a
	| i<i_a
		# (ai,a) = a![i];
		# new_n = ushiftr ai (N_bits_in_int-shift);
		# ai = (ai<<shift)+n;
		# a = {a & [i]=ai};
		= shift_left (i+1) new_n shift i_a a;
	| n==0
		= (i_a,a);
		= (i_a+1,{a & [i_a]=n});
