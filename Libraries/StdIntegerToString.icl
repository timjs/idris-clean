implementation module StdIntegerToString;

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

div2t5pd :: !Int !Int !Int !*{#Int} -> (!Int,!*{#Int});
div2t5pd i r i_b a
	| i<i_b
		= (r,a);
		#! ai=a.[i];
//		# (r,q) = divdu r ai I2T5PD;
		# (r,q) = Udivd_by_mul_2t5pd r ai;
		# a = {a & [i]=q};
		= div2t5pd (i-1) r i_b a;

integer_to_string :: !Integer -> {#Char};
integer_to_string {integer_s=s,integer_a=a}
	| size a==0
		= toString s;
		= u_integer_to_string2 {integer_s=s,integer_a={e\\e<-:a}};

u_integer_to_string :: !*Integer -> {#Char};
u_integer_to_string n
	| size n.integer_a==0
		= toString n.integer_s;
		= u_integer_to_string2 n;

u_integer_to_string2 :: !*Integer -> {#Char};
u_integer_to_string2 {integer_s=s,integer_a=a}
	# (sa,a) = usize a;
	# max_chars = sa*(IF_INT_64_OR_32 20 10);
	| s>=0
		# s = createArray max_chars ' ';
		# s = integer_to_string 0 (sa-1) 0 a (max_chars-1) s;
		= remove_leading_spaces 0 s;
		# max_chars = max_chars+1;
		# s = createArray max_chars ' ';
		# s = integer_to_string 0 (sa-1) 0 a (max_chars-1) s;
		= remove_leading_spaces_and_add_minus 0 s;
	{}{
		integer_to_string :: !Int !Int !Int !*{#Int} !Int !*{#Char} -> *{#Char};
		integer_to_string i_a_b i_a_e shift a si s
			| i_a_e<i_a_b
				= s;
			# (rem_d,shift,i_a_b,a) = shift_dm1 shift i_a_b a
			# (r,a) = div2t5pd i_a_e 0 i_a_b a;
			# (e,a) = a![i_a_b];
			  f = e bitand ((1<<shift)-1);
			  e = e bitxor f;
			  a = {a & [i_a_b]=e};
			# (i_a_e,a) = remove_trailing_zeros i_a_b i_a_e a;
			# (h,l) = mulud f I2T5PD;
			  (h,l) = addLU h l r;
			  (h,l) = shift_right_m_dm1 shift h l;
//			| l bitand (IF_INT_64_OR_32 0x3ffffff 0xfff)<>0
//				= abort "integer_to_string";
			# l = l+rem_d;
			#! h=h; l=l;
			| i_a_e<i_a_b
				= convert_integer2_to_string h l si s;
			# s = (IF_INT_64_OR_32 convert27digits_64 convert13digits_32) h l (si-N_digits_in_part_m1) s;
			  si = si-N_digits_in_part;
	  		= integer_to_string i_a_b i_a_e shift a si s;

		shift_right_m_dm1 :: !Int !Int !Int -> (!Int,!Int);
		shift_right_m_dm1 shift h l
			| shift<N_digits_in_part_m1
				# n = N_digits_in_part_m1-shift;
				# h =(h<<n) bitor (l+>>((IF_INT_64_OR_32 38 20)+shift));
				# l = l<<n;
				= (h,l);
			| shift==N_digits_in_part_m1
				= (h,l);
	  		// shift>N_digits_in_part_m1
				# n = shift-N_digits_in_part_m1;
		  		# l =(l+>>n) bitor (h<<((IF_INT_64_OR_32 90 44)-shift));
		  		# h = h+>>n;
		  		= (h,l);

		shift_dm1 :: !Int !Int !*{#Int} -> (!Int,!Int,!Int,!*{#Int});
		shift_dm1 shift i_a_b a
			| shift<IF_INT_64_OR_32 38 20 // 64-26 32-12
			  #	(e,a) = a![i_a_b];
				r = (e+>>shift) bitand (IF_INT_64_OR_32 0x3ffffff 0xfff);
				e = e bitand (-1<<(shift+N_digits_in_part_m1));
				a = {a & [i_a_b] = e};
			  =	(r,shift+N_digits_in_part_m1,i_a_b,a);
			| shift==IF_INT_64_OR_32 38 20
			  #	(e,a) = a![i_a_b];
				r = e+>>(IF_INT_64_OR_32 38 20);
				a = {a & [i_a_b] = 0};
			  =	(r,0,i_a_b+1,a);
//			| shift>IF_INT_64_OR_32 38 20
			  #	(el,a) = a![i_a_b];
				(eh,a) = a![i_a_b+1];
				r = ((el+>>shift) + (eh << (N_bits_in_int-shift))) bitand (IF_INT_64_OR_32 0x3ffffff 0xfff);
				shift = shift-(IF_INT_64_OR_32 38 20);
				eh = eh bitand (-1 << shift);
				a = {a & [i_a_b]=0, [i_a_b+1]=eh};
			  =	(r,shift,i_a_b+1,a);
	}

convert_integer2_to_string h l si s
	| h==0 && l>=0
		| l==0
			= s;
			# rs = toString l;
			= copy_chars (size rs-1) rs si s;
		# (l,h) = (IF_INT_64_OR_32 Udivd_1e18_64 Udivd_1e9_32) h l;
		# si = si-(IF_INT_64_OR_32 17 8);
		# s = (IF_INT_64_OR_32 convert18digits_64 convert9digits) l si s;
		# si = si-1;
		# rs = toString h;
		= copy_chars (size rs-1) rs si s;

int_to_char i :== toChar (i+48);

convert9digits :: !Int !Int !*{#Char} -> *{#Char};
convert9digits n i a
  #	i4_28 = 1 + (IF_INT_64_OR_32
					((n*0xabcc7712)+>>30)
  					(let {
			 			(h,l) = mulud n 0xabcc7712; // 2882303762, ceil 2^58/10^8
  					} in
  						(h<<2)+(l+>>30)));
	a = {a & [i] = int_to_char (i4_28+>>28)};
	i4_28 = (i4_28 bitand 0xfffffff)*5;
	a = {a & [i+1] = int_to_char (i4_28+>>27)};
	i4_28 = (i4_28 bitand 0x7ffffff)*5;
	a = {a & [i+2] = int_to_char (i4_28+>>26)};
	i4_28 = (i4_28 bitand 0x3ffffff)*5;
	a = {a & [i+3] = int_to_char (i4_28+>>25)};
	i4_28 = (i4_28 bitand 0x1ffffff)*5;
	a = {a & [i+4] = int_to_char (i4_28+>>24)};
	i4_28 = (i4_28 bitand 0xffffff)*5;
	a = {a & [i+5] = int_to_char (i4_28+>>23)};
	i4_28 = (i4_28 bitand 0x7fffff)*5;
	a = {a & [i+6] = int_to_char (i4_28+>>22)};
	i4_28 = (i4_28 bitand 0x3fffff)*5;
	a = {a & [i+7] = int_to_char (i4_28+>>21)};
	i4_28 = (i4_28 bitand 0x1fffff)*5;
	a = {a & [i+8] = int_to_char (i4_28+>>20)};
  = a;

convert18digits_64 :: !Int !Int !*{#Char} -> *{#Char};
convert18digits_64 n i a
  #	h = n/I1E9;
  #	l = n-h*I1E9;

  #	nl = l*0xabcc7712; // 2882303762, ceil 2^58/10^8
	nh = h*0xabcc7712; // 2882303762, ceil 2^58/10^8
  	i4_28 = (1+(nl+>>30)) + ((1+(nh+>>30))<<32);

	a = {a & [i]   = int_to_char (i4_28+>>60), [i+9]  = int_to_char ((i4_28+>>28) bitand 15)};
	i4_28 = (i4_28 bitand 0x0fffffff0fffffff)*5;
	a = {a & [i+1] = int_to_char (i4_28+>>59), [i+10] = int_to_char ((i4_28+>>27) bitand 15)};
	i4_28 = (i4_28 bitand 0x07ffffff07ffffff)*5;
	a = {a & [i+2] = int_to_char (i4_28+>>58), [i+11] = int_to_char ((i4_28+>>26) bitand 15)};
	i4_28 = (i4_28 bitand 0x03ffffff03ffffff)*5;
	a = {a & [i+3] = int_to_char (i4_28+>>57), [i+12] = int_to_char ((i4_28+>>25) bitand 15)};
	i4_28 = (i4_28 bitand 0x01ffffff01ffffff)*5;
	a = {a & [i+4] = int_to_char (i4_28+>>56), [i+13] = int_to_char ((i4_28+>>24) bitand 15)};
	i4_28 = (i4_28 bitand 0x00ffffff00ffffff)*5;
	a = {a & [i+5] = int_to_char (i4_28+>>55), [i+14] = int_to_char ((i4_28+>>23) bitand 15)};
	i4_28 = (i4_28 bitand 0x007fffff007fffff)*5;
	a = {a & [i+6] = int_to_char (i4_28+>>54), [i+15] = int_to_char ((i4_28+>>22) bitand 15)};
	i4_28 = (i4_28 bitand 0x003fffff003fffff)*5;
	a = {a & [i+7] = int_to_char (i4_28+>>53), [i+16] = int_to_char ((i4_28+>>21) bitand 15)};
	i4_28 = (i4_28 bitand 0x001fffff001fffff)*5;
	a = {a & [i+8] = int_to_char (i4_28+>>52), [i+17] = int_to_char ((i4_28+>>20) bitand 15)};
  = a;

convert13digits_32 :: !Int !Int !Int !*{#Char} -> *{#Char};
convert13digits_32 h l i a
	# (l,h) = Udivd_1e9_32 h l;
	# a = convert9digits l (i+4) a;
	# q = h/10;
	# a = {a & [i+3]=toChar (h-10*q+48)};
	# h = q;
	# q = h/10;
	# a = {a & [i+2]=toChar (h-10*q+48)};
	# h = q;
	# q = h/10;
	= {a & [i]=toChar (q+48),[i+1]=toChar (h-10*q+48)};

convert27digits_64 :: !Int !Int !Int !*{#Char} -> *{#Char};
convert27digits_64 h l si s
	# (l,h) = Udivd_1e18_64 h l;
	# s = convert18digits_64 l (si+9) s;
	= convert9digits h si s;

copy_chars :: !Int !{#Char} !Int !*{#Char} -> *{#Char};
copy_chars si s di d
	| si>=0
		# d={d & [di]=s.[si]};
		= copy_chars (si-1) s (di-1) d;
		= d;

remove_leading_spaces :: !Int !{#Char} -> {#Char};
remove_leading_spaces i s
	| s.[i]==' '
		= remove_leading_spaces (i+1) s;
	| i==0
		= s;
		= s % (i,size s-1);

remove_leading_spaces_and_add_minus :: !Int !*{#Char} -> {#Char};
remove_leading_spaces_and_add_minus i s
	| s.[i]==' '
		= remove_leading_spaces_and_add_minus (i+1) s;
	| i==0
		= "-"+++s;
		# i=i-1;
		# (size_s,s) = usize s;
		= {s & [i]='-'} % (i,size_s-1);
