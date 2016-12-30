implementation module StdPointer

import StdOverloaded, StdClass, StdArray, StdInt, StdChar, StdString

readInt :: !Pointer !Offset -> Int
readInt pointer offset = IF_INT_64_OR_32 (readInt_64 pointer offset) (readInt_32 pointer offset)

readInt_64 :: !Pointer !Offset -> Int
readInt_64 pointer offset = code {
		pop_b 1
|		mov    (%rbx,%rax,1),%rax
		instruction 72
	 	instruction 139
	 	instruction 4
	 	instruction 3
}

readInt_32 :: !Pointer !Offset -> Int
readInt_32 pointer offset = code {
		pop_b 1
|		mov    (%ebx,%eax,1),%eax
	 	instruction 139
	 	instruction 4
	 	instruction 3
}

readIntP :: !Pointer !Offset -> (!Int,!Pointer)
readIntP pointer offset = IF_INT_64_OR_32 (readIntP_64 pointer offset) (readIntP_32 pointer offset)

readIntP_64 :: !Pointer !Offset -> (!Int,!Pointer)
readIntP_64 pointer offset = code {
|		mov    (%rbx,%rax,1),%rcx
		instruction 72
	 	instruction 139
	 	instruction 12
	 	instruction 3
|		mov    %rbx,%rax
		instruction 72
		instruction 139
		instruction 195
|		mov    %rcx,%rbx
		instruction 72
		instruction 139
		instruction 217
}

readIntP_32 :: !Pointer !Offset -> (!Int,!Pointer)
readIntP_32 pointer offset = code {
|		mov    (%ebx,%eax,1),%ecx
	 	instruction 139
	 	instruction 12
	 	instruction 3
|		mov    %ebx,%eax
		instruction 139
		instruction 195
|		mov    %ecx,%ebx
		instruction 139
		instruction 217
}

readIntElemOffset :: !Pointer !Offset -> Int
readIntElemOffset pointer offset = IF_INT_64_OR_32 (readIntElemOffset_64 pointer offset) (readIntElemOffset_32 pointer offset)

readIntElemOffset_64 :: !Pointer !Offset -> Int
readIntElemOffset_64 pointer offset = code {
		pop_b 1
|		mov    (%rbx,%rax,8),%rax
		instruction 72
	 	instruction 139
	 	instruction 4
	 	instruction 195
}

readIntElemOffset_32 :: !Pointer !Offset -> Int
readIntElemOffset_32 pointer offset = code {
		pop_b 1
|		mov    (%ebx,%eax,4),%eax
	 	instruction 139
	 	instruction 4
	 	instruction 131
}

readIntElemOffsetP :: !Pointer !Offset -> (!Int,!Pointer)
readIntElemOffsetP pointer offset = IF_INT_64_OR_32 (readIntElemOffsetP_64 pointer offset) (readIntElemOffsetP_32 pointer offset)

readIntElemOffsetP_64 :: !Pointer !Offset -> (!Int,!Pointer)
readIntElemOffsetP_64 pointer offset = code {
|		mov    (%rbx,%rax,8),%rcx
		instruction 72
	 	instruction 139
	 	instruction 12
	 	instruction 195
|		mov    %rbx,%rax
		instruction 72
		instruction 139
		instruction 195
|		mov    %rcx,%rbx
		instruction 72
		instruction 139
		instruction 217
}

readIntElemOffsetP_32 :: !Pointer !Offset -> (!Int,!Pointer)
readIntElemOffsetP_32 pointer offset = code {
|		mov    (%ebx,%eax,4),%ecx
	 	instruction 139
	 	instruction 12
	 	instruction 131
|		mov    %ebx,%eax
		instruction 139
		instruction 195
|		mov    %ecx,%ebx
		instruction 139
		instruction 217
}

readInt32Z :: !Pointer !Offset -> Int
readInt32Z pointer offset = code {
		pop_b 1
|		mov    (%eax,%ebx,1),%eax
	 	instruction 139
	 	instruction 4
	 	instruction 24
}

readInt32S :: !Pointer !Offset -> Int
readInt32S pointer offset = IF_INT_64_OR_32 (readInt32S_64 pointer offset) (readInt32S_32 pointer offset)

readInt32S_64 :: !Pointer !Offset -> Int
readInt32S_64 pointer offset = code {
		pop_b 1
|		movsxd rax,dword ptr [rbx+rax]
		instruction 72
	 	instruction 99
	 	instruction 4
	 	instruction 3
}

readInt32S_32 :: !Pointer !Offset -> Int
readInt32S_32 pointer offset = code {
		pop_b 1
|		mov    (%eax,%ebx,1),%eax
	 	instruction 139
	 	instruction 4
	 	instruction 24
}

readInt16Z :: !Pointer !Offset -> Int
readInt16Z pointer offset = code {
		pop_b 1
|		movzwl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 183
	 	instruction 4
	 	instruction 24
}

readInt16S :: !Pointer !Offset -> Int
readInt16S pointer offset = IF_INT_64_OR_32 (readInt16S_64 pointer offset) (readInt16S_32 pointer offset)

readInt16S_64 :: !Pointer !Offset -> Int
readInt16S_64 pointer offset = code {
		pop_b 1
|		movswl (%rax,%rbx,1),%rax
		instruction 72
	 	instruction 15
	 	instruction 191
	 	instruction 4
	 	instruction 24
}

readInt16S_32 :: !Pointer !Offset -> Int
readInt16S_32 pointer offset = code {
		pop_b 1
|		movswl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 191
	 	instruction 4
	 	instruction 24
}

readInt8Z :: !Pointer !Offset -> Int
readInt8Z pointer offset = code {
		pop_b 1
|		movzbl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 182
	 	instruction 4
	 	instruction 24
}

readInt8S :: !Pointer !Offset -> Int
readInt8S pointer offset = IF_INT_64_OR_32 (readInt8S_64 pointer offset) (readInt8S_32 pointer offset)

readInt8S_64 :: !Pointer !Offset -> Int
readInt8S_64 pointer offset = code {
		pop_b 1
|		movsbl (%rax,%rbx,1),%rax
		instruction 72
	 	instruction 15
	 	instruction 190
	 	instruction 4
	 	instruction 24
}

readInt8S_32 :: !Pointer !Offset -> Int
readInt8S_32 pointer offset = code {
		pop_b 1
|		movsbl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 190
	 	instruction 4
	 	instruction 24
}

readChar :: !Pointer !Offset -> Char
readChar pointer offset = code {
		pop_b 1
|		movzbl (%eax,%ebx,1),%eax
	 	instruction 15
	 	instruction 182
	 	instruction 4
	 	instruction 24
}

readReal64 :: !Pointer !Offset -> Real
readReal64 pointer offset = IF_INT_64_OR_32 (readReal64_64 pointer offset) (readReal64_32 pointer offset)

readReal64_64 :: !Pointer !Offset -> Real
readReal64_64 pointer offset = code {
		pushR 0.0
		updatepop_b 0 2
		jmp read_f8_p_64
		:read_f8_p_64
|		movsd xmm0,mmword ptr [rbx+rax]
		instruction 242
		instruction 15
		instruction 16
		instruction 4
		instruction 3
}

readReal64_32 :: !Pointer !Offset -> Real
readReal64_32 pointer offset = code {
		pushR 0.0
		update_b 1 3
		updatepop_b 0 2
		jmp read_f8_p_32
		:read_f8_p_32
|		fldl   (%eax,%ebx,1)
		instruction 221
		instruction 4
		instruction 24
|		fstp   %st(1)
		instruction 221
		instruction 217
}

readReal32 :: !Pointer !Offset -> Real
readReal32 pointer offset = IF_INT_64_OR_32 (readReal32_64 pointer offset) (readReal32_32 pointer offset)

readReal32_64 :: !Pointer !Offset -> Real
readReal32_64 pointer offset = code {
		pushR 0.0
		updatepop_b 0 2
		jmp read_f4_p_64
		:read_f4_p_64
|		cvtss2sd    xmm0,dword ptr [rbx+rax]
		instruction 243
		instruction 15
		instruction 90
		instruction 4
		instruction 3
}

readReal32_32 :: !Pointer !Offset -> Real
readReal32_32 pointer offset = code {
		pushR 0.0
		update_b 1 3
		updatepop_b 0 2
		jmp read_f4_p_32
		:read_f4_p_32
|		flds   (%eax,%ebx,1)
		instruction 217
		instruction 4
		instruction 24
|		fstp   %st(1)
		instruction 221
		instruction 217
}

writeInt :: !Pointer !Offset !Int -> Pointer
writeInt pointer offset i = IF_INT_64_OR_32 (writeInt_64 pointer offset i) (writeInt_32 pointer offset i)

writeInt_64 :: !Pointer !Offset !Int -> Pointer
writeInt_64 pointer offset i = code {
		updatepop_b 0 2
|		mov qword ptr [rbx+r10],rax
		instruction 74
		instruction 137
		instruction 4
		instruction 19
}

writeInt_32 :: !Pointer !Offset !Int -> Pointer
writeInt_32 pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%ecx,(%eax,%ebx,1)
		instruction 137
		instruction 12
		instruction 24
}

writeIntElemOffset :: !Pointer !Offset !Int -> Pointer
writeIntElemOffset pointer offset i = IF_INT_64_OR_32 (writeIntElemOffset_64 pointer offset i) (writeIntElemOffset_32 pointer offset i)

writeIntElemOffset_64 :: !Pointer !Offset !Int -> Pointer
writeIntElemOffset_64 pointer offset i = code {
		updatepop_b 0 2
|		mov qword ptr [r10+rbx*8],rax
		instruction 73
		instruction 137
		instruction 4
		instruction 218
}

writeIntElemOffset_32 :: !Pointer !Offset !Int -> Pointer
writeIntElemOffset_32 pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%ecx,(%ebx,%eax,4)
		instruction 137
		instruction 12
		instruction 131
}

writeInt32 :: !Pointer !Offset !Int -> Pointer
writeInt32 pointer offset i = IF_INT_64_OR_32 (writeInt32_64 pointer offset i) (writeInt32_32 pointer offset i)

writeInt32_64 :: !Pointer !Offset !Int -> Pointer
writeInt32_64 pointer offset i = code {
		updatepop_b 0 2
|		mov dword ptr [rbx+r10],eax
 		instruction 66
		instruction 137
		instruction 4
		instruction 19
}

writeInt32_32 :: !Pointer !Offset !Int -> Pointer
writeInt32_32 pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%ecx,(%eax,%ebx,1)
		instruction 137
		instruction 12
		instruction 24
}

writeInt16 :: !Pointer !Offset !Int -> Pointer
writeInt16 pointer offset i = IF_INT_64_OR_32 (writeInt16_64 pointer offset i) (writeInt16_32 pointer offset i)

writeInt16_64 :: !Pointer !Offset !Int -> Pointer
writeInt16_64 pointer offset i = code {
		updatepop_b 0 2
|		mov word ptr [rbx+r10],ax
		instruction 102
 		instruction 66
		instruction 137
		instruction 4
		instruction 19
}

writeInt16_32 :: !Pointer !Offset !Int -> Pointer
writeInt16_32 pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movw	%cx,(%eax,%ebx,1)
		instruction 102
		instruction 137
		instruction 12
		instruction 24
}

writeInt8 :: !Pointer !Offset !Int -> Pointer
writeInt8 pointer offset i = IF_INT_64_OR_32 (writeInt8_64 pointer offset i) (writeInt8_32 pointer offset i)

writeInt8_64 :: !Pointer !Offset !Int -> Pointer
writeInt8_64 pointer offset i = code {
		updatepop_b 0 2
|		mov byte ptr [rbx+r10],al
		instruction 66
		instruction 136
		instruction 4
		instruction 19
}

writeInt8_32 :: !Pointer !Offset !Int -> Pointer
writeInt8_32 pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%cl,(%eax,%ebx,1)
		instruction 136
		instruction 12
		instruction 24
}

writeChar :: !Pointer !Offset !Char -> Pointer
writeChar pointer offset i = IF_INT_64_OR_32 (writeChar_64 pointer offset i) (writeChar_32 pointer offset i)

writeChar_64 :: !Pointer !Offset !Char -> Pointer
writeChar_64 pointer offset i = code {
		updatepop_b 0 2
|		mov byte ptr [rbx+r10],al
		instruction 66
		instruction 136
		instruction 4
		instruction 19
}

writeChar_32 :: !Pointer !Offset !Char -> Pointer
writeChar_32 pointer offset i = code {
		updatepop_b 0 2
|		mov    (%esp),%ecx
		instruction	139
		instruction 12
		instruction 36
|		movl	%cl,(%eax,%ebx,1)
		instruction 136
		instruction 12
		instruction 24
}

writeReal64 :: !Pointer !Offset !Real -> Pointer
writeReal64 pointer offset double = IF_INT_64_OR_32 (writeReal64_64 pointer offset double) (writeReal64_32 pointer offset double)

writeReal64_64 :: !Pointer !Offset !Real -> Pointer
writeReal64_64 pointer offset double = code {
		updatepop_b 0 2
|		movsd mmword ptr [rbx+rax],xmm0
		instruction 242
		instruction 15
		instruction 17
		instruction 4
		instruction 3
}

writeReal64_32 :: !Pointer !Offset !Real -> Pointer
writeReal64_32 pointer offset double = code {
		updatepop_b 0 3
|		fstl  (%eax,%ebx,1)
		instruction	221
		instruction 20
		instruction 24
}

writeReal32 :: !Pointer !Offset !Real -> Pointer
writeReal32 pointer offset double = IF_INT_64_OR_32 (writeReal32_64 pointer offset double) (writeReal32_32 pointer offset double)

writeReal32_64 :: !Pointer !Offset !Real -> Pointer
writeReal32_64 pointer offset double = code {
		updatepop_b 0 2
|		cvtsd2ss xmm0,xmm0
		instruction 242
		instruction 15
		instruction 90
		instruction 192
|		movss dword ptr [rbx+rax],xmm0
		instruction 243
		instruction 15
		instruction 17
		instruction 4
		instruction 3
}

writeReal32_32 :: !Pointer !Offset !Real -> Pointer
writeReal32_32 pointer offset double = code {
		updatepop_b 0 3
|		fsts  (%eax,%ebx,1)
		instruction	217
		instruction 20
		instruction 24
}

derefInt :: !Pointer -> Int
derefInt ptr = code {
		load_i 0
}

derefString :: !Pointer -> String
derefString ptr = copy ptr 0 (createArray len '\0')
where
	len = skip_to_zero ptr - ptr

	skip_to_zero ptr
		| load_char ptr <> '\0'	= skip_to_zero (ptr+1)
								= ptr

	copy :: !Pointer !Offset *{#Char} -> *{#Char}
	copy ptr off arr
		# char = load_char (ptr+off)
		| char <> '\0'	= copy ptr (off + 1) {arr & [off] = char}
						= arr

derefCharArray :: !Pointer !Int -> {#Char}
derefCharArray ptr len = copy 0 (createArray len '\0')
where
	copy :: !Offset *{#Char} -> *{#Char}
	copy off arr
		# char = load_char (ptr+off)
		| off < len	= copy (inc off) {arr & [off] = char}
					= arr

load_char :: !Pointer -> Char
load_char ptr = code inline {
		load_ui8 0
	}

writeCharArray :: !Pointer !{#Char} -> Pointer
writeCharArray ptr array = copy ptr 0
where
	len = size array

	copy :: !Pointer !Offset -> Pointer
	copy ptr off
		# char = array.[off]
		| off < len	= copy (writeChar ptr off char) (inc off)
					= ptr

packInt :: !Int -> {#Int}
packInt i = {i}

packString :: !String -> {#Char}
packString s = s +++ "\0"

unpackString :: !{#Char} -> String
unpackString s = unpack 0
where
	unpack :: Int -> String
	unpack off	| s.[off] == '\0' = s % (0, off - 1)
				| otherwise       = unpack (off + 1)

unpackInt16Z :: !{#Char} !Offset -> Int
unpackInt16Z s off
	= 		(toInt s.[off])
	bitor	(toInt s.[off + 1] << 8)

unpackInt16S :: !{#Char} !Offset -> Int
unpackInt16S s off = IF_INT_64_OR_32 (((unpackInt16Z s off) << 48) >> 48) (((unpackInt16Z s off) << 16) >> 16)

unpackInt32Z :: !{#Char} !Offset -> Int
unpackInt32Z s off
	= 		(toInt s.[off])
	bitor	(toInt s.[off + 1] << 8)
	bitor	(toInt s.[off + 2] << 16)
	bitor	(toInt s.[off + 3] << 24)

unpackInt32S :: !{#Char} !Offset -> Int
unpackInt32S s off = IF_INT_64_OR_32  (((unpackInt32Z s off) << 32) >> 32) (unpackInt32Z s off)

unpackInt64 :: !{#Char} !Offset -> Int
unpackInt64 s off
	= 		(toInt s.[off])
	bitor 	(toInt s.[off + 1] << 8)
	bitor 	(toInt s.[off + 2] << 16)
	bitor 	(toInt s.[off + 3] << 24)
	bitor 	(toInt s.[off + 4] << 32)
	bitor 	(toInt s.[off + 5] << 40)
	bitor 	(toInt s.[off + 6] << 48)
	bitor 	(toInt s.[off + 7] << 56)

unpackBool :: !{#Char} !Offset -> Bool
unpackBool s off = unpackInt32Z s off <> 0

forceEval :: !a !*env -> *env
forceEval _ world = world

forceEvalPointer :: !Pointer !*env -> *env
forceEvalPointer _ world = world

global_argc :: Pointer
global_argc = code {
	pushLc global_argc
}

global_argv :: Pointer
global_argv = code {
	pushLc global_argv
}
