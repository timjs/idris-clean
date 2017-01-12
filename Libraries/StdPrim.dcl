system module StdPrim

// TODO
// - add primitives for gt, le, ge, ne
// - add primitives for arrays
// - add primitives for files

/// # Miscellaneous

clean_Misc_abort :: !String -> .a
clean_Misc_noop :: .a
clean_Misc_id :: .a -> .a
clean_Misc_coerce :: .a -> .b
clean_Misc_force :: !.a -> .a

/// # Booleans

clean_Bool_eq :: !Bool !Bool -> Bool

clean_Bool_true :: Bool
clean_Bool_false :: Bool

clean_Bool_and :: !Bool Bool -> Bool
clean_Bool_or :: !Bool Bool -> Bool
clean_Bool_not :: !Bool -> Bool

/// # Characters

clean_Char_eq :: !Char !Char -> Bool
clean_Char_lt :: !Char !Char -> Bool
// clean_Char_gt :: !Char !Char -> Bool
// clean_Char_min :: !Char !Char -> Char
// clean_Char_max :: !Char !Char-> Char

clean_Char_setLowercaseBit :: !Char -> Char
clean_Char_unsetLowercaseBit :: !Char -> Char

/// # Integers

clean_Int_zero :: Int
clean_Int_one :: Int
clean_Int_upper :: Int
clean_Int_lower :: Int

clean_Int_eq :: !Int !Int -> Bool
clean_Int_lt :: !Int !Int -> Bool
// clean_Int_gt :: !Int !Int -> Bool

clean_Int_inc :: !Int -> Int
clean_Int_dec :: !Int -> Int
clean_Int_min :: !Int !Int -> Int
clean_Int_max :: !Int !Int-> Int

clean_Int_neg :: !Int -> Int
clean_Int_add :: !Int !Int -> Int
clean_Int_sub :: !Int !Int -> Int
clean_Int_mul :: !Int !Int -> Int

clean_Int_quot :: !Int !Int -> Int
clean_Int_rem :: !Int !Int -> Int
clean_Int_div :: !Int !Int -> Int
clean_Int_mod :: !Int !Int -> Int
clean_Int_quotRem :: !Int !Int -> (!Int,!Int)
clean_Int_divMod :: !Int !Int -> (!Int,!Int)

clean_Int_isEven :: !Int -> Bool
clean_Int_isOdd :: !Int -> Bool

clean_Int_and :: !Int !Int -> Int
clean_Int_or :: !Int !Int -> Int
clean_Int_xor :: !Int !Int -> Int
clean_Int_not :: !Int -> Int
clean_Int_shl :: !Int !Int -> Int
clean_Int_shr :: !Int !Int -> Int

/// # Reals

clean_Real_eq :: !Real !Real -> Bool
clean_Real_lt :: !Real !Real -> Bool
// clean_Real_gt :: !Real !Real -> Bool
// clean_Real_min :: !Real !Real -> Real
// clean_Real_max :: !Real !Real -> Real

clean_Real_zero :: Real
clean_Real_one :: Real
clean_Real_pi :: Real
clean_Real_e :: Real

clean_Real_neg :: !Real -> Real
clean_Real_add :: !Real !Real -> Real
clean_Real_sub :: !Real !Real -> Real
clean_Real_mul :: !Real !Real -> Real
clean_Real_div :: !Real !Real -> Real
clean_Real_pow :: !Real !Real -> Real
clean_Real_abs :: !Real -> Real

clean_Real_floor :: !Real -> Int
// clean_Real_ceil :: !Real -> Int
// clean_Real_truncate :: !Real -> Int

clean_Real_log :: !Real -> Real
clean_Real_log10 :: !Real -> Real
clean_Real_exp :: !Real -> Real
clean_Real_sqrt :: !Real -> Real

clean_Real_sin :: !Real -> Real
clean_Real_cos :: !Real -> Real
clean_Real_tan :: !Real -> Real
clean_Real_asin :: !Real -> Real
clean_Real_acos :: !Real -> Real
clean_Real_atan :: !Real -> Real

/// # Strings

clean_String_eq :: !String !String -> Bool
clean_String_lt :: !String !String -> Bool

clean_String_empty :: String

clean_String_len :: !String -> Int

clean_String_cons :: !Char !String -> String
clean_String_head :: !String -> Char
clean_String_tail :: !String -> String

clean_String_index :: !Int !String -> Char
clean_String_slice :: !Int !Int !String -> String
clean_String_substring :: !Int !Int !String -> String

clean_String_concat :: !String !String -> String
clean_String_reverse :: !String -> String

/// # Files

clean_File_readTextMode   :== 0 /// Read from a text file
clean_File_writeTextMode  :== 1 /// Write to a text file
clean_File_appendTextMode :== 2 /// Append to an existing text file
clean_File_readDataMode   :== 3 /// Read from a data file
clean_File_writeDataMode  :== 4 /// Write to a data file
clean_File_appendDataMode :== 5 /// Append to an existing data file

clean_File_absoluteSeekMode :== 0 /// New position is the seek offset
clean_File_relativeSeekMode :== 1 /// New position is the current position plus the seek offset
clean_File_fromEndSeekMode  :== 2 /// New position is the size of the file plus the seek offset

clean_File_open :: !String !Int -> (!Bool,!*File)
clean_File_close :: !*File -> Bool
clean_File_reopen :: !*File !Int -> (!Bool,!*File)

clean_File_stdio :: *File
clean_File_stderr :: *File

clean_File_position :: !*File -> (!Int,!*File)
clean_File_seek :: !*File !Int !Int -> (!Bool,!*File)

clean_File_isEnd :: !*File -> (!Bool,!*File)
clean_File_isError :: !*File -> (!Bool,!*File)

clean_File_readChar :: !*File -> (!Bool,!Char,!*File)
clean_File_readInt :: !*File -> (!Bool,!Int,!*File)
// clean_File_readReal :: !*File -> (!Bool,!Real,!*File)
clean_File_readString :: !*File !Int -> (!*String,!*File)
clean_File_readLine :: !*File -> (!*String,!*File)

clean_File_writeChar :: !Char !*File -> *File
clean_File_writeInt :: !Int !*File -> *File
// clean_File_writeReal :: !Real !*File -> *File
clean_File_writeString :: !String !*File -> *File

/// # Conversions

clean_Bool_toString :: !Bool -> String

clean_Char_toInt :: !Char -> Int
clean_Char_toString :: !Char -> String

clean_Int_toChar :: !Int -> Char
clean_Int_toReal :: !Int -> Real
clean_Int_toString :: !Int -> String

clean_Real_toInt :: !Real -> Int
// clean_real_toString :: !Real -> String

clean_String_toInt :: !String -> Int
