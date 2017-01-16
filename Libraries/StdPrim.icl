// TODO
// - consitent argument names
// - ABC implementations of clean funcs
implementation module StdPrim

//FIXME remove this import
import _SystemArray
import _SystemEnum
from StdList import reverse

unimplemented :== clean_Misc_abort "StdPrim: unimplemented"

/// # Miscellaneous

clean_Misc_abort :: !String -> .a
clean_Misc_abort s = code inline {
    .d 1 0
        jsr print_string_
    .o 0 0
        halt
}

clean_Misc_noop :: .a
clean_Misc_noop = code inline {
    no_op
}

clean_Misc_id :: !.a -> .a
clean_Misc_id x = code inline {
    pop_a 0
}

clean_Misc_coerce :: !.a -> .b
clean_Misc_coerce x = code inline {
    pop_a 0
}

clean_Misc_force :: !.a -> .a
clean_Misc_force x
    #! y = x
    = y

clean_Misc_forceEval :: !.a !*World -> *World
clean_Misc_forceEval a w = w

/// # Booleans

/// ## Ordering

clean_Bool_eq :: !Bool !Bool -> Bool
clean_Bool_eq a b = code inline {
    eqB
}

/// ## Literals

clean_Bool_true :: Bool
clean_Bool_true = code inline {
    pushB TRUE
}

clean_Bool_false :: Bool
clean_Bool_false = code inline {
    pushB FALSE
}

/// ## Logic

clean_Bool_and :: !Bool Bool -> Bool
clean_Bool_and a b = code {
        push_b 0
        jmp_false l1
        pop_b 1
        jsr_eval 0
        pushB_a 0
        pop_a 1
    .d 0 1 b
        rtn
    :l1
        pop_a 1
    .d 0 1 b
        rtn
}

clean_Bool_or :: !Bool Bool -> Bool
clean_Bool_or a b = code {
        push_b 0
        jmp_true l2
        pop_b 1
        jsr_eval 0
        pushB_a 0
        pop_a 1
    .d 0 1 b
        rtn
    :l2
        pop_a 1
    .d 0 1 b
        rtn
}

clean_Bool_not :: !Bool -> Bool
clean_Bool_not b = code inline {
    notB
}

/// # Characters

/// ## Ordering

clean_Char_eq :: !Char !Char -> Bool
clean_Char_eq a b = code inline {
    eqC
}

clean_Char_lt :: !Char !Char -> Bool
clean_Char_lt a b = code inline {
    ltC
}

/// ## Casing

clean_Char_setLowercaseBit :: !Char -> Char
clean_Char_setLowercaseBit c = code inline {
    pushI 32
    or%
}

clean_Char_unsetLowercaseBit :: !Char -> Char
clean_Char_unsetLowercaseBit c = code inline {
    pushI 223
    and%
}

/// ## Arithmetic

clean_Char_add :: !Char !Char -> Char
clean_Char_add a b = code inline {
    addI
    ItoC
}

clean_Char_sub :: !Char !Char -> Char
clean_Char_sub a b = code inline {
    subI
    ItoC
}

/// # Integers

/// ## Values

clean_Int_zero :: Int
clean_Int_zero = code inline {
    pushI 0
}

clean_Int_one :: Int
clean_Int_one = code inline {
    pushI 1
}

clean_Int_upper :: Int
clean_Int_upper = unimplemented

clean_Int_lower :: Int
clean_Int_lower = unimplemented

/// ## Ordering

clean_Int_eq :: !Int !Int -> Bool
clean_Int_eq a b = code inline {
    eqI
}

clean_Int_lt :: !Int !Int -> Bool
clean_Int_lt a b = code inline {
    ltI
}

/// ## Operations

clean_Int_inc :: !Int -> Int
clean_Int_inc a = code inline {
    incI
}

clean_Int_dec :: !Int -> Int
clean_Int_dec a = code inline {
    decI
}

clean_Int_min :: !Int !Int -> Int
clean_Int_min a b = unimplemented // code inline {
    // minI
// }

clean_Int_max :: !Int !Int -> Int
clean_Int_max a b = unimplemented // code inline {
    // maxI
// }

/// ## Arithmetic

clean_Int_neg :: !Int -> Int
clean_Int_neg a = code inline {
    negI
}

clean_Int_add :: !Int !Int -> Int
clean_Int_add a b = code inline {
    addI
}

clean_Int_sub :: !Int !Int -> Int
clean_Int_sub a b = code inline {
    subI
}

clean_Int_mul :: !Int !Int -> Int
clean_Int_mul a b = code inline {
    mulI
}

/// ## Integer Arithmetic

clean_Int_quot :: !Int !Int -> Int
clean_Int_quot a b = code inline {
    divI
}

clean_Int_rem :: !Int !Int -> Int
clean_Int_rem a b = code inline {
    remI
}

clean_Int_div :: !Int !Int -> Int
clean_Int_div a b = unimplemented
// clean_Int_div a b = code inline {
//     floordivI
// }

clean_Int_mod :: !Int !Int -> Int
clean_Int_mod a b = unimplemented
// clean_Int_mod a b = code inline {
//     modI
// }

clean_Int_quotRem :: !Int !Int -> (!Int,!Int)
clean_Int_quotRem a b = code inline {
    push_b 1
    push_b 1
    divI
    push_b 2
    push_b 1
    mulI
    push_b 2
    subI
    update_b 0 3
    update_b 1 2
    pop_b 2
}

clean_Int_divMod :: !Int !Int -> (!Int,!Int)
clean_Int_divMod a b = unimplemented
// clean_Int_divMod a b = code inline {
//     push_b 1
//     push_b 1
//     floordivI
//     push_b 2
//     push_b 1
//     mulI
//     push_b 2
//     subI
//     update_b 0 3
//     update_b 1 2
//     pop_b 2
// }

/// ## Tests

clean_Int_isEven :: !Int -> Bool
clean_Int_isEven a = code inline {
    pushI 1
    and%
    pushI 0
    eqI
}

clean_Int_isOdd :: !Int -> Bool
clean_Int_isOdd a = code inline {
    pushI 1
    and%
    pushI 0
    eqI
    notB
}

/// ## Logic

clean_Int_and :: !Int !Int -> Int
clean_Int_and a b = code inline {
    and%
}

clean_Int_or :: !Int !Int -> Int
clean_Int_or a b = code inline {
    or%
}

clean_Int_xor :: !Int !Int -> Int
clean_Int_xor a b = code inline {
    xor%
}

clean_Int_not :: !Int -> Int
clean_Int_not a = code inline {
    not%
}

clean_Int_shl :: !Int !Int -> Int
clean_Int_shl a b = code inline {
    shiftl%
}

clean_Int_shr :: !Int !Int -> Int
clean_Int_shr a b = code inline {
    shiftr%
}

/// # Reals

/// ## Ordering

clean_Real_eq :: !Real !Real -> Bool
clean_Real_eq a b = code inline {
    eqR
}

clean_Real_lt :: !Real !Real -> Bool
clean_Real_lt a b = code inline {
    ltR
}

/// ## Basic Values

clean_Real_zero :: Real
clean_Real_zero = code inline {
    pushR 0.0
}

clean_Real_one :: Real
clean_Real_one = code inline {
    pushR 1.0
}

clean_Real_pi :: Real
clean_Real_pi = code inline {
    pushR 3.141592653589793238
}

clean_Real_e :: Real
clean_Real_e = code inline {
    pushR 2.718281828459045235
}

/// ## Arithmetic

clean_Real_neg :: !Real -> Real
clean_Real_neg a = code inline {
    negR
}

clean_Real_add :: !Real !Real -> Real
clean_Real_add a b = code inline {
    addR
}

clean_Real_sub :: !Real !Real -> Real
clean_Real_sub a b = code inline {
    subR
}

clean_Real_mul :: !Real !Real -> Real
clean_Real_mul a b = code inline {
    mulR
}

clean_Real_div :: !Real !Real -> Real
clean_Real_div a b = code inline {
    divR
}

clean_Real_pow :: !Real !Real -> Real
clean_Real_pow a b = code inline {
    powR
}

clean_Real_abs :: !Real -> Real
clean_Real_abs a = code inline {
    absR
}

/// ## Rounded

clean_Real_round :: !Real -> Int
clean_Real_round r = code inline {
    RtoI
}

clean_Real_floor :: !Real -> Int
clean_Real_floor r = code inline {
    entierR
}

////TODO Stable?
//clean_Real_ceil :: !Real -> Int
//clean_Real_ceil r = code inline {
//    ceilingR
//}

////TODO Stable?
//clean_Real_truncate :: !Real -> Int
//clean_Real_truncate r = code inline {
//    truncateR
//}

/// ## Algebraic

clean_Real_log :: !Real -> Real
clean_Real_log x = code inline {
    lnR
}

clean_Real_log10 :: !Real -> Real
clean_Real_log10 x = code inline {
    log10R
}

clean_Real_exp :: !Real -> Real
clean_Real_exp x = code inline {
    expR
}

clean_Real_sqrt :: !Real -> Real
clean_Real_sqrt x = code inline {
    sqrtR
}

/// ## Trigoniometric

clean_Real_sin :: !Real -> Real
clean_Real_sin x = code inline {
    sinR
}

clean_Real_cos :: !Real -> Real
clean_Real_cos x = code inline {
    cosR
}

clean_Real_tan :: !Real -> Real
clean_Real_tan x = code inline {
    tanR
}

clean_Real_asin :: !Real -> Real
clean_Real_asin x = code inline {
    asinR
}

clean_Real_acos :: !Real -> Real
clean_Real_acos x = code inline {
    acosR
}

clean_Real_atan :: !Real -> Real
clean_Real_atan x = code inline {
    atanR
}

/// # Strings

/// ## Comparing

clean_String_eq :: !String !String -> Bool
clean_String_eq a b = code inline {
    .d 2 0
        jsr eqAC
    .o 0 1 b
}

clean_String_lt :: !String !String -> Bool
clean_String_lt a b = code inline {
    .d 2 0
        jsr cmpAC
    .o 0 1 i
        pushI 0
        gtI
}

/// ## Basic Values

//FIXME use create_array
clean_String_empty :: String
clean_String_empty = ""

/// ## Properties

clean_String_len :: !String -> Int
clean_String_len s= code inline {
    push_arraysize CHAR 0 1
}

/// ## Constructing

//FIXME order of char and string ok before concat?
clean_String_cons :: !Char !String -> String
// clean_String_cons chr str = clean_String_concat (clean_Char_toString chr) str
clean_String_cons c s = code inline {
        CtoAC
    .d 2 0
        jsr catAC
    .o 1 0
}

clean_String_head :: !String -> Char
clean_String_head str = clean_String_index 0 str

clean_String_tail :: !String -> String
clean_String_tail str = clean_String_slice 1 (clean_Int_dec (clean_String_len str)) str

clean_String_index :: !Int !String -> Char
clean_String_index idx str = code inline {
    select CHAR 0 1
}

//FIXME change from tuple to two args, does this work?
clean_String_slice :: !Int !Int !String -> String
clean_String_slice fr to str = code inline {
    .d 1 2 ii
        jsr sliceAC
    .o 1 0
}

clean_String_substring :: !Int !Int !String -> String
clean_String_substring ofs len str = clean_String_slice ofs (clean_Int_dec (clean_Int_add ofs len)) str

clean_String_concat :: !String !String -> String
clean_String_concat a b = code inline {
    .d 2 0
        jsr catAC
    .o 1 0
}

clean_String_reverse :: !String -> String
clean_String_reverse str = { str.[i] \\ i <- reverse [0 .. clean_Int_dec (clean_String_len str)] }

/// # Files

/// ## Opening and Closing

/// Opens a file for the first time in a certain mode (read, write or append, text or data).
/// The boolean output parameter reports success or failure.
clean_File_open :: !String !Int -> (!Bool,!*File)
clean_File_open s i = code inline {
    .d 1 1 i
        jsr openF
    .o 0 3 b f
}

/// Closes a file.
/// The boolean output parameter reports whether the file was successfully closed.
clean_File_close :: !*File -> Bool
clean_File_close f = code inline {
    .d 0 2 f
        jsr closeF
    .o 0 1 b
}

/// Re-opens an open file in a possibly different mode.
/// The boolean indicates whether the file was successfully closed before reopening.
clean_File_reopen :: !*File !Int -> (!Bool,!*File)
clean_File_reopen f m = code inline {
    .d 0 3 f i
        jsr reopenF
    .o 0 3 b f
}

/// ## Standard IO

/// Open the 'Console' for reading and writing.
clean_File_stdio :: *File
clean_File_stdio = code inline {
    .d 0 0
        jsr stdioF
    .o 0 2 f
}

/// Open the 'Errors' file for writing only. May be opened more than once.
clean_File_stderr :: *File
clean_File_stderr = code inline {
    .d 0 0
        jsr stderrF
    .o 0 2 f
}

/// ## Seeking

/// Returns the current position of the file pointer as an integer.
/// This position can be used later on for the fseek function.
clean_File_position :: !*File -> (!Int,!*File)
clean_File_position f = code inline {
    .d 0 2 f
        jsr positionF
    .o 0 3 i f
}

/// Move to a different position in the file, the first integer argument is the offset,
/// the second argument is a seek mode. (see above). True is returned if successful.
clean_File_seek :: !*File !Int !Int -> (!Bool,!*File)
clean_File_seek f p m = code inline {
    .d 0 4 f i i
        jsr seekF
    .o 0 3 b f
}

/// ## Tests

/// Tests for end-of-file.
clean_File_isEnd :: !*File -> (!Bool,!*File)
clean_File_isEnd f = code inline {
    .d 0 2 f
        jsr endF
    .o 0 3 b f
}

/// Has an error occurred during previous file I/O operations?
clean_File_isError :: !*File -> (!Bool,!*File)
clean_File_isError f = code inline {
    .d 0 2 f
        jsr errorF
    .o 0 3 b f
}

/// ## Reading

/// Reads a character from a text file or a byte from a datafile.
clean_File_readChar :: !*File -> (!Bool,!Char,!*File)
clean_File_readChar f = code inline {
    .d 0 2 f
        jsr readFC
    .o 0 4 b c f
}

/// Reads an integer from a textfile by skipping spaces, tabs and newlines and
/// then reading digits, which may be preceeded by a plus or minus sign.
/// From a datafile freadi will just read four bytes (a Clean Int).
clean_File_readInt :: !*File -> (!Bool,!Int,!*File)
clean_File_readInt f = code inline {
    .d 0 2 f
        jsr readFI
    .o 0 4 b i f
}

/// Reads a real from a textfile by skipping spaces, tabs and newlines and then
/// reading a character representation of a real number.
/// From a datafile freadr will just read eight bytes (a Clean Real).
clean_File_readReal::!*File -> (!Bool,!Real,!*File)
clean_File_readReal f = code inline {
    .d 0 2 f
        jsr readFR
    .o 0 5 b r f
}

/// Reads n characters from a text or data file, which are returned as a String.
/// If the file doesn't contain n characters the file will be read to the end
/// of the file. An empty String is returned if no characters can be read.
clean_File_readString :: !*File !Int -> (!*String,!*File)
clean_File_readString f l = code inline {
    .d 0 3 f i
        jsr readFS
    .o 1 2 f
}

/// Reads a line from a textfile. (including a newline character, except for the last
/// line) freadline cannot be used on data files.
clean_File_readLine :: !*File -> (!*String,!*File)
clean_File_readLine f = code inline {
    .d 0 2 f
        jsr readLineF
    .o 1 2 f
}

/// ## Writing

/// Writes a character to a textfile.
/// To a datafile fwritec writes one byte (a Clean Char).
clean_File_writeChar :: !Char !*File -> *File
clean_File_writeChar c f = code inline {
    .d 0 3 c f
        jsr writeFC
    .o 0 2 f
}

/// Writes an integer (its textual representation) to a text file.
/// To a datafile fwritec writes four bytes (a Clean Int).
clean_File_writeInt :: !Int !*File -> *File
clean_File_writeInt i f = code inline {
    .d 0 3 i f
        jsr writeFI
    .o 0 2 f
}

/// Writes a real (its textual representation) to a text file.
/// To a datafile fwriter writes eight bytes (a Clean Real).
clean_File_writeReal :: !Real !*File -> *File
clean_File_writeReal r f = code inline {
    .d 0 4 r f
        jsr writeFR
    .o 0 2 f
}

/// Writes a String to a text or data file.
clean_File_writeString :: !String !*File -> *File
clean_File_writeString s f = code inline {
    .d 1 2 f
        jsr writeFS
    .o 0 2 f
}

/// # World

clean_World_make :: *World
clean_World_make = code inline {
    fillI 65536 0
}

/// # Conversions

/// ## Booleans

clean_Bool_toString :: !Bool -> String
clean_Bool_toString b = code inline {
    .d 0 1 b
        jsr BtoAC
    .o 1 0
}

/// ## Characters

clean_Char_toInt :: !Char -> Int
clean_Char_toInt c = code inline {
    CtoI
}

clean_Char_toString :: !Char -> String
clean_Char_toString c = code inline {
    CtoAC
}

/// ## Integers

clean_Int_toChar :: !Int -> Char
clean_Int_toChar i = code inline {
    ItoC
}

clean_Int_toReal :: !Int -> Real
clean_Int_toReal i = code inline {
    ItoR
}

clean_Int_toString :: !Int -> String
clean_Int_toString i = code inline {
    .d 0 1 i
        jsr ItoAC
    .o 1 0
}

/// ## Reals

clean_Real_toInt :: !Real -> Int
clean_Real_toInt x = code inline {
    RtoI
}

clean_Real_toString :: !Real -> String
clean_Real_toString x = unimplemented /*code inline {
    .d 0 2 r
        jsr RtoAC
    .o 1 0
}*/

/// ## Strings

clean_String_toInt :: !String -> Int
clean_String_toInt s = unimplemented

clean_String_toReal :: !String -> Real
clean_String_toReal s = unimplemented

/*
clean_String_toInt :: !String -> Int
clean_String_toInt s
    | len==0 = 0
    | neg = ~signedval
    | pos = signedval
    | otherwise = other
where
    len = size s
    neg = s.[0] == '-'
    pos = s.[0] == '+'
    signedval = go 1 0 s
    other = go 0 0 s

    go :: !Int !Int !String -> Int
    go posn val s
        | len==posn = val
        | isDigit = go (posn+1) (n+val*10) s
        | otherwise = 0
    where
        n = toInt s.[posn] - toInt '0'
        isDigit = 0<=n && n<= 9

clean_String_toReal :: !String -> Real
clean_String_toReal s
    | len == 0 = 0.0
    | first == '-' = ~signedval
    | first == '+' = signedval
    | otherwise = val
where
    len = size s
    signedval = go s 1 0.0 False 1.0 False 0 0
    val = go s 0 0.0 False 1.0 False 0 0
    first = s.[0]

    go :: !String !Int !Real !Bool !Real !Bool !Int !Int -> Real
    go s posn val dec_new dval exp eneg eval
        | posn == len = val*dval*10.0 ^ toReal (eneg*eval)
        | isDigit && not dec_new && not exp = go s (posn+1) (toReal n + 10.0*val) dec_new dval exp eneg eval
        | isDigit && dec_new && not exp = go s (posn+1) (toReal n + 10.0*val) dec_new (dval/10.0) exp eneg eval
        | isDigit && exp = go s (posn+1) val dec_new dval exp eneg (n + 10*eval )
        | not dec_new && not exp && c == '.' = go s (posn+1) val True 1.0 exp eneg eval
        | not exp && (c== 'e' || c== 'E')
            | posn<len-2 && s.[posn+1] == '-' = go s (posn+2) val dec_new dval True (-1) 0
            | posn<len-2 && s.[posn+1] == '+' = go s (posn+2) val dec_new dval True (+1) 0
            | posn<len-1 = go s (posn+1) val dec_new dval True 1 0
            | otherwise = 0.0
        | otherwise = 0.0
    where
        c = s.[posn]
        n = toInt c - toInt '0'
        isDigit = 0<=n && n<=9
*/
