definition module StdIntegerDiv;

from StdInteger import ::Integer;

div_integer :: !Integer !Integer -> *Integer;
rem_integer :: !Integer !Integer -> *Integer;
div_rem_integer :: !Integer !Integer -> (!*Integer,!*Integer);

floordiv_integer :: !Integer !Integer -> *Integer;
mod_integer :: !Integer !Integer -> *Integer;
floordiv_mod_integer :: !Integer !Integer -> (!*Integer,!*Integer);

div_integer_a :: !*{#Int} !{#Int} -> *{#Int};

div_rem_integer_a :: !*{#Int} !{#Int} -> (!*{#Int},!*Integer);
