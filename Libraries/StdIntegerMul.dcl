definition module StdIntegerMul;

from StdInteger import ::Integer;

mul_integer :: !Integer !Integer -> *Integer;

mul_integer_a :: !{#Int} !{#Int} -> *{#Int};
