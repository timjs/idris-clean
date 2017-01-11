definition module StdInteger

import StdOverloaded

:: Integer = {integer_s ::!Int, integer_a ::!.{#Int}};

instance +				Integer
instance *  			Integer
instance zero			Integer
instance one			Integer

instance ~				Integer
instance -  			Integer

instance abs			Integer
instance sign			Integer

instance /				Integer
instance rem            Integer

//instance ^				Integer

instance ==				Integer
instance <  			Integer

class toInteger a :: !a -> Integer

instance toChar Integer
instance toInt Integer
//instance toReal Integer
instance toString Integer

instance toInteger Char
instance toInteger Int
//instance toInteger Real
instance toInteger String

/*
integer_a_to_double :: !Int !{#Int} -> Real
*/
