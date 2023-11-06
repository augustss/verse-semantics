# Literals

'!'
0u00021
'"'
'	'    # This is a tab character
'ࠀ'
0o22
0x23
0b101
0
10
0.0
3.14
4.2e+1
4200e-2

# I belive integers with exponents are allowed
1e6

# Currently treating everything with a negative exponent as a float
10e-1


"hello world"
"This is not a {
}multi line string"
"You have { count } points"

# Some literals can have units

1.8mile
20m/s   # Not 20 m/s but "20m / s" where 's' is an identifier :-(

# Several kinds of identifiers

apa1
(monkey:)apa3
/apa2
/abc.def@whatever.com/apa4
/abc.def@whatever.com/(/here/be/path:)apa4
/abc.def@whatever.com/(/here/be/path:)apa4/(/here/be/path:)apa4/(/here/be/path:)apa4   # One long complicated path
/abc.def@whatever.com/(/here/be/path:)ap.a4                                            # This is a path '/abc.def@whatever.com/(/here/be/path:)ap', infix '.', and an identifier 'a4'
/abc/cde/fgh/i.k/l_n/o.q                                                               # /abc/cde/fgh/i . k / l_n / o . q
# /abc.def@whatever.com/(not_a_path:)ap.a4                                             # This a syntax error, paths must be qualified with paths

# Indentation differs between ShipVerse and ebnf

A:
  B:
  C

A{ B{ C }}  # ebnf

A{ B{}; C} # Shipverse

# This one breaks ebnf
#A:
# B:
#  C:
#   D:
# E

#expected
A { B { C { D } }; E}


# Comments and indentation
# ShpVerse accept blank lines, and comments that are indented at least as much as the rest of the block, without ending it.
# The grammar seem to break for blank lines.

A:
  B

  # Not more to the left
  C

A{ B ; C }

# Comment with less indentation breaks the block
A:
  B
 # Only B is part of A
  C

A{ B }; C


# Comments ends the prefix part of the line
A:
       B
       <# Confusing but will work #> C
 <# #> D   # D is only indented one space
       E

# is the same as
A { B; C }; D ; E

# Does C belong to A or not? It does according to ebnf
A:
    B
    <# block comment
#>C

# Indentation comments do end with blank lines. Maybe they shouldn't

A:
  <#>
    Some comment
    That ends soon

    B # Will be inside A



# The equal sign (=) is complicated
# Equals that are parsed by Def starts indentation (and binds weaker than "or"),
# other equals disables indentation if they are last on the line (comments doesn't count) (and they bind stronger than "or", and weaker than <>)

# Not ok
#A:
#  B:X =
# C

# Ok
A:
  B:X =
  C
  D

# ebnf parse it as
A { B:X = {C; D} }

# But I think it ought to be either an error or
A { B:X = {}; C; D }

# Not Ok
#A:
#  B:X = C =
# D

# Ok
A:
  B:X = C =
  D
  E

A { B:X = (C=D); E }


# Some other unepected things to parse

A or B : X = C

((A or B):X) = C


A:
  B =
  C
    D

A { B {} }; C; D;

A:
  B = C =
  D
    E

A { B = (C = D);  E }


# These two are quivalent

X = 1

X =
  1



# Which comnbinations are ok?

# Must var have type?
var X:int := 1
var X:int = 1

# Can set have type?
set X = 1
set X += 1
set X -= 1
set X *= 1
set X /= 1

# How are ref and alias used?
ref X:int := 1
ref X:int = 1

alias X:int := Y
alias X:int = Y


# These are in the grammar

@editable
X:int = 0

@editable @public X:int = 0

# .. and this one is in the existing verse code

@editable; X:int = 0


return
return 1
break
# break 1    Not allowed
yield
# yield 1    Not allowed
continue
# continue 1    Not allowed


# Accessing a field on an object
xxx.c
xxx .c
xxx .<# the comment prevent's the '.' from seeing the ' ' before 'c' #> c


# Instansiating a macro
xxx . c
xxx. c


## Extensions fields

x.field
0.field
1.field
2.field

# The enum macro is special since it allows ',' in addition to ';' between its subexpressions.

X := enum { A; B, C }

Y := enum:
  A, B
  C; D

Z := enum:
  A
  B
  C

# Attributes are also allowed
# These are used in code and therefore allowed.

Z := enum<whatever>:
  A
  @deprecated B
  @deprecated
  C

# I guess this one shold also be allowed
#Z := enum:
#  A<whatever>