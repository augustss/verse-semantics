# Operators

Operators are special functions defined in the Verse programming language to perform actions such as the math operations for addition and multiplication.
Operators are special functions defined in the Verse programming language to perform actions such as math operations on their operands. For example, in the expression 1 + 2, the + is an operator, and 1 and 2 are both operands.
There are three formats for operators that you’ll see in Verse:
Prefix: There is only one operand and the operator is before the operand.
Infix: There are two operands and the operator is between the operands.
Postfix: There is only one operand and the operator is after the operand.
This page describes all the operators you can use in Verse, how they work, and their order of evaluation when used in combination with other operators.
List of All Operators and Operator Precedence
When multiple operators are used in the same expression, they are evaluated in the order of highest to lowest precedence. The table below lists all built-in operators in Verse and their precedence.
Name Operator Description Operator Format Operator Precedence Example

### Query

The ? operator checks if a logic value is true. See Query for more details.

Postfix
9

BossDefeated?
Not
not

The not operator negates the success or failure of an expression. See Not for more details.
Prefix
8

not BossDefeated?
Positive
+

You can use the + operator as a prefix to a number to help align your code visually, but it won't change the value of the number. See Math for more details.
Prefix
8

```
+MyScore
Negative
-
```

You can use the operator - as a prefix to a number to negate the number value. See Math for more details.

Prefix
8

-MyScore

Multiplication

The * multiplies two number values together. See Math for more details.

Infix
7

MyScore * ScoreMultiplier

Division

/

The / operator divides the first number operand by the second number operand. Integer division is failable. See
Math for more details.

Infix

7

MyScore / ScorePenalty

Addition

*

The + operator adds two number values together. When used with strings and arrays, the two values are
concatenated. See Math for more details.

Infix

6

MyScore + ScoreBonus

Subtraction

*

The - operator subtracts the second number operand from the first operand. See Math for more details.

Infix

6

MyScore - ScorePenalty

Addition assignment

set +=

With this operator, you can combine addition and assignment in the same operation to update a variable's value.

See Math for more details.

Infix

5
set MyScore += ScoreBonus

Subtraction assignment

set -=

With this operator, you can combine subtraction and assignment in the same operation to update a variable's value. See Math for more details.

Infix

5

set MyScore -= ScorePenalty

Multiplication assignment

set *=

With this operator, you can combine multiplication and assignment in the same operation to update a variable's
value. See Math for more details.

Infix

5

set MyScore*= ScoreMultiplier

Division assignment

set /=

With this operator, you can combine division and assignment in the same operation to update a variable's value,

unless the variable is an integer. See Math for more details.

Infix

5

set MyScore /= ScorePenalty

Equal to

=

The = operator succeeds when the left operand is equal to the right operand. Fails otherwise. See Comparison for more details.

Infix

4
MyScore = HighScore

Not equal to
<>

The <> operator succeeds when the left operand is not equal to the right operand. Fails otherwise. See
Comparison for more details.
Infix

4

MyScore <> HighScore

Less than

<

The < operator succeeds when the left operand is less than the right operand. Fails otherwise. See Comparison
for more details.

Infix

4

MyScore < HighScore
Less than or equal to

<=

The <= operator succeeds when the left operand is less than or equal to the right operand. Fails otherwise. See Comparison for more details.

Infix

4
MyScore <= HighScore

Greater than

>

The > operator succeeds when the left operand is greater than the right operand. Fails otherwise. See Comparison

for more details.
Infix
4
MyScore > HighScore

Greater than or equal to

>=

The >= operator succeeds when the left operand is greater than or equal to the right operand. Fails otherwise.
See Comparison for more details.

Infix

4

MyScore >= HighScore

And

and

The and operator succeeds only when all the operands succeed. See And / Or Operators for more details.

Infix

3

BossDefeated? and TargetScoreReached?

Or

or

The or operator succeeds if at least one of the operands succeeds. See And / Or Operators for more details.

Infix

2

BossDefeated? or TargetScoreReached?

Variable and constant initialization

: =

With this operator, you can store values in a constant or variable. See Constants and Variables for more details.

Infix

1

MyScore : int = 42

Variable assignment

set =

With this operator, you can update the values stored in a variable. See Constants and Variables for more details.

Infix

1

set MyScore = 42

If there are operators with the same precedence in the same expression, then they are evaluated left to right.

For example in the expression 3*2/4, both operators * and / have the same precedence, so 3*2 is evaluated first
and its result becomes the left operand for the / operator.

You can change the order in which operators are evaluated by grouping expressions with (). For example, (1+2)*3
and 1+(2*3) don't evaluate to the same result. See Grouping for more details.

### Comparison

You can control the success and failure flow with comparison expressions, which use the inequality and equality operators. Comparison expressions are failable, so you can only use comparison operators in failure contexts, such as in if expressions.

The table below describes each operator and what types it supports. All comparison operators use the infix
format.

Operator Supported BUilt-In Types Description

<

float

int

The < operator succeeds when the left operand is less than the right operand. Fails otherwise.
<=

float
int

The <= operator succeeds when the left operand is less than or equal to the right operand. Fails otherwise.

>

float

int

The > operator succeeds when the left operand is greater than the right operand. Fails otherwise.

>=
float

int

The >= operator succeeds when the left operand is greater than or equal to the right operand. Fails otherwise.

<>
float

int

logic

string
enum
The <> operator succeeds when the left operand is not equal to the right operand. Fails otherwise.

=

float
int

logic

string
enum

The = operator succeeds when the left operand is equal to the right operand. Fails otherwise.

Both <>and = are also supported for array, map, tuple, and class instances, but with restrictions. The array,
map, and tuple instances can only contain supported types, and class instances are only supported if they contain at least one var member.

Decision

You can control the success and failure flow with decision expressions, which use the operators not, and, and or. Decision expressions are failable, so you can only use comparison operators in failure contexts, such as in
if expressions. You can use any expressions that succeed or fail with decision operators.

Not Operator

The decision operator not negates the success or failure of an expression. The not operator uses the prefix format.

For example, when expression fails, not expression will succeed. When expression succeeds, not expression will
fail and the effects of expression are never committed (as if the expression never happened).
For example, after the following code is executed, Example will still have the initial value 0:

```verse
var Example : int = 0
if (not (set Example = ExampleArray[0])) { … }
```

You can use not not expression as a way to check if an expression will succeed but make it so the expression never happens.

Outcome of the Expression p Outcome of the Expression not p Outcome of the Expression not not p

Succeeds and the result is p

The expression fails, and the effects of p are not committed. The result of the expression is no value.

The expression succeeds, but the effects of p are not committed. The result of the expression is true.

Fails and the result is no value

The expression succeeds. The result of the expression is true.

The expression fails. The result of the expression is no value.

Reference for the not operator evaluating an expression, represented by p.

And / Or Operators

The decision operator and uses the infix format and is a failable expression that succeeds if both operands
succeed, or fails if at least one operand fails.

The decision operator or uses the infix format and is:

A failure context for the first operand.

A failable expression only if the second operand is failable.

The or operator skips evaluation of the second operand if the first operand succeeds.

The table below describes the results of all the operand combinations of success and failure for decision
expressions using the operators and and or.

Outcome of the Expression p Outcome of the Expression q Outcome of the Expression p and q Outcome of the

Expression p or q

Succeeds and result is p

Succeeds and the result is q
The expression succeeds, so the effects of both p and q are committed. The result of the expression is q.
The expression succeeds, and only the effects of p are committed. q is not executed because p succeeded. The
result of the expression is p.

Succeeds and the result is p
Fails and the result is no value

The expression fails, and the effects of both p and q are not committed. The result is no value.
The expression succeeds, and only the effects of p are committed. q is not executed because p succeeded. The
result of the expression is p.
Fails and the result is no value

Succeeds and the result is q

The expression fails, and the effects of both p and q are not committed. The result is no value.
The expression succeeds, and only the effects of q are committed. The result of the expression is q.

Fails and the result is no value

The expression fails, and the effects of both p and q are not committed. The result is no value.
The expression fails, and the effects of both p and q are not committed. The result of the expression is no value.

Reference for the and and or operators evaluating expressions, represented by p and q.

#### Math

With math expressions, you can do the four basic math operations (addition, subtraction, multiplication, and
division) with number values, and add strings together. All the operators use the infix format, except + and -
can also be a prefix for number values. There are also assign operators, e.g., set X += 10. They are almost the
same as doing the operation and then assigning the result, set X = X + 10, the difference is that the X in this
case is only evaluated once. The result of an assignment operator is the value used to update the variable.

The table below describes each operator and what types it supports.

Operator Supported Built-In Types Description

*

float

int

string

array

The + operator adds two number values together. When used with strings and arrays, the two values are
concatenated. You can also use the operator + as a prefix to a number, for example +6, to help align your code
visually, but it won't change the value of the number.

*

float

int
The - operator subtracts the second number operand from the first operand. You can also use the operator - as a
prefix to a number to negate the number value, for example -3.2.

*

float

int
The * multiplies two number values together.

/

float

int (failable)

The / operator divides the first number operand by the second number operand Integer division is failable and
returns the rational type. For more details on integer division, see int.

set +=
float

int
string

array

With this operator, you can combine addition and assignment in the same operation to update a variable's value.
set -=

float

int

With this operator, you can combine subtraction and assignment in the same operation to update a variable's

value.

set *=

float

int

With this operator, you can combine multiplication and assignment in the same operation to update a variable's

value.

set /=

float

With this operator, you can combine division and assignment in the same operation to update a variable's value,
unless the variable is an integer. For more details on integer division, see int.

### Query

Query expressions use the operator ? (query) and check if a logic value is true. Otherwise, the expression fails. The ? (query) operator uses the postfix format.

Outcome of the Expression p Outcome of the Expression p?

true

Succeeds and result is true.

false
Fails and result is no value.

Reference for the ? (query) operator evaluating expressions, represented by p.

For example:

```verse
if (IsMorning?):
    Say("Good Morning!")
```
