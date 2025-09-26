# Failure

Failure is a way to control the sequence in which a program performs actions, called the control flow.
Failure is control flow in Verse.

Unlike other programming languages that use the Boolean values true and false to change the flow of a program, Verse uses expressions that can either succeed or fail. These expressions are called failable expressions, and can only be executed in a failure context.

But failure is more than a simple Boolean value in Verse. Failure drives the flow of control. A failable expression can succeed and yield a value, or fail and return no value, and failable expressions are only allowed in a failure context, unlike other programming languages, where control flow is decided through Boolean values.
Using failure for control flow means that work doesn’t have to be duplicated, and that you can avoid subtle errors.

For example, in other languages, you have to check that an index for an array is valid before accessing the array element at that index, which is a common cause of errors in other languages.

In Verse, validation and access are combined to avoid this.

For example:

```verse
if (Element := MyArray[Index]):
        Log(Element)
```verse

Failable Expression

A failable expression is an expression that can either succeed and produce a value, or fail and return no value. 
Examples of failable expressions include indexing into an array because an invalid index will fail, and using operators such as comparing two values.

Code that you write isn’t failable by default. For example, to write a function that can fail, you must add the effect specifier <decides> to the function definition. Currently it is also necessary to add <transacts> when using <decides>.

For a full list of expressions that are failable, refer to the list of Expressions in Verse.

Failure Context

A failure context is a context where it is allowable to execute failable expressions. The context defines what happens if the expression fails. Any failure within a failure context will cause the entire context to fail.
A failure context allows nested expressions to be failure expressions, such as function arguments or expressions in a block expression.

A useful aspect of failure contexts in Verse is that they are a form of speculative execution, meaning that you can try out actions without committing them. When an expression succeeds, the effects of the expression are committed, such as changing the value of a variable. If the expression fails, the effects of the expression are rolled back, as though the expression never happened.
This way, you can execute a series of actions that accumulate changes, but those actions will be undone if they fail anywhere.

To make this work, all functions called in the failure context must have the effect specifier <transacts>, and the compiler will complain if they don't.
User-defined functions do not have the transacts effect by default. An explicit <transacts> specifier must be added to their definitions. Some native functions also do not have the transacts effect and can't be called in failure contexts.

An example of a native function without transacts could be an audio_component with a BeginSound() method. If the sound is started then even if it is stopped it could have been noticed.
The following list includes all of the failure contexts in Verse:

The condition in if expressions.

```verse
if (test-arg-block) { … }
```

The iteration expressions and filter expressions in for expressions. Note that for is special in that it creates a failure context for each iteration. If iterations are nested, then the failure contexts will also be nested. When an expression fails, the innermost failure context is aborted, and the enclosing iteration, if any, continues with the next iteration.

```verse
for (Item : Collection, test-arg-block) { … }
```

The body of a function or method that has the `<decides>` effect specifier.

```verse
IsEqual()<decides><transacts> : void = { … }
```

The operand for the not operator.

```verse
not expression
```

The left operand for or.

expression1 or expression2

Initializing a variable that has the option type.

option{expression}

failure
