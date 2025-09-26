# Time Flow and Concurrency

You can author time flow the way you author control flow, by executing expressions simultaneously using built-in concurrency expressions in Verse.

An important aspect of games and simulations is specifying the order and overlap of operations that take time. Need two or two hundred monsters all acting simultaneously? Planning a swarm of robots that can march in (or out of) step? Thinking about a fleet of spaceships that invade over time?
Time-flow control is at the heart of the Verse programming language, and this is accomplished with concurrent expressions.

You could say that time flow is a type of flow control, but where control flow is about the order in which a computer executes instructions based on the order of expressions in the program, time flow controls the execution in time, not sequence, based on how concurrency expressions are used.

Time flow is another way of saying concurrency.

* Concurrency Overview: See how concurrency expressions impact time flow in Verse.

* Sync: Run two or more async expressions concurrently using a sync expression.

* Race: Use a race expression to run two or more async expressions concurrently and cancel whichever expressions don't finish first.

* Rush: Use a rush expression to run two or more async expressions without canceling the slower expressions.

* Branch: Use a branch expression to start one or more async expressions, then immediately execute following expressions.

* Spawn: Use a spawn expression to start one async expression in any context, then immediately execute the following expressions.

* Task: A task is an object that represents the state of a currently-executing async function.
language

### Concurrency Overview

See how concurrency expressions impact time flow in Verse.

An expression in Verse can be either immediate or async. This describes the time an expression can take to evaluate relative to simulation updates.
Think of a simulation update as when a new frame is shown.
There are cases when multiple simulation updates can occur before a new frame, such as if an online game goes out of sync with the server.

immediate async

An immediate expression evaluates with no delay, meaning that the evaluation will complete within the current simulation update.
An async expression has the possibility of taking time to evaluate, but doesn’t necessarily have to. An async expression may or may not complete in the current simulation update, or in a later one.

Async Contexts

Async expressions can be used in any Verse code that has an async context.
An async context is the body of a function that has the suspends effect specifier. The suspends effect indicates that async functions can suspend and cooperatively transfer control to other concurrent expressions at various points over several simulation updates before they complete.
The OnBegin() function in a Verse device is a common async function used as a starting point for async code.
Calling an async function has the same syntax as calling an immediate function:

```Verse
OnBegin<override>()<suspends> : void =
    HideAllPlatforms()
HideAllPlatforms()<suspends> : void =
    for (Platform : Platforms):
        Platform.Hide()
        Sleep(Delay)
```

Like any other expression, an async expression can have a result. The result of an async expression is only available once it has completed.

```verse