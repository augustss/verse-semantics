# Testing that continue is a valid identifier, not a keyword

# Using continue as a variable name
continue := 42
x := continue + 10

# Using continue as a function name
continue() : int =
    return 5

# Using continue as a parameter name
test_func(continue : int) : int =
    return continue * 2

# Using continue in various contexts
result := test_func(continue)
my_continue := continue()