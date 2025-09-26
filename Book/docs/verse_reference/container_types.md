# Container Types

Store multiple values together by using a container type. Verse has a number of container types to store values in.

* Option: The option type can contain one value or can be empty.
* Range: The range expression contains all the numbers in a specified range, and can only be used in specific expressions.
* Array: An array is a container where you can store elements of the same type, and access the elements by their position in the array.
* Map: A map is a container where you can store values associated with other values, called key-value pairs, and access the elements by their unique keys.
* Tuple: A tuple is a container where you can group two or more expressions of mixed types and access the elements in the tuple by their position.

### Option

The option type can contain one value or can be empty.

In the following example, MaybeANumber is an optional integer ?int that contains no value. A new value for MaybeANumber is then set to 42.

```verse
var MaybeANumber : ?int = false # unset optional value
set MaybeANumber := option{42} # assigned the value 42
Creating an option variable in Verse
```

```verse
MaybeANumber : ?int = option{42} # initialized as 42
MaybeAnotherNumber : ?int = false # unset optional value
```

Creating an option: You can initialize an option with one of the following:

No value: Assign false to the option to mark it as unset.

Initial value: Use the keyword option followed by {}, and an expression between the {}. If the expression fails,
the option will be unset and have the value false.

Specify the type by adding ? before the type of value expected to be stored in the option. For example ?int.

```verse
if (Number := MaybeANumber?):
```

Number # if MaybeANumber is not empty, then its value is stored in Number for you to use.

Accessing an element in an option: Use the query operator ? with the option, such as MaybeANumber?. Accessing the value stored in an option is a failable expression because there might not be a value in the option, and so must be used in a failure context.

The following is an example of using an option type to save a reference to a spawned player and, when a player is spawned, to have the trigger device react:

```verse
my_device := class<concrete>(creative_device):
```

```verse
    var SavedPlayer : ?player = false # unset optional value
    @editable
    PlayerSpawn : player_spawner_device = player_spawner_device{}
    @editable
    Trigger : trigger_device = trigger_device{}
    OnBegin<override>() : void =
        PlayerSpawn.PlayerSpawnedEvent.Subscribe(OnPlayerSpawned)
    OnPlayerSpawned(Player : player) : void =
        set SavedPlayer = option{Player}
        if (TriggerPlayer := SavedPlayer?):
            Trigger.Trigger(TriggerPlayer)
```

Persistable Type

An option is persistable if its value is persistable, which means that you can use them in your module-scoped weak_map variables and have their values persist across game sessions. For more details on persistence in Verse, check out Using Persistable Data in Verse.
option

### Range

The range expression contains all the numbers in a specified range, and can only be used in specific expressions.

The range type represents a series of integers, for example 0..3, and Min..Max.

The start of the range is the first value in the expression, for example 0, and the end of the range is the value following .. in the expression, for example 3. The range contains all the integers between, and including, the start and end values. For example, the range expression 0..3 contains the numbers 0, 1, 2, and 3.

Range expressions only support int values, and can only be used in for, sync, race, and rush expressions.

For example:

```verse
for (Index := 0..5):
    Print("{Index}")
```

### Array

An array is a container where you can store elements of the same type, and access the elements by their position in the array.

When you have variables of the same type, you can collect them into an array. An array is a container type where you specify the type of the elements with []type, such as []float. An array is useful because it scales to however many elements you store in it without changing your code for accessing the elements.
For example, if you have multiple players in your game, you can create an array and initialize it with all the players.

```verse
Players : []player = array{Player1, Player2}
```

An array containing 2 players

Verse has the pattern where definition mirrors use. Defining an array and using it follows that pattern.

### Array Length

You can get the number of elements in an array by accessing the member Length on the array. For example, array{10, 20, 30}.Length returns 3.

Accessing Elements in an Array

Elements in an array are ordered in the same position in the array as you inserted them, and you can access the element at that position, called its index, in the array. For example, to get the first player, you’d access the Players array with Players[0].

The first element in an array has an index of 0 and each subsequent element’s index increases in number. For example, array{10, 20, 30}[0] is 10 and array{10, 20, 30}[1] is 20.

### Index

0
1
2
Element
10
20
30

The last index in an array is one less than the length of the array. For example, array{10, 20, 30}.Length is 3 and the index for 30 in array{10, 20, 30} is 2.

Accessing an element in an array is a failable expression and can only be used in a failure context, such as an if expression. For example:

```verse
ExampleArray : []int = array{10, 20, 30, 40, 50}
for (Index := 0..ExampleArray.Length - 1):
    if (Element := ExampleArray[Index]):
        Print("{Element} in ExampleArray at index {Index}")
```

This code will print:

```Verse
10 in ExampleArray at index 0
    20 in ExampleArray at index 1
    30 in ExampleArray at index 2
    40 in ExampleArray at index 3
    50 in ExampleArray at index 4
```

Changing an Array and its Elements

Arrays, like all other values in Verse, are immutable. If you define an array variable, that allows you to assign a new array to the variable, or mutate individual elements.

For example:

```verse