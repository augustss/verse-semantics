# Persistable Types

Persistable types allow you to store data that persists beyond the current game session. This is essential for saving player progress, preferences, and other game state that should be maintained across multiple play sessions.

Persistable data is stored using module-scoped `weak_map(player, t)` variables, where `t` is any persistable type. When a player joins a game, their previously saved data is automatically loaded into all module-scoped variables of type `weak_map(player, t)`.

## Basic Example

```verse
using { /Fortnite.com/Devices }
using { /UnrealEngine.com/Temporary/Diagnostics }
using { /Verse.org/Simulation }

# Global persistable variable storing player data
MySavedPlayerData : weak_map(player, int) = map{}

# Initialize data for a player if not already present
InitializePlayerData(Player : player) : void =
    if (not MySavedPlayerData[Player]):
        if (set MySavedPlayerData[Player] = 0) {}
```

## Built-in Persistable Types

The following primitive types are persistable by default:

### Numeric Types

- **`logic`** - Boolean values (true/false)
- **`int`** - Integer values (currently 64-bit signed)
- **`float`** - Floating-point numbers

### Text Types

- **`string`** - Text values
- **`char`** - Single UTF-8 character
- **`char32`** - Single UTF-32 character

### Container Types

Container types are persistable if their element types are persistable:

- **`array`** - Persistable if element type is persistable
- **`map`** - Persistable if both key and value types are persistable
- **`option`** - Persistable if the wrapped type is persistable
- **`tuple`** - Persistable if all element types are persistable

## Custom Persistable Types

You can create custom persistable types using the `<persistable>` specifier with classes, structs, and enums.

### Persistable Classes

Classes must meet specific requirements to be persistable:

```verse
player_profile_data := class<final><persistable>:
    Version:int = 1
    Class:player_class = player_class.Villager
    XP:int = 0
    Rank:int = 0
    CompletedQuestCount:int = 0
```

**Requirements for persistable classes:**

- Must have the `<persistable>` specifier
- Must have the `<final>` specifier (no subclasses allowed) - error 3663 if missing
- Cannot be `<unique>` - error 3664 if combined with persistable
- Cannot have a superclass (including interfaces) - error 3665 if inheriting from another type
- Cannot be parametric (generic) - error 3502 if type parameters present
- Can only contain persistable field types (see Prohibited Field Types below) - error 3662 for invalid types
- Cannot have variable members (`var` fields) - error 3662 if mutable fields present
- Field initializers must be effect-free (cannot use `<transacts>`, `<decides>`, etc.) - error 3582 if effects present

### Persistable Structs

Structs are ideal for simple data structures that won't change after publication:

```verse
coordinates := struct<persistable>:
    X:float = 0.0
    Y:float = 0.0
```

**Requirements for persistable structs:**

- Must have the `<persistable>` specifier
- Cannot be parametric (generic) - error 3502 if type parameters present
- Can only contain persistable field types (see Prohibited Field Types below) - error 3662 for invalid types
- Field initializers must be effect-free (cannot use `<transacts>`, `<decides>`, etc.) - error 3582 if effects present
- Cannot be modified after island publication

### Persistable Enums

Enums represent a fixed set of named values:

```verse
day := enum<persistable>:
    Monday
    Tuesday
    Wednesday
    Thursday
    Friday
    Saturday
    Sunday
```

**Important notes:**

- Non-persistent enums cannot be used with persistable data
- Closed persistable enums cannot be changed to open after publication
- Open persistable enums can have new values added after publication

## Prohibited Field Types

Persistable types have strict restrictions on what field types they can contain. The following types **cannot** be used as fields in persistable classes or structs (all produce error 3662):

### Abstract and Dynamic Types

- **`any`** - Cannot be persisted (too dynamic)
- **`comparable`** - Abstract interface type
- **`type`** - Type values cannot be persisted

### Non-Serializable Types

- **`rational`** - Exact rational numbers (not persistable)
- **Function types** (e.g., `int -> int`) - Functions cannot be serialized
- **`weak_map`** - Weak references are not persistable
- **Interface types** - Abstract interfaces cannot be persisted

### Non-Persistable User Types

- **Non-persistable enums** - Enums without `<persistable>` specifier cannot be used
- **Non-persistable classes** - Classes without `<persistable>` specifier cannot be used
- **Non-persistable structs** - Structs without `<persistable>` specifier cannot be used

### Example of Invalid Persistable Types

```verse
# Error 3662: prohibited field types
invalid_struct := struct<persistable>:
    A:any                    # Error: any not allowed
    B:comparable             # Error: comparable not allowed
    C:type                   # Error: type not allowed
    D:rational               # Error: rational not allowed
    E:int -> int             # Error: function types not allowed
    F:weak_map(int, int)     # Error: weak_map not allowed

# Error 3662: non-persistable enum
regular_enum := enum:
    Value1
    Value2

bad_struct := struct<persistable>:
    E:regular_enum           # Error: enum is not persistable
```

### Valid Persistable Field Types

Only these types can be used in persistable classes and structs:

- **Primitives:** `int`, `float`, `logic`, `string`, `char`, `char32`
- **Collections:** `array`, `map`, `option`, `tuple` (if element types are persistable)
- **User types:** Classes, structs, and enums marked with `<persistable>`
- **External types:** Persistable types from libraries (if marked as persistable)

## Working with Persistent Data

### Initializing Player Data

```verse
# Define a persistable player stats structure
player_stats := struct<persistable>:
    Level:int = 1
    Experience:int = 0
    GamesPlayed:int = 0

# Global persistent storage
PlayerData : weak_map(player, player_stats) = map{}

# Initialize or retrieve player data
GetOrCreatePlayerStats(Player : player) : player_stats =
    if (ExistingStats := PlayerData[Player]):
        ExistingStats
    else:
        NewStats := player_stats{}
        if (set PlayerData[Player] = NewStats):
            NewStats
        else:
            player_stats{}  # Fallback
```

### Updating Persistent Data

```verse
# Update player experience
AddExperience(Player : player, Amount : int) : void =
    if (Stats := PlayerData[Player]):
        NewStats := player_stats:
            Level := Stats.Level
            Experience := Stats.Experience + Amount
            GamesPlayed := Stats.GamesPlayed
        set PlayerData[Player] = NewStats
```

## JSON Serialization

Verse provides JSON serialization functions for persistable types, enabling manual serialization and deserialization of data. While the primary persistence mechanism uses `weak_map(player, t)` for automatic player data, JSON serialization can be useful for debugging, data migration, or integration with external systems.

### Serialization Functions

**`Persistence.ToJson`** - Converts a persistable value to JSON string:

```verse
player_data := class<final><persistable>:
    Level:int = 1
    Score:int = 100

Data := player_data{Level := 5, Score := 250}
JsonString := Persistence.ToJson[Data]
# Produces: {"$package_name":"/...", "$class_name":"player_data", "x_Level":5, "x_Score":250}
```

**`Persistence.FromJson`** - Deserializes JSON string to typed value:

```verse
JsonString := "{\"$package_name\":\"/.../\", \"$class_name\":\"player_data\", \"x_Level\":10, \"x_Score\":500}"
if (Restored := Persistence.FromJson[JsonString, player_data]):
    # Restored.Level = 10
    # Restored.Score = 500
```

**`Persistence.FromJsonV1`** - Deserializes legacy V1 format with mangled field names:

```verse
# Old format with mangled names
OldJson := "{\"$package_name\":\"/.../\", \"$class_name\":\"player_data\", \"i___verse_0x123_Level\":10}"
if (Restored := Persistence.FromJsonV1[OldJson, player_data]):
    # Successfully migrates from old format
```

### JSON Format Structure

All serialized persistable objects include metadata fields:

```json
{
  "$package_name": "/SolIdeDataSources/_Verse",
  "$class_name": "player_data",
  "x_Level": 5,
  "x_Score": 250
}
```

**Metadata fields:**
- `$package_name` - Package path of the type
- `$class_name` - Qualified class/struct name

**Field names:**
- Prefixed with `x_` in current format
- Old format used mangled names like `i___verse_0x123_FieldName`

### Type-Specific Serialization

**Primitives:**
```verse
int_ref := class<final><persistable>:
    Value:int

# Serialized as JSON number
JsonString := Persistence.ToJson[int_ref{Value := 42}]
# {"$package_name":"...", "$class_name":"int_ref", "x_Value":42}
```

**Optional types:**
```verse
optional_ref := class<final><persistable>:
    Value:?int

# None serialized as false
Persistence.ToJson[optional_ref{Value := false}]
# {..., "x_Value":false}

# Some serialized as object with empty key
Persistence.ToJson[optional_ref{Value := option{42}}]
# {..., "x_Value":{"":42}}
```

**Tuples:**
```verse
tuple_ref := class<final><persistable>:
    Pair:tuple(int, int)

# Serialized as JSON array
Persistence.ToJson[tuple_ref{Pair := (4, 5)}]
# {..., "x_Pair":[4,5]}

# Empty tuple
empty_tuple_ref := class<final><persistable>:
    Empty:tuple()

Persistence.ToJson[empty_tuple_ref{Empty := ()}]
# {..., "x_Empty":[]}
```

**Arrays:**
```verse
array_ref := class<final><persistable>:
    Numbers:[]int

Persistence.ToJson[array_ref{Numbers := array{1, 2, 3}}]
# {..., "x_Numbers":[1,2,3]}
```

**Maps:**
```verse
map_ref := class<final><persistable>:
    Lookup:[string]int

Persistence.ToJson[map_ref{Lookup := map{"a" => 1, "b" => 2}}]
# {..., "x_Lookup":[{"k":{"":"a"},"v":{"":1}}, {"k":{"":"b"},"v":{"":2}}]}
```

**Enums:**
```verse
day := enum<persistable>:
    Monday
    Tuesday

enum_ref := class<final><persistable>:
    Day:day

Persistence.ToJson[enum_ref{Day := day.Monday}]
# {..., "x_Day":"day::Monday"}
```

### Default Value Handling

When deserializing, missing fields are automatically filled with their default values:

```verse
versioned_data := class<final><persistable>:
    Version:int = 1
    NewField:int = 0  # Added in v2

# Old JSON without NewField
OldJson := "{\"$package_name\":\"...\", \"$class_name\":\"versioned_data\", \"x_Version\":1}"

# Deserializes successfully with default for NewField
if (Data := Persistence.FromJson[OldJson, versioned_data]):
    Data.Version = 1
    Data.NewField = 0  # Uses default value
```

This enables forward-compatible schema evolution - new fields with defaults can be added without breaking old saved data.

### Block Clauses During Deserialization

**Important:** Block clauses do not execute when deserializing from JSON:

```verse
logged_class := class<final><persistable>:
    Value:int
    block:
        Print("Constructed!")

# Normal construction triggers block
Instance1 := logged_class{Value := 1}  # Prints "Constructed!"

# Deserialization does NOT trigger block
Json := Persistence.ToJson[Instance1]
Instance2 := Persistence.FromJson[Json, logged_class]  # No print
```

Block clauses are only executed during normal construction, not during deserialization. This means initialization logic in blocks won't run for loaded data.

### Integer Range Limitations

Verse protects against integer overflow during serialization. Integers that exceed the safe serialization range cause runtime errors:

```verse
int_ref := class<final><persistable>:
    Value:int

# Safe range integers work fine
SafeData := int_ref{Value := 1000000000000000000}
Persistence.ToJson[SafeData]  # OK

# Overflow protection - runtime error for very large integers
var BigInt:int = 1
for (I := 1..63):
    set BigInt *= 2

# Runtime error: Integer too large for safe serialization
# Persistence.ToJson[int_ref{Value := BigInt}]
```

This prevents silent precision loss that could occur with floating-point representation of large integers.

### Backward Compatibility

The serialization system maintains backward compatibility with older JSON formats:

**Field name migration:**
```verse
# Old format (V1) with mangled names
OldJson := "{\"$package_name\":\"...\", \"i___verse_0x123_Value\":42}"

# Deserializes correctly
Data := Persistence.FromJsonV1[OldJson, int_ref]

# Re-serializes with new format
NewJson := Persistence.ToJson[Data]
# {"$package_name":"...", "x_Value":42}
```

**Legacy precision loss:**

Older versions used floating-point representation for integers, which could cause precision loss for large values. This behavior is preserved for V1 deserialization:

```verse
# Old V1 format with float representation
OldJson := "{\"$package_name\":\"...\", \"___verse_0x123_Value\":9.2233720368547748e+18}"

# Deserializes with silent precision loss (matches legacy behavior)
Data := Persistence.FromJsonV1[OldJson, int_ref]

# Current format avoids this issue by using proper integer serialization
```

<!-- ### Practical Considerations

**Serialization is for persistable types only:**

Only types marked `<persistable>` can be serialized:

```verse
# ERROR: Cannot serialize non-persistable type
regular_class := class:
    Value:int

# Persistence.ToJson[regular_class{Value := 1}]  # Compilation error
```

**Nested persistable types:**

Persistable types can contain other persistable types:

```verse
inner := struct<persistable>:
    X:int
    Y:int

outer := class<final><persistable>:
    Position:inner

Persistence.ToJson[outer{Position := inner{X := 10, Y := 20}}]
# {..., "x_Position":{"x_X":10, "x_Y":20}}
```

**Native type integration:**

Persistable native (C++) types integrate seamlessly:

```verse
wrapper := class<final><persistable>:
    NativeData:native_persistable_class

# Native types include their own package metadata
Persistence.ToJson[wrapper{NativeData := ...}]
# {..., "x_NativeData":{"$package_name":"/Engine/...", "$class_name":"native_persistable_class", ...}}
```
-->

## Best Practices

- **Schema Stability:**
Design your persistable types carefully, as they cannot be easily changed after publication. Consider versioning strategies for future updates.

- **Use Structs for Simple Data:**

For data that won't need inheritance or complex behavior, prefer persistable structs over classes.

- **Handle Missing Data:**
Always check if data exists for a player before accessing it, and provide appropriate defaults.

- **Atomic Updates:**
When updating persistent data, create new instances rather than trying to modify existing ones (Verse uses immutable data structures).

- **Consider Memory Usage:**
Persistent data is loaded for all players when they join, so be mindful of the amount of data stored per player.

## Error Codes Reference

Understanding error codes helps diagnose persistable type violations quickly:

- **Error 3663:** Persistable class missing `<final>` specifier
- **Error 3664:** Persistable class cannot be `<unique>`
- **Error 3665:** Persistable class cannot inherit from superclass or interface
- **Error 3662:** Invalid field type in persistable type (prohibited types, mutable fields, or non-persistable user types)
- **Error 3502:** Persistable types cannot be parametric (generic)

## Example: Player Profile System

```verse
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }

# Player class enum
player_class := enum<persistable>:
    Warrior
    Mage
    Archer
    Rogue

# Achievement data
achievement := struct<persistable>:
    Name:string = ""
    Completed:logic = false
    CompletedDate:int = 0  # Timestamp

# Complete player profile
player_profile := class<final><persistable>:
    Username:string = "Player"
    Level:int = 1
    Experience:int = 0
    SelectedClass:player_class = player_class.Warrior
    TotalPlayTime:float = 0.0
    Achievements:[]achievement = array{}

# Global player profiles
PlayerProfiles : weak_map(player, player_profile) = map{}

# Profile management device
profile_manager := class(creative_device):

    OnBegin<override>()<suspends>:void =
        # Initialize all players
        AllPlayers := GetPlayspace().GetPlayers()
        for (Player : AllPlayers):
            InitializeProfile(Player)

    InitializeProfile(Player : player) : void =
        if (not PlayerProfiles[Player]):
            DefaultProfile := player_profile{}
            set PlayerProfiles[Player] = DefaultProfile
```

This demonstrates how to create and manage persistable player data, ensuring that player progress and achievements are maintained across game sessions.
