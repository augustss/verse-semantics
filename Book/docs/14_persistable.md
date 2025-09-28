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
- Must have the `<final>` specifier (no subclasses allowed)
- Cannot be unique
- Cannot have a superclass
- Cannot be parametric
- Can only contain persistable members
- Cannot have variable members

### Persistable Structs

Structs are ideal for simple data structures that won't change after publication:

```verse
coordinates := struct<persistable>:
    X:float = 0.0
    Y:float = 0.0
```

**Requirements for persistable structs:**

- Must have the `<persistable>` specifier
- Cannot be parametric
- Can only contain persistable members
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
