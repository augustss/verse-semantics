# Access Specifiers

Access specifiers define what can interact with a member and how. You can apply access specifiers to the following:

The identifier for a member: name<specifier> : type = value

The keyword var for a member: var<specifier> name : type = value

You can have an access specifier on both the identifier and the var keyword for a variable, to differentiate between who has access to read and write the variable. For example, the following variable MyInteger has the public specifier on the identifier so anyone can read the value, but the var keyword has the protected specifier so only the current class and subtypes can write to the variable.

```verse
var<protected> MyInteger<public>:int = 2
```

### Specifier Description Usage Example

public

The identifier is universally accessible.

You can use this specifier on:

module

class

interface

struct

enum
method

data

```verse
name<public> : type = value
```

protected

The identifier can only be accessed by the current class and any subtypes.

You can use this specifier on:

class

interface

struct
functions within a class

enum
non-module method

data
Verse

name<protected> : type = value

private
The identifier can only be accessed in the current, immediately enclosing, scope (be it a module, class, struct,
etc.).

You can use this specifier on:
class
interface
struct

functions within a class

enum

non-module method

data

Verse

name<private> : type = value

internal

The identifier can only be accessed in the current immediately enclosing, module. This is the default access
level.

You can use this specifier on:

module

class

interface
struct

enum
method

data

```Verse
name<internal> : type = value
```

scoped

The identifier can only be accessed in the current scope and any enclosing scopes. Any assets you expose to
Verse that appear in the Assets.digest.Verse file will have the <scoped> specifier.
You can use this specifier on:
module

class

interface

functions

struct

enum

non-module method
data

```verse