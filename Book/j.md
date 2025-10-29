# Standalone Verse Specification

**Version 0.2 • October 2025**

**Target:** ShipVerse (prioritized over MaxVerse)
**Stakeholders:** Phil (C1), Tim (C2, C4), Jan (C3, C5, C6), Andrew (C2, C3, C6)

---

## Abstract

Standalone Verse defines a Verse implementation that executes outside the Unreal Editor and Fortnite metaverse environment. This specification addresses dependency resolution, executable detection, entry point semantics, command-line interface, and I/O operations for local machine execution. The design preserves Verse's core semantics while enabling traditional command-line programming workflows.

**Design Goals:**
1. Execute `.verse` files with minimal ceremony
2. Provide familiar Unix-style command-line interface
3. Maintain Verse's flat language semantics
4. Enable local dependency management without environment variables

---

## Table of Contents

1. [Terminology](#terminology)
2. [Design Constraints](#design-constraints)
3. [Language Semantics](#language-semantics)
4. [Dependency Resolution](#dependency-resolution)
5. [Executable Detection](#executable-detection)
6. [Entry Point Transformation](#entry-point-transformation)
7. [Module System](#module-system)
8. [Command-Line Interface](#command-line-interface)
9. [I/O Subsystem](#io-subsystem)
10. [File Format Specifications](#file-format-specifications)

---

## Terminology

### Core Definitions

**Snippet**
: A `.verse` file containing Verse code. The physical file scope in Verse's module system. Multiple snippets in a directory aggregate to form a module.

**Module**
: A directory containing one or more snippets. Modules are defined by directory structure, not linguistic constructs.

**Verse Path**
: An absolute path in Verse's global namespace (e.g., `/localhost/MyLib/Utils`). Distinct from local filesystem paths.

**Verse Script**
: A `.verse` file with a shebang (`#!/usr/bin/env verse`) or passed directly to the `verse` executable. Automatically transformed via float-out pass.

**Package**
: A registered library defined by a `.vpackage` file, providing reusable modules for import.

**Project**
: An application structure defined by a `.vproject` file, declaring executables and dependencies.

**Registry**
: A mapping file (`~/.verse_registry`) associating Verse paths with local filesystem paths.

**Float-Out Pass**
: Compiler transformation separating top-level definitions from executable statements in scripts.

**MainWrapper**
: Compiler-injected entry point function wrapping executable statements in scripts.

**Mount Point**
: A virtual Verse path aliasing multiple packages into a unified namespace.

### Type Signatures

**Main-like Function**
: A function matching one of these signatures:
```verse
F<public>():void
F<public>(Args:[]string):void
F<public>(Args:[]string, Env:[string]string):void
```

---

## Design Constraints

### C1: Dependency Resolution

**Problem:** Determine module locations without environment variables or hardcoded local paths.

**Requirements:**
- No environment variables
- Version controllable
- Portable across machines
- Support overlapping directory hierarchies

**Solution:** Local registry mapping Verse paths to filesystem paths.

### C2: Executable Definition

**Problem:** Distinguish executables from libraries without shadow languages.

**Requirements:**
- No specialized file extensions (`.versetest`, etc.)
- Maintain single-language property

**Solution:** Shebangs for scripts, `.vproject` declarations for project executables.

### C3: Entry Point Detection

**Problem:** Identify entry points while preserving Verse's flat language invariant.

**Constraint:** Verse disallows nested modules, classes, and functions. Reordering may not be semantics-preserving.

**Solution:** Float-out transformation separating definitions from statements.

### C4: Shebang Arguments

**Problem:** Pass command-line arguments through shebangs with Unix limitations.

**Constraint:** Traditional Unix shebangs allow only one argument.

**Solution:** `-S` flag for argument splitting, similar to `/usr/bin/env -S`.

### C5: I/O Operations

**Problem:** Define I/O semantics compatible with Verse's transactional model.

**Constraint:** Verse VM executes transactions between suspends points.

**Solution:** I/O operations use `<suspends>` effect to mark transaction boundaries.

### C6: BetaVerse Compatibility

**Requirement:** Minimize disruption to existing Verse implementation (BetaVerse).

**Preserved Semantics:**
- Modules as directories
- Snippet aggregation
- Physical vs logical scope hierarchy
- `using` statement behavior

---

## Language Semantics

### Preserved Semantics

The following Verse semantics remain unchanged:

1. **Modules are directories** - A directory defines a module; `.verse` files within are snippets
2. **Snippet aggregation** - All snippets in a directory contribute to the module
3. **No closures** - Functions cannot close over mutable variables
4. **Flat language** - Modules, classes, functions cannot nest inside functions
5. **Effect system** - Effect propagation and checking unchanged
6. **Type system** - Type checking and inference unchanged
7. **Concurrency model** - Structured concurrency semantics preserved

### New Semantics

1. **Shebang recognition** - Files with `#!/usr/bin/env verse` treated as scripts
2. **Float-out transformation** - Scripts transformed to valid modules
3. **MainWrapper injection** - Compiler injects entry point wrapper
4. **Registry-based resolution** - Module lookup via registry
5. **Mount point syntax** - Virtual path aliasing

---

## Dependency Resolution

### Registry Specification

**Location:** `~/.verse_registry` (configurable via `-R` flag)

**Format:** Line-separated mappings
```
<verse-path> → <local-filesystem-path>
```

**Example:**
```
/localhost/MyLib        → /Users/jeff/projects/MyLib
/localhost/AnotherLib   → /Users/jeff/libs/AnotherLib
```

**Invariants:**
1. All Verse paths must be unique
2. All local paths must contain valid `.vpackage` or `.vproject`
3. All registered packages must compile successfully

### Resolution Algorithm

Given a `using { <path> }` statement:

1. **Registry lookup** - Check if `<path>` exists in registry
2. **Project context** - If in `.vproject`, check declared dependencies
3. **Package context** - If in `.vpackage`, check package root
4. **Local context** - Scripts can import same-directory snippets only
5. **Error** - If no match found, compilation fails

### Module Path Rules

**Module Definition:**
- A module is the set of all snippets in a directory
- Directory path determines module path
- Subdirectories define submodules

**Path Resolution:**
```
MyLib/
└── Utils/
    ├── Math.verse    # Module: /localhost/MyLib/Utils
    └── String.verse  # Module: /localhost/MyLib/Utils
```

Both snippets contribute to `/localhost/MyLib/Utils`.

**Import Semantics:**
```verse
using { /localhost/MyLib/Utils }  # Imports entire Utils module
using { /localhost/MyLib }        # Imports MyLib package root
```

### Path Uniqueness

**Requirement:** All registered Verse paths must be unique.

**Conflict Detection:**
```bash
$ verse register MyLib
Error: duplicate paths:
  - /localhost/MyLib at /Users/jeff/projects/MyLib
  - /localhost/MyLib at /Users/jeff/other/MyLib
```

**Resolution:** Remove conflicting registration or rename package.

---

## Executable Detection

### Script Detection

A file is a Verse script if:

1. It contains shebang: `#!/usr/bin/env verse` (line 1, column 1)
2. OR it is passed as argument to `verse` executable

**Shebang Format:**
```verse
#!/usr/bin/env verse [flags]
```

**Valid Flags:**
- `-e <symbol>` - Specify entry point function
- `-S` - Split remaining text as whitespace-separated arguments

### Project Executable Declaration

Executables declared in `.vproject`:

```json
{
  "executables": {
    "<subdir>": {
      "entry": "<function-name>"
    }
  }
}
```

**Requirements:**
- `<subdir>` must be a valid directory in project
- `<function-name>` must exist in that directory's module namespace
- Function must have a Main-like signature

---

## Entry Point Transformation

### Float-Out Pass

**Purpose:** Transform scripts into valid Verse modules while preserving flat language invariant.

**Algorithm:**

1. **Parse** script into AST
2. **Classify** top-level forms:
   - **Definitions:** Functions, classes, structs, interfaces, constants
   - **Statements:** `var` declarations, `set` statements, expressions, control flow
   - **Forbidden:** Modules, functions closing over top-level `var`
3. **Validate** no forbidden forms present
4. **Separate** definitions from statements (preserving lexical order within each category)
5. **Float** definitions to top level
6. **Wrap** statements in `MainWrapper`
7. **Inject** `using` statements for required modules

### MainWrapper Specification

**Signature:**
```verse
MainWrapper<public>(Args:[]string, Env:[string]string)<succeeds><transacts><suspends>:void
```

**Effect Semantics:**
- `<succeeds>` - Function always returns normally (no `<decides>`)
- `<transacts>` - Can read and write mutable state
- `<suspends>` - Can perform I/O and concurrency operations

**Automatic Injection:**
- Compiler always injects `using { /Verse.org/Verse }`
- Compiler always injects `using { /Verse.org/VerseCLR }`
- Compiler appends `return` statement to end of body

### Transformation Examples

#### Input Script
```verse
#!/usr/bin/env verse

Greet(Name:string):void =
    Print("Hello, {Name}!")

var Counter:int = 0
Greet["World"]
set Counter = Counter + 1
```

#### Output Snippet
```verse
using { /Verse.org/Verse }
using { /Verse.org/VerseCLR }

Greet(Name:string):void =
    Print("Hello, {Name}!")

MainWrapper<public>(Args:[]string, Env:[string]string)<succeeds><transacts><suspends>:void =
    var Counter:int = 0
    Greet["World"]
    set Counter = Counter + 1
    return
```

### Classification Rules

**Top-Level Permitted (float to top):**
- Functions (non-closing over mutable variables)
- Classes
- Structs
- Interfaces
- Constants

**Top-Level Wrapped (move to MainWrapper):**
- `var` declarations
- `set` statements
- Expressions
- Control flow (`if`, `for`, `loop`)
- Function calls
- Concurrency blocks (`sync`, `race`, `rush`, `branch`, `spawn`)

**Top-Level Forbidden:**
- `module` definitions
- Functions referencing top-level `var` (would require closures)
- Bare failable expressions (must be in failure context)

### Explicit Entry Points

When `-e <symbol>` specified:

1. Float-out pass still applies
2. `MainWrapper` calls user-specified function
3. Arguments passed if function signature includes them

**Transformation:**
```verse
#!/usr/bin/env verse -e Main

Main(Args:[]string):void =
    Print("Entry point")

X:int = 42  # Not executed
```

**Becomes:**
```verse
using { /Verse.org/Verse }
using { /Verse.org/VerseCLR }

Main(Args:[]string):void =
    Print("Entry point")

MainWrapper<public>(Args:[]string, Env:[string]string)<succeeds><transacts><suspends>:void =
    X:int = 42
    Main(Args)  # User function called
    return
```

---

## Module System

### Package Definition (.vpackage)

**Location:** Root directory of package

**Purpose:** Declare package metadata and configure module paths

**Required Fields:**
- `name` - Unique package identifier
- `path_root` - Verse path for package root

**Optional Fields:**
- `version` - Semantic version string
- `exports` - Array of exported subdirectories (default: all)
- `dependencies` - Array of required Verse paths
- `description` - Human-readable description

**Example:**
```json
{
  "name": "MyUtilities",
  "path_root": "/localhost/MyUtils",
  "version": "1.2.0",
  "exports": ["Math", "String"],
  "dependencies": ["/localhost/CoreLib"]
}
```

**Semantics:**
1. Sets root of Verse path for all modules in package
2. Package must compile successfully to register
3. Path conflicts detected at registration time
4. Nested `.vpackage` files define separate packages

### Project Definition (.vproject)

**Location:** Root directory of project

**Purpose:** Declare project structure, executables, and dependencies

**Required Fields:**
- `name` - Project identifier
- `executables` - Map of subdirectories to entry points

**Optional Fields:**
- `version` - Semantic version string
- `dependencies` - Array of required Verse paths
- `libraries` - Array of internal library subdirectories
- `mount_points` - Map of virtual paths to package lists

**Example:**
```json
{
  "name": "MyApplication",
  "version": "2.0.0",
  "dependencies": ["/localhost/MyUtils"],
  "executables": {
    "src": { "entry": "Main" },
    "tests": { "entry": "RunTests" }
  },
  "libraries": ["lib"],
  "mount_points": {
    "/MyApp/Utils": ["/localhost/MyUtils", "/localhost/Other/Helpers"]
  }
}
```

**Semantics:**
1. Each executable subdir must contain valid entry point
2. Library subdirectories can have `.vpackage` files
3. Dependencies resolved via registry
4. Mount points rewritten at compile time

### Mount Points

**Syntax:**
```verse
mount <virtual-path>
using {
    <verse-path1>,
    <verse-path2>,
    ...
}
```

**Semantics:**
- Creates alias `<virtual-path>` encompassing listed paths
- Maintains subpath structure from original paths
- Rewritten to real paths during compilation
- Ambiguity errors if paths conflict

**Example:**
```verse
mount /MyOrg/Utils
using {
    /localhost/MyLib/Utils,
    /localhost/AnotherLib/Helpers
}

# Now can import:
using { /MyOrg/Utils/Math }      # Resolves to /localhost/MyLib/Utils/Math
using { /MyOrg/Utils/Network }   # Resolves to /localhost/AnotherLib/Helpers/Network
```

**Compile-Time Rewrite:**
Mount declarations replaced with actual paths before module resolution.

---

## Command-Line Interface

### Synopsis

```
verse [options] <input> [args...]
verse <command> [command-args...]
```

### Execution Modes

#### Script Execution
```bash
verse <file.verse> [args...]
```

**Behavior:**
1. Treat `<file.verse>` as Verse script
2. Apply float-out transformation
3. Compile to executable
4. Execute with `args` available in `Env.Args`

#### Project Compilation
```bash
verse <directory>
```

**Behavior:**
1. Locate `.vproject` in `<directory>`
2. Compile project per project specification
3. Execute primary executable if defined

### Registry Commands

#### Register Package
```bash
verse register <path>
```

**Behavior:**
1. Locate `.vpackage` or `.vproject` at `<path>`
2. Validate package compiles successfully
3. Add Verse path → filesystem path mapping to registry
4. Error if path already registered

**Exit Codes:**
- 0 - Success
- 103 - No `.vpackage` or `.vproject` found

#### List Registry
```bash
verse list
```

**Behavior:**
Display all registry mappings.

**Output Format:**
```
<verse-path> → <filesystem-path>
...
```

#### Remove from Registry
```bash
verse remove <verse-path>
```

**Behavior:**
Remove entry from registry.

**Exit Codes:**
- 0 - Success
- 104 - Path not found in registry

#### Sync Registry
```bash
verse sync
```

**Behavior:**
1. Check all registered paths for file modifications
2. Recompile changed packages

### Options

#### `-e <symbol>`
Specify entry point function for script execution.

**Requirements:**
- `<symbol>` must be a Main-like function
- Function must exist in script's namespace

#### `-p <path>`
Add local filesystem path for dependency resolution.

**Behavior:**
- Can be specified multiple times
- Takes precedence over registry
- Warning if path duplicates registry entry

#### `-P <directory>`
Search directory for packages.

**Behavior:**
- Recursively finds `.vpackage` files in `<directory>`
- Adds all found packages for dependency resolution
- Warning on duplicates

#### `-R <registry-file>`
Use alternate registry file.

**Default:** `~/.verse_registry`

**Exit Codes:**
- 105 - Registry file not found

#### `-S` (Shebang Only)
Split remaining shebang text as whitespace-separated arguments.

**Usage:**
```verse
#!/usr/bin/env verse -S -e Main arg1 arg2 arg3
```

**Equivalent To:**
```bash
verse -e Main "arg1" "arg2" "arg3" script.verse
```

### Exit Codes

| Code | Description |
|------|-------------|
| 0 | Success |
| 1 | Runtime error or user-specified exit |
| 101 | Forbidden term at script top-level |
| 102 | Type checking failed |
| 103 | Cannot find `.vproject` or `.vpackage` (register command) |
| 104 | Path not found in registry (remove command) |
| 105 | Registry file not found (`-R` flag) |

### Argument Passing

**Script Arguments:**
Arguments after script filename available via:
- `Env.Args` array (from `/Verse.org/System/Environment`)
- Function parameters (if using explicit entry point)

**Example:**
```bash
$ verse script.verse arg1 arg2 arg3
```

In script:
```verse
using { /Verse.org/System/Environment }

Print(Env.Args[0])  # "arg1"
Print(Env.Args[1])  # "arg2"
Print(Env.Args[2])  # "arg3"
```

---

## I/O Subsystem

### Module: /Verse.org/System/Environment

**Purpose:** Access system environment and process control

**Provided Definitions:**

```verse
Env:environment

environment := struct:
    Args:[]string          # Command-line arguments
    Vars:[string]string    # Environment variables
    Exit(Code:int):void    # Terminate process
```

**Semantics:**
- `Args` populated by compiler from command-line
- `Vars` populated by runtime from system environment
- `Exit` terminates process with specified code

### Module: /Verse.org/System/IO

**Purpose:** File and standard I/O operations

**Provided Definitions:**

```verse
ReadFile(Path:string)<suspends>:string
WriteFile(Path:string, Contents:string)<suspends>:void
ReadLine()<suspends>:string
Print(Text:string):void
PrintErr(Text:string)<suspends>:void
```

**Effect Semantics:**
- All I/O operations have `<suspends>` effect
- I/O is blocking (suspends current transaction)
- Files automatically closed after suspends point

**Transaction Boundaries:**

Per Verse VM operational semantics:
1. Current transaction commits before I/O
2. I/O operation executes (passing time)
3. New transaction begins after I/O

This ensures transactional consistency around I/O operations.

### Rationale: Why `<suspends>` for I/O

The Verse VM executes transactions between suspends points. For blocking I/O:

1. **Commit** - Current transaction must commit
2. **Perform** - I/O operation executes (potentially takes time)
3. **Resume** - New transaction starts

The `<suspends>` effect marks these transaction boundaries, making I/O's transactional semantics explicit in the type system.

---

## File Format Specifications

### .vpackage Format

**Syntax:** JSON

**Schema:**
```typescript
{
  "name": string,              // Required: Package name
  "path_root": string,         // Required: Verse path
  "version"?: string,          // Optional: Semantic version
  "exports"?: string[],        // Optional: Exported subdirs (default: all)
  "dependencies"?: string[],   // Optional: Required packages
  "description"?: string       // Optional: Description
}
```

**Validation Rules:**
1. `name` must be non-empty string
2. `path_root` must be valid Verse path
3. `path_root` must be globally unique
4. `version` must follow semantic versioning if present
5. `exports` entries must be valid subdirectory names
6. `dependencies` entries must be valid Verse paths
7. Package must compile successfully

**Example:**
```json
{
  "name": "MyUtilities",
  "path_root": "/localhost/MyUtils",
  "version": "1.2.0",
  "exports": ["Math", "String", "Data"],
  "dependencies": ["/localhost/CoreLib"],
  "description": "General-purpose utility functions"
}
```

### .vproject Format

**Syntax:** JSON

**Schema:**
```typescript
{
  "name": string,                     // Required: Project name
  "version"?: string,                 // Optional: Semantic version
  "dependencies"?: string[],          // Optional: Required packages
  "executables": {                    // Required: Executable definitions
    [subdir: string]: {
      "entry": string                 // Required: Entry point function
    }
  },
  "libraries"?: string[],             // Optional: Internal libraries
  "mount_points"?: {                  // Optional: Path aliases
    [virtual_path: string]: string[]
  }
}
```

**Validation Rules:**
1. `name` must be non-empty string
2. `executables` must have at least one entry
3. Each executable subdir must exist
4. Each entry point must be Main-like function
5. `dependencies` must be valid Verse paths
6. `libraries` must be valid subdirectories
7. `mount_points` must reference valid packages
8. Project must compile successfully

**Example:**
```json
{
  "name": "MyApplication",
  "version": "2.0.0",
  "dependencies": [
    "/localhost/MyUtils",
    "/localhost/GraphicsLib"
  ],
  "executables": {
    "src": { "entry": "Main" },
    "tests": { "entry": "RunAllTests" }
  },
  "libraries": ["lib", "internal"],
  "mount_points": {
    "/MyApp/Utils": [
      "/localhost/MyUtils",
      "/localhost/GraphicsLib/Helpers"
    ]
  }
}
```

---

## Formal Semantics

### Script Import Restrictions

**Context: Non-Project Script**

A script NOT in a directory containing `.vproject` or `.vpackage`:

**Can import:**
- Snippets in same directory
- Registered packages (via registry)

**Cannot import:**
- Snippets in parent directories
- Snippets in subdirectories
- Unregistered local packages

**Rationale:** Maintain separation between local paths and Verse paths. Encourage project structure for complex codebases.

**Context: Project Script**

A script in a directory containing `.vproject` or `.vpackage`:

**Can import:**
- Snippets in same directory
- Packages declared in project dependencies
- Registered packages (via registry)

### Module Aggregation Semantics

Given directory `D` containing snippets `S1, S2, ..., Sn`:

1. Module `M` corresponds to directory `D`
2. Module `M` namespace is union of all exports from `S1, S2, ..., Sn`
3. Name conflicts within `M` are compilation errors
4. Each snippet sees exports from all other snippets in `M`

### Float-Out Preservation Theorem

**Theorem:** Float-out transformation preserves semantics for valid scripts.

**Proof Sketch:**
1. Definitions have no execution order semantics (declarative)
2. Floating preserves lexical order among definitions
3. Wrapping statements preserves execution order
4. `MainWrapper` effect signature permits all wrapped operations
5. Injected `return` is semantics-preserving (void return type)

**Counterexample (Invalid):**
Function closing over top-level `var` would require closure semantics, which Verse lacks. Therefore forbidden.

---

## Design Rationale

### Registry vs Environment Variables

**Why Registry:**
- Version control friendly (no .env files)
- Portable (no machine-specific setup)
- Explicit (visible in `verse list`)
- Flexible (multiple registries via `-R`)

**Why Not Environment Variables:**
- Not version controlled
- Machine-specific
- Hidden dependencies
- Difficult to debug

### Float-Out vs Allow Nested Modules

**Why Float-Out:**
- Preserves flat language invariant
- No new language features needed
- Clear separation of definitions vs execution
- Compatible with BetaVerse

**Why Not Nested Modules:**
- Would require closure semantics
- Break flat language property
- Create dialect (shadow language)
- Complicate semantics

### Shebangs vs File Extensions

**Why Shebangs:**
- Unix-native convention
- Works with existing tools
- No special extensions needed
- Clear executable indication

**Why Not Extensions:**
- Would create dialect (`.vsh`, `.verse-script`)
- Less portable
- Non-standard

### Three Entry Point Styles

**Implicit (Bare Script):**
- **Use Case:** Quick prototypes, REPLs
- **Tradeoff:** Less explicit, harder to find entry point

**Explicit (-e Flag):**
- **Use Case:** Organized scripts, utilities
- **Tradeoff:** More ceremony, clearer structure

**Project (.vproject):**
- **Use Case:** Applications, complex programs
- **Tradeoff:** Most ceremony, most control

Different use cases justify different approaches rather than forcing one style.

---

## Implementation Notes

### Compilation Pipeline

1. **Parse** - Lex and parse `.verse` file
2. **Detect** - Determine if script, package, or project
3. **Transform** - Apply float-out if script
4. **Resolve** - Resolve dependencies via registry
5. **Type Check** - Run type checker
6. **Lower** - Lower to VM bytecode
7. **Execute** - Execute via Verse VM

### Performance Characteristics

- **Startup Overhead:** Script transformation adds negligible compile time
- **Runtime Performance:** Identical to regular Verse (same VM)
- **Registry Lookup:** O(1) hash table lookup
- **Compilation Caching:** Standard Verse compilation caching applies

### Compatibility Layer

**BetaVerse Integration:**
- Float-out pass produces valid BetaVerse snippets
- No VM changes required
- Effect system unchanged
- Module system unchanged
- Only addition: registry-based resolution

---

## References

- [Verse Language Specification 0.1.5](https://docs.google.com/document/d/1B7hnOMNMjIRRWtEZVGIhyIYpVyXMnzMq5GuIKGnTGPs/)
- [Verse Module System](https://dev.epicgames.com/documentation/en-us/fortnite/modules-and-paths-in-verse)
- [Verse VM Operational Semantics](https://www.youtube.com/live/UI1wApT2t1w?si=84sdfA4JySMhKWoN&t=13315)
- [Directory-Driven Module Hierarchy Discovery 2.0](https://docs.google.com/document/d/1yhJulI85RWD9MZZ2SFsQC9t7Mww1Puib_U-8JNhD62k/)

---

**Document Version:** 0.2
**Last Modified:** October 2025
**Status:** Specification
