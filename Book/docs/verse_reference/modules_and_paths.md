# Modules and Paths

A Verse module is an atomic unit of code that can be redistributed and depended upon, and that you can import into your Verse file to use code definitions from other Verse files.

A Verse module is an atomic unit of code that can be redistributed and depended upon, and can evolve over time without breaking dependencies. You can import a module into your Verse file to use code definitions from other Verse files.

A Verse module is specified by the folder in the file hierarchy of the project, and the module's name is the name of the folder. All .verse files in the same folder as the file are part of that Verse module, and can
access definitions from the other Verse files in the module without explicitly importing the module.

A module is identified by its path; for example, /Verse.org/Verse. Verse paths provide a global namespace for identifying things, and borrow from the idea of web domains. These paths are persistent and unique, and discoverable by any Verse programmer.

For a list of existing Verse modules, see the Verse API Reference.

Module Members

You can create modules within a .verse file using the following syntax:

```verse
module1 := module:
    ...
## Similar to classes and function, bracket syntax is also supported
module2 := module
{
    ...
}
```

A module defined in the Verse file can contain anything that is contained at the top level of a .verse file. This includes functions, constants, various types, as well as other module definitions. For example:

```verse
module := module:
    submodule<public> := module:
        submodule_class<public> := class {}
    module_class<public> := class{}
```

The contents of a submodule can be referred to using the name of the module qualified with the name of the base module. For example, class1 can be referred to outside of module1 as module1.module2.class1.

Importing Definitions from Other Verse Files

To use the contents of a Verse module, you must import the module by its path. For example, the following code imports the Random module, identified by the path /Verse.org/Random:

```Verse
using { /Verse.org/Random }
```

When the Random module is imported into your Verse file, you can use its code definitions, such as the function GetRandomInt().

Other common module examples include the Devices, Simulation, and Diagnostics modules, all of which are imported by default when you create a new Verse file through UEFN.

```Verse
using { /Fortnite.com/Devices }
using { /Verse.org/Simulation }
using { /UnrealEngine.com/Temporary/Diagnostics }
```

To import modules from other Verse files, you can use either a local path such as using { /YourVerseFolder/your_module }, or if the file is in the same directory, just using { your_module }.

You can import nested modules either by importing base modules before submodule modules, or by using dotted notation. For instance, given the modules:

```verse
base_module<public> := module:
    submodule<public> := module:
        submodule_class := class:
            ...
```

If you want to access members in submodule, you can either import it by importing base_module before submodule, or by importing base_module.submodule. Note that importing submodule before base_module will produce an error.

```verse
## Works and imports both base and submodules

using { base_module }
using { submodule }

## Works and imports only the submodule

using { base_module.submodule }

## Does not work

using { submodule }
using { base_module }
```

When you create a subfolder in a Verse project, a module is automatically created for that folder. For example, if base_module was defined in a folder module_folder, then module_folder would have its own module that contains base_module.

Another way to view this is that the file structure module_folder/base_module is the same as the following:

```verse
module_folder := module:
    base_module := module:
        submodule := module:
            submodule_class := class:
                ...
```

Note that the module for module_folder would need to be imported before base_module.

```verse