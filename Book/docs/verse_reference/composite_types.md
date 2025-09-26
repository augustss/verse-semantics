# Composite Types

Create a unique type through the use of composite types. Verse offers several ways you can do this.

* Class: A class is a template for creating objects with similar behaviors and properties (fields and methods).
* Enum: An enum is a type used to store named sets of things.
* Struct: A struct is a way to group several related variables together.
* Subclass: A subclass is a class that extends the definition of another class by adding or modifying the fields and methods of the other class.
* Interface: An interface provides a contract for how to interact with any class that implements the interface.
* Constructor: A constructor is a special function that creates an instance of the class that it's associated with.

### Class

A class is a template for creating objects with similar behaviors and properties (fields and methods).

In Verse, a class is a template for creating objects with similar behaviors and properties. It is a composite type, which means that it’s bundled data of other types and functions that can operate on that data.
Classes are hierarchical, which means that a class can inherit information from its parent (superclass) and share its information with its children (subclasses). Classes can be a custom type defined by the user. Compare to instance.

For example, let’s say you want to have multiple cats in your game. A cat has a name and an age, and they can meow. Your cat class could look like this:

```verse
cat := class:
    Name : string
    var Age : int = 0
    Sound : string
    Meow() : void = DisplayMessage(Sound)
```

Definitions of variables that are nested inside the class define fields of the class. Functions defined inside a class may also be called methods. Fields and methods are referred to as class members. In the above example, Sound is a field, and Meow is a method of cat.

Fields of a class may or may not have a default value, or may only define a type that limits the values that the field may have. In the above example, Name does not have a default value, while Age does. The value may be specified using an expression that has the <converges> effects. The default value expression may not use the identifier Self, which you'll learn about below.

For instance, lets say you want your cats to be able to tilt their heads. You can initialize an initial rotation HeadTilt in the following code using the IdentityRotation() method because it has the <converges> specifier and is guaranteed to complete with no side effects.

```verse
cat := class:
    ...
    # A valid expression
    HeadTilt:rotation = IdentityRotation()
```

Constructing a Class

With a class that defines what a cat is and what the cat can do, you can construct an instance of the class from an archetype. An archetype defines the values of the class fields. For example, let’s make an old cat named Percy from the cat class:

```verse
OldCat := cat{Name := ”Percy”, Age := 20, Sound:= ”Rrrr”}
```

In this example, the archetype is the part between { and }. It doesn't need to define values for all fields of the class, but must at least define values for all fields that don't have a default value. If any field is omitted, then the instance constructed will have the default value for that field.

In this case, the cat class Age field has a default value assigned to it of (0). Since the field has a default value, you’re not required to provide a value for it when constructing an instance of the class. The field is a variable, which means that though you might provide a value at construction time, the value of that variable can be changed after construction.

By contrast, the Name field of cat is not a mutable variable, and so is immutable by default. This means that you can provide a default value for it at construction time, but after construction, it cannot change: it is immutable.

Since a class in Verse is a template, you can make as many instances as you want from the cat class. Let’s make a kitten named Flash:

```verse
Kitten := cat{Name := ”Flash”, Age := 1, Sound := ”Mew”}
```

### Accessing Fields

Now that you have some instances of cat, you can access each cat’s Name field with OldCat.Name or Kitten.Name, and call each cat's Meow method with OldCat.Meow() or Kitten.Meow().
Both cats have the same named fields, but those fields have different values. For example, OldCat.Meow() and Kitten.Meow() behave differently because their Sound fields have different values.

Self

Self is a special identifier in Verse that can be used in a class method to refer to the instance of the class that the method was called on. You can refer to other fields of the instance the method was called on without using Self, but if you want to refer to the instance as a whole, you must use Self.
For example, if DisplayMessage required an argument for which pet to associate a message with:

```Verse
DisplayMessage(Pet:pet, Message:string) : void = …
cat := class:
    …
    Meow() : void = DisplayMessage(Self, Sound)
```

If you wanted to initialize a louder version of your cats meow, you might think you could build off Sound variable you already set up. This will not work in the following code however, because LoudSound cannot reference the instance member Sound, since default value expressions cant use the identifier Self.

```verse
cat := class:
    ...
    Sound : string
    Meow() : void = DisplayMessage(Self, Sound)
    # The following will fail since default class values
    # can't reference Self
    LoudSound : string = "Loud " + Self.Sound
    LoudMeow() : void = DisplayMessage(Self, LoudSound)
```

Subclasses and Inheritance
Classes can inherit from a superclass, which includes all fields of the superclass in the inheriting class. Such classes are said to be a subclass of the superclass. For example:

```verse
pet := class:
    Name : string
    var Age : int = 0
cat := class(pet):
    Sound : string
    Meow() : void = DisplayMessage(Self, Sound)
dog := class(pet):
    Trick : string
    DoTrick() : void = DisplayMessage(Self, Trick)
```

Here, the use of class(pet) when defining cat and dog declares that they inherit from the pet class. In other words, they are subclasses of pet.

This has several advantages:

Since both cats and dogs have names and ages, those fields only have to be defined once, in the pet class. Both the cat and the dog field will inherit those fields.
The pet class can be used as a type to refer to instance of any subclass of pet. For example, if you want to write a function that just needs the name of a pet, you can write the function once for both cats and dogs, and any other pet subclasses you might introduce in the future:

```Verse
IncreaseAge(Pet : pet) : void=
         set Pet.Age += 1
```

For more information, see the Subclass page.

Overrides

When you define a subclass, you can override fields defined in the superclass to make their type more specific, or change their default value. To do so, you must write the definition of the field in your subclass again, but with the <override> specifier on its name. For example, you can add a Lives field to pet with a default value of 1, and override the default value for cats to be 9:

```verse
pet := class:
    …
    Lives : int = 1
cat := class(pet):
    …
    Lives<override> : int = 9
```

Method Calls

When you access a field of a class instance, you access that instance's value for the field. For methods, the field is a function, and overriding it replaces the field's value with a new function. Calling a method calls the value of the field. This means that the method called is determined by the instance. Consider the follow example:

```verse
pet := class:
    …
    OnHearName() : void = {}
cat := class(pet):
    …
    OnHearName<override>() : void = Meow()
dog := class(pet):
    …
    OnHearName<override>() : void = DoTrick()
CallFor(Pet:pet):void=
    DisplayMessage("Yoo hoo {Pet.Name}!")
    Pet.OnHearName()
```verse

If you write CallFor(Percy), it will call the OnHearName method as defined by cat. If you write CallFor(Fido) where Fido is an instance of the dog class, then it will call the OnHearName method as defined by dog.

Visibility Specifiers

You can add visibility specifiers to class fields and methods to control who has access to them. For example, you can add the private specifier to the Sound field so only the owning class can access that private field.


```verse
cat := class:
    …
    Sound<private> : string
MrSnuffles := cat{Sound := "Purr"}
MrSnuffles.Sound # Error: cannot access a private field
```

The following are all the visibility specifiers you can use with classes:
public: Unrestricted access.

internal: Access limited to current module. This is the default visibility.

protected: Access limited to current class and any subclasses.

private: Access limited to current class.

Access Specifiers

You can add access specifiers to a class to control who can construct them. This is useful, for example, if you want to make sure an instance of a class can only be constructed at a certain scope.

```verse
pets := module:
    cat<public> := class<internal>:
        Sound<public> : string = "Meow"
GetCatSound(InCat:pets.cat):string =
    return InCat.Sound # Valid: References the cat class but does not call its constructor
MakeCat():void =
    MyNewCat := pets.cat{} # Error: Invalid access of internal class constructor
```

Calling the constructor for the cat class outside of its module pets wil fail because the class keyword is marked as internal. This is true even though the class identifier itself is marked as public, which means cat can be referenced by code outside the pets module.

The following are all the access specifiers you can use with the class keyword:

public: Unrestricted access. This is the default access.

internal: Access limited to the current module.

### Concrete Specifier

When a class has the concrete specifier, it is possible to construct it with an empty archetype, such as cat{}. This means that every field of the class must have a default value. Furthermore, every subclass of a concrete class must itself be concrete.
For example:

```verse
class1 := class<concrete>:
    Property : int = 0