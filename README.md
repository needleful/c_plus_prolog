# C Plus Prolog

Prolog is the only good programming language. I should know, [my website](https://needleful.net) is [written in Prolog](https://github.com/needleful/nng).

Unfortunately, C is the only useful programming language.

Scientists have been trying to find an answer to this problem for nearly 50 years. Some make their [C more like Prolog](https://doc.rust-lang.org/book/ch19-06-macros.html). Others make their [Prolog more like C](https://prescheme.org/).

I offer a new solution: simply add Prolog and C together. I call it, “C Plus Prolog”, or “C+P” for short.

```prolog
:- include(stdio).

int func main
{ 
	puts("Hello, world!");
	return 0
}.
```

If you're familiar with C, you'll notice this is some sort of weird, bad C. You're mistaken, however. This is valid Prolog, using some [non-standard features of SWI-Prolog](https://www.swi-prolog.org/pldoc/man?section=ext-blockop) for the curly braces.

This Prolog is read as a list of terms, and converted to valid C code. So this:

```prolog
int func main(int var argc, char:ptr:array var argv)
```

Translates to this:

```C
int main(int argc, char*( argv[]))
```

Beyond some obvious changes like `var` and `func` operators, which make the syntax expressible as Prolog, there are some unexpected quirks:

- Symbols starting with capital letters or underscores are for use with Prolog, and won't translate to C unless wrapped in single quotes:

```prolog
int:ptr var string = 'NULL';
'_Bool' var v = true;
```

- As a result of the above, character literals start with `#`:

```prolog
char var c = #c;    % quotes not required for lowercase letters
char var c2 = #'C'; 
char var nl = #'\n';
```

- Operator precedence was maintained for existing Prolog operators. For example, `=` and comparison operators like `==`, `<`, and `>` have the same precedence, so you'll need parentheses where you wouldn't in C:

```prolog
greater = (a > b).
```

This is also why the arrow operator `->` was replaced with `@`, since it's used in a completely different way in Prolog and has a completely different precedence.

C+P:
```prolog
a = this@member;
```

C:

```C
a = this->member;
```

- Complex declarations are very different, mostly because I don't particularly like C's type declarations.

```prolog
char:ptr:array var argv
```

Becomes

```C
char *(argv[])
```

It may also help you remember the difference between `const char *` and `char const *`:

```prolog
const(char):ptr,
char:const(ptr)
```

The examples provide a more complete picture of the syntax.
In the end, it's C but more verbose and particular about semicolons. Not exactly a silver bullet.

Let's introduce the `*=>` operator.

## The `*=>` Operator

Take this snippet from example 04:

```prolog
max(A, B) *=> A > B then A else B.
```

I said C+P does no processing beyond translating terms to C, but it does one small step. The compiler will gather all the terms defined with `*=>`, and then substitute the left-hand for the right-hand in the rest of the code until no rules more apply.

Because this operates on Prolog terms, not text, we don't need any extra parentheses in our `max(A, B)` macro to prevent mishaps with operator precedence. It's inserted into code as a single term, and looks exactly like a function call in use:

```prolog
float var f = max(12.3, 0) + 20;
printf("Should be 32.3: %f\n", f);`
```

It's converted to the following C:
```C
float f=(((12.3>0) ? 12.3 : 0)+20);
printf("Should be 32.3: %f\n", f);
```

Also, I'm tired of adding `\n` at the end of all my `printf` statements. Let's defined another macro, `println`:

```prolog
PrintLn *=> PrintF :-
	PrintLn =.. [println,Format| Args],
	string_concat(Format, "\n", Format2),
	PrintF =.. [printf,Format2| Args].
```

We have full access to Prolog at compile time using `:-`, allowing us to do just about anything.
This macro gets any instance of `println(Format, Args...)` with a string literal `Format`, and converts it to `printf` with a newline appended to `Format`.

Simple enough. Let's implement poor man's generics.

## Poor Man's Generics in C Plus Prolog

Example 05 defines a generic type, `list[T]`, using the following syntax:

```prolog
list[T]
{
	struct list[T] {
		T:ptr:ptr var items
	};

	% Let's also define syntax for type-specific functions, in this case “list[T]:capacity”
	size_t:ptr func list[T]:capacity(list[T] var this)
	{
		...
	};
	...
}.
```

This will work similarly to C++ templates.
For simplicity of implementation, we'll require the user to instantiate the template.

```prolog
declare(list[int]).
declare(list[const(char):ptr]).
```

We could probably do this automatically by scanning the code for any usage of `list[T]` and instantiating the template right above it, but I'll leave that as an exercise for the reader.

We also have some syntax to get a function name with `list[T]:Method`:

```prolog
int func main {
	list[int] var my_ints = list[int]:new(17);

	size_t var size = *(list[int]:capacity(my_ints));
	for(int var i = 0; i < size; i += 1)
	{
		list[int]:append(my_ints, i*i)
	};
	for(int var i = 0; i < size; i+= 1)
	{
		printf("%d squared = %d.\n", i, list[int]:get(my_ints, i))
	};
	return 0
}.
```

Not exactly C++, but it keeps the namespaces clear.

Let's read the macro.

It matches on our template as you might have expected:

```prolog
Name[T] {Body} *=> Comment :-
	\+ground(T), atom(Name),
	atom_concat('//Defined template type: ', Name, Comment),
```
We have a template with a name `Name`, type parameterss `T`, and the body `Body`. 
The macro removes this code and inserts a comment. Everything else is handled in the Prolog world.

```prolog
	assertz(
		declare(Name[Z]) *=> NewBody
```
By God. Our macro `assert`s another macro, `declare(Name[Z])`. It also has conditions:

```prolog
		:- (
			ground(Z),
			T=Z,
			Body=NewBody,
```
Those three lines are the bulk of the macro. It unifies the template type `T` with the real (ground) type `Z`, then returns the body of the template. This is what turns `declare(list[int])` into the code for the type.

But that's not all it does, the macro we're asserting with itself assert more macros:
```prolog
			('*mangled_name'(Name[Z]) *=> ZName),
			assertz(Name[Z] *=> ZName),
			assertz((ZName:Method *=> ZMethod :-
				% Crudely hacking some shorthand for method names
				Method \= ptr, Method \= array,
				(	atom(Method)
				->	MName = Method,
					ZArgs = []
				;	Method =.. [MName|ZArgs]
				),
				('*mangled_name'(ZName:MName) *=> MZ_Name),
				ZMethod =.. [MZ_Name|ZArgs]
			))
		)).
```

This generates the C names for things like `list[int]` and `list[int]:function`. `'*mangled_name'` is itself a macro, but you can just read example 05. The `*` and single quotes are nothing special, they just prevent this macro from accidentally colliding with some user-defined `mangled_name`.

C+P provides no type information we could use for method syntax, like `my_list.append(...)`, instead of `list[int]:append(my_list, ...)`.

We could use macros to gather the types of every variable and function, and automatically swap out method-like syntax for typed functions, but at a certain point I'm just writing a full-on XLang-to-C compiler in the macro system, which is an interesting idea, but I'm employed.

Still, I've provided several other examples of the wonders of C Plus Prolog:
- Example 06 allows for iterating fields of a struct by overloading the `struct` keyword.
- Example 08a can be compiled as either a header or C source, depending on the extension of the output, and then compiled with Example 08b.

I find it strangely compelling to write in C+P, adding features that sound impossible. It's like a puzzle game, which is why I reach for Prolog so frequently.

## Installation and Usage

C Plus Prolog is easy to install. All you need is a C compiler, SWI-Prolog, and this repository. You can figure it out.

Then you can run `cpp.pl` from this repository with the following syntax:
`swipl -s cpp.pl -- <input file> <output file>`

By convention, C Plus Prolog files end in the extension `.c+p`.

Check `test.sh` and `test.ps1` for more example usage, plus a fast way to run all the tests (which are probably not rootkits).

## What is the point of this?

C Plus Prolog is a half-serious exploration of ideas for a programming language. The long-term vision is something with live-editing tools akin to [this demonstration from Bret Victor](https://www.youtube.com/watch?v=PUv66718DII), suitable for games. Imagine viewing some code, having tools to tweak variables visually, and then getting instant feedback while the game is running.

I'd prefer using a statically-typed, compiled language for the compile-time validation and performance, which complicates this sort of dynamic activity.
The way I envisioned it working is by parsing the code and generating the infrastructure to tune it in real-time, without recompiling. For example, pulling all constant values out of a function and turning them into variables that can be configured in-game, simultaneously updating the source text. For more extensive changes, the code could be unloaded and recompiled like you would do with a typical DLL-based plugin, at the cost of freezing the game for a few seconds.

The correct answer is to do what Bret Victor seems to have done, which is to build the tools on top of an existing programming language, but I hate writing parsers and I hate reading specs. One thing I love, however, is using Prolog in places where it's not expected.

C+P is surprisingly close to what I'd need, for how little work I've put into it.
It's a thin wrapper around C, so interop and platform support are easily-solved problems, but Prolog's term rewriting allows for basically whatever code generation I want, with much less effort than most language's macro systems or trying to parse it externally.

Of course, the hard part is making it do all of that live-editing stuff, not building a weird mess of incomprehensible Prolog code. But that part will take much longer, and I'm employed.

The main things I would want for a real take on this language would be the following:

- _Type and scope information._ Most of the limitations on the reflection are from an inability to detect what's what, since the macros operate strictly on the syntax, with no semantic information. Various extensions, like method syntax, would require figuring out things like the type of a value, where it's declared, where it's used, and things like that. I'd probably put this into the compiler, but they could also be a particularly complex “macro”.
- _Better error messaging._ I've put in almost no error handling or messaging, and that makes troubleshooting code very difficult. Typically, I just check the generated C and figure out what went wrong from there. The fact that the compiler errors will only refer to C is also a bit troublesome, but it's usually pretty easy to cross-reference the code and see where it was generated.
- _Less confusing syntax._ There are various places where C and Prolog syntax interact poorly, like the akward `}.` to end functions, or the fact it's invalid to put a semicolon after the last statement in a block. I could either make the syntax more Prolog-esque, perhaps something more like Erlang, or make my own term rewriting language and design whatever syntax primitives I want. But if I made my own, at that point making a parser and code generator for an existing language would probably be a more reasonable option.
- _An alternate, dynamic runtime._ If the goal is to make something that can be used like Bret Victor's demonstration, I'd want an environment where I can freely edit and rearrange ALL the code, not just tweak some numbers. This is obviously a lot of work, not just making the runtime and compiler, but losing access to the C standard library. An option is a C interpreter, but there's only a handful of them, and I'm not sure they support the sort of features I'm looking for. Maybe extending one would be an option.

I think to know for sure if C+P has any legs, I'll need to make a prototype of this editing system with it.

Until then, it's a fun little oddity, an entertaining puzzle to make things in.