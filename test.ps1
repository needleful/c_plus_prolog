param([string] $cc)

swipl -s cpp.pl -- .\examples\00_hello_world.c+p .\output\00_hello_world.c
swipl -s cpp.pl -- .\examples\01_functions.c+p .\output\01_functions.c
swipl -s cpp.pl -- .\examples\02_structs.c+p .\output\02_structs.c
swipl -s cpp.pl -- .\examples\03_parser.c+p .\output\03_parser.c
swipl -s cpp.pl -- .\examples\04_macros.c+p .\output\04_macros.c
swipl -s cpp.pl -- .\examples\05_generics.c+p .\output\05_generics.c
swipl -s cpp.pl -- .\examples\06_reflection.c+p .\output\06_reflection.c
swipl -s cpp.pl -- .\examples\07_directives.c+p .\output\07_directives.c
swipl -s cpp.pl -- .\examples\08a_headers.c+p .\output\08a_headers.c
swipl -s cpp.pl -- .\examples\08a_headers.c+p .\output\08a_headers.h
swipl -s cpp.pl -- .\examples\08b_headers.c+p .\output\08b_headers.c
swipl -s cpp.pl -- .\examples\09_switch.c+p .\output\09_switch.c
swipl -s cpp.pl -- .\examples\10_function_pointers.c+p .\output\10_function_pointers.c
swipl -s cpp.pl -- .\examples\11_variadic.c+p .\output\11_variadic.c

echo "Compiling C..."
iex "$($cc) -Wall .\output\00_hello_world.c -o .\output\00_hello_world.exe"
iex "$($cc) -Wall .\output\01_functions.c -o .\output\01_functions.exe"
iex "$($cc) -Wall .\output\02_structs.c -o .\output\02_structs.exe"
iex "$($cc) -Wall ./output/03_parser.c -o ./output/03_parser.exe"
iex "$($cc) -Wall ./output/04_macros.c -o ./output/04_macros.exe"
iex "$($cc) -Wall ./output/05_generics.c -o ./output/05_generics.exe"
iex "$($cc) -Wall ./output/06_reflection.c -o ./output/06_reflection.exe"
iex "$($cc) -Wall ./output/07_directives.c -o ./output/07_directives.exe"
iex "$($cc) -Wall ./output/08a_headers.c -c -o ./output/08a_headers.o"
iex "$($cc) -Wall ./output/08b_headers.c -c -o ./output/08b_headers.o"
iex "$($cc) -Wall ./output/08a_headers.o ./output/08b_headers.o  -o ./output/08_headers.exe"
iex "$($cc) -Wall ./output/09_switch.c -o ./output/09_switch.exe"
iex "$($cc) -Wall ./output/10_function_pointers.c -o ./output/10_function_pointers.exe"
iex "$($cc) -Wall ./output/11_variadic.c -o ./output/11_variadic.exe"

echo "Running code..."
.\output\00_hello_world.exe
.\output\01_functions.exe 45 62
.\output\02_structs.exe
.\output\03_parser.exe
.\output\04_macros.exe
.\output\05_generics.exe
.\output\06_reflection.exe
.\output\07_directives.exe
.\output\08_headers.exe
.\output\09_switch.exe
.\output\10_function_pointers.exe
.\output\11_variadic.exe