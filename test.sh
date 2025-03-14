if [ ! -d "./output" ]; then
	echo Creating output folder.
	mkdir output
fi
rm output/*.c
rm output/*.out

echo "Converting to C..."
swipl -s cpp.pl -- ./examples/00_hello_world.c+p ./output/00_hello_world.c
swipl -s cpp.pl -- ./examples/01_functions.c+p ./output/01_functions.c
swipl -s cpp.pl -- ./examples/02_structs.c+p ./output/02_structs.c
swipl -s cpp.pl -- ./examples/03_parser.c+p ./output/03_parser.c
swipl -s cpp.pl -- ./examples/04_macros.c+p ./output/04_macros.c
swipl -s cpp.pl -- ./examples/05_generics.c+p ./output/05_generics.c
swipl -s cpp.pl -- ./examples/06_reflection.c+p ./output/06_reflection.c
swipl -s cpp.pl -- ./examples/07_directives.c+p ./output/07_directives.c
swipl -s cpp.pl -- ./examples/08a_headers.c+p ./output/08a_headers.c
swipl -s cpp.pl -- ./examples/08a_headers.c+p ./output/08a_headers.h
swipl -s cpp.pl -- ./examples/08b_headers.c+p ./output/08b_headers.c
swipl -s cpp.pl -- ./examples/09_switch.c+p ./output/09_switch.c
swipl -s cpp.pl -- ./examples/10_function_pointers.c+p ./output/10_function_pointers.c
swipl -s cpp.pl -- ./examples/11_variadic.c+p ./output/11_variadic.c

echo "Compiling C..."
gcc -Wall ./output/00_hello_world.c -g -o ./output/00_hello_world.out
gcc -Wall ./output/01_functions.c -g -o ./output/01_functions.out
gcc -Wall ./output/02_structs.c -g -o ./output/02_structs.out
gcc -Wall ./output/03_parser.c -g -o ./output/03_parser.out
gcc -Wall ./output/04_macros.c -g -o ./output/04_macros.out
gcc -Wall ./output/05_generics.c -g -o ./output/05_generics.out
gcc -Wall ./output/06_reflection.c -g -o ./output/06_reflection.out
gcc -Wall ./output/07_directives.c -g -o ./output/07_directives.out
gcc -Wall ./output/08a_headers.c -c -g -o ./output/08a_headers.o
gcc -Wall ./output/08b_headers.c -c -g -o ./output/08b_headers.o
gcc -Wall ./output/08a_headers.o ./output/08b_headers.o  -g -o ./output/08_headers.out
gcc -Wall ./output/09_switch.c -g -o ./output/09_switch.out
gcc -Wall ./output/10_function_pointers.c -g -o ./output/10_function_pointers.out
gcc -Wall ./output/11_variadic.c -g -o ./output/11_variadic.out

echo "Running code..."
./output/00_hello_world.out
./output/01_functions.out 3 4
./output/02_structs.out
./output/03_parser.out
./output/04_macros.out
./output/05_generics.out
./output/06_reflection.out
./output/07_directives.out
./output/08_headers.out
./output/09_switch.out
./output/10_function_pointers.out
./output/11_variadic.out