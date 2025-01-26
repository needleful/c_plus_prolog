if [ ! -d "./output" ]; then
	echo Creating output folder.
	mkdir output
fi
echo "Converting to C..."
swipl -s cpp.pl -- ./examples/00_hello_world.c+p ./output/00_hello_world.c
swipl -s cpp.pl -- ./examples/01_functions.c+p ./output/01_functions.c
swipl -s cpp.pl -- ./examples/02_structs.c+p ./output/02_structs.c
swipl -s cpp.pl -- ./examples/03_parser.c+p ./output/03_parser.c
swipl -s cpp.pl -- ./examples/04_macros.c+p ./output/04_macros.c
swipl -s cpp.pl -- ./examples/05_generics.c+p ./output/05_generics.c
swipl -s cpp.pl -- ./examples/06_reflection.c+p ./output/06_reflection.c
swipl -s cpp.pl -- ./examples/07_directives.c+p ./output/07_directives.c

echo "Compiling C..."
gcc -Wall ./output/00_hello_world.c -o ./output/00_hello_world.out
gcc -Wall ./output/01_functions.c -o ./output/01_functions.out
gcc -Wall ./output/02_structs.c -o ./output/02_structs.out
gcc -Wall ./output/03_parser.c -o ./output/03_parser.out
gcc -Wall ./output/04_macros.c -o ./output/04_macros.out
gcc -Wall ./output/05_generics.c -o ./output/05_generics.out
gcc -Wall ./output/06_reflection.c -o ./output/06_reflection.out
gcc -Wall ./output/07_directives.c -o ./output/07_directives.out

echo "Running code..."
./output/00_hello_world.out
./output/01_functions.out 3 4
./output/02_structs.out
./output/03_parser.out
./output/04_macros.out
./output/05_generics.out
./output/06_reflection.out
./output/07_directives.out