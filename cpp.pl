:- module(cpp, [
	compile/2
]).

:- use_module(cpp_reader).
:- use_module(cpp_writer).

compile(In, Out) :-
	read_file(In, Terms),
	write_file(Terms, Out).