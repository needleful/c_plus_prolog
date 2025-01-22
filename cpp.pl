#!/usr/bin/env swipl

:- module(cpp, [
	compile/2
]).

:- use_module(cpp_reader).
:- use_module(cpp_writer).

:- initialization(main, main).

main([In, Out]) :-
	compile(In, Out).
main(_) :-
	write("Usage: cpp <in file> <out file>").

compile(In, Out) :-
	retractall(cpp_reader:custom_data(_)),
	retractall(cpp_reader:consulted(_)),
	retractall(cpp_reader:'*=>'(_,_)),
	read_file(In, Terms),
	write_file(Terms, Out).