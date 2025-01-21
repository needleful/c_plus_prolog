:- module(cpp_writer, [
	write_file/2,
	format_cpp_line/2
]).

:- use_module(cpp_reader).

write_file(Terms, File) :-
	setup_call_cleanup(
		open(File, write, Stream, [encoding(utf8)]),
		encode_lines(Terms, Stream),
		close(Stream)).

encode_lines([], _).
encode_lines([Term|Others], Stream) :-
	encode_line(Term, Stream, ""),
	encode_lines(Others, Stream).

encode_line(Term, Stream, Indent) :-
	format_cpp_line(Term, Line),
	format(Stream, "~w~w~n", Indent, Line).

format_cpp_line((:- include(Name)), String) :- !,
	with_output_to(string(String),
		(	Name = local(LocalName)
		->	format("#include \"~w.h\"", [LocalName])
		;	format("#include <~w.h>", [Name]))).

format_cpp_line(Unknown, _) :- !,
	format("ERROR: `~W` is not valid C+P code.~n", [Unknown]),
	fail.