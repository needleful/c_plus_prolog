:- module(cpp_writer, [
	write_file/2
]).

:- use_module(cpp_common).
:- use_module(cpp_ops).

write_file(Terms, File) :-
	setup_call_cleanup(
		open(File, write, S, [encoding(utf8)]),
		write_lines((S,""), Terms),
		close(S)).

write_lines(_, []).
write_lines((S,I), [Term|Others]) :-
	format_line((S,I), Term),
	write_lines((S,I), Others).

format_line((S,I), Term) :-
	write(S, I),
	format_cpp_line((S,I), Term),
	nl(S).

format_cpp_line((S,I), (:- Directive)) :- !,
	Directive =.. [Name| Args],
	format_directive((S,I), Name, Args).

format_cpp_line((S,I), (Head => Body)) :-
	format_block_head((S,I), Head),
	format_block((S,I), Body),
	format_block_tail((S,I), Head).

format_cpp_line((S,I), if(then(Cond, Result))) :-
	write(S, 'if'),
	in_parens(S,
		format_cpp_exp((S,I), Cond)),
	format_block((S,I), Result).

format_cpp_line((S,I), return(Value)) :-
	write(S, "return "),
	format_cpp_exp((S,I), Value),
	write(S, ";").

format_cpp_line((S,_), Atom) :- atom(Atom),
	write(S, Atom),
	% Standalone atoms are either a keyword or a function
	% taking zero arguments.
	(	c_standalone(Atom)
	->	write(S, ";")
	;	write(S, "();")
	).

format_cpp_line((S,I), Functor) :-
	(	format_block_head((S,I), Functor),
		!,
		format_block_tail((S,I), Functor)
	;	format_cpp_exp((S,I), Functor)),
	write(S, ";").

format_cpp_line(_, Unknown) :- !,
	format("ERROR: {~W} is not valid C+P code.~n",
		[Unknown, [character_escapes(true), quoted(true)]]),
	fail.

format_block_head((S,I), func(Name)) :-
	format_block_head((S,I), func(void, Name)).

format_block_head((S,I), func(Type, Fn)) :-
	format(S, "~w ", [Type]),
	(	atom(Fn)
	->	format(S, "~w()", [Fn])
	;	Fn =.. [Name|Args],
		write(S, Name),
		in_parens(S,
			format_cpp_args((S,I), Args))
	).

format_block_head((S,_), Head) :- Head =.. [Type, Name],
	c_type_block(Type),
	format(S, "typedef ~w ~w_t", [Type, Name]).

format_block_head((S,_), Head) :- c_type_block(Head),
	write(S, Head). 

format_block_tail((S,_), Head) :-
	Head =.. [Type, Name],
	c_type_block(Type),
	format(S, " ~w;", Name).
format_block_tail(_,_).

format_block((S,I), Block) :-
	write(S, " {"),
	nl(S),
	string_concat(I, "\t", I2),
	write_func_lines((S,I2), Block),
	write(S, I),
	write(S, "}").

format_directive(_, include, []).
format_directive((S,I), include, [Name|Names]) :-
	(	Name = local(LocalName)
	->	format(S, "#include \"~w.h\"~n", [LocalName])
	;	format(S, "#include <~w.h>~n", [Name])
	),
	format_directive((S,I), include, Names).

write_func_lines(SS, First;Next) :-
	format_line(SS, First),
	write_func_lines(SS, Next).

write_func_lines(SS, Final) :-
	format_line(SS, Final).

format_cpp_args(_, [], _).
format_cpp_args((S,I), [V|A]) :- !,
	(	V = var(Type, Args)
	->	format_var((S,I), Type, Args, ", ")
	;	format_type((S,I), V)
	),
	(	A = [_|_]
	->	write(S, ", "),
		format_cpp_args((S,I), A)
	;	true
	).

format_type((S,I), Type) :-
	format_var((S,I), Type, [], " ").

format_var((S,I), Type, (Name,Names), Sep) :-
	format_var((S,I), Type, Name, _),
	write(S, Sep),
	format_var((S,I), Type, Names, Sep).
format_var(SS, Type, Name, _) :-
	format_single_var(SS, Type, Name).

format_single_var(SS, BaseType:Special, Name) :-
	format_qual_type(SS, BaseType, false),
	format_special_var(SS, Special, Name).
format_single_var(SS, Type, []) :- !,
	format_qual_type(SS, Type, false).
format_single_var((S,I), Type, Name) :- !,
	format_qual_type((S,I), Type, false),
	write(S, " "),
	format_cpp_exp((S,I), Name).


format_qual_type((S,_), ptr, CouldBePointer) :-
	(	CouldBePointer = true
	->	write(S, "*")
	;	write(S, ptr)
	).
format_qual_type((S,_), Atom, _) :- atom(Atom),
	write(S, Atom).
format_qual_type(SS, atomic(Type), CBP) :-
	format_qual_type(SS, '_Atomic'(Type), CBP).
format_qual_type(SS, Head => Type, _) :-
	format_cpp_line(SS, Head => Type).
format_qual_type((S,I), Type, CBP) :- Type =..[Qual,SubType],
	write(S, Qual),
	write(S, " "),
	format_qual_type((S,I), SubType, CBP).

format_special_var((S,I), First:Next, Name) :-
	type_prefix((S,I), First),
	format_special_var((S,I), Next, Name),
	type_suffix(S, First).

format_special_var((S,I), Last, Name) :-
	type_prefix((S,I), Last),
	write(S, Name),
	type_suffix(S, Last).

type_prefix((S,_), ptr) :- write(S, "*(").
type_prefix((S,_), A) :- (A = array; A = array(_)),
	write(S, "(").
type_prefix(SS, Qual) :- format_qual_type(SS, Qual, true).

type_suffix(S, array) :- write(S, ")[]").
type_suffix(S, array(Len)) :- format(S, ")[~w]", [Len]).
type_suffix(S, _) :- write(S, ")").

format_list(_, [], _).
format_list(S, [Last], _) :-
	write(S, Last).
format_list(S, [H|T], Sep) :-
	write(S, H),
	write(S, Sep),
	format_list(S, T, Sep).

format_cpp_exp((S,_), A) :- atomic(A),
	write_term(S, A, [quoted(true)]).

format_cpp_exp((S,I), Array:Index) :-
	format_cpp_exp((S,I), Array),
	write(S, "["),
	format_cpp_exp((S,I), Index),
	write(S, "]").

format_cpp_exp(SS, var(Type, Name)) :-
	format_var(SS, Type, Name, "; ").

format_cpp_exp((S,I), then(Cond, else(IfThen, IfElse))) :-
	in_parens(S, 
	(	format_cpp_exp((S,I), Cond),
		write(S, " ? "),
		format_cpp_exp((S,I), IfThen),
		write(S, " : "),
		format_cpp_exp((S,I), IfElse))).

format_cpp_exp((S,I), '<-'(Type, Exp)) :-
	in_parens(S, format_type((S,I), Type)),
	format_cpp_exp((S,I), Exp).

format_cpp_exp((S,I), {Val}) :-
	write(S, "{"),
	format_struct_literal((S,I), Val),
	write(S, "}").

format_cpp_exp((S,I), Fn) :- Fn =.. [Name|Args],
	format_functor((S,I), Name, Args).

format_struct_literal((S,I), (A,B)) :-
	format_struct_field((S,I), A),
	write(S, ", "),
	format_struct_literal((S,I), B).
format_struct_literal((S,I), A) :-
	format_struct_field((S,I), A). 

format_struct_field((S,I), Name=Value) :-
	format(S, ".~w = ", [Name]),
	format_cpp_exp((S,I), Value).

format_struct_field((S,I), V) :-
	format_cpp_exp((S,I), V).

format_functor((S,I), Op, [A, B]) :- c_op(Op, Type),
	format_bin_op((S,I), Type, Op, A, B).

format_functor((S,I), Op, [A, B]) :- op_rename(Op, COp),
	c_op(COp, Type),
	format_bin_op((S,I), Type, COp, A, B).

format_functor((S,I), Fn, Args) :-
	write(S, Fn),
	in_parens(S,
		exp_list((S,I), Args)).

format_bin_op((S,I), Type, Op, A, B) :-
	(	(Type = assign; Type = control)
	->	format_cpp_exp((S,I), A),
		write(S, Op),
		format_cpp_exp((S,I), B)
	;
		in_parens(S,
		(	format_cpp_exp((S,I), A),
			write(S, Op),
			format_cpp_exp((S,I), B)))
	).

exp_list(_, []).
exp_list((S,I), [A]) :-
	format_cpp_exp((S,I), A).
exp_list((S,I), [A|Args]) :-
	format_cpp_exp((S,I), A),
	write(S, ", "),
	exp_list((S,I), Args).

in_parens(S, Format) :-
	write(S, "("),
	call(Format),
	write(S, ")").