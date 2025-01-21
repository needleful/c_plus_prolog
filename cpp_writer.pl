:- module(cpp_writer, [
	write_file/2
]).

:- use_module(cpp_common).
:- use_module(cpp_ops).

write_file(Terms, File) :-
	setup_call_cleanup(
		open(File, write, Stream, [encoding(utf8)]),
		write_lines(Stream, Terms, ""),
		close(Stream)).

write_lines(_, [], _).
write_lines(Stream, [Term|Others], Indent) :-
	format_line(Stream, Term, Indent),
	write_lines(Stream, Others, Indent).

format_line(Output, Term, Indent) :-
	write(Output, Indent),
	format_cpp_line(Output, Term, Indent),
	nl(Output).

format_cpp_line(S, (:- Directive), _) :- !,
	Directive =.. [Name| Args],
	format_directive(S, Name, Args).

format_cpp_line(S, (Head => Body), Indent) :-
	format_block_head(S, Head),
	format_block(S, Indent, Body),
	format_block_tail(S, Head).

format_cpp_line(S, if(then(Cond, Result)), Indent) :-
	write(S, 'if'),
	in_parens(S,
		format_cpp_exp(S, Cond)),
	format_block(S, Indent, Result).

format_cpp_line(S, return(Value), _) :-
	write(S, "return "),
	format_cpp_exp(S, Value),
	write(S, ";").

format_cpp_line(S, Atom, _) :- atom(Atom),
	write(S, Atom),
	% Standalone atoms are either a keyword or a function
	% taking zero arguments.
	(	c_standalone(Atom)
	->	write(S, ";")
	;	write(S, "();")
	).

format_cpp_line(S, Functor, _) :-
	(	format_block_head(S, Functor),
		!
	;	format_cpp_exp(S, Functor)),
	write(S, ";").

format_cpp_line(_, Unknown, _) :- !,
	format("ERROR: {~W} is not valid C+P code.~n",
		[Unknown, [character_escapes(true), quoted(true)]]),
	fail.

format_block_head(S, struct(Name)) :-
	format(S, "typedef struct ~w_t", [Name]).

format_block_head(S, union(Name)) :-
	format(S, "typedef union ~w_t", [Name]).

format_block_head(S, func(Name)) :-
	format_block_head(S, func(void, Name)).

format_block_head(S, func(Type, Fn)) :-
	format(S, "~w ", [Type]),
	(	atom(Fn)
	->	format(S, "~w()", [Fn])
	;	Fn =.. [Name|Args],
		write(S, Name),
		in_parens(S,
			format_cpp_args(S, Args))
	).

format_block_tail(S, struct(Name)) :-
	format(S, " ~w;", Name).
format_block_tail(S, union(Name)) :-
	format(S, " ~w;", Name).
format_block_tail(_,_).

format_block(S, Indent, Block) :-
	write(S, " {"),
	nl(S),
	string_concat(Indent, "\t", Indent2),
	write_func_lines(S, Block, Indent2),
	write(S, Indent),
	write(S, "}").

format_directive(_, include, []).
format_directive(S, include, [Name|Names]) :-
	(	Name = local(LocalName)
	->	format(S, "#include \"~w.h\"~n", [LocalName])
	;	format(S, "#include <~w.h>~n", [Name])
	),
	format_directive(S, include, Names).

write_func_lines(S, First;Next, Indent) :-
	format_line(S, First, Indent),
	write_func_lines(S, Next, Indent).

write_func_lines(S, Final, Indent) :-
	format_line(S, Final, Indent).

format_cpp_args(_, []).
format_cpp_args(S, [V|A]) :- !,
	(	V = var(Type, Args)
	->	format_var(S, Type, Args, ", ")
	;	format_type(S, V)
	),
	(	A = [_|_]
	->	write(S, ", "),
		format_cpp_args(S, A)
	;	true
	).

format_type(S, Type) :-
	format_var(S, Type, [], " ").

format_var(S, Type, (Name,Names), Sep) :-
	format_var(S, Type, Name, _),
	write(S, Sep),
	format_var(S, Type, Names, Sep).
format_var(S, Type, Name, _) :-
	format_single_var(S, Type, Name).

format_single_var(S, BaseType:Special, Name) :-
	format_qual_type(S, BaseType, false),
	format_special_var(S, Special, Name).
format_single_var(S, Type, []) :- !,
	format_qual_type(S, Type, false).
format_single_var(S, Type, Name) :- !,
	format_qual_type(S, Type, false),
	write(S, " "),
	format_cpp_exp(S, Name).

format_qual_type(S, ptr, CouldBePointer) :-
	(	CouldBePointer = true
	->	write(S, "*")
	;	write(S, ptr)
	).
format_qual_type(S, Atom, _) :- atom(Atom),
	write(S, Atom).
format_qual_type(S, atomic(Type), CBP) :-
	format_qual_type(S, '_Atomic'(Type), CBP).
format_qual_type(S, Type, CBP) :- Type =..[Qual,SubType],
	write(S, Qual),
	write(S, " "),
	format_qual_type(S, SubType, CBP).

format_special_var(S, First:Next, Name) :-
	type_prefix(S, First),
	format_special_var(S, Next, Name),
	type_suffix(S, First).

format_special_var(S, Last, Name) :-
	type_prefix(S, Last),
	write(S, Name),
	type_suffix(S, Last).

type_prefix(S, ptr) :- write(S, "*(").
type_prefix(S, A) :- (A = array; A = array(_)),
	write(S, "(").
type_prefix(S, Qual) :- format_qual_type(S, Qual, true).

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

format_cpp_exp(S, A) :- atomic(A),
	write_term(S, A, [quoted(true)]).

format_cpp_exp(S, Array:Index) :-
	format_cpp_exp(S, Array),
	write(S, "["),
	format_cpp_exp(S, Index),
	write(S, "]").

format_cpp_exp(S, var(Type, Name)) :-
	format_var(S, Type, Name, "; ").

format_cpp_exp(S, then(Cond, else(IfThen, IfElse))) :-
	in_parens(S, 
	(	format_cpp_exp(S, Cond),
		write(S, " ? "),
		format_cpp_exp(S, IfThen),
		write(S, " : "),
		format_cpp_exp(S, IfElse))).

format_cpp_exp(S, '<-'(Type, Exp)) :-
	in_parens(S, format_type(S, Type)),
	format_cpp_exp(S, Exp).

format_cpp_exp(S, {Val}) :-
	write(S, "{"),
	format_struct_literal(S, Val),
	write(S, "}").

format_cpp_exp(S, Fn) :- Fn =.. [Name|Args],
	format_functor(S, Name, Args).

format_struct_literal(S, (A,B)) :-
	format_struct_field(S, A),
	write(S, ", "),
	format_struct_literal(S, B).
format_struct_literal(S, A) :-
	format_struct_field(S, A). 

format_struct_field(S, Name=Value) :-
	format(S, ".~w = ", [Name]),
	format_cpp_exp(S, Value).

format_struct_field(S, V) :-
	format_cpp_exp(S, V).

format_functor(S, Op, [A, B]) :- c_op(Op, Type),
	format_bin_op(S, Type, Op, A, B).

format_functor(S, Op, [A, B]) :- op_rename(Op, COp),
	c_op(COp, Type),
	format_bin_op(S, Type, COp, A, B).

format_functor(S, Fn, Args) :-
	write(S, Fn),
	in_parens(S,
		exp_list(S, Args)).

format_bin_op(S, Type, Op, A, B) :-
	(	(Type = assign; Type = control)
	->	format_cpp_exp(S, A),
		write(S, Op),
		format_cpp_exp(S, B)
	;
		in_parens(S,
		(	format_cpp_exp(S, A),
			write(S, Op),
			format_cpp_exp(S, B)))
	).

exp_list(_, []).
exp_list(S, [A]) :-
	format_cpp_exp(S, A).
exp_list(S, [A|Args]) :-
	format_cpp_exp(S, A),
	write(S, ", "),
	exp_list(S, Args).

in_parens(S, Format) :-
	write(S, "("),
	call(Format),
	write(S, ")").