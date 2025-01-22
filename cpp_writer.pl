:- module(writer, [
	write_file/2,
	this_file/2
]).

:- use_module(library(lists)).
:- use_module(cpp_common).
:- use_module(cpp_reader).
:- use_module(cpp_ops).

:- dynamic this_file/2.

write_file(Terms, File) :-
	retractall(this_file),
	consult_files(Terms),
	expand_macros(Terms, NewTerms),
	setup_call_cleanup(
		open(File, write, S, [encoding(utf8)]),
		(	asserta(this_file(File, S)),
			write_lines((S,""), NewTerms)
		),
		close(S)), !.

consult_files([]).
consult_files([:- consult(File)|X]) :-
	consult(File),
	consult_files(X).
consult_files([_|X]) :- consult_files(X).

expand_macros(Var, Var) :- var(Var).
expand_macros('*=>'(_, _), '*=>').
expand_macros('*=>'(_, _) :- _, '*=>').

expand_macros(Term, NewTerm) :-
	% Expand sub-items
	(	atomic(Term),
		Term2 = Term,
		!
	;	is_list(Term),
		!, 
		maplist(expand_macros, Term, Term2)
	;	Term =..[Fn|Args],
		!,
		expand_macros(Fn, NewFn),
		expand_macros(Args, NewArgs),
		Term2 =.. [NewFn|NewArgs]
	;	Term2 = Term,
		!
	),
	% Apply Macros until they no longer apply
	(	\+ var(Term2),
		'*=>'(Term2, Term3)
	->	expand_macros(Term3, NewTerm)
	;	NewTerm = Term2
	).

write_lines(_, []).
write_lines((S,I), [Term|Others]) :-
	expand_macros(Term, ETerm),
	indented_line((S,I), ETerm),
	write_lines((S,I), Others).

indented_line(SS, A;B) :-
	indented_line(SS, A),
	indented_line(SS, B).

indented_line((S,I), Term) :-
	write(S, I),
	plain_line((S,I), Term),
	nl(S).

plain_line(SS, (:- Directive)) :- !,
	write_directive(SS, Directive).

plain_line(_, '*=>').

plain_line((S,I), (Head => Body)) :-
	block_head((S,I), Head),
	functor(Head, Type, _),
	block((S,I), Type, Body),
	block_tail((S,I), Head).

plain_line((S,I), do(H,B)) :- H =..[Type|Do],
	write(S, Type),
	in_parens(S,
		exp_list((S,I), Do)),
	block((S,I), do, B).

plain_line(SS, then(C,B)) :-
	cond_block(SS, then(C,B)).

plain_line((S,I), return(Value)) :-
	write(S, "return "),
	exp((S,I), Value),
	write(S, ";").

plain_line((S,_), Atom) :- atom(Atom),
	write(S, Atom),
	% Standalone atoms are either a keyword or a function
	% taking zero arguments.
	(	c_standalone(Atom)
	->	write(S, ";")
	;	write(S, "();")
	).

plain_line((S,I), Functor) :-
	(	block_head((S,I), Functor),
		!,
		block_tail((S,I), Functor)
	;	exp((S,I), Functor)),
	write(S, ";").

plain_line(_, Unknown) :- !,
	format("ERROR: {~W} is not valid C+P code.~n",
		[Unknown, [character_escapes(true), quoted(true)]]),
	fail.

cond_block((S,I), then(Cond, Result)) :-
	write(S, 'if '),
	in_parens(S,
		exp((S,I), Cond)),
	cond_block((S,I), Result).

cond_block((S,I), else(IfTrue, IfFalse)) :-
	cond_block((S,I),IfTrue),
	write(S, ' else '),
	cond_block((S,I), IfFalse).

cond_block(SS, B) :-
	block(SS, if, B).

block_head((S,I), func(Name)) :-
	block_head((S,I), func(void, Name)).

block_head((S,I), func(Type, Fn)) :-
	type((S,I), Type),
	write(S, " "),
	(	atom(Fn)
	->	format(S, "~w()", [Fn])
	;	Fn =.. [Name|Args],
		write(S, Name),
		in_parens(S,
			args((S,I), Args))
	).

block_head((S,_), Head) :- Head =.. [Type, Name],
	c_type_block(Type),
	format(S, "typedef ~w ~w_t", [Type, Name]).

block_head((S,_), Head) :- c_type_block(Head),
	write(S, Head). 

block_tail((S,_), Head) :-
	Head =.. [Type, Name],
	c_type_block(Type),
	format(S, " ~w;", Name).
block_tail(_,_).

block((S,I), Type, Block) :-
	write(S, " {"),
	nl(S),
	string_concat(I, "\t", I2),
	(	Type=enum
	->	write_enum((S,I2), Block)
	;	write_func_lines((S,I2), Block)
	),
	write(S, I),
	write(S, "}").

write_directive((S,I), Directive) :-
	Directive =.. [Name| Args],
	(	directive((S,I), Name, Args)
	;	call(Directive)
	;	format('WARNING: Directive failed: `~w`~n', [Directive])).

directive(_, include, []).
directive((S,I), include, [Name|Names]) :-
	(	Name = local(LocalName)
	->	format(S, "#include \"~w.h\"~n", [LocalName])
	;	format(S, "#include <~w.h>~n", [Name])
	),
	directive((S,I), include, Names).

write_func_lines(SS, First;Next) :-
	indented_line(SS, First),
	write_func_lines(SS, Next).

write_func_lines(SS, Final) :-
	indented_line(SS, Final).

write_enum((S,I), First;Next) :-
	write_enum_value((S,I), First),
	write(S, ",\n"),
	write_enum((S,I), Next).

write_enum((S,I), Final) :-
	write_enum_value((S,I), Final),
	nl(S).

write_enum_value((S,I), Name) :- atom(Name),
	write(S, I),
	write(S, Name).
write_enum_value((S,I), Name=Value) :- atom(Name),
	write_enum_value((S,I), Name),
	write(S, " = "),
	exp(Value). 

args(_, [], _).
args((S,I), [V|A]) :- !,
	(	V = var(Type, Args)
	->	var((S,I), Type, Args, ", ")
	;	type((S,I), V)
	),
	(	A = [_|_]
	->	write(S, ", "),
		args((S,I), A)
	;	true
	).

type((S,I), Type) :-
	var((S,I), Type, [], " ").

var((S,I), Type, (Name,Names), Sep) :-
	var((S,I), Type, Name, _),
	write(S, Sep),
	var((S,I), Type, Names, Sep).
var(SS, Type, Name, _) :-
	single_var(SS, Type, Name).

single_var(SS, (Base:Type):Sp, Name) :- !,
	single_var(SS, Base:Type:Sp, Name).
single_var(SS, BaseType:Special, Name) :- !,
	qual_type(SS, BaseType, false),
	special_var(SS, Special, Name).
single_var(SS, Type, []) :- !,
	qual_type(SS, Type, false).
single_var((S,I), Type, Name) :- !,
	qual_type((S,I), Type, false),
	write(S, " "),
	exp((S,I), Name).


qual_type((S,_), ptr, CouldBePointer) :-
	(	CouldBePointer = true
	->	write(S, "*")
	;	write(S, ptr)
	).

qual_type((S,_), Atom, _) :- atom(Atom),
	write(S, Atom).
qual_type((S,I), typeof(Exp), _) :-
	write(S, 'typeof'),
	in_parens(S,
		exp((S,I), Exp)).
qual_type(SS, atomic(Type), CBP) :-
	qual_type(SS, '_Atomic'(Type), CBP).
qual_type(SS, Head => Type, _) :-
	plain_line(SS, Head => Type).
qual_type((S,I), Type, CBP) :- Type =..[Qual,SubType],
	write(S, Qual),
	write(S, " "),
	qual_type((S,I), SubType, CBP).
qual_type(_, Type, _) :- !,
	throw(error(domain_error('A valid C+P Type Term', Type), qual_type/3)).

special_var(SS, (A:B):C,Name) :-
	special_var(SS, A:B:C, Name).

special_var((S,I), First:Next, Name) :-
	type_prefix((S,I), First),
	(	Name = []
	->	special_var((S,I), Next, Name)
	;	in_parens(S,
			special_var((S,I), Next, Name))
	),
	type_suffix(S, First).

special_var((S,I), Last, Name) :-
	type_prefix((S,I), Last),
	(	Name = []
	;	write(S, " "), 
		write(S, Name)),
	type_suffix(S, Last).

type_prefix((S,_), ptr) :- write(S, "*").
type_prefix(_, array).
type_prefix(_, array(_)).
type_prefix(SS, Qual) :- qual_type(SS, Qual, true).
type_prefix(_,_).

type_suffix((S,_), array) :- write(S, "[]").
type_suffix(S, array(Len)) :- format(S, "[~w]", [Len]).
type_suffix(_, _).

list(_, [], _).
list(S, [Last], _) :-
	write(S, Last).
list(S, [H|T], Sep) :-
	write(S, H),
	write(S, Sep),
	list(S, T, Sep).

exp((S,_), A) :- atom(A),
	write(S, A).

exp((S,_), A) :- atomic(A),
	write_term(S, A, [quoted(true)]).

exp(SS, Type:Attrib) :-
	type(SS, Type:Attrib).

exp((S,I), [](Index, Array)) :-
	exp((S,I), Array),
	write(S, "["),
	exp_list((S,I), Index),
	write(S, "]").

exp(SS, var(Type, Name)) :-
	var(SS, Type, Name, "; ").

exp((S,I), then(Cond, else(IfThen, IfElse))) :-
	in_parens(S, 
	(	exp((S,I), Cond),
		write(S, " ? "),
		exp((S,I), IfThen),
		write(S, " : "),
		exp((S,I), IfElse))).

exp((S,I), as(Exp, Type)) :-
	in_parens(S, (
		in_parens(S, 
			type((S,I), Type)),
		exp((S,I), Exp))).

exp((S,I), {Val}) :-
	write(S, "{"),
	struct_literal((S,I), Val),
	write(S, "}").

exp((S,I), Fn) :- Fn =.. [Name|Args],
	cpp_functor((S,I), Name, Args).

struct_literal((S,I), (A,B)) :-
	struct_field((S,I), A),
	write(S, ", "),
	struct_literal((S,I), B).
struct_literal((S,I), A) :-
	struct_field((S,I), A). 

struct_field((S,I), Name=Value) :-
	format(S, ".~w = ", [Name]),
	exp((S,I), Value).

struct_field(SS, V) :-
	exp(SS, V).

cpp_functor(SS, Op, [A]) :- c_op(Op, Type),
	unary_op(SS, Op, Type, A).

cpp_functor(SS, Op, [A, B]) :- c_op(Op, Type),
	bin_op(SS, Type, Op, A, B).

cpp_functor(SS, Op, [A]) :- op_rename(Op, COp),
	c_op(COp, Type),
	unary_op(SS, COp, Type, A).

cpp_functor(SS, Op, [A, B]) :- op_rename(Op, COp),
	c_op(COp, Type),
	bin_op(SS, Type, COp, A, B).

cpp_functor((S,I), Fn, Args) :-
	write(S, Fn),
	in_parens(S,
		exp_list((S,I), Args)).

bin_op((S,I), Type, Op, A, B) :-
	(	(Type = assign; Type = control)
	->	exp((S,I), A),
		write(S, Op),
		exp((S,I), B)
	;
		in_parens(S,
		(	exp((S,I), A),
			write(S, Op),
			exp((S,I), B)))
	).

unary_op((S, I), Op, _, A) :-
	in_parens(S,
	(	write(S, Op),
		exp((S, I), A))).

exp_list(_, []).
exp_list((S,I), [A]) :-
	exp((S,I), A).
exp_list((S,I), [A|Args]) :-
	exp((S,I), A),
	write(S, ", "),
	exp_list((S,I), Args).

in_parens(S, Format) :-
	write(S, "("),
	call(Format),
	write(S, ")").