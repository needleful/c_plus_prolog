:- module(cpp_reader, [
	read_file/2,
	'*=>'/2,
	custom_data/1,
	consult/1,
	consulted/1
]).

:- dynamic '*=>'/2.
:- dynamic consulted/1.
:- dynamic custom_data/1.

:- op(1150, xfx, '*=>').
:- op(975,  fx,  return).
:- op(950,  xfy, then).
:- op(950,  xfy, do).
:- op(950,  xfy, else).
:- op(800,  yfx, or).
:- op(750,  yfx, and).
:- op(700,  xfx, +=).
:- op(700,  xfx, -=).
:- op(700,  xfx, *=).
:- op(700,  xfx, '/=').
:- op(700,  xfx, '&=').
:- op(700,  xfx, '|=').
:- op(700,  xfx, ^=).
:- op(690,  yfx, as).
:- op(675,  yfx, @).
:- op(660,  yf, {}).
:- op(650,  xfx, func).
:- op(650,  fx,  func).
:- op(650,  fx,  struct).
:- op(650,  fx,  union).
:- op(650,  fx,  enum).
:- op(650,  xfx, var).
:- op(650,  fx, var).
:- op(610,  fx,  '&').
:- op(400,  yfx, '/?').
:- op(150,  fx, !).
:- op(120,  fx, @).
:- op(110,  yf, []).

read_file(Name, Terms) :-
	assert(consulted(Name)),
	read_file_to_terms(Name, Terms, [module(cpp_reader)]),
	find_macros(Terms).

consult(Name) :- \+ consulted(Name),
	(	atom(Name)
	->	atom_concat(Name, '.c+p', Path)
	;	Path = Name
	),
	read_file(Path, _).

find_macros(Terms) :-
	maplist(find_macro, Terms).

find_macro(C) :- var(C), !, fail.
find_macro('*=>'(A, B)) :- !,
	assertz('*=>'(A, B)).
find_macro('*=>'(A, B) :- C) :- !,
	assertz('*=>'(A, B) :- C).
find_macro(_).
