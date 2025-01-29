:- module(cpp_reader, [
	read_file/2,
	'*=>'/2,
	custom_data/1,
	consult/1,
	consulted/1,
	this_file/2
]).

:- dynamic '*=>'/2.
:- dynamic consulted/1.
:- dynamic custom_data/1.
:- dynamic this_file/2.

:- op(1150, xfx, '*=>').
:- op(975,  fx,  return).
:- op(950,  xfy, then).
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
:- op(680,  xfx, var).
:- op(680,  fx,  var).
:- op(675,  yfx, @).
:- op(660,  yf,  {}).
:- op(650,  xfx, func).
:- op(650,  fx,  func).
:- op(650,  fx,  struct).
:- op(650,  fx,  union).
:- op(650,  fx,  enum).
:- op(610,  fx,  '&').
:- op(400,  yfx, '/?').
:- op(150,  fx,  !).
:- op(110,  yf,  []).
:- op(105,  fx,  *).
:- op(105,  xfy, @).
:- op(1,    fx,  #).

read_file(Name, Terms) :- \+ consulted(Name),
	assert(consulted(Name)),
	read_file_to_terms(Name, InTerms, [module(cpp_reader)]),
	find_macros(InTerms, Terms).

consult(Name) :-
	(	atom(Name)
	->	atom_concat(Name, '.c+p', Path)
	;	Path = Name
	),
	read_file(Path, _).

find_macros([], []).
find_macros([M|Terms], Result) :-
	(	find_macro(M)
	->	find_macros(Terms, Result)
	;	find_macros(Terms, Next),
		Result = [M|Next]
	).

find_macro('*=>'(A, B)) :- !,
	assertz('*=>'(A, B)).
find_macro('*=>'(A, B) :- C) :- !,
	assertz('*=>'(A, B) :- C).
