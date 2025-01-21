:- module(cpp_reader, [
	read_file/2
]).

:- op(1150, xfx, '*=>'). 
:- op(975,  fx,  if).
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
:- op(690,  xfx, <-).
:- op(675,  yfx, @).
:- op(650,  xfx, func).
:- op(650,  fx,  func).
:- op(650,  fx,  struct).
:- op(650,  fx,  union).
:- op(650,  fx,  enum).
:- op(650,  xfx, var).
:- op(610,  fx,  '&').
:- op(400,  yfx, '/?').
:- op(150, fx, !).
:- op(100,  fx, @).

read_file(Name, Terms) :-
	read_file_to_terms(Name, Terms, [module(cpp_reader)]).