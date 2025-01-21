:- module(cpp_reader, [
	read_file/2
]).

:- op(1075, fx,  if).
:- op(1050, xfy, then).
:- op(1050, xfy, else).
:- op(700,  xfx, +=).
:- op(700,  xfx, -=).
:- op(700,  xfx, *=).
:- op(700,  xfx, /=).
:- op(690, xfx, <-).
:- op(675,  yfx, @).
:- op(650,  xfx, func).
:- op(650,  fx,  func).
:- op(650,  fx,  struct).
:- op(650,  fx,  union).
:- op(650,  xfx, var).
:- op(620,  fx,  return).
:- op(400,  yfx, '/?').
:- op(2,    fx,  '&').

read_file(Name, Terms) :-
	read_file_to_terms(Name, Terms, [module(cpp_reader)]).