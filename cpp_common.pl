:- module(cpp_common, [
	expect/2
]).

expect(Term, Message) :-
	(	call(Term)
	->	true
	;	!, writeln(Message), fail).
