:- module(cpp_ops, [
	c_op/2,
	c_standalone/1,
	op_rename/2
]).

c_op('+',	math).
c_op('-',	math).
c_op('*',	math).
c_op('/',	math).
c_op('%',	math).

c_op('==',	cmp).
c_op('!=',	cmp).
c_op('>',	cmp).
c_op('<',	cmp).
c_op('>=',	cmp).
c_op('<=',	cmp).

c_op('!',	bool).
c_op('&&',	bool).
c_op('||',	bool).

c_op('~',	bin).
c_op('|',	bin).
c_op('&',	bin).
c_op('^',	bin).
c_op('<<',	bin).
c_op('>>',	bin).

c_op('=',	assign).
c_op('+=',	assign).
c_op('-=',	assign).
c_op('*=',	assign).
c_op('/=',	assign).
c_op('%=',	assign).
c_op('&=',	assign).
c_op('|=',	assign).
c_op('^=',	assign).
c_op('<<=',	assign).
c_op('>>=',	assign).

c_op(',',	control).

c_op('.',	access).

op_rename('/?', '%').

c_standalone(return).
c_standalone(break).
c_standalone(continue).