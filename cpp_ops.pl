:- module(cpp_ops, [
	c_op/2,
	c_standalone/1,
	c_type_block/1,
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

c_type_block(enum).
c_type_block(struct).
c_type_block(union).

c_standalone(return).
c_standalone(break).
c_standalone(continue).

op_rename('/?', '%').
op_rename('or', '||').
op_rename('and', '&&').
op_rename('=<', '<=').
op_rename('@', '*').