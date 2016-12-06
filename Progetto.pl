%%% -*- Mode: Prolog -*-

%%% Progetto.pl%

is_monomial(m(_C, TD, VPs)) :-
integer(TD),
TD >= 0,
is_list(VPs).

is_varpower(v(Power, VarSymbol)) :-
integer(Power),
Power >= 0,
atom(VarSymbol).

is_polynomial(poly(Monomials)) :-
is_list(Monomials),
foreach(member(M, Monomials), is_monomial(M)).

as_monomial(C, m(C, 0, [])):-
	     integer(C),
	     !.

as_monomial(Var^Esp, m(1, Esp, [v(Esp, Var)])):-
	integer(Esp).

as_monomial(A*B, m(Cr, TDr, VPsr)):-
	integer(A),
	as_monomial(B, m(C, TD, VPs)),
	C is C * Cr,
	TD is TD * TDr,
	append(VPs,VPsr,VPs).

