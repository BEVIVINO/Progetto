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

as_monomial(A*B, m(C, TD, VPsr)):-
	as_monomial(A, m(Ca, TDa, VPsa)),
	as_monomial(B, m(Cb, TDb, VPsb)),
	C is Ca * Cb,
	TD is TDa + TDb,
	append(VPsa,VPsb,VPsr).

%%% end of file -- Progetto.pl
