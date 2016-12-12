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

% casi Base di as_monomial %

as_monomial(C, m(C, 0, [])) :-
    integer(C),
    !.
	    
as_monomial(Var^Esp, m(1, Esp, [v(Esp, Var)])) :-
    integer(Esp),
    atom(Var).

as_monomial(Var, m(1, 1, [v(1,Var)])) :-
    atom(Var).
    

% passo ricorsivo as_monomial %

as_monomial(A*B, m(C, TD, VPss)) :-
	as_monomial(A, m(Ca, TDa, VPsa)),
	as_monomial(B, m(Cb, TDb, VPsb)),
	C is Ca * Cb,
	TD is TDa + TDb,
	append(VPsa,VPsb,VPsr),
	sort(2, @=<, VPsr, VPss).

% casi base as_polynomial %

as_polynomial(C, poly(Monomial)) :-
    as_monomial(C,MonomialC),
    Monomial=[MonomialC].

% passo ricorsivo as_polynomial %
   
as_polynomial(A+B, poly(MonomialsF)) :-
    as_polynomial(A, poly(MonomialsA)),
    as_polynomial(B, poly(MonomialsB)),
    append(MonomialsA, MonomialsB, Monomials),
    sort(([3,2]),@=<, Monomials, MonomialsJ),
    sort(2,@=<, MonomialsJ, MonomialsF).

%%% DA FARE %%%
%%% caso base as_monomial che permetta di accettare sen ecc %%%
%%% sort, quello presente non é corretto serve a dare un idea %%%
%%% caso in cui i monomi sono separati da - e non solo da + %%%

