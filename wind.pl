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

parsing_monomial(C, m(C, 0, [])) :-
    integer(C),
    !.
	    
parsing_monomial(Var^Esp, m(1, Esp, [v(Esp, Var)])) :-
    integer(Esp),
    atom(Var).

parsing_monomial(Var, m(1, 1, [v(1,Var)])) :-
    atom(Var).

parsing_monomial(C, m(C, 0, [])) :-
    compound(C),
    functor(C, sin|cos|tan, 1).
    
parsing_monomial(A*B, m(C, TD, VPs)) :-
    parsing_monomial(A, m(Ca, TDa, VPsa)),
    parsing_monomial(B, m(Cb, TDb, VPsb)),
    C is Ca * Cb,
    TD is TDa + TDb,
    append(VPsa,VPsb,VPs).

% compress %

compress([],[]):-
    !.
compress([v(Ca,V)],[v(Ca,V)]):-
    !.
compress([v(Ca,V),v(Cb,V)|R], R2) :-
    C is Ca + Cb,
    compress([v(C,V)|R], R2),
    !.
compress([v(Ca,V),v(Cb,Z)|R],[v(Ca,V)|R2]):-
    !,
    V\=Z,
    compress([v(Cb,Z)|R],R2).

% as_monomial %

as_monomial(A*B, m(C, TD, VPssc)) :-
	parsing_monomial(A*B, m(C, TD, VPsi)),
	sort(2, @=<, VPsi, VPss),
	compress(VPss, VPssc).


% casi base as_polynomial %

as_polynomial(C, poly(Monomial)) :-
    as_monomial(C,MonomialC),
    Monomial=[MonomialC].

% passo ricorsivo as_polynomial %
   
as_polynomial(A+B, poly(Monomials)) :-
   as_polynomial(A, poly(MonomialsA)),
   as_polynomial(B, poly(MonomialsB)),
   append(MonomialsA, MonomialsB, Monomials).

as_polynomial(A-B, poly(Monomials)) :-
   as_polynomial(A, poly(MonomialsA)),
   as_polynomial(-B, poly(MonomialsB)),
   append(MonomialsA, MonomialsB, Monomials).


%% prova del sort %%%

%returntot(m(_,Td,_), Td) :-
 %   integer(Td).

%sametd(Tda,Tdb):-
 %   Tda = Tdb.

%sort_pol([P,S | Monomil], Monomialsort) :-
 %   ( sametd(P,S) -> sort([3,2], @=<, [P,S], OrderPs),
  %  append(OrderPs, Monomialsort, Monomialsort),
   % sort_pol([S,T | Monomil], Monomialsort);
    %sort_pol([S,T| Monomil], Monomialsort)).


    
%%% DA FARE %%%
%%% caso base as_monomial che permetta di accettare sen ecc %%%
%%% sort, quello presente non é corretto serve a dare un idea %%%
%%% caso in cui i monomi sono separati da - e non solo da + %%%
 

