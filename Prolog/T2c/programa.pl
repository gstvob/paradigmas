/*
   Programacao Logica - Prof. Alexandre G. Silva - 30set2015
     Versao inicial     : 30set2015
     Adicao de gramatica: 15out2015
     Atualizacao        : 12out2016
     Atualizacao        : 10mai2017
     Ultima atualizacao : 12set2017

   RECOMENDACOES:

   - O nome deste arquivo deve ser 'programa.pl'
   - O nome do banco de dados deve ser 'desenhos.pl'
   - O nome do arquivo de gramatica deve ser 'gramatica.pl'

   - Dicas de uso podem ser obtidas na execucação:
     ?- menu.

   - Exemplo de uso:
     ?- load.
     ?- searchAll(1).

   - Exemplos de uso da gramatica:
     ?- comando([pf, '10'], []).
     Ou simplesmente:
     ?- cmd("pf 10").

     ?- comando([repita, '5', '[', pf, '50', gd, '45', ']'], []).
     Ou simplesmente:
     ?- cmd("repita 5[pf 50 gd 45]").

   - Colocar o nome e matricula de cada integrante do grupo
     nestes comentarios iniciais do programa

     15100741 Gustavo Borges França
     15100752 Lucas João Martins
*/

:- set_prolog_flag(double_quotes, codes).
:- initialization(load).

% Exibe menu principal
menu :-
    write('load.         -> Carrega todos os desenhos do banco de dados para a memoria'), nl,
    write('commit.       -> Grava alteracoes de todos os desenhos no banco de dados'), nl,
    write('new(Id,X,Y).  -> Insere ponto/deslocamento no desenho com identificador <Id>'), nl,
    write('search.       -> Consulta pontos/deslocamentos dos desenhos'), nl,
    write('remove.       -> Remove pontos/deslocamentos dos desenhos'), nl,
    write('svg.          -> Cria um arquivo de imagem vetorial SVG (aplica "commit." antes'), nl.

% Apaga os predicados 'xy' da memoria e carrega os desenhos a partir de um arquivo de banco de dados
load :-
    consult('gramatica.pl'),
    retractall(xy(_,_,_)),
    retractall(xylast(_,_,_)),
    retractall(angle(_)),
    retractall(active(_)),
    open('desenhos.pl', read, Stream),
    repeat,
        read(Stream, Data),
        (Data == end_of_file -> true ; assert(Data), fail),
        !,
        close(Stream).

% Grava os desenhos da memoria em arquivo
commit :-
    open('desenhos.pl', write, Stream),
    telling(Screen),
    tell(Stream),
    listing(xylast),  %listagem dos predicados 'xylast'
    listing(angle),   %listagem dos predicados 'angle'
    listing(active),  %listagem dos predicados 'active'
    listing(xy),      %listagem dos predicados 'xy'
    tell(Screen),
    close(Stream).

% Ponto de deslocamento, se <Id> existente
new(Id,X,Y) :-
    xy(Id,_,_),
    assertz(xy(Id,X,Y)),
    !.

% Ponto inicial, caso contrario
new(Id,X,Y) :-
    asserta(xy(Id,X,Y)),
    !.

% Exibe opcoes de busca
search :-
    write('searchId(Id,L).  -> Monta lista <L> com ponto inicial e todos os deslocamentos de <Id>'), nl,
    write('searchFirst(L).  -> Monta lista <L> com pontos iniciais de cada <Id>'), nl,
    write('searchLast(L).   -> Monta lista <L> com pontos/deslocamentos finais de cada <Id>'), nl.

% Exibe opcoes de remocao
remove :-
    write('removeLast.      -> Remove todos os pontos/deslocamentos de <Id>'), nl,
    write('removeLast(Id).  -> Remove o ultimo ponto de <Id>'), nl.

% Grava os desenhos em SVG
svg :-
    commit,
    open('desenhos.svg', write, Stream),
    telling(Screen),
    tell(Stream),
    consult('db2svg.pl'),  %programa para conversao
    tell(Screen),
    close(Stream).

%------------------------------------
% t2A
% -----------------------------------

% Questao 1 (resolvida)
% Monta lista <L> com ponto inicial e todos os deslocamentos de <Id>
searchId(Id,L) :-
    bagof([X,Y], xy(Id,X,Y), L).

% Questao 2
% Monta lista <L> com pontos iniciais de cada <Id>
getFirsts(A, [H|T], L) :-
        searchId(H, List), nth0(0, List, Aux),
            append(A, [Aux], PI), getFirsts(PI, T, L), !.
getFirsts(A, [H|T], L) :-
        last([H|T], H), searchId(H, List),
            nth0(0, List, Aux), append(A, [Aux], PI), append([L], PI), !.
searchFirst(L) :- setof(Id, X^Y^xy(Id,X,Y), List), getFirsts([], List, L).

% Questao 3
% Monta lista <L> com pontos ou deslocamentos finais de cada <Id>
getLasts(A, [H|T], L) :-
    searchId(H, List), length(List, N), nth1(N, List, Aux), append(A, [Aux], PF), getLasts(PF, T, L), !.

getLasts(A, [H|T], L):-
    last([H|T], H), searchId(H, List), length(List, N), nth1(N, List, Aux), append(A, [Aux], PF), append([L], PF), !.

searchLast(L) :-
    setof(Id, X^Y^xy(Id, X, Y), List), getLasts([], List, L).

% Questao 4
% Remove todos os pontos ou deslocamentos do ultimo <Id>
lastId(List, Lid) :- length(List, A), nth1(A, List, Lid).
removeLast :-
    setof(Id, X^Y^xy(Id, X, Y), List), lastId(List, Lid), retractall(xy(Lid, _,_)).

% Questao 5
% Remove o ultimo ponto ou deslocamento de <Id>
getLastP(List, Pontos) :- length(List, A), nth1(A, List, Pontos), !.
splitP(Lista, X, Y) :- nth0(0, Lista, X), nth0(1, Lista, Y).
removeLast(Id) :- bagof([X,Y], xy(Id, X, Y), List),
            getLastP(List, Pontos), splitP(Pontos, X, Y), retract(xy(Id,X,Y)).

% Questao 6
% Determina um novo <Id> na sequencia numerica existente
incId(Id, Lista) :- length(Lista, A), nth1(A, Lista, Aux), Id is Aux + 1.
newId(Id) :- setof(Id, X^Y^xy(Id, X, Y), Lista), incId(Id, Lista).

% Questao 7
% Duplica a figura com <Id> a partir de um nova posicao (X,Y)
% Deve ser criado um <Id_novo> conforme a sequencia (questao 6)
pegaPrim(L, X1, Y1) :-
    nth0(0, L, Aux), splitP(Aux, X1, Y1), !.

pegaUltimo(L, X1, Y1) :-
    length(L, A), nth1(A, L, Aux), splitP(Aux, X1, Y1), !.

insereRest([H|T], Id) :-
    splitP(H, X1, Y1), assert(xy(Id, X1, Y1)), write([Id, X1, Y1]), insereRest(T, Id).

insereRest([H|T], _) :-
    last([H|T], H), !.

cloneId(Id,X,Y) :-
    searchId(Id, L),
    pegaPrim(L, X1, Y1),
    newId(Idnew),
    Xnovo is (X + X1),
    Ynovo is (Y + Y1),
    assert(xy(Idnew, Xnovo, Ynovo)),
    L = [_|Lnew],
    insereRest(Lnew, Idnew).

%------------------------------------
% t2B
% -----------------------------------

% nao garantido funcionamento correto dessas respostas

% Questao 1 (resolvida, mas pode ser alterada se necessario)
% Limpa os desenhos e reinicia no centro da tela (de 1000x1000)
tartaruga :-
    retractall(xy(_,_,_)),
    retractall(xylast(_,_,_)),
    retractall(angle(_)),
    retractall(active(_)),
    asserta(xylast(1, 500, 500)),
    assertz(angle(90)),
    assertz(active(1)).

% Questao 2
% Para frente N passos (conforme angulo atual)
parafrente(N) :- angle(Angulo),
                 X is cos((Angulo * pi) / 180) * N,
                 Y is sin((Angulo * pi) / 180) * N,
                 xylast(Id, XVelho, YVelho),
                 XNovo is XVelho + X, YNovo is YVelho + Y,
                 retractall(xylast(_, _, _)), asserta(xylast(Id, XNovo, YNovo)),
                 active(L), (L =:= 1 -> new(Id, X, Y); true).

% Questao 3
% Para tras N passos (conforme angulo atual)
paratras(N) :- angle(Angulo),
               X is cos((Angulo * pi) / 180) * N * (-1),
               Y is sin((Angulo * pi) / 180) * N * (-1),
               xylast(Id, XVelho, YVelho),
               XNovo is XVelho + X, YNovo is YVelho + Y,
               retractall(xylast(_, _, _)), asserta(xylast(Id, XNovo, YNovo)),
               active(L), (L =:= 1 -> new(Id, X, Y); true).

% Questao 4
% Gira a direita G graus
giradireita(G) :- angle(AnguloVelho), AnguloNovo is AnguloVelho - G,
                  retractall(angle(_)), asserta(angle(AnguloNovo)).

% Questao 5
% Gira a esquerda G graus
giraesquerda(G) :- angle(AnguloVelho), AnguloNovo is AnguloVelho + G,
                   retractall(angle(_)), asserta(angle(AnguloNovo)).

% Questao 6
% Use nada (levanta lapis)
usenada :- retractall(active(_)), assertz(active(0)).

% Questao 7
% Use lapis
uselapis :- retractall(active(_)), xylast(IdVelho, X, Y),
            IdNovo is IdVelho + 1, retractall(xylast(_, _, _)),
            assertz(xylast(IdNovo, X, Y)), assertz(active(1)),
            new(IdNovo, X, Y).

%------------------------------------
% t2C
% -----------------------------------

%QUESTÃO 1 T2C, seachId, newId, insereRest são do T2A.
figuraclone(Id, X, Y):-
    searchId(Id, L),
    newId(Idnew),
    L = [_|Lnew],
    assert(xy(Idnew, X, Y)),
    write([Idnew, X, Y]),
    insereRest(Lnew, Idnew).


%Questão 2 T2C.
figuraparafrente(Id, N) :-
	angle(Teta),
	searchId(Id, L),
	pegaPrim(L, X1, Y1),
	Alfa is (Teta*pi)/180,
	X is N* Alfa,
	Y is -1 * (N * Alfa),
	Xn is (X1 + X),
	Yn is (Y1 + Y),
	write([Id, X1, Y1]), nl,
	write([Id, Xn, Yn]), nl,
	retract(xy(Id, X1, Y1)),
	assert(xy(Id, Xn, Yn)).


%questao 3 T2c - basicamente o andar pra frente sem o -1 no Y.
figuraparatras(Id, N) :-
	angle(Teta),
	searchId(Id, L),
	pegaPrim(L, X1, Y1),
	Alfa is (Teta*pi)/180,
	X is N * Alfa,
	Y is N * Alfa,
	Xn is (X1 + X),
	Yn is (Y1 + Y),
	write([Id, X1, Y1]), nl,
	write([Id, Xn, Yn]), nl,
	retract(xy(Id, X1, Y1)),
	assert(xy(Id, Xn, Yn)).

%questao 4 T2c
figuragiraesquerda(Angle) :-
    true.

%questao 5 T2c
figuragiradireita(Angle) :-
    true.






