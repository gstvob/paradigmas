:- set_prolog_flag(double_quotes, codes).

testefigclone :-
  consult('programa.pl'),
  cmd("figclone 1 80 80"),
  commit.

testefigpf :-
  consult('programa.pl'),
  cmd("figpf 1 40"),
  commit.

testefigpt :-
  consult('programa.pl'),
  cmd("figpt 1 100"),
  commit.

testefiggd :-
  consult('programa.pl'),
  cmd("figgd 1 45"),
  commit.

testefigge :-
  consult('programa.pl'),
  cmd("figge 1 90"),
  commit.
