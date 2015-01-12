
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Prepare translation templates.

   One template is created for each string that can possibly be
   "emitted" by RITS.

   In this example, we are preparing Portuguese translations.

   Usage:

       $ swipl -q -f prepare_translation.pl < rits_engine.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

run :-
        repeat,
            read(Term),
            (   Term == end_of_file -> halt
            ;   Term = (_ --> Body),
                (   process_grammar_body(Body) -> true
                ;   writeln(could_not_process(Body)),
                    halt
                )
            ;   true
            ),
            false.

process_grammar_body([]).
process_grammar_body(!).
process_grammar_body({_}) :- !.
process_grammar_body((G1,G2)) :- !,
        process_grammar_body(G1),
        process_grammar_body(G2).
process_grammar_body([L|Ls]) :- !, maplist(process_token, [L|Ls]).
process_grammar_body(( _ -> Then ; Else)) :- !,
        process_grammar_body(Then),
        process_grammar_body(Else).
% for information, see what is being ignored
%process_grammar_body(I) :- !, format("%% ignoring: ~w\n", [I]).
process_grammar_body(_).

process_token(format(Xs, _)) :- !,
        format("translation(~q, pt,\n~t~12+\"\").\n", [Xs]).
process_token(format(Xs))    :- !,
        format("translation(~q, pt,\n~t~12+\"\").\n", [Xs]).
process_token(_). % ignore

:- initialization(run).
