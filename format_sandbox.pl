
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   To prove safety of format calls, we need to make the format
   strings explicit. This is similar to preparing translations.

   One call is created for each format call that can possibly be
   "emitted" by RITS.

   Example usage:

       $ swipl -q -f format_sandbox.pl < rits_fractions.pl
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
process_grammar_body(Str) :-
        string(Str),
        !,
        process_token(format(Str)).
process_grammar_body(format(Str,Args)) :- !,
        process_token(format(Str,Args)).
process_grammar_body(_).

process_token(format(Fs)) :- !,
        portray_clause((poutput(Fs) :-
                          format(atom(Atom), Fs, []),
                          pengine_output(Atom))).
process_token(format(Fs, Args)) :- !,
        portray_clause((poutput(Fs, Args) :-
                          format(atom(Atom), Fs, Args),
                          pengine_output(Atom))).
process_token(_). % ignore

:- initialization(run).
