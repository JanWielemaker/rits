
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Prepare translation templates.

   Usage: $ swipl -q -f prepare_translation.pl < rits_engine.pl
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

run :-
        repeat,
            read(Term),
            (   Term == end_of_file -> halt
            ;   Term = (A --> B),
                (   process_grammar_body(B) -> true
                ;   error(could_not_process(B))
                )
            ;   true
            ),
            false.

process_grammar_body({_Goal}).
process_grammar_body([L|Ls]) :- process_list([L|Ls]).
process_grammar_body(( Cond -> Then ; Else)) :-
        process_grammar_body(Cond),
        process_grammar_body(Then),
        process_grammar_body(Else).