
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   RRRRR   II  TTTTTTTT  SSSS
   R  RR   II     TT    SS             Rule-based Intelligent
   RRRR    II     TT     SSSSS         Tutoring System.
   R   R   II     TT         SS
   R    R  II     TT   SSSSSSS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(rits, [
                 rits_start/1,       % -S0
                 rits_next_action/4, % +Action0, -Action, +S0, -S
                 rits_history/2      % +S, -History
                ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   RITS learns about domains via rits:solve//1 and rits:actions//3.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
        rits:solve//1,
        rits:actions//3,
        rits:test/1.

:- use_module(rits_fractions).
:- use_module(rits_common_multiple).
:- use_module(rits_multiple_choice).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   The interface to RITS.

   Given an action and the current state, decide what to do next.

   A single student action may require several actions to respond
   (printing a message, giving a hint, asking the student etc.).

   The history is represented as a list of previous interactions.
   Initially, this list is empty. Internal actions and history items
   are used to recall what the current question is. They have the form
   internal(I) and do not appear to the outside.

   The state is a term s(Nexts,Hist). Nexts are actions that still
   need to be executed. It is a mixture of a stack and a queue: For a
   single student interaction, several actions might be put into the
   queue, and these are executed FIFO-style. However, an erroneous
   answer may trigger an auxiliary interaction, and these interactions
   are collectively pushed and executed LIFO-style.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

rits_start(s([],[])).

rits_history(s(_,Hist0), Hist) :- reverse(Hist0, Hist).

rits_next_action(Action0, Action, S0, S) :-
        rits_next_action_(Action0, Action1, S0, S1),
        (   (   internal(Action1) ;  Action1 = subproblem(_) ) ->
            rits_next_action(Action1, Action, S1, S)
        ;   Action = Action1,
            S = S1
        ).

rits_next_action_(Action0, Action, S0, s(Nexts,Hist)) :-
        (   var(Action0) -> throw(action_uninstantiated)
        ;   true
        ),
        S0 = s(Nexts0,Hist0),
        (   phrase(next_actions(Action0,Hist0,Hist), As0) -> true
        ;   throw(no_action_found(Action0,Hist0,Hist))
        ),
        append(As0, Nexts0, Nexts1),
        %format("the next actions are: ~w\n", [Nexts1]),
        nexts_action_nexts(Nexts1, Action, Nexts).

nexts_action_nexts([], done, []).
nexts_action_nexts([Action|Nexts], Action, Nexts).

internal(internal(_)).

list_internals(Ls, Is) :-
        include(internal, Ls, Is0),
        maplist(arg(1), Is0, Is).


next_actions(next, Hist, Hist) -->
        (   { Hist = [internal(_)|_] } ->
            { throw(expecting_student_answers) }
        ;   []
        ).
next_actions(done, Hist, Hist) --> [done].
next_actions(internal(I), Hist, [internal(I)|Hist]) --> [].
next_actions(student_answers(A), Hist0, Hist) -->
        { Hist0 = [internal(Expr)|Rest],
          Hist = [internal(Expr=A)|Rest],
          list_internals(Hist, Is) },
        actions(Expr, A, Is),
        !. % commit to first solution
next_actions(subproblem(Ls), Hist, Hist) --> [enter], Ls, [exit].
next_actions(solve(Expression), Hist, [solve(Expression)|Hist]) -->
        (   { Hist = [_,solve(Expression)|_] } ->
            [format("So, let's try again!\n")]
        ;   []
        ),
        solve(Expression),
        !, % commit to first solution
        [internal(Expression),read_answer].

% run_tests :-
%         findall(T, test(T), Ts),
%         maplist(run_test, Ts).

% run_test([First|Rest]) :-
%         rits_start(S0),
%         rits_next_action(First, A, S0, S),
%         test_rest(Rest, A, S).

% test_rest([], A, _) :-
%         (   A == done -> true
%         ;   throw(not_yet_done_but_no_actions)
%         ).
% test_rest([R|Rs], A0, S) :-
%         test_action(R, A0, A),
%         test_rest(Rs, 

% test_action(substring(S), format(F)) :-
%         string_concat(Pre, Post, F),
%         string_concat(S, _, Post).

observe(Start, Next, S) :-
        rits_start(S0),
        rits_next_action(Start, A0, S0, S1),
        observe_(A0, Next, S1, S).

observe_(A, A, S, S).
observe_(_, A, S0, S) :-
        rits_next_action(next, A1, S0, S1),
        observe_(A1, A, S1, S).

%?- rits:observe(solve(cm(1,2)), A, S).

%?- rits_start(S0), rits_next_action(next, A, S0, S).

% :- initialization(run_tests).

/** <examples>

?- rits_start(S0), rits_next_action(solve(1/2+3/4), A, S0, S).

*/
