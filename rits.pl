
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
        (   { Hist = [internal(I)|_], I \= (_=_) } ->
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

run_tests :-
        findall(T, test(T), Ts),
        maplist(run_test, Ts).

run_test([T|Ts]) :-
        rits_start(S0),
        rits_next_action(T, A, S0, S),
        action_test(A, Ts, S).

action_test(done, [], _) :- !.
action_test(A0, [], _)   :- dif(A0, done), throw(not_done-A0).
action_test(A0, [T|Ts0], S0) :-
        test_action_rest(T, A0, A, Ts0, Ts),
        rits_next_action(A, Next, S0, S),
        action_test(Next, Ts, S).

test_action_rest(Var, A0, next, Ts, Ts) :-
        var(Var),
        !,
        A0 = Var.
test_action_rest(*(Sub), A0, next, Ts0, Ts) :-
        (   A0 = format(F) -> true
        ;   A0 = format(F, _) -> true
        ;   throw(format_expected-A0)
        ),
        (   string_concat(_, Post, F),
            string_concat(Sub, _, Post) ->
            Ts = Ts0
        ;   Ts = [*(Sub)|Ts0] % keep looking
        ).
test_action_rest(=>(Answer), A0, student_answers(Answer), Ts, Ts) :-
        (   A0 = read_answer -> true
        ;   throw(read_answer_expected)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   High-level debugging interface to RITS.

   You can observe the actions that RITS responds with and its states.

   Example:

      ?- rits:observe(solve(3/5+3/6), A, S).
      A = format("Please solve:\n\n~t~10+"),
      S = ...
      A = fraction_layout(3/5+3/6),
      S = ...

   You can copy & paste the state and use it to test rits_next_action/4.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

observe(Start, Next, S) :-
        rits_start(S0),
        rits_next_action(Start, A0, S0, S1),
        observe_(A0, Next, S1, S).

observe_(A, A, S, S).
observe_(_, A, S0, S) :-
        rits_next_action(next, A1, S0, S1),
        observe_(A1, A, S1, S).


% :- initialization(run_tests).

/** <examples>

?- rits_start(S0), rits_next_action(solve(1/2+3/4), A, S0, S).

?- rits:observe(solve(cm(1,2)), A, S).
?- rits:run_test([solve(cm(1,2)),_,=>(3),*("wrong"),_,S]).
?- rits:run_test([solve(cm(1,2)),_,=>(2),_,*("minimal")]).
?- rits:run_test([solve(cm(1,2)),*("multiple"),=>(4),*("correct"),*("smaller")]).
*/
