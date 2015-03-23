## RITS: Rule-based Intelligent Tutoring System

RITS implements a domain-specific language for guiding students
through solving tasks. See [rits_fractions.pl](rits_fractions.pl) for
the definition of a concrete use case.

See [rits_architecture.pdf](rits_architecture.pdf) for more information.

See the [web](web) directory for web-based access. 

*Developed in cooperation with Vamobi. Intended for inclusion in
Vamobi TA to help pupils, teachers and administrators in Mozambique.*



Example invocation on the console:

    $ swipl -q -f rits_interaction.pl -g "solve_with_student(1/2+3/4)"

Example interaction:

    Please solve:
    
              1/2 + 3/4
    |: 4/6.
    This is wrong.
    You cannot just sum the numerators when the denominators are different!
    
    Let us first find a common multiple of 2 and 4!
    Please enter a common multiple of 2 and 4:
    
    
    |: 2.
    This is wrong.
    2 is no common multiple of 2 and 4, since 2 is not divisible by 4!
    So, let's try again!
    Please enter a common multiple of 2 and 4:
    
    
    |: 3.
    This is wrong.
    3 is not a common multiple of 2 and 4, since 3 is not divisible by 2!
    So, let's try again!
    Please enter a common multiple of 2 and 4:
    
    
    |: 5.
    This is wrong.
    I see you are having a hard time with this.
    Hint: 2 * 4 = 8 is a possible solution.
    So, let's try again!
    Please enter a common multiple of 2 and 4:
    
    
    |: 8.
    Good, the solution is correct. There is also a smaller solution!
    Now apply this knowledge to the original task!
    Please solve:
    
              1/2 + 3/4
    |: 10/8.
    Good, the solution is correct, but not minimal.
    Please cancel common divisors in:
    
              10/8
    |: 1/4.
    This is wrong!
    Unfortunately, I cannot give any useful hints here.
    So, let's try again!
    Please cancel common divisors in:
    
              10/8
    |: 5/0.
    The denominator of a fraction cannot be 0.
    So, let's try again!
    Please cancel common divisors in:
    
              10/8
    |: 5/4.
    Good, the solution is correct and also minimal. Very nice!
    
    the interaction history: [solve(1/2+3/4),internal(1/2+3/4=4/6),solve(cm(2,4)),internal(cm(2,4)=2),solve(cm(2,4)),internal(cm(2,4)=3),solve(cm(2,4)),internal(cm(2,4)=5),solve(cm(2,4)),internal(cm(2,4)=8),solve(1/2+3/4),internal(1/2+3/4=10/8),solve(cancel(10/8)),internal(cancel(10/8)=1/4),solve(cancel(10/8)),internal(cancel(10/8)=5/0),solve(cancel(10/8)),internal(cancel(10/8)=5/4)]
    true.
