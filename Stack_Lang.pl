% Part 1. It's a bird-eat-bug world out there!

% A small database of animals. Each relation gives the animal\'s name,
% it's habitat, and its biological class.
animal(cranefly, trees, insects).
animal(duck, ponds, birds).
animal(minnow, ponds, fish).
animal(scrubjay, trees, birds).
animal(squirrel, trees, mammals).
animal(waterstrider, ponds, insects).
animal(woodpecker, trees, birds).

% A small database capturing what each animal eats. Note that most animals eat
% more than one kind of food, but craneflies don\'t eat anything after they
% reach adulthood!
diet(scrubjay, insects).
diet(scrubjay, seeds).
diet(squirrel, nuts).
diet(squirrel, seeds).
diet(duck, algae).
diet(duck, fish).
diet(duck, insects).
diet(minnow, algae).
diet(minnow, insects).
diet(waterstrider, insects).
diet(woodpecker, insects).

% A binary predicate that includes all of the animals and where they live.
habitat(Animal, Where) :- animal(Animal, Where, _).

% A binary predicate that includes each animal and its biological class.
class(Animal, Class) :- animal(Animal, _, Class).


% 1. Define a predicate neighbor/2 that determines whether two animals live
%    in the same habitat. Note that two animals of the same kind always
%    live in the same habitat.
neighbor(A1, A2) :- habitat(A1, Where), habitat(A2, Where).


% 2. Define a predicate related/2 that includes all pairs of animals that
%    are in the same biological class but are not the same kind of animal.
related(A1, A2) :- class(A1, Class), class(A2, Class), A1 \= A2.

% 3. Define a predicate competitor/3 that includes two kinds of animals and
%    the food they compete for. Two animals are competitors if they live in
%    the same place and eat the same food.
competitor(A1, A2, Food) :- neighbor(A1, A2), diet(A1, Food), diet(A2, Food).


% 4. Define a predicate would_eat/2 that includes all pairs of animals where
%    the first animal would eat the second animal (because the second animal
%    is a kind of food it eats), if it could.
would_eat(A1, A2) :- diet(A1, Food), class(A2, Food).

% 5. Define a predicate does_eat/2 that includes all pairs of animals where
%    the first animal would eat the second, and both animals live in the same
%    place, so it probably does.
does_eat(A1, A2) :- would_eat(A1, A2), neighbor(A1, A2).

% 6. Define a predicate cannibal/1 that includes all animals that might eat
%    their own kind--eek!
cannibal(A) :- does_eat(A, A).



% Part 2. Implementing a stack language

% A slightly larger example program to use in testing.
example(P) :-
  P = [ 2, 3, 4, lte,            % [tru, 2]
        if([5, 6, add], [fls]),  % [11, 2]
        3, swap,                 % [11, 3, 2]
        4, 5, add,               % [9, 11, 3, 2]
        lte, if([tru], [mul]),   % [6]
        "whew!", swap,           % [6, "whew!"]
        "the answer is" ].       % ["the answer is", 6, "whew!"]

% 1. Define the predicate `cmd/3`.
cmd(dup, [Top|Stack], [Top, Top|Stack]) :- ! .
cmd(swap, [First, Second|Stack], [Second, First|Stack]) :- ! .
cmd(add, [First, Second|Stack], [Top|Stack]) :- number(First), number(Second), Top is First+Second, !.
cmd(mul, [First, Second|Stack], [Top|Stack]) :- number(First), number(Second), Top is First*Second, !.
cmd(lte, [First, Second|Stack], [tru|Stack]) :- number(First), number(Second), First >= Second, !.
cmd(lte, [First, Second|Stack], [fls|Stack]) :- number(First), number(Second), First < Second, !.
cmd(Lit, Stack, [Lit|Stack]) :- number(Lit); string(Lit), Lit \= dup, Lit \= swap, Lit \= add, Lit \= mul, Lit \= lte, Lit \= if(_,_), !.


% 2. Define the predicate `prog/3`.
prog([], Stack, Stack).
prog([if(P1, _)|Cmds], [Bool|Init_S], Fin_S) :- Bool = tru, prog(P1, Init_S, Int_S), prog(Cmds, Int_S, Fin_S), !.
prog([if(_, P2)|Cmds], [Bool|Init_S], Fin_S) :- Bool = fls, prog(P2, Init_S, Int_S), prog(Cmds, Int_S, Fin_S), !.
prog([C|Cmds], Init_S, Fin_S) :- cmd(C, Init_S, Int_S), prog(Cmds, Int_S, Fin_S), !.
