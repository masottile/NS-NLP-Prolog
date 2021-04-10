:- [model].

% Code structure based off of geographyq.pl in class

% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(L0,L4,Entity,C0,C4) :-
    det(L0,L1,Entity,C0,C1),
    adjectives(L1,L2,Entity,C1,C2),
    noun(L2,L3,Entity,C2,C3),
    mp(L3,L4,Entity,C3,C4).
noun_phrase(L0,L4,Entity,C0,C4) :-
    proper_noun(L0,L4,Entity,C0,C4).

% query is used for simluation queries, there will be no constraints as simluation function only works given correct format
query(L0,L7,Entity,C0,C4) :-
    det(L0,L1,Entity,C0,C1),
    requiredWords(L1,L2,Entity,C1,C2),
    prep(L2,L3,Entity,C2,C3),
    num(L3,L4,N),
    extractAnimal(L4,L5,A),
    extractEnvironment(L5,L6,[],E),
    optionalWords(L6,L7,E,C3,C4),
    onedaypopulation(population(A,N),E,Entity).
query(L0,L5,Entity,C0,C3) :-
    det(L0,L1,Entity,C0,C1),
    requiredWords(L1,L2,Entity,C1,C2),
    prep(L2,L3,Entity,C2,C3),
    extractEnvironments(L3,L4,LE),
    extractDays(L4,L5,N),
    ns(LE,N,Entity).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constraints.
det([the | L],L,_,C,C).
det([a | L],L,_,C,C).
det([an | L],L,_,C,C).
det(L,L,_,C,C).

% Adverbs are used to provide a different type of question.
% They do not provide any extra constraints.
adverb([does | L],L,_,C,C).
adverb([do | L],L,_,C,C).

% Preposition are used to provide a different type of question.
% They do not provide any extra constraints.
prep([of | L],L,_,C,C).
prep(L,L,_,C,C).

% num is used to extract the number out of a list of words
num([X | L],L,X) :-
    integer(X).

% extractAnimal is used to extract the animal in question from words
extractAnimal([A | L],L,A) :-
    species(A,_,_,_,_).

% extractDays is used to extract a the days to simulate
extractDays([after,X,days | L],L,X) :-
    integer(X).
extractDays([after,X,day | L],L,X) :-
    integer(X).

% extractEnvironments is used to extract a list of environment from words
extractEnvironments(L0,L2,[E|LE]) :-
    extractEnvironment(L0,L1,[],E),
    extractEnvironments(L1,L2,LE).
extractEnvironments(L,L,[]).

% extractEnvironment is used to extract the environment from words
extractEnvironment([from,an,environment,of | L],RL,LP,E) :-
    extractEnvironment(L,RL,LP,E).
extractEnvironment([N,A | L],RL,LP,E) :-
    integer(N),
    species(A,_,_,_,_),
    extractEnvironment(L,RL,[population(A,N) | LP],E).
extractEnvironment([',',N,A | L],RL,LP,E) :-
    integer(N),
    species(A,_,_,_,_),
    extractEnvironment(L,RL,[population(A,N) | LP],E).
extractEnvironment([and,N,A | L],RL,LP,E) :-
    integer(N),
    species(A,_,_,_,_),
    extractEnvironment(L,RL,[population(A,N) | LP],E).
extractEnvironment([in,a,G | L],L,LP,environment(LP,G)) :-
    geography(G,_,_,_).
extractEnvironment([in,G | L],L,LP,environment(LP,G)) :-
    geography(G,_,_,_).

% requiredWords is a list of words that must exist in a list. It is used to ask specific questions
requiredWords([resulting,population | L],L,_,C,C).
requiredWords([resulting,populations | L],L,_,C,C).
requiredWords([result,population | L],L,_,C,C).
requiredWords([result,populations | L],L,_,C,C).
requiredWords([resulting,environment | L],L,_,C,C).
requiredWords([resulting,environments | L],L,_,C,C).
requiredWords([result,environment | L],L,_,C,C).
requiredWords([result,environments | L],L,_,C,C).

% optionalWords is a list of optional words that can exist in a list. It is used to ask specific questions
optionalWords([after,a,day | L],L,_,C,C).
optionalWords([after,1,day | L],L,_,C,C).
optionalWords([after,one,day | L],L,_,C,C).
optionalWords([in,a,day | L],L,_,C,C).
optionalWords([in,1,day | L],L,_,C,C).
optionalWords([in,one,day | L],L,_,C,C).
optionalWords(L,L,_,C,C).

% adjectives(L0,L2,Entity,C0,C2) is true if 
% L0-L2 is a sequence of adjectives imposes constraints C0-C2 on Entity
adjectives(L0,L2,Entity,C0,C2) :-
    adj(L0,L1,Entity,C0,C1),
    adjectives(L1,L2,Entity,C1,C2).
adjectives(L,L,_,C,C).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing 
mp(L0,L2,Subject,C0,C2) :-
    reln(L0,L1,Subject,Object,C0,C1),
    noun_phrase(L1,L2,Object,C1,C2).
mp([that|L0],L2,Subject,C0,C2) :-
    reln(L0,L1,Subject,Object,C0,C1),
    noun_phrase(L1,L2,Object,C1,C2).
mp([that,has|L0],L1,Subject,C0,C1) :-
    reln([has | L0],L1,Subject,_,C0,C1).
mp([has|L0],L1,Subject,C0,C1) :-
    reln([has|L0],L1,Subject,_,C0,C1).
mp(L,L,_,C,C).

% DICTIONARY
% adj(L0,L1,Entity,C0,C1) is true if L0-L1 
% is an adjective that imposes constraints C0-C1 Entity
adj([carnivorous | L],L,Entity, [carnivorous(Entity)|C],C).
adj([herbivorous | L],L,Entity, [herbivorous(Entity)|C],C).

noun([animal | L],L,Entity, [species(Entity,_,_,_,_)|C],C).
noun([environment | L],L,Entity, [geography(Entity,_,_,_)|C],C).

% Countries and languages are proper nouns.
% We could either have it check a language dictionary or add the constraints. We chose to check the dictionary.
proper_noun([X | L],L,X,C,C) :- species(X,_,_,_,_).
proper_noun([a, X | L],L,X,C,C) :- species(X,_,_,_,_).
proper_noun([X | L],L,X,C,C) :- geography(X,_,_,_).
proper_noun([a, X | L],L,X,C,C) :- geography(X,_,_,_).

reln([eat | L],L,O1,O2,[eats(O1,O2)|C],C).
reln([hunt | L],L,O1,O2,[eats(O1,O2)|C],C).
reln([eats | L],L,O1,O2,[eats(O1,O2)|C],C).
reln([hunts | L],L,O1,O2,[eats(O1,O2)|C],C).
reln([the,prefered,temperature,for | L],L,O1,O2, [species(O2,_,_,O1,_)|C],C).
reln([the,suitable,temperature,for | L],L,O1,O2, [species(O2,_,_,O1,_)|C],C).
reln([the,prefered,rain,condition,for | L],L,O1,O2, [species(O2,_,_,_,O1)|C],C).
reln([the,suitable,rain,condition,for | L],L,O1,O2, [species(O2,_,_,_,O1)|C],C).
reln([the,temperature,of | L],L,O1,O2, [geography(O2,O1,_,_)|C],C).
reln([has,O2,temperature | L],L,O1,O2, [geography(O1,O2,_,_)|C],C).
reln([has,O2,temperatures | L],L,O1,O2, [geography(O1,O2,_,_)|C],C).
reln([the,rain,condition,of | L],L,O1,O2, [geography(O2,_,O1,_)|C],C).
reln([has,O2,rain,condition | L],L,O1,O2, [geography(O1,_,O2,_)|C],C).
reln([has,O2,rain,conditions | L],L,O1,O2, [geography(O1,_,O2,_)|C],C).
reln([the,food,availability,of | L],L,O1,O2, [geography(O2,_,_,O1)|C],C).
reln([has,O2,food,availability | L],L,O1,O2, [geography(O1,_,_,O2)|C],C).
reln([has,O2,resources | L],L,O1,O2, [geography(O1,_,_,O2)|C],C).

% question(Question,QR,Entity) is true if Query provides an answer about Entity to Question
question(['What',is | L0], L1, Entity,C0,C1) :-
    mp(L0,L1,Entity,C0,C1).
question(['What',is | L0],L1,Entity,C0,C1) :-
    noun_phrase(L0,L1,Entity,C0,C1).
question(['What',is | L0], L1, Entity,C0,C1) :-
    query(L0,L1,Entity,C0,C1).
question(['What' | L0],L2,Entity,C0,C2) :-
    noun_phrase(L0,L1,Entity,C0,C1),
    mp(L1,L2,Entity,C1,C2).
question(['What' | L0],L3,Object,C0,C3) :-
    adverb(L0,L1,Entity,C0,C1),
    noun_phrase(L1,L2,Entity,C1,C2),
    reln(L2,L3,Entity,Object,C2,C3).

% ask(Q,A) gives answer A to question Q
ask(Q,A) :-
    get_constraints_from_question(Q,A,C),
    prove_all(C).

/* Try the following queries:
?- ask(['What',is,an,animal],A).
?- ask(['What',is,an,environment],A).
?- ask(['What',is,the,prefered,temperature,for,a,wolf],A).
?- ask(['What',is,the,temperature,of,a,desert],A).
?- ask(['What',is,the,rain,condition,of,a,desert],A).
?- ask(['What',is,the,prefered,temperature,for,a,herbivorous,animal],A).
?- ask(['What',is,a,herbivorous,animal],A).
?- ask(['What',is,an,animal,that,hunts,a,rabbit],A).
?- ask(['What',is,an,animal,that,hunts,an,animal,that,eats,a,rabbit],A).
?- ask(['What',is,the,prefered,temperature,for,an,animal,that,eats,tortoise],A).
?- ask(['What',animal,hunts,a,rabbit],A).
?- ask(['What',animal,that,hunts,a,rabbit,eats,a,tortoise],A).
?- ask(['What',animal,hunts,a,rabbit,hunts,a,tortoise],A).
?- ask(['What',does,a,bear,eat],A).
?- ask(['What',is,an,environment,that,has,hot,temperatures],A).
?- ask(['What',environment,has,cold,temperatures],A).
?- ask(['What',is,an,environment,that,has,humid,rain,condition],A).

Simulation queries
1 day simulation for a population given an environment
?- ask(['What',is,the,resulting,population,of,9,rabbit,from,an,environment,of,1,wolf,',',9,rabbit,and,1,bear,in,a,desert],A).
?- ask(['What',is,the,resulting,population,of,9,rabbit,from,an,environment,of,1,wolf,',',9,rabbit,and,1,bear,in,a,desert,after,a,day],A).
N days simulation given a list of environment
?- ask(['What',is,the,resulting,environments,of,1,wolf,',',9,rabbit,and,1,bear,in,a,desert,',',10,rabbit,',',1,tortoise,and,2,wolf,in,a,plain,and,2,tortoise,in,a,jungle,after,3,days],A).
?- ask(['What',is,the,resulting,environments,of,1,wolf,and,9,rabbit,in,a,desert,and,10,rabbit,in,a,plain,after,4,days],A).
?- ask(['What',is,the,resulting,environment,of,5,tortoise,in,a,desert,after,10,days],A).
*/

% get_constraints_from_question(Q,A,C) is true if C is the constraints on A to infer question Q
get_constraints_from_question(Q,A,C) :-
    question(Q,End,A,C,[]),
    member(End,[[],['?'],['.']]).

% prove_all(L) is true if all elements of L can be proved from the knowledge base
prove_all([]).
prove_all([H|T]) :-
    call(H),      % built-in Prolog predicate calls an atom
    prove_all(T).

% To get the input from a line:
q(Ans) :-
    write("Ask me: "), flush_output(current_output),
    readln(Ln),
    ask(Ln,Ans).

/*
Some questions:
What is the prefered temperature for a herbivorous animal
What is a herbivorous animal
What is an animal that hunts a rabbit
What is an animal that hunts an animal that eats a rabbit
What is the prefered temperature for an animal that eats tortoise
What does a bear eat
What is an environment that has hot temperatures
What is an environment that has humid rain condition

Simulation queries:
What is the resulting population of 9 rabbit from an environment of 1 wolf, 9 rabbit and 1 bear in a desert after a day
What is the resulting population of 10 rabbit from an environment of 1 wolf in a plain after a day

What is the resulting environments of 1 wolf, 9 rabbit and 1 bear in a desert, 10 rabbit, 1 tortoise and 2 wolf in a plain and 2 tortoise in a jungle after 3 days
What is the resulting environments of 1 wolf and 9 rabbit in a desert and 10 rabbit in a plain after 4 days
What is the resulting environment of 5 tortoise in a desert after 10 days
*/