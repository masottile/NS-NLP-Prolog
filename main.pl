% Actual code for parts of the project


% BASE STATS -------------------------------------------------------------
% geography(Biome, Temp, Rain, Food) 
%   describes the temperature (hot, normal, cold), rainfall (humid, normal, dry), 
%   and food availability (plentiful, normal, scarce) of each biome.
geography(desert, hot, dry, scarce).
geography(jungle, hot, humid, plentiful).
geography(plains, normal, normal, normal).

% species(Species, PS, PR, Temp, Rain)
%   gives the base survival probability PS, reproduction probability PR,
%   and preferred temperature and rain levels.
species(wolf, 0.65, 0.35, cold, normal).
species(rabbit, 0.5, 0.75, normal, normal).
species(tortise, 0.9, 0.25, hot, dry).


% SURVIVAL AND REPRODUCTION PROBABILITY CALCULATIONS ---------------------
% Increase of decrease the survival and reproduction probabilities
decrease(PS, PR, APS, APR) :- APS is PS*0.9, APR is PR*0.9.
increase(PS, PR, APS, APR) :- APS is PS*1.1, APR is PR*1.1.

% adjust(PS, PR, LPrefered, LActual, APS, APR)
%   adjustes the survival probability PS and reproduction probability PR
%   based on the lists of prefered and actual environmental conditions to 
%   get the adjusted survival probability APS and reproduction probabiliyt APR.
% 
%   In preferd conditions probability increases, in normal conditions it 
%   doesn't change, and in different conditions it decreases.
adjust(PS, PR, [], [], PS, PR).
adjust(PS, PR, [Prefered|T1], [Prefered|T2], APS, APR) :- 
    increase(PS, PR, APS1, APR1), adjust(APS1, APR1, T1, T2, APS, APR).
adjust(PS, PR, [Prefered|T1], [normal|T2], APS, APR) :- 
    dif(Prefered, normal), adjust(PS, PR, T1, T2, APS, APR).
adjust(PS, PR, [Prefered|T1], [Actual|T2], APS, APR) :- 
    dif(Prefered, Actual), dif(Actual, normal), decrease(PS, PR, APS1, APR1), adjust(APS1, APR1, T1, T2, APS, APR).

% survival(Species, Location, APS, APR)
%   gives the adjusted survival probability APS and reproduction probability APR 
%   based on geography factors for the Species living in Location
survival(Species, Location, APS, APR) :- 
    species(Species, PS, PR, Temp1, Rain1), 
    geography(Location, Temp2, Rain2, Food),
    adjust(PS, PR, [Temp1, Rain1, plentiful], [Temp2, Rain2, Food], APS, APR).

% population(Species, N) 
%   is a data structure that says there are N individuals of Species

% interaction(Population, LP, PS, PR, APS, APR)
%   Adjusts the survival and reproduction probabilities PS and PR of Species based on
%   based on the presence of other populations LP to get the adjusted survival and
%   reproduction probabilities APS and APR
interaction(_, [], PS, PR, PS, PR).
interaction(population(rabbit,NR), [population(wolf, NW)|T], PS, PR, APS, APR) :- 
    APS1 is PS*NR/(NR+NW),
    interaction(population(rabbit,NR), T, APS1, PR, APS, APR).
interaction(population(wolf,NW), [population(rabbit, NR)|T], PS, PR, APS, APR) :- 
    APS1 is PS*(2-NW/(NR+NW)),
    interaction(population(wolf,NW), T, APS1, PR, APS, APR).

interaction(population(rabbit,N), [population(S,_)|T], PS, PR, APS, APR) :- 
    dif(S, wolf),
    interaction(population(rabbit,N), T, PS, PR, APS, APR).
interaction(population(wolf,N), [population(S,_)|T], PS, PR, APS, APR) :- 
    dif(S, rabbit),
    interaction(population(wolf,N), T, PS, PR, APS, APR).
interaction(population(S1,N), [_|T], PS, PR, APS, APR) :- 
    dif(S1, rabbit),
    dif(S1, wolf),
    interaction(population(S1,N), T, PS, PR, APS, APR).
% interaction(population(rabbit,10), [population(wolf, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.454545, APR = 0.5
% interaction(population(rabbit,10), [population(wolf, 1), population(rabbit,10), population(tortise, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.454545, APR = 0.5
% interaction(population(tortise,10), [population(wolf, 1), population(tortise, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.5, APR = 0.5
% interaction(population(rabbit,10), [population(tortise, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.5, APR = 0.5
% interaction(population(wolf,10), [population(tortise, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.5, APR = 0.5
% interaction(population(wolf,10), [population(rabbit, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.5454545, APR = 0.5
% interaction(population(wolf,10), [population(tortise, 1), population(rabbit, 20)], 0.5, 0.5, APS, APR).
%   => APS = 0.8333, APR = 0.5


% RUNNING A NATURAL SELECTION SIMULATION ---------------------------------
% flag(PS, PR, S_float, R_float, S_flag, R_flag)
%   given the survival and reproduction probabilities PS and PR, and the random numbers S_float and R_float, 
%   gives the flags S_flag and R_flag describing if an individual will survive and reproduce
flag(PS, _, S_float, _, dns, dnr) :- S_float >= PS.
flag(PS, PR, S_float, R_float, survives, dnr) :- S_float < PS, R_float >= PR.
flag(PS, PR, S_float, R_float, survives, reproduces) :- S_float < PS, R_float < PR.

% onedaychange(APS, APR, Survives, Reproduces, N, F)
%   Tells how the number of individuals N will change over one day 
%   based on the adjusted survival and reproduction probabilities APS and APR
%   F is the final population count after one day
%   (a helper function for onedaypopulation below)
onedaychange(_,_,_,_,0,0).
onedaychange(APS, APR, dns, _, N1, F) :-
    N1 > 0,
    N2 is N1-1, 
    S_float is random_float(),
    R_float is random_float(),
    flag(APS, APR, S_float, R_float, Survives, Reproduces),
    onedaychange(APS, APR, Survives, Reproduces, N2, F).
onedaychange(APS, APR, survives, dnr, N1, F1) :-
    N1 > 0,
    N2 is N1-1, 
    S_float is random_float(),
    R_float is random_float(),
    flag(APS, APR, S_float, R_float, Survives, Reproduces),
    onedaychange(APS, APR, Survives, Reproduces, N2, F2),
    F1 is F2+1.
onedaychange(APS, APR, survives, reproduces, N1, F1) :-
    N1 > 0,
    N2 is N1-1, 
    S_float is random_float(),
    R_float is random_float(),
    flag(APS, APR, S_float, R_float, Survives, Reproduces),
    onedaychange(APS, APR, Survives, Reproduces, N2, F2),
    F1 is F2+2.
% onedaychange(0.5, 0.5, survives, reproduces, 2, F).
%   => should answer 2 about 50% of time, 3 with 25%, and 4 with 25%

% environment(LP, Location)
%   is a data structure contianing a list of all the populations LP that live in the given Location

% onedaypopulation(population(Species, N), environment(LP, Location), population(Species, F))
%   Starting with population(Species, N) living in the environment(LP, Location), 
%   population(Species, F) is how this initial population changes after one day
onedaypopulation(population(_, 0), _, population(_, 0)).
onedaypopulation(population(Species, N), environment(LP, Location), population(Species, F)) :- 
    N > 0,
    survival(Species, Location, PS, PR),
    interaction(population(Species, N), LP, PS, PR, APS, APR),
    S_float is random_float(),
    R_float is random_float(),
    flag(APS, APR, S_float, R_float, Survives, Reproduces),
    onedaychange(APS, APR, Survives, Reproduces, N, F).

% onedaypopulation(population(rabbit,10), environment([population(wolf, 1)], plains), POP).
% onedaypopulation(population(rabbit,10), environment([population(wolf, 1), population(rabbit,10), population(tortise, 1)], plains), POP).
%   => both of these give the rabbit the same chance of survival. On average the rabbit population will slightly increase
% onedaypopulation(population(wolf,10), environment([population(tortise, 1)], desert), POP).
%   => on average will slightly decrease the wolf population
