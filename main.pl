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
%   and preferent temperatur and rain levels.
species(wolf, 0.75, 0.35, cold, normal).
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

% population(Species, Location, N) 
%   is a data structure that says there are N individuals of Species in Location

% interaction(Population, LP, PS, PR, APS, APR)
%   Adjusts the survival and reproduction probabilities PS and PR of Species based on
%   based on the presence of other populations LP to get the adjusted survival and
%   reproduction probabilities APS and APR
interaction(_, [], PS, PR, PS, PR).
interaction(population(rabbit,L,NR), [population(wolf, _, NW)|T], PS, PR, APS, APR) :- 
    APS1 is PS*NR/(NR+NW),
    interaction(population(rabbit,L,NR), T, APS1, PR, APS, APR).
interaction(population(wolf,L,NW), [population(rabbit, _, NR)|T], PS, PR, APS, APR) :- 
    APS1 is PS*(1.5-NW/(NR+NW)),
    interaction(population(wolf,L,NW), T, APS1, PR, APS, APR).

interaction(population(rabbit,L,N), [population(S,_,_)|T], PS, PR, APS, APR) :- 
    dif(S, wolf),
    interaction(population(rabbit,L,N), T, PS, PR, APS, APR).
interaction(population(wolf,L,N), [population(S,_,_)|T], PS, PR, APS, APR) :- 
    dif(S, rabbit),
    interaction(population(wolf,L,N), T, PS, PR, APS, APR).
interaction(population(S1,L,N), [_|T], PS, PR, APS, APR) :- 
    dif(S1, rabbit),
    dif(S1, wolf),
    interaction(population(S1,L,N), T, PS, PR, APS, APR).
% interaction(population(rabbit,l,10), [population(wolf, l, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.454545, APR = 0.5
% interaction(population(rabbit,l,10), [population(wolf, l, 1), population(rabbit,l,10), population(tortise, l, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.454545, APR = 0.5
% interaction(population(tortise,l,10), [population(wolf, l, 1), population(tortise, l, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.5, APR = 0.5
% interaction(population(rabbit,l,10), [population(tortise, l, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.5, APR = 0.5
% interaction(population(wolf,l,10), [population(tortise, l, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.5, APR = 0.5
% interaction(population(wolf,l,10), [population(rabbit, l, 1)], 0.5, 0.5, APS, APR).
%   => APS = 0.29545, APR = 0.5
% interaction(population(wolf,l,10), [population(tortise, l, 1), population(rabbit, l, 20)], 0.5, 0.5, APS, APR).
%   => APS = 0.58333, APR = 0.5

% environment(LP, Region)
%   is a data structure contianing a list of all the populations LP that live in the given Region
%   Region describes an area of the map (north, east, south, west)



% Basic Natural Selection Functionality ------------------------------------------------------------------------
% entity(Species, PS, PR)
%   Species is the type of species
%   PS is the probability that the entity survives
%   PR is the conditional probability that it reproduces (if it survives)
entity(blob, 0.75, 0.5).

% pop1day(N, Species, Survive, Reproduce, T, F)
%   N is the number of individuals in the population
%   Specied is the species of this population
%   Survive is true if this individual will survive this day, false otherwise
%   Reproduce (conditional on survival) is true is the individual reproduces, false otherwise
%   T is the accumulator
%   F is the final population of these N individuals plus the value of the accumulator

pop1day(0,_,_,_,F,F).
% case that individual dies
pop1day(N1, Species, RNS, _ , T1, F) :-
    N1 > 0,
    entity(Species, PS, _),
    RNS > PS, % This individual does not survive
    N2 is N1-1, 
    T2 is T1, 
    pop1day(N2, Species, random_float(), random_float(), T2, F).
% case individual survives but doesn't reproduce
pop1day(N1, Species, RNS, RNR , T1, F) :- 
    N1 > 0,
    entity(Species, PS, PR),
    RNS < PS, % This individual survives
    RNR > PR, % This individual does not reproduce
    N2 is N1-1, 
    T2 is T1+1, 
    pop1day(N2, Species, random_float(), random_float(), T2, F).
% case individual survives and reproduces
pop1day(N1, Species, RNS, RNR , T1, F) :- 
    N1 > 0,
    entity(Species, PS, PR),
    RNS < PS, % This individual survives
    RNR < PR, % This individual reproduce
    N2 is N1-1, 
    T2 is T1+2, 
    pop1day(N2, Species, random_float(), random_float(), T2, F).
