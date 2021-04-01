% Actual code for parts of the project

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
