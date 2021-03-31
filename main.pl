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
pop1day(N1, Species, false, _ , T1, F) :- N1 > 0, N2 is N1-1, T2 is T1, entity(Species, PR, PS), pop1day(N2, Species, random_float() < PS, random_float() < PR, T2, F).
% case individual survives but doesn't reproduce
pop1day(N1, Species, true, false , T1, F) :- N1 > 0, N2 is N1-1, T2 is T1+1, entity(Species, PR, PS), pop1day(N2, Species, random_float() < PS, random_float() < PR, T2, F).
% case individual survives and reproduces
pop1day(N1, Species, true, true , T1, F) :- N1 > 0, N2 is N1-1, T2 is T1+2, entity(Species, PR, PS), pop1day(N2, Species, random_float() < PS, random_float() < PR, T2, F).