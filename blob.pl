% entity_traits(type_of_entity, reproduction_probability, death_probability)
entity_traits(blob, 0.5, 0.75).
entity(blob, 0.5).

% population( initial_number_of_entities, type_of_entity, number_of_days_to_simulate, final_number_of_entities)
population(N, _, 0, N). % Without advancing any days initial = final
population(N, Entity, D1, F) :-  N > 0, D1 > 0, entity_traits(Entity, Birth, Death), I is N + N*Birth - N*Death, D2 is D1-1, population(I, Entity, D2, F).
% need to make sure that D and N are greater than zero, or we get an infinite loop

% How the population changes over one day
% pop_1day(N, Entity, RNB, T, F)
%   N is the number of entities left to examine
%   Entity is the type/species of this population
%   RNB is a random number that determines if this individual reproduces
%   T is the accumulator that stores the final population count of individuals already examine
%   F is the final population count including all individuals

pop_1day(0,_,_,T,T). % A population with no additional individuals 
% If entity does not die, and succeeds in reproducing, we add an additional individual to the population
pop_1day(N1, Entity, RNB1, T1, F) :- N1 > 0, entity(Entity, PBirth), RNB1 < PBirth, N2 is N1-1, T2 is T1+2, random(RNB2), pop_1day(N2, Entity, RNB2, T2, F).
pop_1day(N1, Entity, RNB1, T1, F) :- N1 > 0, entity(Entity, PBirth), RNB1 >= PBirth, N2 is N1-1, T2 is T1+1, random(RNB2), pop_1day(N2, Entity, RNB2, T2, F).