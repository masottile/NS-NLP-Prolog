% entity_traits(type_of_entity, reproduction_rate, death_rate)
entity_traits(blob, 0.5, 0.75).

% population( initial_number_of_entities, type_of_entity, number_of_days_to_simulate, final_number_of_entities)
population(N, _, 0, N). % Without advancing any days initial = final
population(N, Entity, D1, F) :-  N > 0, D1 > 0, entity_traits(Entity, Birth, Death), I is N + N*Birth - N*Death, D2 is D1-1, population(I, Entity, D2, F).
% need to make sure that D and N are greater than zero, or we get an infinite loop