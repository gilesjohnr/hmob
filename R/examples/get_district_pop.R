# Add district population sizes
tmp <- get.district.pop(
     a=read.csv("BeyondCommuting2/trip_lengths/trip_durations_longform.csv"), 
     b=read.csv("BeyondCommuting2/data/namibia_pop_from_worldpop_adm2.csv")
)
head(tmp)