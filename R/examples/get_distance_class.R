# Add district population sizes and route classifications 
tmp <- get.distance.class(
     a=read.csv("BeyondCommuting2/trip_lengths/trip_durations_longform.csv"),
     b=read.csv("BeyondCommuting2/data/route_classification.csv")
)
head(tmp)