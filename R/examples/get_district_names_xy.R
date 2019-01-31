# Add district names and coordinates
t <- Sys.time()
tmp <- get.district.names.xy(
     a=read.csv("BeyondCommuting2/trip_lengths/trip_durations_longform.csv"), 
     b=read.csv("BeyondCommuting2/data/namibia_adm2_centroids_name_number_mapping.csv")
)
head(tmp)