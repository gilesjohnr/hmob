# process trip length data into long form data set with dates
t <- Sys.time()
tmp <- parse.longform(
     d=read.table('BeyondCommuting2/trip_lengths/trip_duration_counts.txt', header = FALSE, sep = "|"), 
     n.cores=6
)

head(tmp)

write.csv(tmp, file="BeyondCommuting2/trip_lengths/trip_durations_longform.csv", row.names=FALSE)
Sys.time() - t