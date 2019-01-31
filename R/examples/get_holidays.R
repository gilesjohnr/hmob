# Add indicator for trips that occur during national holidays or school breaks
tmp <- get.holidays(
     d=read.csv("BeyondCommuting2/trip_lengths/trip_durations_longform.csv"),
     hol=read.csv('BeyondCommuting2/data/national_holidays_clean.csv', stringsAsFactors = FALSE),
     sch=read.csv('BeyondCommuting2/data/school_terms_clean.csv', stringsAsFactors = FALSE)
)
head(tmp)