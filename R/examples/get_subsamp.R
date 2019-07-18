load('./data/duration_data_arrays_3day_full.Rdata') # y.month, y.route, y.pop 

subsamp <- get.subsamp(y.route, min.locations=30, min.samp=10)

district.subset <- dimnames(subsamp)$origin
trip.map(as.numeric(district.subset))