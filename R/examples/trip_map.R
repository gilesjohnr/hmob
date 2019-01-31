par(mfrow=c(1,2))
trip.map(28, 20) # Generate map with district names
title(get.names(28, 20))
trip.map(42, c(16, 20, 1)) # Get map with multiple destinations
title(get.names(42))