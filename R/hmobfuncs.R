##' Parse raw trip duration data
##'
##' This function takes the \code{d.raw} data object (trip_duration_counts.txt) or the \code{d.raw.42} data object (trip_durations_counts_sub_42.txt)
##' and parses the complex character string in the third column into a long form data set with date information. This function takes a while to run on the raw 
##' trip duration data, more cores are better.
##'
##' @param d raw text file containing trip duration data
##' @param n.cores number cores to use when parallel computing
##' 
##' @return a dataframe with the district IDs and names of the origin and destination districts, counts of each trip duration and detailed date information
##' 
##' @author John Giles
##'
##' @example R/examples/parse_longform.R
##' 
##' @family data synthesis
##' 
##' @export
##' 

parse.longform <- function(d,           # expects 'trip_durations_counts_sub_42.txt'
                            n.cores     # number cores to use when parallel computing
){
     
     require(stringr)
     require(foreach)
     require(doParallel)
     
     print("Loading data...", quote=F)
     
     d[,3] <- as.character(d[,3])
     
     registerDoParallel(cores=n.cores) 
     out <- foreach(i=1:nrow(d), .combine=rbind) %dopar% {
          
          # split day and all length,count terms
          tmp <- unlist(stringr::str_split(d[i,3], '[-]')) 
          day <- tmp[1]
          
          # split langth and count terms
          tmp <- do.call(rbind, 
                         stringr::str_split(
                              unlist(stringr::str_split(tmp[2],'[;]')),
                              '[,]'))
          
          # recombine with origin, destination, and day
          tmp <- apply(cbind(rep(d[i,1], nrow(tmp)),
                             rep(d[i,2], nrow(tmp)),
                             rep(day, nrow(tmp)), 
                             tmp), 
                       2, 
                       as.numeric)
     }
     
     print("Adding date info...", quote=F)
     
     row.names(out) <- NULL
     out <- as.data.frame(out)
     names(out) <- c('from', 'to', 'day', 'duration', 'count')
     
     out$date <- as.Date(out$day-1, origin='2010-10-01')
     out$year <- format(out$date,"%Y")
     out$month <- format(out$date,"%m")
     out$week <- format(out$date,"%V")
     tmp <- factor(format(out$date,"%A")) 
     out$dow <- as.numeric(factor(tmp, levels(tmp)[c(2,6,7,5,1,3,4)])) # Monday = day 1
     out$doy <- format(out$date,"%j")
     
     print("Done.", quote=F)
     
     return(out)
}

##' Add district names and coordinates to long form data set
##'
##' This function takes output from the \code{parse.long.form} function and appends columns containing district names and coordinates
##'
##' @param a expects output from \code{parse.long.form} (trip_durations_longform.csv)
##' @param b expects data object \code{d.meta.1} (namibia_adm2_centroids_name_number_mapping.csv)
##' 
##' @return dataframe
##' 
##' @author John Giles
##'
##' @example R/examples/get_district_names_xy.R
##' 
##' @family data synthesis
##' 
##' @export
##' 

get.district.names.xy <- function(a, # expects "trip_durations_longform.csv"
                                  b  # expects "namibia_adm2_centroids_name_number_mapping.csv"
){
     
     require(foreach)
     
     print("Loading data...", quote=F)
     
     NA -> a$name_from -> a$name_to -> a$x_from -> a$y_from -> a$x_to -> a$y_to
     
     # get unique district IDs
     unique.district.IDs <- unique(c(a$from, a$to))
     
     foreach(i=unique.district.IDs) %do% {
          
          print(i)
          
          # get name of unique district ID
          district.name <- as.character(b[which(b$ID_2 == i), 'NAME_2'])
          
          # assign name to corresponding district IDs in long form data
          a[which(a$from == i), 'name_from'] <- district.name
          a[which(a$to == i), 'name_to'] <- district.name
          
          # get coordinates of unique district ID
          district.x <- b[which(b$ID_2 == i), 'X']
          district.y <- b[which(b$ID_2 == i), 'Y']
          
          # assign coordinates to corresponding district IDs in long form data
          a[which(a$from == i), 'x_from'] <- district.x
          a[which(a$from == i), 'y_from'] <- district.y
          a[which(a$to == i), 'x_to'] <- district.x
          a[which(a$to == i), 'y_to'] <- district.y
     }
     
     print("Done.", quote=F)
     return(a)
}

##' Add district population sizes to long form data set
##'
##' This function takes output from the \code{parse.long.form} function and appends columns containing population size
##'
##' @param a expects output from \code{parse.long.form} (trip_durations_longform.csv)
##' @param b expects data object \code{d.meta.2} (namibia_pop_from_worldpop_adm2.csv)
##' 
##' @return dataframe
##' 
##' @author John Giles
##'
##' @example R/examples/get_district_pop.R
##' 
##' @family data synthesis
##' 
##' @export
##' 

get.district.pop <- function(a, # expects "trip_durations_longform2.csv" or similar
                             b  # expects "namibia_pop_from_worldpop_adm2.csv"
){
     
     require(foreach)
     
     print("Loading data...", quote=F)
     
     NA -> a$pop_from -> a$pop_to
     
     # get unique district IDs
     unique.district.IDs <- unique(c(a$from, a$to))
     
     foreach(i=unique.district.IDs) %do% {
          
          print(i)
          
          # get population size for unique district ID
          district.pop <- b[which(b$ID_2 == i), 'popsum']
          
          # assign population size to corresponding district IDs in long form data
          a[which(a$from == i), 'pop_from'] <- district.pop
          a[which(a$to == i), 'pop_to'] <- district.pop
     }
     
     a$pop_ratio <- a$pop_to/a$pop_from
     print("Done.", quote=F)
     return(a)
}

##' Add distance between origin and destination districts to long form data set
##'
##' This function takes output from the \code{parse.long.form} function and appends columns containing distance between origin and destination districts.
##' This function also adds two columns indicating the commuting classification of the travel:
##' \enumerate{
##'      \item \code{rcl_same_adm1} = whether trip was within same admin unit
##'      \item \code{cl_contig} = whether trip was within same, to adjacent, or to non-adjacent admin unit
##'      }
##'
##' @param a expects output from \code{parse.long.form} (trip_durations_longform.csv)
##' @param b expects data object \code{d.meta.3} (route_classification.csv)
##' 
##' @return dataframe
##' 
##' @author John Giles
##'
##' @example R/examples/get_distance_class.R
##' 
##' @family data synthesis
##' 
##' @export
##' 

get.distance.class <- function(a,  # expects 'trip_durations_longform3.csv'
                               b   # expects 'route_classification.csv'
                               
){
     
     require(foreach)
     require(stringr)
     
     print("Loading data...", quote=F)
     
     a$from_to <- stringr::str_c(a$from, "_", a$to)
     i_j <- stringr::str_c(b$i, "_", b$j)
     
     NA -> a$distance -> a$class_same -> a$class_contig
     
     foreach(i=unique(a$from_to)) %do% {
          
          print(i)
          
          # get distance between unique from_to trip
          a[which(a$from_to == i), 'distance'] <- b[which(i_j == i), 'd_km']
          
          # class_same: whether trip was within same admin unit
          a[which(a$from_to == i), 'class_same'] <- as.character(b[which(i_j == i), 'cl_same_adm1'])
          
          # class_contig: trip was within same, to adjacent, or to non-adjacent admin unit
          a[which(a$from_to == i), 'class_contig'] <- as.character(b[which(i_j == i), 'cl_contig'])
     }
     
     print("Done.", quote=F)
     return(a)
}

##' Add national holidays and school breaks to long form data set
##'
##' This function takes output from the \code{parse.long.form} function and appends two columns with a binary response, 
##' where a 1 indicates that the date for that trip observation occured on a national holiday or a school break
##' 
##' @param d expects output from \code{parse.long.form} (trip_durations_longform.csv)
##' @param hol expects data object \code{national.holidays} (national_holidays_clean.csv)
##' @param sch expects data object \code{school.terms} (school_terms_clean.csv)
##' 
##' @return dataframe
##' 
##' @author John Giles
##'
##' @example R/examples/get_holidays.R
##' 
##' @family data synthesis
##' 
##' @export
##' 

get.holidays <- function(d,       # expects longform trip data
                         hol,     # expects national_holidays_clean.csv
                         sch      # expects school_terms_clean.csv
                         
) {
     
     print("Loading data...", quote=F)
     
     # month-day format of trip date
     trip.days <- format(as.Date(d$date), '%m-%d') # get month-day of trip observations
     
     # get mon-day format for national holidays
     print("Attaching national holiday indicator...", quote=F)
     hol <- as.Date(hol$date)
     hol <- sort(unique(c(hol, hol-1, hol+1))) # days immediately prior or post holidays
     hol.days <- format(as.Date(hol), '%m-%d')
     
     # trip days that are during holidays (or days immediately prior or post)
     d$holiday <- as.integer(trip.days %in% hol.days) 
     
     # add indicator taht trip occurred during school break (NOT during school term)
     print("Attaching school break indicator", quote=F)
     sch$start <- as.Date(sch$start)
     sch$stop <- as.Date(sch$stop)
     
     sch.days <- as.Date(vector())
     for(i in 1:3) sch.days <- c(sch.days, as.Date(seq(sch$start[i], sch$stop[i], by='day')))
     
     d$school <- as.integer(!(trip.days %in% sch.days)) # trip days that are NOT during school terms
     
     return(d)
}

##' Plot map of origin and destination district(s)
##'
##' A function that maps the origin and destination(s) given district IDs
##' 
##' @param a origin district (can take integer ID or character name)
##' @param b destination district (can take integer ID or character name), default = NULL
##' @param distID district IDs and names, expacts the \code{districtIDs} data object (NamNames.csv)
##' @param adm administrative level 2 data, expects NAM_adm2.shp
##' @param tol tolerance argument passed to \code{gSimplify}, default = 0.1, set to zero for original
##' 
##' @return plotted map
##' 
##' @author John Giles
##'
##' @example R/examples/trip_map.R
##' 
##' @family utility
##' 
##' @export
##' 

trip.map <- function(a, # origin district (can take integer ID or character name)
                     b=NULL, # destination district (can take integer ID or character name)
                     distID=districtIDs, # district IDs and names (expects NamNames.csv)
                     admin=adm,     # administrative level 2 data (expects NAM_adm2.shp)
                     tol=0.1      # tolerance argument for gSimplify, set to zero for original
){
     
     # District name required for accessing shapefiles slots
     # Get district name if integer ID provided
     if(is.character(a) == FALSE) a <- distID[which(distID[,1] %in% a), 2] 
     
     # Get origin shapefile
     a <- admin[which(admin$NAME_2 %in% a),]
     
     if (is.null(b)) {
          
          # Plot all districts
          plot(rgeos::gSimplify(admin, tol=tol, topologyPreserve=TRUE), border='grey60', lwd=1.25)
          
          # Add origin
          plot(rgeos::gSimplify(a, tol=tol, topologyPreserve=TRUE), col=rgb(0,0,1, alpha=0.3), add=T)
          plot(rgeos::gCentroid(a, byid=TRUE), pch=21, cex=1.75, bg="blue", lwd=2, add=T)
          
     } else {
          
          # Get district name if integer ID provided
          if(is.character(b) == FALSE) b <- distID[which(distID[,1] %in% b), 2]
          
          # Get destination shapefile
          b <- admin[which(admin$NAME_2 %in% b),]
          
          # Plot all districts
          plot(rgeos::gSimplify(admin, tol=tol, topologyPreserve=TRUE), border='grey60', lwd=1.25)
          
          # Add origin
          plot(rgeos::gSimplify(a, tol=tol, topologyPreserve=TRUE), col=rgb(0,0,1, alpha=0.3), border='black', add=T)
          plot(rgeos::gCentroid(a, byid=TRUE), pch=21, cex=1.75, bg="blue", lwd=2, add=T)
          
          # Add destination
          plot(rgeos::gSimplify(b, tol=tol, topologyPreserve=TRUE), col=rgb(1,0,0, alpha=0.3), border='black', add=T)
          plot(rgeos::gCentroid(b, byid=TRUE), pch=21, cex=1.75, bg="red", lwd=2, add=T)
     }
}

##' Get title for any origin and destination
##'
##' Makes character strings given origin and destination and districtIDs. Use with \code{trip.map} function:
##' \enumerate{
##' \item The \code{trip.map} function takes the origin \code{a} and destination \code{b} and makes a simple map to visualize the districts being used. Origin is a blue triangle, destination is a red circle.
##' \item The \code{get.names} function prints a 'from and to' character string with district names.
##' } 
##' 
##' @param a origin district (can take integer ID or character name)
##' @param b destination district (can take integer ID or character name), default = NULL
##' @param d district IDs and names, expacts the \code{districtIDs} data object (NamNames.csv)
##' 
##' @return character string
##' 
##' @author John Giles
##'
##' @example R/examples/get_names.R
##' 
##' @family utility
##' 
##' @export
##' 

get.names <- function(a, # integer ID of origin
                      b=NULL, # integer ID of destination 
                      d=districtIDs
){
     if (is.null(b)) {
          paste('From ', d[d[,1] == a, 2], ' (', a, ')', sep='')
     } else {
          paste('From ', d[d[,1] == a, 2], ' (', a, ') ',
                'to ', d[d[,1] == b, 2], ' (', b, ')',
                sep='')  
     }
}

##' Quick look at matrices
##'
##' Displays the first 6 rows and columns of a matrix object
##' 
##' @param m matrix object
##' @param i number of rows to preview, default = 6
##' @param j number of columns to display, default = 6
##' 
##' @return matrix
##' 
##' @author John Giles
##'
##' @example R/examples/see.R
##' 
##' @family utility
##' 
##' @export
##' 

see <- function(m, i=6, j=6) m[1:i, 1:j] 

##' Get matrix of trip distance counts
##'
##' The \code{get.distance.counts} function calculates a matrix or long-form dataframe of trip distance counts for a temporal 
##' interval \code{t} given an origin \code{a}. Only takes origin district because it uses distances across all destinations.
##' 
##' @param a origin district (can take integer ID or character name)
##' @param t character string indicating the temporal interval the count trip distances (e.g. 'month', 'week', 'doy')
##' @param d longform mobility data, expects \code{d.42} data object (trip_durations_longform_metadata_42.csv) or similar
##' @param type return a 'matrix' or a 'dataframe' with counts in longform, default = 'matrix'
##' @param distID district IDs and names, expects \code{districtIDs} data object (NamNames.csv), default = districtIDs
##' @param n.cores number cores to use when parallel computing (default = NULL, which uses half available cores)
##' 
##' @return matrix or dataframe
##' 
##' @author John Giles
##'
##' @example R/examples/get_distance_counts.R
##' 
##' @family data synthesis
##' 
##' @export
##' 

get.distance.counts <- function(a, # integer ID of origin district
                                t, # character string of temporal interval (e.g. 'month', 'week', 'doy')
                                d,  # expects "trip_durations_longform2.csv" or similar
                                type='matrix', # return a matrix or a 'dataframe' with counts in longform
                                distID=districtIDs, # district IDs and names (expects NamNames.csv)
                                n.cores=NULL 
){
     
     # Get integer ID if district name provided 
     if(is.character(a) == TRUE) a <- distID[which(distID[,2] == a),1]
     
     # Subset trip
     d <- d[which(d$from == a),]
     
     x <- sort(unique(d[,t])) # time
     y <- sort(unique(d$distance)) # distance
     
     registerDoParallel(cores=n.cores)
     if (type == 'matrix') {
          
          out <- foreach(i=seq_along(x), .combine='rbind') %:%
               foreach(j=seq_along(y), .combine='c') %dopar% {
                    sum(d[which(d[,t] == x[i] & d$distance == y[j]), 'count'])
               }
          
          dimnames(out) <- list('time'=x, 'distance'=y) 
          
     } else if (type == 'dataframe') {
          
          out <- expand.grid(x, y)
          names(out) <- c(t, 'distance')
          out$count <- foreach(i=1:nrow(out), .combine='c') %dopar% {
               sum(d[which(d[,t] == out[i, 1] & d$distance == out[i,2]), 'count'])
          }
     }
     
     return(out)
}

##' Get matrix of trip duration counts
##'
##' The \code{get.duration.counts} function calculates a matrix or long-form dataframe of trip duration counts for a temporal 
##' interval \code{t} given an origin \code{a} and destination \code{b}. Only takes origin district because it uses distances across all destinations.
##' 
##' @param a origin district (can take integer ID or character name)
##' @param b destination district (can take integer ID or character name)
##' @param t character string indicating the temporal interval the count trip distances (e.g. 'month', 'week', 'doy')
##' @param d longform mobility data, expects \code{d.42} data object (trip_durations_longform_metadata_42.csv) or similar
##' @param type return a 'matrix' or a 'dataframe' with counts in longform, default = 'matrix'
##' @param distID district IDs and names, expects \code{districtIDs} data object (NamNames.csv), default = districtIDs
##' @param n.cores number cores to use when parallel computing (default = NULL, which uses half available cores)
##' 
##' @return matrix or dataframe
##' 
##' @author John Giles
##'
##' @example R/examples/get_duration_counts.R
##' 
##' @family data synthesis
##' 
##' @export
##' 

get.duration.counts <- function(a, # integer ID of origin district
                                b, # integer ID of destination district
                                t, # character string of temporal interval (e.g. 'month', 'week', 'doy')
                                d,  # expects "trip_durations_longform2.csv" or similar
                                type='matrix', # return a matrix or a 'dataframe' with counts in longform
                                distID=districtIDs, # district IDs and names (expects NamNames.csv)
                                n.cores=NULL 
){
     
     # Get integer ID if district name provided 
     if(is.character(a) == TRUE) a <- distID[which(distID[,2] == a),1] 
     if(is.character(b) == TRUE) b <- distID[which(distID[,2] == b),1]
     
     # Subset trip
     d <- d[d$from_to == stringr::str_c(a, '_', b),]
     
     x <- sort(unique(d[,t])) # time values
     y <- sort(unique(d$duration)) # duration values
     
     registerDoParallel(cores=n.cores)
     if (type == 'matrix') {
          
          out <- foreach(i=seq_along(x), .combine='rbind') %:%
               foreach(j=seq_along(y), .combine='c') %dopar% {
                    sum(d[which(d[,t] == x[i] & d$duration == y[j]), 'count'])
               }
          
          dimnames(out) <- list('time'=x, 'duration'=y)  
          
     } else if (type == 'dataframe') {
          
          out <- expand.grid(x, y)
          names(out) <- c(t, 'duration')
          out$count <- foreach(i=1:nrow(out), .combine='c') %dopar% {
               sum(d[which(d[,t] == out[i, 1] & d$duration == out[i,2]), 'count'])
          }
     }
     
     return(out)
}

##' Get counts of any two variables (e.g. distance vs duration)
##'
##' The \code{get.xy.counts} function counts the number of trips made conditioned on two variables (\code{x.var} and \code{y.var}) 
##' in the data set. This is set up to explore how trip duration counts vary with other variables in the data set 
##' (e.g. distance, district population size). Can be restricted to an origin district \code{a} or all origins \code{a} = NULL.
##' 
##' @param a origin district (can take integer ID or character name)
##' @param d longform mobility data, expects \code{d.42} data object (trip_durations_longform_metadata_42.csv) or similar
##' @param x.var name of x variable
##' @param y.var name of y variable (default = 'duration')
##' @param x.int interval for aggregating x variable
##' @param y.int interval for aggregating y variable
##' @param type return a 'matrix' or a 'dataframe' with counts in longform (default = 'matrix')
##' @param distID district IDs and names, expects \code{districtIDs} data object (NamNames.csv), default = districtIDs
##' @param n.cores number cores to use when parallel computing (default = NULL, which uses half available cores)
##' 
##' @return matrix or dataframe
##' 
##' @author John Giles
##'
##' @example R/examples/get_xy_counts.R
##' 
##' @family data synthesis
##' 
##' @export
##' 

get.xy.counts <- function(a, # integer ID of origin district, if NULL all origins are used!
                          d, # expects "trip_durations_longform2.csv" or similar
                          x.var, # name of x variable
                          y.var='duration', # name of y variable (duration is default response variable)
                          x.int, # distance interval for aggregating x variable
                          y.int, # duration interval for aggregating y variable
                          type='matrix', # or 'dataframe'
                          distID=districtIDs, # district IDs and names (expects NamNames.csv)
                          n.cores=NULL # number of cores to run parallel, if NULL, uses half
){
     
     require(foreach)
     
     # Get integer ID if district name provided 
     if (is.character(a) == TRUE) a <- distID[which(distID[,2] == a),1]
     
     # If specific origin defined subset based on this
     if (!is.null(a)) d <- d[which(d$from == a),] 
     
     x.seq <- seq(x.int, round(max(d[,x.var])/x.int)*x.int, x.int) # x variable intervals
     y.seq <- seq(y.int, round(max(d[,y.var])/y.int)*y.int, y.int) # y variable intervals
     
     registerDoParallel(cores=n.cores)
     if (type == 'matrix') {
          
          out <- foreach(i=seq_along(x.seq), .combine='rbind') %:%
               foreach(j=seq_along(y.seq), .combine='c') %do% {
                    
                    sel <- d[,x.var] > (x.seq[i] - x.int) & d[,x.var] <= x.seq[i] & 
                         d[,y.var] > (y.seq[j] - y.int) & d[,y.var] <= y.seq[j]
                    
                    if (sum(sel) == 0) {
                         out <- 0
                    } else if (sum(sel) > 0) {
                         out <- sum(d[sel, 'count'])
                    }
                    out
               }
          
          dimnames(out) <- list('x'=(x.seq - x.int), 'y'=(y.seq - y.int)) 
          
     } else if (type == 'dataframe') {
          
          out <- expand.grid(x.seq, y.seq)
          names(out) <- c(x.var, y.var)
          out$count <- foreach(i=1:nrow(out), .combine='c') %dopar% {
               
               sel <- d[,x.var] > (out[i,x.var] - x.int) & d[,x.var] <= out[i,x.var] & 
                    d[,y.var] > (out[i,y.var] - y.int) & d[,y.var] <= out[i,y.var]
               
               sum(d[sel, 'count'])
          }
     }
     return(out)
}

##' Build mobility data array
##'
##' This function builds data arrays of total trip counts from longform mobility data (such as that produced by the \code{parse.longform} function). 
##' If \code{time=NULL} (default), route-level counts are returned. If a vector of times (e.g. week or month) is provided, an additional dimension is added to 
##' the array. If a covariate is provided in \code{variable}, the function will return total trip counts that are aggregated according to this variable (e.g. trip distance or trip duration).
##' 
##' 
##' @param orig a vector of origin districts
##' @param dest a vector of destination districts
##' @param time a vector of the time of each observation (e.g. day, week, month, year). When \code{time = NULL} (default), function returns route-level matrix.
##' @param count a vector of total counts of each observation
##' @param variable a vector of covariate values with which to condition trip counts (e.g. distance or trip duration)
##' @param name a character string giving the name of the variable; expects either \code{'distance'}, \code{'duration'}, \code{'movement'}, or \code{'leave'}
##' @param agg.int an integer giving the interval by which to aggregate the given variable, default = 1 (not aggregated). When \code{variable} is trip duration, this is the length 
##' of generation time (in days). When \code{variable} is distance, this is the distance interval in km. Ignored when \code{name} is \code{'movement'} or \code{'leave'}.
##'  
##' @return A 2-, 3-, or 4-dimensional array depending on input parameters. Cells in output array represent total trip counts.
##' 
##' @author John Giles
##' 
##' @example R/examples/mob_data_array.R
##'
##' @family data synthesis
##' 
##' @export
##' 


mob.data.array <- function(orig,                            
                           dest=NULL,                     
                           time=NULL,
                           count,
                           variable=NULL,
                           name,                       # name of the response variable
                           agg.int=1                   # aggregation interval for the response variable (for duration: length of epidemic generation in days, for distance it is number of km)
) {
     
     message(paste(":: Variable is", name, "::"))
     
     if (name == 'distance') {
          
          message(paste("Aggregating to", agg.int, "km...", sep=' '))
          
          n.int <- ceiling(max(variable, na.rm=TRUE)/agg.int)
          ints <- seq(0, agg.int*n.int, agg.int)
          index <- factor(.bincode(variable, ints, right=FALSE), levels=1:n.int)
          
          message("Building data array...")
          
          if (is.null(time)) {
               
               d <- data.frame(orig, index, count)
               out <- reshape2::acast(d, orig ~ index, sum, value.var='count', drop=FALSE, fill=NaN)
               names(dimnames(out)) <- c('origin', name) 
               dimnames(out)[[2]] <- ints[-1]
               
          } else if (!is.null(time)) {
               
               d <- data.frame(orig, time, index, count)
               out <- reshape2::acast(d, orig ~ time ~ index, sum, value.var='count', drop=FALSE, fill=NaN)
               names(dimnames(out)) <- c('origin', 'time', name) 
               dimnames(out)[[3]] <- ints[-1]
          }
          
     } else if (name == 'duration') {
          
          message(paste("Aggregating to generation time of", agg.int, "days...", sep=' '))
          
          n.int <- ceiling(max(variable, na.rm=TRUE)/agg.int)
          ints <- seq(0, agg.int*n.int, agg.int)
          index <- factor(.bincode(variable, ints, right=TRUE), levels=1:n.int)
          
          message("Building data array...")
          
          if (is.null(time)) {
               
               d <- data.frame(orig, dest, index, count)
               out <- reshape2::acast(d, orig ~ dest ~ index, sum, value.var='count', drop=FALSE, fill=NaN)
               names(dimnames(out)) <- c('origin', 'destination', name) 
               dimnames(out)[[3]] <- ints[-1]
               
          } else if (!is.null(time)) {
               
               d <- data.frame(orig, dest, time, index, count)
               out <- reshape2::acast(d, orig ~ dest ~ time ~ index, sum, value.var='count', drop=FALSE, fill=NaN)
               names(dimnames(out)) <- c('origin', 'destination', 'time', name) 
               dimnames(out)[[4]] <- ints[-1]
          }
          
     } else if (name == 'movement') {
          
          message("Building data array...")
          
          if (is.null(time)) {
               
               d <- data.frame(orig, dest, count)
               out <- reshape2::acast(d, orig ~ dest, sum, value.var='count', drop=FALSE, fill=NaN)
               names(dimnames(out)) <- c('origin', 'destination') 
               
          } else if (!is.null(time)) {
               
               d <- data.frame(orig, dest, time, count)
               out <- reshape2::acast(d, orig ~ dest ~ time, sum, value.var='count', drop=FALSE, fill=NaN)
               names(dimnames(out)) <- c('origin', 'destination', 'time') 
          }
          
     } else if (name == 'leave') {
          
          message("Building data array...")
          
          if (is.null(time)) {
               
               d <- data.frame(orig, dest, count)
               out <- reshape2::acast(d, orig ~ dest, sum, value.var='count', drop=FALSE, fill=NaN)
               out <- apply(out, 1, function(x) sum(x, na.rm=TRUE))
               
          } else if (!is.null(time)) {
               
               d <- data.frame(orig, dest, time, count)
               out <- reshape2::acast(d, orig ~ dest ~ time, sum, value.var='count', drop=FALSE, fill=NaN)
               out <- apply(out, c(1,3), function(x) sum(x, na.rm=TRUE))
               names(dimnames(out)) <- c('origin', 'time') 
               
          }
     }
     
     return(out)
}



##' Reduce a mob.data.array object to route-level
##' 
##' This function reduces data array output from the \code{\link{mob.data.array}} function to a route-level matrix that contains the mean trip count. The mean trip count
##' will represent the mean taken across whatever temporal interval was used in the \code{\link{mob.data.array}} function. 
##' 
##' @param x An array produced by the \code{\link{mob.data.array}} function
##' @param variable The variable in the array (expects either \code{'distance'}, \code{'duration'}, or \code{'movement'})
##' 
##' @return A 2-dimensional array when \code{variable = 'distance'} or \code{'movement'}, and a 3-dimensional array when \code{variable = 'duration'}. Cells give the mean trip count taken across the temporal interval of the input array.
##' 
##' @author John Giles
##' 
##' @example R/examples/mob_data_array_route.R
##'
##' @family model processing
##' 
##' @export
##' 

mob.data.array.route.level <- function(x,              # output from mob.data.array function
                                        variable       # 'distance' or 'duration'
){
     
     if (variable == 'distance') {
          
          x <- apply(x, 
                     c(1,3), 
                     function(x) as.integer(round(mean(x, na.rm=TRUE))))
          
     } else if (variable == 'duration') {
          
          x <- apply(x, 
                     c(1,2,4), 
                     function(x) as.integer(round(mean(x, na.rm=TRUE))))
          
     } else if (variable == 'movement') {
          
          x <- apply(x, 
                     c(1,2), 
                     function(x) as.integer(round(mean(x, na.rm=TRUE))))
     }
     
     x[is.nan(x)] <- NA
     return(x)
}

##' Reduce a mob.data.array object to population-level
##' 
##' This function reduces data array output from the \code{\link{mob.data.array}} function to the population-level by taking the mean trip count all routes and temporal intervals.
##' 
##' @param x Array output from the \code{\link{mob.data.array}} function
##' @param variable The variable in the data object (expects either \code{'distance'} or \code{'duration'})
##' 
##' @return vector
##' 
##' @author John Giles
##' 
##' @example R/examples/mob_data_array_route.R
##'
##' @family model processing
##' 
##' @export
##' 

mob.data.array.pop.level <- function(x,              # output from mob.data.array function
                                     variable        # 'distance' or 'duration'
){
     
     if (variable == 'distance') {
          
          x <- apply(x, 
                     3, 
                     function(x) as.integer(round(mean(x, na.rm=TRUE))))
          
     } else if (variable == 'duration') {
          
          x <- apply(x, 
                     4, 
                     function(x) as.integer(round(mean(x, na.rm=TRUE))))
          
     }
     
     x[is.nan(x)] <- NA
     return(x)
}


##' Get posterior parameter estimates from a model object
##' 
##' A function to build a matrix or dataframe of parameter estimates in a \code{runjags} or \code{coda} model object.
##' The function gets the mean, median, standard deviation, and lower and upper 95 % confidence intervals of the estimated posterior distribution 
##' for route-level (\eqn{ij}) or month-level (\eqn{ijt}).
##' 
##' @param n.districts Number of districts in model
##' @param n.t Number of months or other time interval
##' @param name Name of the parameter as it is in the model output
##' @param level The hierarchical level of the model that correpsonds to \code{name} (e.g. 'route' or 'month')
##' @param stats Expects statistics output of \code{\link{coda::summary}} function. If model out put is a \code{runjags} 
##' object, this can be given by: \code{summary(as.mcmc.list(out))$statistics}
##' @param type Return a matrix or dataframe (default = 'matrix')
##' @param n.cores Number of cores to use in parallel computation
##' 
##' @return If level = 'route', a list containing two matrices named \code{mean} and \code{sd}. 
##' If level = 'month', a list containing two 3-dimensional arrays named \code{mean} and \code{sd}.
##' 
##' @author John Giles
##' 
##' @example R/examples/get_param_vals.R
##'
##' @family model processing
##' 
##' @export
##' 

get.param.vals <- function(
     n.districts,          # number of districts
     n.t,                  # number of time intervals (months, defaults to 12)
     name,                 # name of variable
     level='route',        # only route or month at this point
     stats,                # expects statistics output of coda::summary function
     type='matrix',        # 'matrix' or 'dataframe' format to return parameter values (default = 'matrix')
     n.cores=2
) {
     
     mean.labs <- c('Mean', 'mean')
     sd.labs <- c('SD', 'sd')
     lo95.labs <- c('Lower95', '2.5%')
     hi95.labs <- c('Upper95', '97.5%')
     
     registerDoParallel(cores=n.cores)
     
     if (level == 'route') {
          if (type == 'dataframe') {
               out <- foreach(i=1:n.districts, .combine='rbind') %:%
                    foreach(j=1:n.districts, .combine='rbind') %dopar% {
                         
                         sel <- paste(name, '[', i, ',', j, ']', sep='')
                         
                         data.frame(name=sel,
                                    from=i,
                                    to=j,
                                    mean=stats[row.names(stats) == sel, colnames(stats) %in% mean.labs],
                                    sd=stats[row.names(stats) == sel, colnames(stats) %in% sd.labs],
                                    lo95=stats[row.names(stats) == sel, colnames(stats) %in% lo95.labs],
                                    hi95=stats[row.names(stats) == sel, colnames(stats) %in% hi95.labs])
                    }
          }
          
          if (type == 'matrix') {
               
               out.mean <- foreach(i=1:n.districts, .combine='rbind') %:%
                    foreach(j=1:n.districts, .combine='c') %dopar% {
                         
                         stats[row.names(stats) == paste(name, '[', i, ',', j, ']', sep=''), colnames(stats) %in% mean.labs]
                    }
               
               out.sd <- foreach(i=1:n.districts, .combine='rbind') %:%
                    foreach(j=1:n.districts, .combine='c') %dopar% {
                         
                         stats[row.names(stats) == paste(name, '[', i, ',', j, ']', sep=''), colnames(stats) %in% sd.labs]
                    }
               
               out.lo95 <- foreach(i=1:n.districts, .combine='rbind') %:%
                    foreach(j=1:n.districts, .combine='c') %dopar% {
                         
                         stats[row.names(stats) == paste(name, '[', i, ',', j, ']', sep=''), colnames(stats) %in% lo95.labs]
                    } 
               
               out.hi95 <- foreach(i=1:n.districts, .combine='rbind') %:%
                    foreach(j=1:n.districts, .combine='c') %dopar% {
                         
                         stats[row.names(stats) == paste(name, '[', i, ',', j, ']', sep=''), colnames(stats) %in% hi95.labs]
                    } 
               
               dimnames(out.mean) <- dimnames(out.sd) <- dimnames(out.lo95) <- dimnames(out.hi95) <- NULL
               out <- list(mean=out.mean, sd=out.sd, lo95=out.lo95, hi95=out.hi95)
          }
     } else if (level == 'month') {
          
          if (is.null(n.t)) n.t <- 12 # will need to change to make other time levels possible
          
          if (type == 'dataframe') {
               
               out <- foreach(t = 1:n.t, .combine='rbind') %:% 
                    foreach(i=1:n.districts, .combine='rbind') %:%
                    foreach(j=1:n.districts, .combine='rbind') %dopar% {
                         
                         sel <- paste(name, '[', i, ',', j, ',', t, ']', sep='')
                         
                         data.frame(name=sel,
                                    from=i,
                                    to=j,
                                    month=t,
                                    mean=stats[row.names(stats) == sel, colnames(stats) %in% c('Mean', 'mean')],
                                    sd=stats[row.names(stats) == sel, colnames(stats) %in% c('SD', 'sd')],
                                    lo95=stats[row.names(stats) == sel, colnames(stats) %in% c('Lower95', '2.5%')],
                                    hi95=stats[row.names(stats) == sel, colnames(stats) %in% c('Upper95', '97.5%')],)
                    }
          }
          
          if (type == 'matrix') {
               
               out.mean <- foreach(t = 1:n.t, .combine=function(a, b) abind(a, b, along=3)) %:% 
                    foreach(i = 1:n.districts, .combine='rbind', .multicombine=TRUE) %:% 
                    foreach(j = 1:n.districts, .combine='c', .multicombine=TRUE) %dopar% {
                         
                         sel <- paste(name, '[', i, ',', j, ',', t, ']', sep='')
                         
                         stats[row.names(stats) == sel, colnames(stats) %in% mean.labs]
                    }
               
               out.sd <- foreach(t = 1:n.t, .combine=function(a, b) abind(a, b, along=3)) %:% 
                    foreach(i = 1:n.districts, .combine='rbind', .multicombine=TRUE) %:% 
                    foreach(j = 1:n.districts, .combine='c', .multicombine=TRUE) %dopar% {
                         
                         sel <- paste(name, '[', i, ',', j, ',', t, ']', sep='')
                         
                         stats[row.names(stats) == sel, colnames(stats) %in% sd.labs]
                    }
               
               out.lo95 <- foreach(t = 1:n.t, .combine=function(a, b) abind(a, b, along=3)) %:% 
                    foreach(i = 1:n.districts, .combine='rbind', .multicombine=TRUE) %:% 
                    foreach(j = 1:n.districts, .combine='c', .multicombine=TRUE) %dopar% {
                         
                         sel <- paste(name, '[', i, ',', j, ',', t, ']', sep='')
                         
                         stats[row.names(stats) == sel, colnames(stats) %in% lo95.labs]
                    }
               
               out.hi95 <- foreach(t = 1:n.t, .combine=function(a, b) abind(a, b, along=3)) %:% 
                    foreach(i = 1:n.districts, .combine='rbind', .multicombine=TRUE) %:% 
                    foreach(j = 1:n.districts, .combine='c', .multicombine=TRUE) %dopar% {
                         
                         sel <- paste(name, '[', i, ',', j, ',', t, ']', sep='')
                         
                         stats[row.names(stats) == sel, colnames(stats) %in% hi95.labs]
                    }
               
               dimnames(out.mean) <- dimnames(out.sd) <- dimnames(out.lo95) <- dimnames(out.hi95) <- NULL
               out <- list(mean=out.mean, sd=out.sd, lo95=out.lo95, hi95=out.hi95)
          }
     }
     
     return(out)     
}



##' Get route-type
##' 
##' This function determines if a route is one of four types based on population density. The four route-types are:
##' \enumerate{
##'   \item High density to high density (High:high)
##'   \item High density to low density (High:low)
##'   \item Low density to low density (Low:low)
##'   \item Low density to high density (Low:high)
##' }
##' 
##' @param x A dataframe containing a column for the origin and a column for the destination
##' @param orig column index of origin district
##' @param dest column index of destination district
##' @param hi vector of district IDs in the high density category
##' @param lo vector of district IDs in the low density category
##' 
##' @return A character vector giving the corresponding route type
##' 
##' @author John Giles
##' 
##' @example R/examples/get_route_type.R
##'
##' @family utilities
##' 
##' @export
##' 

get.route.type <- function(x, # dataframe
                           orig,    # column index of origin district
                           dest,    # column index of destination district
                           hi,      # vector of district IDs in the high density category
                           lo       # vector of district IDs in the low density category
) {
     if (!is.data.frame(x)) stop('x must be a dataframe')
     
     hi <- as.character(hi)
     lo <- as.character(lo)
     x[,orig] <- as.character(x[,orig])
     x[,dest] <- as.character(x[,dest])
     
     type <- rep(NA, nrow(x))
     
     for (i in 1:nrow(x)) {
          
          if (x[i,orig] %in% hi & x[i,dest] %in% hi) {
               
               tmp <- "High:high"
               
          } else if (x[i,orig] %in% hi & x[i,dest] %in% lo) {
               
               tmp <- "High:low"
               
          } else if (x[i,orig] %in% lo & x[i,dest] %in% lo) {
               
               tmp <- "Low:low"
               
          }  else if (x[i,orig] %in% lo & x[i,dest] %in% hi) {
               
               tmp <- "Low:High"
          }
          type[i] <- tmp
     }
     
     return(type)
}



##' Proportion of individuals remaining for full epidemic generation
##'
##' This function calculates the proportion of individuals that remain in a location for
##' all of epidemic generation \eqn{n}. The value is represented by the variable \eqn{p_jt}, which
##' is defined as the effective probability if individuals that stay for a full epidemic generation 
##' when they travel to destination \eqn{j} at time \eqn{t}:\cr
##' \cr
##' \eqn{p_ij} = Pr(remaining for all of \eqn{n^th} epidemic generation | generation time \eqn{g})\cr
##' \cr
##' Because the Namibia mobility data spans 4 years there are many potential time intervals for which to calculate 
##' the proportion of individuals remaining for full epidemic generation. The \code{sub.samp} argument randomly 
##' selects X number of these generations to reduce computation time.
##' 
##' @param d a three- or four-dimensional array containing route- or month-level trip duration counts produced by the \code{\link{mob.data.array}} function. 
##' Note that \code{calc.prop.remain} assumes that the duration data array is NOT aggregated (e.g. \code{mob.data.array} argument \code{agg.int}=1)
##' @param gen.t the time interval in days used to define the epidemic generation
##' @param max.gen the maximum number of generations to evaluate proportion of individuals remaining
##' @param sub.samp scalar indicating the number of generations to subsample, if NULL (default), will use all observed generation times in the data (which will increase computation time for large numbers of locations)
##' 
##' @return if \code{d} is a month-level duration data array, then a 4D array with values between 0 and 1 is returned. If \code{d} is a route-level duration data array, 
##' then returns a 3D array is returned.
##' 
##' @author John Giles
##' 
##' @example R/examples/calc_prop_remain.R
##'
##' @family simulation
##' 
##' @export
##' 

calc.prop.remain <- function(d,                # 4D duration data array produced by the mob.data.array function
                             gen.t,            # interval used to define the epidemic generation
                             max.gen=NULL,
                             sub.samp=NULL     # number of generation to subsamp, if NULL (default) will use all observed generation times in the data
) {
     
     if (is.null(dimnames(d)$duration)) {
          stop("The calc.prop.remain function assumes that the duration values in the data array are NOT aggregated. Check that you have run the mob.data.array function with agg.int=1")
     }
        
     if (is.null(max.gen)) max.gen <- ceiling(max(as.numeric(dimnames(d)$duration))/gen.t)
     
     if (is.null(sub.samp)) {
          
          gens <- 1:max.gen
          
     } else if (!is.null(sub.samp)) {
          
          if (sub.samp >= max.gen) {
               
               gens <- 1:max.gen
               
          } else if (sub.samp < max.gen) {
               
               gens <- sort(sample(1:max.gen, sub.samp))
          }
     }
     
     if (length(dim(d)) == 4) { # Month-level duration data
          
          out <- array(NA, dim=c(dim(d)[1:3], length(gens)))
          
          for (i in 1:dim(d)[1]) {
               for (j in 1:dim(d)[2]) {
                    for (t in 1:dim(d)[3]) {
                         for (k in 1:length(gens)) {
                              
                              print(paste(round(i/dim(d)[1], 2),
                                          round(j/dim(d)[2], 2),
                                          round(t/dim(d)[3], 2),
                                          round(k/length(gens), 2),
                                          sep=" | "), quote=FALSE)
                              
                              if (i == j) (next)
                              
                              x <- d[i,j,t,which(as.numeric(dimnames(d)$duration) > gen.t*(gens[k]-1) & 
                                                      as.numeric(dimnames(d)$duration) <= gen.t*gens[k])]
                              
                              out[i,j,t,k] <- (x[!is.na(x)] %*% (seq_along(x)[!is.na(x)]/length(x))) / (sum(x, na.rm=T))
                         }
                    }
               }
          }
          
          out[is.nan(out)] <- NA
          dimnames(out) <- list(origin=dimnames(d)$origin,
                                destination=dimnames(d)$destination,
                                time=dimnames(d)$time,
                                generation=as.character(gens))
          
     } else if (length(dim(d)) == 3) { # Route-level duration data
          
          out <- array(NA, dim=c(dim(d)[1:2], length(gens)))
          
          for (i in 1:dim(d)[1]) {
               for (j in 1:dim(d)[2]) {
                    for (k in 1:length(gens)) {
                         
                         print(paste(round(i/dim(d)[1], 2),
                                     round(j/dim(d)[2], 2),
                                     round(k/length(gens), 2),
                                     sep=" | "), quote=FALSE)
                         
                         if (i == j) (next)
                         
                         x <- d[i,j,which(as.numeric(dimnames(d)$duration) > gen.t*(gens[k]-1) & as.numeric(dimnames(d)$duration) <= gen.t*gens[k])]
                         out[i,j,k] <- (x[!is.na(x)] %*% (seq_along(x)[!is.na(x)]/length(x))) / (sum(x, na.rm=T))
                    }
               }
          }
          
          out[is.nan(out)] <- NA
          dimnames(out) <- list(origin=dimnames(d)$origin,
                                destination=dimnames(d)$destination,
                                generation=as.character(gens))
          
     }
     
     return(out)
}

##' Get parameters for Beta distribution
##'
##' This function finds the two shape parameters for the Beta distribution of a random variable between 0 and 1.
##' Note that the function uses a different method depending on the arguments supplied. The three methods are:
##' \enumerate{
##' \item When the mean (\code{mu}) and variance (\code{sigma}) are supplied, the solution is found analytically.
##' \item When observed probabilities (\code{probs}) at each quantile (\code{quantiles}) are given, the 
##' solution is found by minimizing the Sum of the Squared Errors (SSE) using the Nelder-Mead optimization 
##' algorithm. Note that the fitting algorithm performs best when the five standard quantiles are supplied (Min, 25th, Median, 75th, Max).
##' \item When only observed probabilities (\code{probs}) are supplied, the function uses Maximum Likelihood Estimation (MLE).
##' }
##' 
##' @param mu scalar giving the mean \eqn{\mu}
##' @param sigma scalar giving the variance \eqn{\sigma^2} 
##' @param quantiles vector of quantiles for which proportions are observed. Expects: c('min', '25th', 'median', '75th', 'max').
##' @param probs vector of observed probabilities or proportions
##' 
##' @return A list containing the two shape parameters of the Beta distribution
##' 
##' @author John Giles
##' 
##' @example R/examples/get_beta_params.R
##'
##' @family simulation
##' @family susceptibility
##' 
##' @export
##' 


get.beta.params <- function(
     mu=NULL, 
     sigma=NULL,
     quantiles=NULL,
     probs=NULL
) {
     
     if (all(!is.null(mu), !is.null(sigma), is.null(quantiles), is.null(probs))) {
          
          message('Calculating Beta distribution parameters analytically from mean (mu) and variance (sigma)')
          
          shape1 <- ((1-mu) / sigma - 1/mu) * mu^2
          
          return(list(shape1=shape1, shape2=shape1 * (1 / mu-1)))
          
     } else if (all(is.null(mu), is.null(sigma), !is.null(quantiles), !is.null(probs))) {
          
          if(!(identical(length(quantiles), length(probs)))) {
               stop("Dimensions of 'quant' and 'probs' arguments must match.")
          }
          
          message('Calculating Beta distribution parameters from quantiles using sums of squares')
          
          fit.beta <- function(x,           # vector of shape and rate parameters for beta distribution
                               quantiles,   # the quantiles for which proportions are observed
                               probs        # the observed proportions
          ) {
               sum((qbeta(quantiles, x[1], x[2]) - probs)^2) 
          }
          
          suppressWarnings(
               params <- optim(par=c(1,1), # initialize with flat beta distribution
                               fn=fit.beta,
                               quantiles=quantiles, 
                               probs=probs,
                               method='Nelder-Mead',
                               control=list(maxit=1e5))$par
          )
          
          return(list(shape1=params[1], shape2=params[2]))
          
     } else if (all(is.null(mu), is.null(sigma), is.null(quantiles), !is.null(probs))) {
          
          message('Calculating Beta distribution parameters from probabilities using maximum likelihood')
          message(paste('n =', length(probs), sep=' '))
          
          suppressWarnings(
               params <- MASS::fitdistr(probs, 'beta', list(shape1=2, shape2=2), control=list(maxit=1e5))
          )
          
          return(list(shape1=as.numeric(params$estimate[1]), 
                      shape2=as.numeric(params$estimate[2])))
          
     } else {
          
          stop('Arguments must be only: mu & sigma | quantiles & probs | probs only')
     }
}


##' Get Beta parameters for age groups
##'
##' This function finds the two shape parameters for the Beta distribution for aggregated age groups given individual-level 
##' vaccination data. The individual-level data is comprised of the age of the individual (\code{age}) and a binary indicator of vaccination status 
##' (\code{vacc}). The function first uses a Binomial GAM to estimate the mean (mu) and variance (sigma) of the proportion vaccinated for each 
##' of the defined age groups and then finds the shape parameters of the Beta distribution analytically using \code{\link{get.beta.params}}.
##' 
##' Note that a Binomial GLM is used if the number of age groups is 2 or less.
##' 
##' @param age a vector giving the age at vaccination of each individual
##' @param vacc a binary vector indicating the vaccination status of each individual
##' @param breaks a scalar or vector of age group breaks (passed to \code{\link{.bincode}}). If NULL (default), calculates Beta parameters for all data together.
##' 
##' @return A dataframe containing the sample size, mu, sigma, and Beta distribution paramters for each age group
##' 
##' @author John Giles
##' 
##' @example R/examples/get_age_beta.R
##'
##' @family simulation
##' @family susceptibility
##' 
##' @export
##' 


get.age.beta <- function(
     age=NULL,     # age of individual
     vacc=NULL,    # binary indicator of vaccination status
     breaks=NULL   # if beta params are to be calculated for age groups, give vector of breaks (see .bincode: a numeric vector of two or more cut points, sorted in increasing order)
) {
     
     tmp <- cbind(age, vacc) 
     tmp <- tmp[complete.cases(tmp),]
     message(paste('Complete observations: n =', nrow(tmp), 'of', length(age), sep=' '))
     age <- tmp[,1]; vacc <- tmp[,2]
     
     if (is.null(breaks)) breaks <- c(0, ceiling(max(age)))
     if (breaks[1] != 0) breaks <- c(0, breaks)
     if (max(breaks) < max(age)) breaks <- c(breaks, ceiling(max(age)))
     breaks <- sort(breaks)
     
     age.group <- .bincode(age, breaks, right=FALSE)
     age.label <- factor(age.group, labels=stringr::str_c(breaks[-length(breaks)], "-", breaks[-1]))
     
     message(paste('Estimating mean (mu) and variance (sigma) for', nlevels(age.label), 'age groups', sep=' '))
     fit <- tryCatch({
          
          suppressWarnings(
               mgcv::gam(vacc ~ s(age.group, bs='tp', k=nlevels(age.label)-1), family='binomial')
          )
          
     }, error = function (e) {
          
          glm(vacc ~ age.group, family='binomial')
     })
     
     suppressWarnings(
          pred <- predict(fit, newdata=data.frame(age.group=sort(unique(age.group))), type='response', se.fit=TRUE)
     )
     
     params <- get.beta.params(mu=pred$fit, sigma=pred$se.fit)
     
     return(
          data.frame(age=levels(age.label),
                     n=as.vector(table(age.group)),
                     mu=pred$fit,
                     sigma=pred$se.fit,
                     shape1=params$shape1,
                     shape2=params$shape2,
                     row.names=NULL)
     )
}



##' Calculate vaccination coverage given routine vaccination coverage
##'
##' This function calculates the proportion immune using conditional probabilities. The method 
##' assumes that vaccination events are dependent, where individuals that have recieved the first
##' dose are the most likely to recieve the second dose and those that have received both the first 
##' and second doses are the most likely to receive the third.
##' 
##' When \code{v3 = NULL}, the function uses the simpler two dose method.
##' 
##' @param v1 a scalar giving the proportion vaccinated with first routine immunization
##' @param v2 a scalar giving the proportion vaccinated with second routine immunization
##' @param v3 a scalar giving the proportion vaccinated with third routine immunization (default = NULL)
##' 
##' @return A dataframe containing the relative proportions of the population that have recieved 0, 1, 2, or 3 doses
##' 
##' @author John Giles
##' 
##' @example R/examples/calc_prop_vacc.R
##'
##' @family susceptibility
##' 
##' @export
##' 

calc.prop.vacc <- function(
     v1,  # Proportion vaccinated with first campaign
     v2,  # proportion vaccinated with second campaign
     v3=NULL   # proportion vaccinated with third campaign
){
     
     if (!all(v1 >= 0 & v1 <= 1, v2 >= 0 & v2 <= 1)) stop('Arguments must be between 0 and 1')
     
     if (is.null(v3)) {
          
          if (v2 > v1) {
               d_12 <- 0           # Dropout rate from 1 to 2
               s_12 <- v2 - v1     # Surplus of dose 2 not given to ppl who received dose 1
          } else {
               d_12 <- (v1 - v2)/v1
               s_12 <- 0
          }
          
          # Conditional probability terms for two doses
          p_1_2 <- v1*(1-d_12)          # Pr( v2 | v1 )
          p_1_n2 <- v1*d_12             # Pr( notv2 | v1 )
          
          if (v2 <= v1) { # Pr( v2 | not v1 )
               p_n1_2 <- 0
          } else {
               p_n1_2 <- (1-v1)*(v2-v1) 
          }
          
          if (v2 <= v1) { # Pr( notv2 | notv1 )
               p_n1_n2 <- (1-v1)
          } else {
               p_n1_n2 <- (1-v1)*(1-(v2-v1))      
          }
          
          den <- sum(p_1_2, p_1_n2, p_n1_2, p_n1_n2)
          out <- data.frame(doses=2:0, prop=c(p_1_2/den, (p_1_n2 + p_n1_2)/den, p_n1_n2/den))
          
     } else if (!is.null(v3)) {
          
          if(!(v3 >= 0 & v3 <= 1)) stop('Arguments must be between 0 and 1')
          
          if (v2 > v1) {
               d_12 <- 0           # Dropout rate from 1 to 2
               s_12 <- v2 - v1     # Surplus of dose 2 not given to ppl who received dose 1
          } else {
               d_12 <- (v1 - v2)/v1
               s_12 <- 0
          }
          
          p_1_2 <- v1*(1-d_12)     # Pr( v2 | v1 )
          
          if (v3 >= p_1_2) {
               d_23 <- 0            # Dropout rate from 2 doses to 3 
               s_23 <- v3 - p_1_2   # Surplus of dose 3 not given to ppl that already received 2 doses
          } else {
               d_23 <- (p_1_2 - v3)/p_1_2
               s_23 <- 0
          }
          
          # Conditional probability terms for three doses
          
          p_1_2_3 <- p_1_2*(1-d_23)
          p_1_2_n3 <- p_1_2*d_23
          
          if (v3 <= p_1_2) {
               p_1_n2_3 <- 0
          } else {
               p_1_n2_3 <- v1*d_12*s_23
          }
          
          if (v2 <= v1) {
               p_n1_2_3 <- 0
          } else if (v3 <= p_1_2) {
               p_n1_2_3 <- 0
          } else {
               p_n1_2_3 <- (1-v1)*s_12*s_23
          }
          
          if (v3 <= p_1_2) {
               p_1_n2_n3 <- v1*d_12
          } else {
               p_1_n2_n3 <- v1*d_12*(1-s_23)
          }
          
          if (v2 <= v1) {
               p_n1_2_n3 <- 0
          } else if (v3 <= p_1_2) {
               p_n1_2_n3 <- 0
          } else {
               p_n1_2_n3 <- (1-v1)*s_12*(1-s_23)
          }
          
          if (v3 <= p_1_2) {
               p_n1_n2_3 <- 0
          } else if (v3 > p_1_2 & v2 <= v1) {
               p_n1_n2_3 <- (1-v1)*(s_23 - v1*d_12 - (1-v1)*(v2-v1)) 
          } else if (v3 > p_1_2 & v2 > v1) {
               p_n1_n2_3 <- (1-v1)*s_12*(s_23 - v1*d_12 - (1-v1)*(v2-v1)) 
          }
          
          if (v3 <= p_1_2 & v2 <= v1) {
               p_n1_n2_n3 <- (1-v1)
          } else if (v3 <= p_1_2 & v2 > v1) {
               p_n1_n2_n3 <- (1-v1)*(1-s_12)
          } else if (v3 > p_1_2 & v2 <= v1) {
               p_n1_n2_n3 <- (1-v1)*(1-s_23)
          } else if (v3 > p_1_2 & v2 > v1) {
               p_n1_n2_n3 <- (1-v1)*(1-s_12)*(1-s_23)
          }
          
          den <- sum(p_1_2_3, p_1_2_n3, p_1_n2_3, p_n1_2_3,
                     p_1_n2_n3, p_n1_2_n3, p_n1_n2_3, p_n1_n2_n3)
          
          out <- data.frame(doses=3:0,
                            prop=c(p_1_2_3/den,
                                   sum(p_1_2_n3, p_1_n2_3, p_n1_2_3)/den,
                                   sum(p_1_n2_n3, p_n1_2_n3, p_n1_n2_3)/den,
                                   p_n1_n2_n3/den))  
     }
     
     return(out)
}


##' Calculate vaccination coverage given routine vaccination and a supplementary campaign
##'
##' This function calculates the proportion immune by calculating the conditional probability of routine
##' vaccination with two- or three-dose routine coverage and one SIA campaign. The method 
##' assumes that vaccination events are dependent, where individuals that have recieved the first
##' dose are the most likely to recieve the second dose and those that have received both the first 
##' and second doses are the most likely to receive the third. Receipt of a dose through the SIA campaign
##' is dependent upon vaccination with at least one dose prior to the SIA campaign.
##' 
##' When \code{v3 = NULL}, the function uses the simpler two dose method.
##' 
##' @param v1 a scalar giving the proportion vaccinated with first routine immunization
##' @param v2 a scalar giving the proportion vaccinated with second routine immunization
##' @param v3 a scalar giving the proportion vaccinated with third routine immunization (default = NULL)
##' @param S a scalar giving the proportion vaccinated with supplemental campaign
##' 
##' @return A dataframe containing the relative proportions of the population that have recieved 0, 1, 2, 3, or 4 doses
##'
##' @author John Giles
##' 
##' @example R/examples/calc_prop_vacc_SIA.R
##'
##' @family susceptibility
##' 
##' @export
##' 

calc.prop.vacc.SIA <- function(
     v1,        # Proportion vaccinated with first routine immunization
     v2,        # proportion vaccinated with second routine immunization
     v3=NULL,   # proportion vaccinated with third routine immunization
     S          # proportion vaccinated with supplemental campaign
){
     
     if (!all(v1 >= 0 & v1 <= 1, v2 >= 0 & v2 <= 1, S >= 0 & S <= 1)) stop('Arguments must be between 0 and 1')
     
     p_prior <- sum(calc.prop.vacc(v1=v1, v2=v2)[1:2,2])
     
     if (is.null(v3)) {
          
          if (v2 >= v1) {
               d_12 <- 0           # Dropout rate from 1 to 2
          } else {
               d_12 <- (v1 - v2)/v1
          }
          
          if (S >= p_prior) {
               d_S <- 0
          } else {
               d_S <- (p_prior - S)/p_prior
          }
          
          # Conditional probability terms for 3 doses
          p_1_2_S <- v1*(1-d_12)*(1-d_S)
          
          # Conditional probability terms for 2 doses
          p_1_2_nS <- v1*(1-d_12)*d_S
          
          
          p_1_n2_S <- v1*d_12*(1-d_S)  
          p_1_n2_nS <- v1*d_12*d_S 
          
          
          if (v2 <= v1) { 
               
               p_n1_2_S <- 0
               p_n1_2_nS <- 0
               p_n1_n2_S <- (1-v1)*(1-d_S)
               p_n1_n2_nS <- (1-v1)*d_S
               
          } else {
               
               p_n1_2_S <- (1-v1)*(v2-v1)*(1-d_S) 
               p_n1_2_nS <- (1-v1)*(v2-v1)*d_S
               p_n1_n2_S <- (1-v1)*(1-(v2-v1))*(1-d_S)  
               p_n1_n2_nS <- (1-v1)*(1-(v2-v1))*d_S  
          }
          
          den <- sum(p_1_2_S, p_1_2_nS, p_1_n2_S, p_1_n2_nS,
                     p_n1_2_S, p_n1_2_nS, p_n1_n2_S, p_n1_n2_nS)
          
          out <- data.frame(doses=3:0, 
                            prop=c(p_1_2_S/den, 
                                   sum(p_1_2_nS, p_1_n2_S, p_n1_2_S)/den, 
                                   sum(p_1_n2_nS, p_n1_2_nS, p_n1_n2_S)/den,
                                   p_n1_n2_nS/den))
          
     } else if (!is.null(v3)) {
          
          if(!(v3 >= 0 & v3 <= 1)) stop('Arguments must be between 0 and 1')
          
          p_prior <- sum(calc.prop.vacc(v1=v1, v2=v2, v3=v3)[1:3,2])
          
          if (v2 >= v1) {
               d_12 <- 0           # Dropout rate from 1 to 2
               s_12 <- v2 - v1     # Surplus of dose 2 not given to ppl who received dose 1
          } else {
               d_12 <- (v1 - v2)/v1
               s_12 <- 0
          }
          
          p_1_2 <- v1*(1-d_12)     # Pr( v2 | v1 )
          
          if (v3 >= p_1_2) {
               d_23 <- 0            # Dropout rate from 2 doses to 3 
               s_23 <- v3 - p_1_2   # Surplus of dose 3 not given to ppl that already received 2 doses
          } else {
               d_23 <- (p_1_2 - v3)/p_1_2
               s_23 <- 0
          }
          
          if (S >= p_prior) {
               d_S <- 0              # Dropout rate from routine immunization to SIA
          } else {
               d_S <- (p_prior - S)/p_prior
          }
          
          # Possible ways to receive 4 doses
          
          p_1_2_3_S <- p_1_2*(1-d_23)*(1-d_S)
          
          # Possible ways to receive 3 doses
          
          p_1_2_3_nS <- p_1_2*(1-d_23)*d_S
          p_1_2_n3_S <- p_1_2*d_23*(1-d_S)
          
          if (v3 <= p_1_2) {
               p_1_n2_3_S <- 0
               
          } else {
               p_1_n2_3_S <- v1*d_12*s_23*(1-d_S)
          }
          
          if (v2 <= v1) {
               p_n1_2_3_S <- 0
          } else if (v3 <= p_1_2) {
               p_n1_2_3_S <- 0
          } else {
               p_n1_2_3_S <- (1-v1)*s_12*s_23*(1-d_S)
          }
          
          # Possible ways to receive 2 doses
          
          p_1_2_n3_nS <- p_1_2*d_23*d_S
          
          if (v3 <= p_1_2) {
               p_1_n2_3_nS <- 0
               
          } else {
               p_1_n2_3_nS <- v1*d_12*s_23*d_S
          }
          
          if (v2 <= v1) {
               p_n1_2_3_nS <- 0
          } else if (v3 <= p_1_2) {
               p_n1_2_3_nS <- 0
          } else {
               p_n1_2_3_nS <- (1-v1)*s_12*s_23*d_S
          }
          
          if (v3 <= p_1_2) {
               p_1_n2_n3_S <- v1*d_12*(1-d_S)
          } else {
               p_1_n2_n3_S <- v1*d_12*(1-s_23)*(1-d_S)
          }
          
          if (v2 <= v1) {
               p_n1_2_n3_S <- 0
          } else if (v3 <= p_1_2) {
               p_n1_2_n3_S <- 0
          } else {
               p_n1_2_n3_S <- (1-v1)*s_12*(1-s_23)*(1-d_S)
          }
          
          if (v3 <= p_1_2) {
               p_n1_n2_3_S <- 0
          } else if (v3 > p_1_2 & v2 <= v1) {
               p_n1_n2_3_S <- (1-v1)*(s_23 - v1*d_12 - (1-v1)*(v2-v1)) * (1-d_S)
          } else if (v3 > p_1_2 & v2 > v1) {
               p_n1_n2_3_S <- (1-v1)*s_12*(s_23 - v1*d_12 - (1-v1)*(v2-v1)) * (1-d_S)
          }
          
          # Possible ways to receive 1 dose
          
          if (v3 <= p_1_2) {
               p_1_n2_n3_nS <- v1*d_12*d_S
          } else {
               p_1_n2_n3_nS <- v1*d_12*(1-s_23)*d_S
          }
          
          if (v2 <= v1) {
               p_n1_2_n3_nS <- 0
          } else if (v3 <= p_1_2) {
               p_n1_2_n3_nS <- 0
          } else {
               p_n1_2_n3_nS <- (1-v1)*s_12*(1-s_23)*d_S
          }
          
          if (v3 <= p_1_2) {
               p_n1_n2_3_nS <- 0
          } else if (v3 > p_1_2 & v2 <= v1) {
               p_n1_n2_3_nS <- (1-v1)*(s_23 - v1*d_12 - (1-v1)*(v2-v1)) * d_S
          } else if (v3 > p_1_2 & v2 > v1) {
               p_n1_n2_3_nS <- (1-v1)*s_12*(s_23 - v1*d_12 - (1-v1)*(v2-v1)) * d_S
          }
          
          if (v3 <= p_1_2 & v2 <= v1) {
               p_n1_n2_n3_S <- (1-v1)*(1-d_S)
          } else if (v3 <= p_1_2 & v2 > v1) {
               p_n1_n2_n3_S <- (1-v1)*(1-s_12)*(1-d_S)
          } else if (v3 > p_1_2 & v2 <= v1) {
               p_n1_n2_n3_S <- (1-v1)*(1-s_23)*(1-d_S)
          } else if (v3 > p_1_2 & v2 > v1) {
               p_n1_n2_n3_S <- (1-v1)*(1-s_12)*(1-s_23)*(1-d_S)
          }
          
          # Possible ways to receive zero doses
          
          if (v3 <= p_1_2 & v2 <= v1) {
               p_n1_n2_n3_nS <- (1-v1)*d_S
          } else if (v3 <= p_1_2 & v2 > v1) {
               p_n1_n2_n3_nS <- (1-v1)*(1-s_12)*d_S
          } else if (v3 > p_1_2 & v2 <= v1) {
               p_n1_n2_n3_nS <- (1-v1)*(1-s_23)*d_S
          } else if (v3 > p_1_2 & v2 > v1) {
               p_n1_n2_n3_nS <- (1-v1)*(1-s_12)*(1-s_23)*d_S
          }
          
          den <- sum(p_1_2_3_S, p_1_2_n3_S, p_1_n2_3_S, p_n1_2_3_S,
                     p_1_n2_n3_S, p_n1_2_n3_S, p_n1_n2_3_S, p_n1_n2_n3_S,
                     p_1_2_3_nS, p_1_2_n3_nS, p_1_n2_3_nS, p_n1_2_3_nS,
                     p_1_n2_n3_nS, p_n1_2_n3_nS, p_n1_n2_3_nS, p_n1_n2_n3_nS)
          
          out <- data.frame(doses=4:0,
                            prop=c(p_1_2_3_S/den,
                                   sum(p_n1_2_3_S, p_1_n2_3_S, p_1_2_n3_S, p_1_2_3_nS)/den,
                                   sum(p_1_2_n3_nS, p_1_n2_3_nS, p_n1_2_3_nS, 
                                       p_1_n2_n3_S, p_n1_2_n3_S, p_n1_n2_3_S)/den,
                                   sum(p_1_n2_n3_nS, p_n1_2_n3_nS, p_n1_n2_3_nS, p_n1_n2_n3_S)/den,
                                   p_n1_n2_n3_nS/den))  
     }
     
     return(out)
}


##' Get parameters of Gamma distribution
##'
##' A function that finds the \code{shape} and \code{rate} parameters required by the Gamma distribution given the observed 
##' mean \code{mu} and standard deviation \code{sigma} of the response variable. Parameters are found numerically using a 
##' two-dimensional Nelder-Mead optimization algorithm.
##'
##' @param mu the desired mean of the Gamma distribution
##' @param sigma the desired standard deviation of the Gamma distribution
##'
##' @return a named numeric vector giving the \code{shape} and \code{rate} parameters of the Gamma distribution
##'
##' @family random number generators
##'
##' @author John Giles
##'
##' @example R/examples/get_gamma_params.R
##'
##' @export
##'

get.gamma.params <- function(mu, sigma) {
     
     message('Calculating shape and rate parameters numerically from mean and sd')
     
     suppressWarnings(
          params <- optim(par=c(mu*2, 2),
                          fn=function(x) abs(mu - x[1]/x[2]) + abs(sigma - sqrt(x[1]/(x[2]^2))),
                          method='Nelder-Mead')$par
     )
     names(params) <- c('shape', 'rate')
     return(params)
}

##' Calculate the probability of a Gamma distributed random variable
##'
##' A function that calcualtes the probability of random variable \code{x} according to a Gamma probability distribution with provided
##' \code{shape} and \code{rate} parameters.
##'
##' @param mu the desired mean of the Gamma distribution
##' @param sigma the desired standard deviation of the Gamma distribution
##'
##' @return a named numeric vector giving the \code{shape} and \code{rate} parameters of the Gamma distribution
##'
##' @family random number generators
##'
##' @author John Giles
##'
##' @example R/examples/sim_gamma.R
##'
##' @export
##'

sim.gamma <- function(
     x,
     shape,
     rate
){
     exp((shape * log(rate)) + ((shape-1) * log(x)) - (rate * x) - log(gamma(shape)))
}


##' Exponential decay function
##'
##' This function calculates exponential decay of a data value \code{y} (e.g. trip duration)
##' given the intercept \eqn{N0} and decay rate \eqn{\lambda} parameters using the functional form:
##' \eqn{N(y) = N0 exp(-\lambda*y)}. If \eqn{y =} duration, the function calculates the number of trips counted for a given duration.
##' 
##' @param N0 intercept (baseline number of expected trips at \eqn{y=0})
##' @param lambda decay rate
##' @param y a scalar or vector giving the data value(s) (e.g. trip duration)
##' 
##' @return A scalar or vector of \eqn{N(y)}
##' 
##' @author John Giles
##'
##' @family simulation
##' 
##' @export
##' 

decay.func <- function(N0,       # intercept (baseline number of expected trips at \eqn{y=0})
                       lambda,     # decay rate parameter
                       y           # data value duration (integer representing days)
) {
     N0 * exp(-lambda*y)
}

##' Simulate connectivity matrix (pi)
##'
##' This function takes the mean \eqn{\mu_\pi_{ijt}} of the estimated posterior distribution of \eqn{\pi_{ijt}} (the probability of movement 
##' from district \eqn{i} to district \eqn{i} in time \eqn{t}) and returns one stochastic realization of the connectivity matrix. Each stochastic 
##' realization is produced by returning the vector \eqn{\boldsymbol\pi_{i\{j\}t}} from a Dirichlet distribution.
##' 
##' @param mu a three dimensional array giving the mean of the posterior distribution for \eqn{\pi_{ijt}}
##' @param level the level of the data for which to generate the stochastic realization of pi (e.g. destination-, route- or month-level) 
##' 
##' @return a numerical matrix (when \code{level = 'route'}) with values between 0 and 1, where rows (all \eqn{j} destination districts) sum to 1
##' 
##' @author John Giles
##' 
##' @example R/examples/sim_pi.R
##'
##' @family simulation
##' 
##' @export
##' 

sim.pi <- function(mu, 
                   level) { # Mean of posterior distribution for pi
     
     if (level == 'route') {
          
          out <- array(NA, dim(mu))
          for (i in 1:dim(mu)[1]) {
               
               out[i,] <- gtools::rdirichlet(1, mu[i,])
          }
          
     }
     
     return(out)  
}

##' Simulate proportion remaining for full generation (rho)
##'
##' A function that takes output from the \code{calc.p} function and simulates one stochastic realization
##' of rho. The function does so by calculating the mean and variance of \code{p} at the level indicated by the \code{level}
##' argument, and then derives the shape and rate parameters for the beta distribution.
##' 
##' @param p an array produced by the \code{calc.p} function giving the proportion of travellers remaining after \eqn{n} generations
##' @param level the level of the data for which to generate the stochastic realization of rho (e.g. destination-, route- or month-level) 
##' 
##' @return a numerical matrix when \code{level = 'route'}
##' 
##' @author John Giles
##' 
##' @example R/examples/sim_rho.R
##'
##' @family simulation
##' 
##' @export
##' 

sim.rho <- function(p, 
                    level
){
     
     if (level == 'route') {
          
          mu.p <- apply(p, c(1,2), mean, na.rm=TRUE)
          
          sigma.p <- foreach(i = 1:dim(p)[1], .combine='rbind') %:% 
               foreach(j = 1:dim(p)[2], .combine='c') %do% {
                    
                    x <- var(as.vector(p[i,j,,]), na.rm=TRUE)
               }
          
          beta.params <- get.beta.params(mu.p, sigma.p)
          
          out <- foreach(i = 1:dim(p)[1], .combine='rbind') %:% 
               foreach(j = 1:dim(p)[2], .combine='c') %do% {
                    
                    suppressWarnings(
                         x <- rbeta(1, beta.params$shape1[i,j], beta.params$shape2[i,j])
                    )
               }
          
          dimnames(out) <- dimnames(sigma.p) <- dimnames(mu.p)
          diag(out) <- NA
     }
     
     return(out) 
}

##' Simulate decay rate (lambda)
##'
##' A function to simulate one stochastic realization of the decay rate lambda given the mean and standard deviation of the posterior 
##' distribution estimated by model of decay rate or a gravity model.
##' 
##' @param mu mean of posterior distribution of decay rate lambda
##' @param sigma standard deviation of posterior distribution of decay rate lambda
##' @param level the level of the data at which the mean and standard deviation of lambda are given (e.g. destination-, route- or month-level) 
##' 
##' @return a numerical matrix when \code{level = 'route'}
##' 
##' @author John Giles
##' 
##' @example R/examples/sim_lambda.R
##'
##' @family simulation
##' 
##' @export
##' 

#############################################################################
# function to simulate one stochastic realization of the decay rate lambda

sim.lambda <- function(mu,       # mean of posterior distribution of decay rate lambda
                       sigma,    # standard deviation of posterior distribution of decay rate lambda
                       level     # the level of the data at which the mean and standard deviation of lambda are given
){
     if (level == 'route') {
          out <- array(NA, dim(mu))
          for (i in 1:dim(mu)[1]) {
               
               out[i,] <- truncnorm::rtruncnorm(dim(mu)[1], a=0, mean=mu[i,], sd=sigma[i,])
          }
          diag(out) <- NA
     }
     
     return(out)
}

##' Simulate probability of leaving origin (tau)
##'
##' A function that takes a matrix containing the empirical proportion of individuals that leave origin \eqn{i}
##' for each date in the trip duration data and simulates one stochastic realization
##' of tau (the overall probability of leaving origin \eqn{i}). The function does so by calculating the mean and variance of \code{p} 
##' for each origin \eqn{i}, and then derives the shape and rate parameters for the beta distribution.
##' 
##' @param p A matrix giving the observed proportion of individuals of leaving origin \eqn{i} for each day in the trip suration data
##' 
##' @return a numerical vector with values between 0 and 1
##' 
##' @author John Giles
##' 
##' @example R/examples/sim_tau.R
##'
##' @family simulation
##' 
##' @export
##' 

sim.tau <- function(p # matrix giving the probability of leaving origin
){
     
     mu.p <- apply(p, 2, mean, na.rm=TRUE)
     sigma.p <- apply(p, 2, var, na.rm=TRUE)
     beta.params <- get.beta.params(mu.p, sigma.p)
     
     out <- rep(NA, ncol(p))
     
     suppressWarnings(
          for (i in 1:ncol(p)) {
               
               out[i] <- rbeta(1, beta.params$shape1[i], beta.params$shape2[i])
          } 
     )
     
     names(out) <- dimnames(p)$origin
     
     return(out) 
}

##' Simulate stochastic TSIR
##'
##' This function produces one stochastic realization of a spatial Time series Susceptible-Infected-Recovered model. The simulation expects estimates 
##' of the posterior distribution for trip duration decay rate (lambda), probability of movement (pi), and proportion of travellers remaining for a 
##' full epidemic generation (rho).
##' 
##' @param districts Vector of district names
##' @param N Vector giving the population size of each district
##' @param lambda Matrix of trip duration decay for route i to j 
##' @param pi Matrix of district connectivity for route i to j
##' @param rho Matrix of proportion of travellers remaining for full generation for route i to j
##' @param tau Vector giving the proportion of individuals that travel in each district
##' @param beta Transmission rate
##' @param gamma Recovery rate
##' @param gen.t Pathogen generation time (days)
##' @param max.t Maximum number of epidemic generations
##' @param I.0 Vector giving number of infected individuals in each district at time 0
##' @param freq.dep Logical indicating frequency (\code{TRUE}) or density dependent (\code{FALSE}) transmission
##' 
##' @return a three-dimensional named array giving the numbers of Susceptible, Infected, and Recovered infividuals in each location and time step
##' 
##' @author John Giles
##' 
##' @example R/examples/sim_TSIR.R
##'
##' @family simulation
##' 
##' @export
##' 

sim.TSIR <- function(districts,                 # Vector of district names
                     N,                         # Vector giving the population size of each district
                     tau=NULL,                  # Vector giving the proportion of individuals that travel in each district
                     lambda=NULL,               # Matrix of trip duration decay for route i to j 
                     pi,                        # Matrix of district connectivity for route i to j
                     rho=NULL,                  # Matrix of proportion of travellers remaining for full generation for route i to j
                     beta,                      # Transmission rate
                     gamma,                     # Recovery rate
                     gen.t,                     # Pathogen generation time
                     I.0,                       # Vector giving number of infected individuals in each district at time 0
                     max.t=100,                 # Maximum number of generations
                     duration=TRUE,             # Logical indicating whether to use information on trip duration in the definition of connectivity
                     freq.dep=TRUE              # Frequency or density dependent transmission
) {
     
     n.districts <- length(districts)
     
     # Frequency or density dependent transmission
     if (freq.dep == TRUE) {
          beta <- beta/N
     } else {
          beta <- rep(beta, n.districts)
     }
     
     # Initialize simulation arrays
     y <- array(0, dim=c(n.districts, 2, max.t))
     dimnames(y) <- list(district=districts, compartment=c('S', 'I'), t=1:max.t)
     y[,'I',1] <- I.0
     y[,'S',1] <- N - I.0
     tot.inf <- I.0
     names(tot.inf) <- districts
     
     if (duration == 'FALSE') { # Basic gravity model
          
          if(!(identical(length(districts), length(N), nrow(pi)))) {
               stop("Dimensions of simulation arguments must match.")
          }
          
          iota <- h <- w <- array(0, dim=c(n.districts, max.t))
          dimnames(iota) <- dimnames(h) <- dimnames(w) <- list(district=districts, t=1:max.t)
          
          for (j in 1:n.districts) {
               
               # Spatial movement of infected
               iota[j,1] <- rpois(1, sum(pi[,j] * y[,'I',1], na.rm=TRUE))
               
               # Spatial hazard
               h[j,1] <- (beta[j] * y[j,'S',1] * (1 - exp(-sum(pi[,j], na.rm=TRUE) * (y[j,'S',1] / sum(y[j,,1])) * (sum(y[-j,"I",1]) / sum(N[-j])) ))) / (1 + beta[j] * y[j,'S',1])
               
               # Waiting time
               w[j,1] <- h[j,1]
          }
          
          for (t in 1:(max.t-1)) {
               for (j in 1:n.districts) {
                    
                    # Stochastic change in Susceptible and Infected
                    dIdt <- rbinom(1, y[j,"S",t], 1 - exp(-beta[j] * ((y[j,'I',t] + iota[j,t])^0.975) ))
                    dRdt <- rbinom(1, y[j,"I",t], 1 - exp(-gamma))
                    
                    y[j,'S',t+1] <- y[j,'S',t] - dIdt
                    y[j,'I',t+1] <- y[j,'I',t] + dIdt - dRdt
                    tot.inf[j] <- tot.inf[j] + dIdt
                    
                    # Spatial movement of infected
                    iota[j,t+1] <- rpois(1, sum(pi[,j] * y[,'I',t], na.rm=TRUE))
                    
                    # Spatial hazard
                    h[j,t+1] <- (beta[j] * y[j,'S',t] * (1 - exp(-sum(pi[,j], na.rm=TRUE) * (y[j,'S',t] / sum(y[j,,t])) * (sum(y[-j,"I",t]) / sum(N[-j])) ))) / (1 + beta[j] * y[j,'S',t])
                    
                    # Waiting time
                    w[j,t+1] <- h[j,t+1] * prod(1 - h[j,1:t])
               }
          }
          
     } else if (duration == 'TRUE') { # Gravity model with trip duration
          
          if(!(identical(length(districts), length(N), nrow(lambda), nrow(pi), nrow(rho), length(tau)))) {
               stop("Dimensions of simulation arguments must match.")
          }
          
          iota <- kappa <- h <- w <- array(0, dim=c(n.districts, max.t))
          dimnames(kappa) <- dimnames(iota) <- dimnames(h) <- dimnames(w) <- list(district=districts, t=1:max.t)
          
          for (j in 1:n.districts) {
               
               # Spatial movement of infected
               iota[j,1] <- rpois(1, sum(rho[,j] * pi[,j] * tau[] * y[,'I',1], na.rm=TRUE))
               
               # Remnant infected individuals
               kappa[j,1] <- rpois(1, mean(rho[,j], na.rm=TRUE) * (iota[j,1] * exp(-1 * mean(lambda[,j], na.rm=TRUE))))
               
               # Spatial hazard
               h[j,1] <- (beta[j] * y[j,'S',1] * (1 - exp(-sum(rho[,j] * pi[,j] * tau[], na.rm=TRUE) * (y[j,'S',1] / sum(y[j,,1])) * (sum(y[-j,"I",1]) / sum(N[-j])) ))) / (1 + beta[j] * y[j,'S',1])
               
               # Waiting time
               w[j,1] <- h[j,1]
          }
          
          for (t in 1:(max.t-1)) {
               for (j in 1:n.districts) {
                    
                    # Stochastic change in Susceptible and Infected
                    dIdt <- rbinom(1, y[j,"S",t], 1 - exp( -beta[j] * ((y[j,'I',t] + iota[j,t] + kappa[j,t])^0.975) ))
                    dRdt <- rbinom(1, y[j,"I",t], 1 - exp(-gamma))
                    
                    y[j,'S',t+1] <- y[j,'S',t] - dIdt
                    y[j,'I',t+1] <- y[j,'I',t] + dIdt - dRdt
                    tot.inf[j] <- tot.inf[j] + dIdt
                    
                    # Spatial movement of infected
                    iota[j,t+1] <- rpois(1, 
                                         sum(rho[,j] * pi[,j] * tau[] * y[,'I',t], na.rm=TRUE)
                    )
                    
                    # Remnant infected
                    kappa[j,t+1] <- rpois(1, 
                                          mean(rho[,j], na.rm=TRUE) * sum(iota[j,1:t] * exp(-(1:t) * mean(lambda[,j], na.rm=TRUE)))
                    )
                    
                    # Spatial hazard
                    h[j,t+1] <- (beta[j] * y[j,'S',t] * (1 - exp(-sum(rho[,j] * pi[,j] * tau[], na.rm=TRUE) * (y[j,'S',t] / sum(y[j,,t])) * (sum(y[-j,"I",t]) / sum(N[-j])) ))) / (1 + beta[j] * y[j,'S',t])
                    
                    # Waiting time
                    w[j,t+1] <- h[j,t+1] * prod(1 - h[j,1:t])
               }
          }
     }
     
     # Normalize waiting time PDFs
     #for (i in 1:nrow(w)) w[i,] <- w[i,]/sum(w[i,], na.rm=TRUE)
     
     return(list(tsir=y, tot.inf=tot.inf, spatial.hazard=h, wait.time=w))
}

##' Calculate observed number of trips of each route type
##'
##' This function calculates the observed number of trips of each of the four route types for 
##' each temporal unit in a mobility data array. The route types are defined by the population density 
##' of the origin and destination districts:
##' \enumerate{
##'   \item High density to high density (HH)
##'   \item High density to low density (HL)
##'   \item Low density to high density (LH)
##'   \item Low density to low density (LL)
##' }
##' 
##' @param m A 3-dimensional data array produced by the \code{\link{mob.data.array}} function containing total trip counts (dimensions are origin, destination, time)
##' @param hi A vector of numerical district IDs in the high population density group
##' @param lo A vector of numerical district IDs in the low population density group
##' @param per.route logical indicating whether or not to scale number of trips by the number of routes in each route type
##' 
##' @return A five column dataframe. If \code{per.route = FALSE}, then counts represent raw number of total trips.  If \code{per.route = TRUE}, then counts represent total trips per route.
##' 
##' @author John Giles
##' 
##' @example R/examples/calc_route_type.R
##'
##' @family data synthesis
##' 
##' @export
##' 

calc.route.type <- function(
     m,      # mobility data array
     hi,     # vector of numerical district IDs in the high population density group
     lo,      # vector of numerical district IDs in the low population density group
     per.route=FALSE #logical indicating whether or not to scale number of trips by the number of routes in each route type
) {
     
     if(!all(c(hi, lo) %in% dimnames(m)[[1]])) {
          stop("Check that all locations in 'hi' and 'lo' groups exist in mobility data array")
     }
     
     func <- function(x) sum(x, na.rm=TRUE)
     
     if (per.route == TRUE) {
          
          message("Calculating total trips per route")
          n.HH <- length(hi)^2
          n.LL <- length(lo)^2
          n.HL <- n.LH <- length(hi)*length(lo)
          
     } else if (per.route == FALSE) {
          
          message("Calculating raw number of total trips")
          n.HH <- n.LL <- n.HL <- n.LH <- 1
     }
     
     return(
          data.frame(time=dimnames(m)[[3]], 
                     HH=apply(m[dimnames(m)$origin %in% hi, dimnames(m)$destination %in% hi,], 3, func)/n.HH, 
                     HL=apply(m[dimnames(m)$origin %in% hi, dimnames(m)$destination %in% lo,], 3, func)/n.HL, 
                     LL=apply(m[dimnames(m)$origin %in% lo, dimnames(m)$destination %in% lo,], 3, func)/n.LL, 
                     LH=apply(m[dimnames(m)$origin %in% lo, dimnames(m)$destination %in% hi,], 3, func)/n.LH)  
     )
}

##' Combine two TSIR scenarios
##'
##' Combine output from two TSIR scenarios into one list object.
##' 
##' @param x list object of simulation output from first scenario
##' @param y list object of simulation output from second scenario
##' 
##' @return a list
##' 
##' @author John Giles
##'
##' @family simulation
##' 
##' @export
##' 

sim.combine <- function(x, 
                        y
){
     list(
          tot.inf=rbind(x$tot.inf, 
                        y$tot.inf),
          epi.curve=abind::abind(x$epi.curve, 
                                 y$epi.curve, 
                                 along=3),
          wait.time=abind::abind(x$wait.time, 
                                 y$wait.time, 
                                 along=3)
     )
}

##' Combine TSIR scenarios containing both gravity model types 
##'
##' Helper function designed to facilitate parallel implementation with the foreach package.This function combines
##' iterations of a TSIR scenario that contain output for both the basic gravity model (B) and the new gravity model
##' with duration (R).
##' 
##' @param x list object of simulation output from first iteration(s)
##' @param y list object of simulation output from second iteration(s)
##' 
##' @return a list
##' 
##' @author John Giles
##'
##' @family simulation
##' 
##' @export
##' 

sim.combine.dual <- function(x, 
                             y
){
     list(
          B=list(
               tot.inf=rbind(x$B$tot.inf, 
                             y$B$tot.inf),
               epi.curve=abind::abind(x$B$epi.curve, 
                                      y$B$epi.curve, 
                                      along=3),
               wait.time=abind::abind(x$B$wait.time, 
                                      y$B$wait.time, 
                                      along=3)),
          R=list(
               tot.inf=rbind(x$R$tot.inf, 
                             y$R$tot.inf),
               epi.curve=abind::abind(x$R$epi.curve, 
                                      y$R$epi.curve, 
                                      along=3),
               wait.time=abind::abind(x$R$wait.time, 
                                      y$R$wait.time, 
                                      along=3)
          )
     )
}

##' Simulate full stochastic TSIR in parallel
##'
##' This function simulates TSIR models using both the basic gravity model and the gravity model with duration for
##' each stochastic realization of the estimated model parameters (lambda, pi, rho, tau). The function runs the \code{N.sim1} level
##' simulations in parallel and the \code{N.sim2} level simulations sequentially.
##' 
##' @param N Vector giving the population size of each district
##' @param D Distance matrix
##' @param B Estimated parameters from basic gravity model
##' @param R Estimated parameters from gravity model with duration
##' @param lambda Estimated parameters from trip duration decay model (lambda), processed by the \code{\link{get.param.vals}} function
##' @param prop.leave Observed proportion individuals leaving origin at time t in trip duration data 
##' @param prop.remain Observed proportion of individuals remaining in destination j
##' @param beta Transmission rate
##' @param gamma Recovery rate
##' @param gen.t Pathogen generation time (days)
##' @param max.t Maximum number of epidemic generations
##' @param I.0 Vector giving number of infected individuals in each district at time 0
##' @param N.sim1 Number of times to simulate matrices of model parameters (lambda, pi, tau, rho)
##' @param N.sim2 Number of times to simulate epidemic outcomes under each realization of model parameters (lambda, pi, tau, rho; default = 100)
##' @param max.t Maximum number of generations (default = 100)
##' @param freq.dep Logical indicating frequency (\code{TRUE}) or density dependent (\code{FALSE}) transmission
##' @param parallel Logical indicating whether to initiate the cluster within \code{sim.TSIR.full} and register as \code{foreach} backend
##' @param n.cores Number of cores to use when running in parallel
##' 
##' @return a list containing simulations using the basic gravity model and the gravity model with duration
##'
##' @author John Giles
##' 
##' @example R/examples/sim_TSIR_full.R
##'
##' @family simulation
##' 
##' @export
##' 

sim.TSIR.full <- function(
     N,                         # Vector giving the population size of each district
     D,                         # Distance matrix
     B,                         # Basic gravity model output
     R,                         # Duration gravity model output
     lambda,                    # Decay model parameters (Lambda)
     prop.leave,                # observed proportion individuals leaving origin at time t in trip duration data 
     prop.remain,               # observed proportion of individuals remaining in destination j
     beta,                      # Transmission rate
     gamma,                     # Recovery rate
     gen,                       # Pathogen generation time
     I.0,                       # Vector giving number of infected individuals in each district at time 0
     N.sim1,                    # Number of times to simulate matrices of model parameters (lambda, pi, tau, rho)
     N.sim2=100,                # Number of times to simulate epidemic outcomes under each realization of model parameters
     max.t=100,                 # Maximum number of generations
     freq.dep=TRUE,             # Frequency or density dependent transmission
     parallel=FALSE,            # Indicate whether to initiate and the cluster within \code{sim.TSIR.full} and register as \code{foreach} backend
     n.cores=NULL                    
){
     
     if(!(identical(length(N), nrow(D), nrow(lambda$mean), nrow(prop.remain)))) {
          stop("Dimensions of simulation arguments must match.")
     }
     
     if (parallel == TRUE) {
          
          cl <- makePSOCKcluster(n.cores)
          registerDoParallel(cl) 
          if (getDoParRegistered()) print(paste(class(cl)[1], 'with', getDoParWorkers(), 'cores named', getDoParName(), 'was initiated.', sep=' '), quote=FALSE)
     }
     
     districts <- dimnames(D)[[1]]
     n.districts <- length(districts)
     
     out <- foreach(i=1:N.sim1, .combine=sim.combine.dual, .packages=c('hmob', 'abind')) %dopar% {
          
          # Simulate one realization of estimated model parameters
          lambda.hat <- sim.lambda(mu=lambda$mean, sigma=lambda$sd, level='route')
          rho.hat <- sim.rho(p=prop.remain, level='route')
          tau.hat <- sim.tau(prop.leave)
          
          pi.hat.basic <- sim.gravity(N=N,
                                      D=D,
                                      theta=B['theta', 'Mean'],
                                      omega.1=B['omega.1', 'Mean'],
                                      omega.2=B['omega.2', 'Mean'],
                                      gamma=B['gamma', 'Mean'])
          
          pi.hat <- sim.gravity.duration(N=N,
                                         D=D,
                                         theta=B['theta', 'Mean'],
                                         omega.1=B['omega.1', 'Mean'],
                                         omega.2=B['omega.2', 'Mean'],
                                         gamma=B['gamma', 'Mean'],
                                         lambda=lambda.hat,
                                         alpha=R[which(row.names(R) == 'alpha[1]'):which(row.names(R) == 'alpha[62]'), 'Mean'])
          
          foreach(j=1:N.sim2, .combine=sim.combine.dual) %do% {
               
               # Basic gravity model    
               sim <- sim.TSIR(districts=districts,                 
                               N=N,                                 
                               pi=pi.hat.basic,        # connectivity comes from basic gravity model formulation 
                               beta=beta,                            # Transmission rate
                               gamma=gamma,                           # Recovery rate                         
                               gen=gen,                         
                               max.t=max.t,                         
                               I.0=I.0,                             
                               duration=F,             # do NOT use trip duration in force of infection                 
                               freq.dep=freq.dep                           
               )
               
               sim.B <- list(tot.inf=sim$tot.inf,
                         epi.curve=sim$tsir[,'I',],
                         wait.time=sim$wait.time)
               
               # Gravity model with duration
               sim <- sim.TSIR(districts=districts,                 
                               N=N,                                 
                               tau=tau.hat,            # Probability of leaving district i
                               lambda=lambda.hat,      # Matrix of trip duration decay for route i to j 
                               pi=pi.hat,              # Matrix of district connectivity (estimated pi_ijt)
                               rho=rho.hat,            # Matrix of proportion of travellers remaining for full generation when moving from i to j
                               beta=beta,              # Transmission rate
                               gamma=gamma,            # Recovery rate                          
                               gen=gen,                         
                               max.t=max.t,                
                               I.0=I.0,                             
                               duration=T,             # Use trip duration in force of infection
                               freq.dep=freq.dep                         
               )
               
               sim.R <- list(tot.inf=sim$tot.inf,
                         epi.curve=sim$tsir[,'I',],
                         wait.time=sim$wait.time)
               
               list(B=sim.B, R=sim.R)
          }
     }
     
     if (parallel == TRUE) {
          stopCluster(cl); print("Cluster stopped.", quote=FALSE)
          registerDoSEQ() # serial backend to allow repetetive tasks
          remove(cl)
     }
     
     return(out)
}

##' Calculate aggregate waiting time distribution
##'
##' This function aggregates the all simulated waiting time distributions into a single probability density for each district. 
##' The method uses simple linear pooling to get the weighted linear average of all waiting time probabilities (sometimes referred to as the 'linear opinion pool').
##' 
##' @param x a three-dimensional array containing waiting time distributions for all districts and simulations
##' 
##' @return a matrix (rows = districts, cols = epidemic generation)
##' 
##' @author John Giles
##' 
##' @example R/examples/calc_wait_time.R
##'
##' @family simulation
##' 
##' @export
##' 

calc.wait.time <- function(x) {
     
     tmp <- array(NA, dim=dim(x)[1:2])
     
     for (i in 1:dim(x)[1]) {
          for (j in 1:dim(x)[2]) {
               
               tmp[i,j] <- sum(x[i,j,], na.rm=T) / dim(x)[3]  
          }
          
          tmp[i,] <- tmp[i,]/sum(tmp[i,])
     }
     
     dimnames(tmp) <- dimnames(x)[1:2]
     return(tmp)
}


##' Calculate HPD of aggregated waiting time distributions
##'
##' This function calculates the highest posterior density (HPD) interval for the aggregated 
##' waiting time distributions returned by the \code{\link{calc.wait.time}} function. The function 
##' calculates the maximum of each aggregated probability distribution along with the HPD intervals given by \code{ci}. 
##' Code adapted from the \code{hpd} function in the \code{TeachingDemos} package.
##' 
##' @param x aggregated waiting time distributions for each district (output from \code{\link{calc.wait.time}})
##' @param ci a scalar or vector giving the HPD intervals to be calculated (default = 0.95)
##' 
##' @return a dataframe
##' 
##' @author John Giles
##' 
##' @example R/examples/calc_hpd.R
##'
##' @family simulation
##' 
##' @export
##' 

calc.hpd <- function(x, ci=0.95) {
     
     districts <- dimnames(x)[[1]]
     
     out <- foreach (i=seq_along(districts), .combine='rbind') %do% {
          
          if (all(is.na(x[i,]))) {
               
               out <- foreach(j=seq_along(ci), .combine='cbind') %do% {
                    
                    out <- data.frame(NA, NA) 
                    colnames(out) <- c(paste('lo', ci[j]*100, sep=''), paste('hi', ci[j]*100, sep=''))
                    out
               }
               
               cbind(max=NA, out) 
               
          } else {
               
               time.steps <- as.numeric(names(x[i,]))
               samp <- sample(time.steps, 1e4, replace = TRUE, prob=x[i,]) 
               
               # Calculate 50% and 95% highest posterior density
               out <- foreach(j=seq_along(ci), .combine='cbind') %do% {
                    
                    samp <- sort(samp)  # order sample for CDF
                    n <- length(samp)   # length of sample
                    tails <- (1-ci[j]) # HPD interval tails
                    
                    cutoff <- round(n*tails)   # index of gross cutoff
                    tmp <- samp[(n-cutoff+1):n] - samp[1:cutoff] # exact cutoff
                    cutoff2 <- which(tmp == min(tmp))[1] 
                    
                    out <- data.frame(samp[cutoff2], samp[n-cutoff+cutoff2]) 
                    colnames(out) <- c(paste('lo', ci[j]*100, sep=''), paste('hi', ci[j]*100, sep=''))
                    out
               }
               
               cbind(max=time.steps[which(x[i,] == max(x[i,]))], out) 
          }
     }
     
     return(cbind(as.data.frame(districts), out))
}


##' Calculate proportion infected in each district
##'
##' This function takes the output from a TSIR simulation and calculates the mean 
##' proportion infected in each district.
##' 
##' @param x matrix of total infected for all districts in simulation 
##' @param N vector of population size of each district
##' 
##' @return a dataframe
##' 
##' @author John Giles
##' 
##' @example R/examples/calc_prop_inf.R
##'
##' @family simulation
##' 
##' @export
##' 

calc.prop.inf <- function(
     x, # matrix of total infected for all districts in simulation
     N  # population size of each district
) {
     for(i in 1:ncol(x)) x[,i] <- x[,i]/N[i]
     return(apply(x, 2, mean))
}

##' Calculate peak timing of introduction and proportion infected
##'
##' This function takes the output from a TSIR simulation and calculates the peak of the waiting time
##' distribution and the mean proportion infected in each district.
##' 
##' @param sim simulation object containing simulations with both basic gravity and gravity with duration
##' @param N a named vector of population size of each district
##' @param pathogen name of pathogen
##' @param intro type of introduction
##' 
##' @return a longform dataframe to use for plotting
##' 
##' @author John Giles
##' 
##' @example R/examples/calc_timing_magnitude.R
##'
##' @family simulation
##' 
##' @export
##' 

calc.timing.magnitude <- function(
     sim,          # simulation object containing simulations with both basic gravity and gravity with duration
     N,            # population size of each district
     pathogen,     # name of pathogen
     intro         # type of introduction
) {
     
     districts <- colnames(sim$B$tot.inf)
     N <- N[which(names(N) %in% districts)]
     
     # Calculate mean proportion infected in each district
     prop.inf.basic <- calc.prop.inf(sim$B$tot.inf, N=N)
     wait.time.basic <- (calc.hpd(calc.wait.time(sim$B$wait.time))$max)
     
     prop.inf.duration <- calc.prop.inf(sim$R$tot.inf, N=N)
     wait.time.duration <- (calc.hpd(calc.wait.time(sim$R$wait.time))$max)
     
     out <- rbind(
          data.frame(district=districts,
                     pathogen=pathogen,
                     intro=intro,
                     model='B',
                     prop.inf=prop.inf.basic,
                     wait.time=wait.time.basic),
          
          data.frame(district=districts,
                     pathogen=pathogen,
                     intro=intro,
                     model='R',
                     prop.inf=prop.inf.duration,
                     wait.time=wait.time.duration)
     )
     
     row.names(out) <- NULL
     return(out)
}

##' Load specific objects in a .Rdata file
##'
##' This function takes a .Rdata file and loads the indexed item(s) into the current environment as an object.
##' 
##' @param x the index corresponding to the desired object (default = 1)
##' @param file filepath to .Rdata object
##' 
##' @return an R object
##' 
##' @author John Giles
##' 
##' @example R/examples/load_object.R
##'
##' @family utility
##' 
##' @export
##' 

load.obj <- function(x=1, file) {
     tmp <- new.env()
     load(file=file, envir=tmp)
     tmp[[ls(tmp)[x]]]
}

##' Get legend from ggplot object
##'
##' This function returns the legend of a ggplot object.
##' 
##' @param x a ggplot object with a legend
##' 
##' @return ggplot object
##' 
##' @author John Giles
##' 
##' @family utility
##' 
##' @export
##' 

get.legend <- function(
     x # a ggplot object with a legend
){
     x <- ggplot_gtable(ggplot_build(x))
     return(x$grobs[[which(sapply(x$grobs, function(y) y$name) == "guide-box")]])
}

##' Calculate model summary and validation statistics for trip duration decay model
##'
##' This function takes the output from a the \code{jags.model} function and calculates summaries of
##' the posterior parameter estimates, convergence diagnostics, and sample sizes for each route.
##' 
##' @param x JAGS model object
##' @param y.route matrix of route-level trip duration counts
##' @param districts vector of district names
##' @param pop.dens vector of district population densites
##' @param dist.mat distance matrix
##' 
##' @return a list containing a dataframe of parameter estimates and diagnostics, and a matrix of actual and predicted trip duration counts
##' 
##' @author John Giles
##' 
##' @example R/examples/calc_decay_stats.R
##'
##' @family model processing
##' 
##' @export
##' 

calc.decay.stats <- function(
     x,              # JAGS model object
     y.route,        # matrix of route-level 
     districts,      # vector of districts
     pop.dens,       # vector of district population densites
     dist.mat        # distance matrix
) {
     
     if(!(identical(length(districts), length(pop.dens), nrow(dist.mat), nrow(y.route)))) {
          stop("Dimensions of arguments must match.")
     }
     
     x <- MCMCvis::MCMCsummary(as.mcmc.list(x), n.eff=TRUE)
     
     x <- data.frame(name=row.names(x), as.data.frame(x), row.names=NULL)[,-5]
     x$smape <- x$mape <- x$mae <- x$r <- x$samp.size <- x$dest.dens <- x$orig.dens <- x$distance <- x$destination <- x$origin <- NA
     colnames(x)[c(4:6)] <- c('lo95', 'hi95', 'psrf')
     
     err <- vector()
     
     for (i in seq_along(districts)) {
          for (j in seq_along(districts)) {
               
               sel <- which(x$name == paste('lambda[', i, ',', j, ']', sep=''))
               
               if (i == j) {
                    
                    x[sel, -1] <- NA
                    
               } else {
                    
                    x$origin[sel] <- districts[i]
                    x$destination[sel] <- districts[j]
                    x$distance[sel] <- dist.mat[i,j]
                    x$orig.dens[sel] <- pop.dens[i]
                    x$dest.dens[sel] <- pop.dens[j]
                    x$samp.size[sel] <- sum(!is.na(y.route[i,j,]))
                    
                    tmp <- decay.func(N0=y.route[i,j,1], 
                                      lambda=x$mean[sel], 
                                      y=1:length(y.route[i,j,]))
                    
                    tmp <- cbind(tmp, y.route[i,j,])
                    tmp <- matrix(tmp[complete.cases(tmp),], ncol=2)
                    
                    x$mae[sel] <- Metrics::mae(tmp[,2], tmp[,1])
                    x$mape[sel] <- Metrics::mape(tmp[,2], tmp[,1])
                    x$smape[sel] <- Metrics::smape(tmp[,2], tmp[,1])
                    
                    err <- rbind(err, 
                                 cbind(tmp[,2], 
                                       tmp[,1], 
                                       Metrics::ape(tmp[,2], tmp[,1]),
                                       rep(x$samp.size[sel], nrow(tmp))
                                 )
                    )
                    
                    if (nrow(tmp) < 5) {
                         
                         x$r[sel] <- NA
                         
                    } else {
                         
                         x$r[sel] <- cor.test(tmp[,1], tmp[,2], na.rm=T)$estimate
                    }
               }
          }
     }
     
     return(list(mod.decay=x, ae=err))
}

##' Get the sample size of a route-level \code{mob.data.array} matrix object
##'
##' This function takes the output from a the \code{mob.data.array} or \code{mob.data.array.route.level} functions and calculates
##' the sample size of unique observations of the variable along the last dimension of the array (e.g. columns of a 2D matrix or the third dimension in a 3D array)
##' 
##' @param x a three dimensional array produced by the \code{mob.data.array.route.level} function 
##' 
##' @return a named array, matrix, or vector of sample sizes
##' 
##' @author John Giles
##' 
##' @example R/examples/calc_samp_size.R
##'
##' @family data synthesis
##' 
##' @export
##' 

calc.samp.size <- function(x) {
     
     dims <- dim(x)
     
     message(paste('Calculating sample sizes for unique observations of', names(dimnames(x))[length(dims)], 'along dimension', length(dims), sep=' '))
     
     return(apply(x, seq_along(dims)[-length(dims)], function(x) sum(!is.na(x), na.rm=T)))
}

##' Find the subset of districts which have a minumim number of samples
##'
##' This function takes the output from a the \code{mob.data.array} function and finds the 
##' largest subset of locations (districts) that have a minumum number of observations for all \eqn{ij} routes. The subset
##' is found by sequentially removing the location with the largest number of routes below the defined 
##' threshold (\code{min.samp}) until all locations contain at least \code{min.samp} number of observations for each route.
##' 
##' @param x a three dimensional array produced by the \code{mob.data.array} function 
##' @param min.locations minimum number of locations (rows and columns) to keep
##' @param min.samp minimum sample size
##' 
##' @return a three dimensional array containing the subset of \code{x}
##' 
##' @author John Giles
##' 
##' @example R/examples/get_subsamp.R
##'
##' @family data synthesis
##' 
##' @export
##' 

get.subsamp <- function(
     x,              # 3D mobility data array
     min.locations,  # minimum number of locations (rows and columns) to keep
     min.samp        # minumum sample size
) {
     
     n.districts <- dim(x)[1]
     samp.size <- calc.samp.size(x)
     
     if(all(diag(samp.size) == 0)) diag(samp.size) <- NA
     
     for (i in 1:(n.districts - min.locations)) {
          
          if (all(as.vector(samp.size) >= min.samp, na.rm=TRUE)) break
          
          tmp <- vector()
          for (j in 1:nrow(samp.size)) {
               
               tmp <- c(tmp, sum(sum(samp.size[,j] < min.samp, na.rm=TRUE), 
                                 sum(samp.size[j,] < min.samp, na.rm=TRUE)))
          }
          
          lose <- which(tmp == max(tmp))
          samp.size <- samp.size[-lose, -lose]
     }
     
     sel <- dimnames(x)$origin %in% dimnames(samp.size)$origin
     return(x[sel,sel,])
}


##' Calculate the cumulative proportion of trips for values of a given variable
##'
##' This function takes a longform dataframe of trip counts and calculates the cumulative proportion of the total number of observed trips for each of 
##' the values provided in the \code{vals} vector.
##' 
##' @param variable a vector of variable values for which to calculate the proportion of total trips (e.g. 'duration' or 'distance')
##' @param counts a vector of observed counts of each value
##' @param vals vector giving numeric values of the variable at which to calculate the proportion of total trips
##' @param parallel run in parallel (when the \code{vals} vector is long), default=FALSE
##' @param n.cores number of cores to use in parallel
##' 
##' @return a two-column dataframe
##' 
##' @author John Giles
##' 
##' @example R/examples/calc_prop_tot_trips.R
##'
##' @family data synthesis
##' 
##' @export
##' 

calc.prop.tot.trips <- function(
     variable,                     # a vector of variable values for which to calculate the proportion of total trips
     counts,                       # observed counts of each value
     type,                         # type pf proportion to calculate: options are 'cumulative' (<= each variable value) or 'interval' (values in \code{vals} are used as bins)
     vals,                         # numaric values of the variable at which to calculate the proportion of total trips
     parallel=FALSE,               # run in parallel (when the \code{vals} vector is long), default=FALSE
     n.cores=2                     # number of cores to use in parallel
) {
     
     if(!(identical(length(variable), length(counts)))) {
          stop("Dimensions of variable and count arguments must match.")
     }
     
     if(length(vals) > 100) {
          print("Calculating the proportion of total trips for a large number of values will increase compuation time.", quote=FALSE)
     }
     
     if (parallel == FALSE) {
          
          if (type == 'cumulative') {
               
               out <- rep(NA, length(vals))
               for (i in seq_along(vals)) {
                    
                    print(paste(i, "of", length(vals), "values ---", round((i/length(vals))*100), "%", sep= " "))
                    
                    out[i] <- sum(counts[variable <= vals[i]], na.rm=TRUE)
               }
          } else if (type == 'interval') {
               
               if (vals[1] != 0) vals <- c(0, vals)
               
               out <- rep(NA, length(vals))
               for (i in 2:length(vals)) {
                    
                    print(paste(i, "of", length(vals), "values ---", round((i/length(vals))*100), "%", sep= " "))
                    
                    out[i] <- sum(counts[variable > vals[i-1] & variable <= vals[i]], na.rm=TRUE)
               }
          }
     } else if (parallel == TRUE) {
          
          print(paste("Running", length(vals), "values in parallel..."), quote=FALSE)
          
          clust <- makeCluster(n.cores)
          registerDoParallel(clust)
          
          if (type == 'cumulative') {
               
               out <- foreach(i=seq_along(vals), .combine='c') %dopar% {
                    
                    sum(counts[variable <= vals[i]], na.rm=TRUE)
               }
          } else if (type == 'interval') {
               
               if (vals[1] != 0) vals <- c(0, vals)
               
               out <- foreach(i=2:(length(vals)), .combine='c') %dopar% {
                    
                    sum(counts[variable > vals[i-1] & variable <= vals[i]], na.rm=TRUE)
               }
               out <- c(NA, out)
          }
     }
     
     out <- out/sum(counts)
     out <- data.frame(vals, out)
     colnames(out) <- c('values', 'prop')
     return(out)
}



##' Simulate connectivity values using gravity model
##'
##' This function uses the gravity model formula to simulate a connectivity matrix based on the supplied model parameters. The 
##' gravity model formula uses a Gamma distribution as the dispersal kernel in the denominator. A null model (where all model parameters = 1) can be
##' simulated by supplying only population sizes (\code{N}) and pairwise distances (\code{D}).
##' \deqn{\theta * ( N_i^\omega_1 N_j^\omega_2 / f(d_ij) )}
##' 
##' @param N vector of population sizes
##' @param D matrix of distances among all \eqn{ij} pairs
##' @param theta scalar giving the proportionality constant of gravity formula (default = 1)
##' @param omega.1 scalar giving exponential scaling of origin population size (default = 1)
##' @param omega.2 scalar giving exponential scaling of destination population size (default = 1)
##' @param gamma scalar giving the dispersal kernel paramater (default = 1)
##' @param counts logical indicating whether or not to return a count variable by scaling the connectivity matrix by origin population size (\eqn{N_i}) (default = FALSE)
##' 
##' @return a matrix with values between 0 and 1 (if \code{counts = FALSE}) or positive integers (if \code{counts = TRUE})
##' 
##' @author John Giles
##' 
##' @example R/examples/sim_gravity_basic.R
##'
##' @family simulation
##' @family gravity
##' 
##' @export
##' 

sim.gravity <- function(
     N,
     D,
     theta=1,
     omega.1=1,
     omega.2=1,
     gamma=1,
     counts=FALSE
) {
     
     if (!(identical(length(N), dim(D)[1], dim(D)[1]))) stop('Check dimensions of input data N and D')
     if (!(length(c(theta, omega.1, omega.2, gamma)) == 4)) stop('theta and omega parameters must be scalars')
     
     n.districts <- length(N)
     x <- f.d <- matrix(NA, n.districts, n.districts)
     
     for (i in 1:n.districts) {
          for (j in 1:n.districts) {
               
               # Gravity model
               if (i == j) {
                    
                    x[i,j] <- 0
                    
               } else {
                    
                    f.d[i,j] <- (D[i,j]^gamma)
                    
                    x[i,j] <- exp(log(theta) + (omega.1*log(N[i]) + omega.2*log(N[j]) - log(f.d[i,j])))
               }          
          }
          
          x[i,] <- (x[i,]/sum(x[i,]))
          if (counts == TRUE) x[i,] <- round(x[i,]*N[i])
     }
     
     dimnames(x) <- list(origin=dimnames(D)[[1]], destination=dimnames(D)[[2]])
     return(x)
}



##' Simulate connectivity values using gravity model with trip duration
##'
##' This function simulates a connectivity matrix supplied model parameters in a gravitym odel formula that incorporates trip duration by using a conditional dispersal kernel (\eqn{f(d_ij | \lambda_ij)}) in 
##' the denominator. The gravity model still uses a Gamma distribution as the dispersal kernel, but this is scaled by the probability \eqn{Pr(\lambda_ij | d_ij)} according to Bayes theorem. If a 
##' vector of shape (\code{s}) and rate (\code{r}) parameters is supplied, the function will simulate route specific dispersal kernels based on the origin location (\eqn{i}). A null model (where all model parameters = 1) 
##' can be simulated by supplying only population sizes (\code{N}) and pairwise distances (\code{D}).
##' \deqn{
##'     \theta * ( N_i^\omega_1 N_j^\omega_2 / f(d_ij | \lambda_ij) )
##' }
##' 
##' @param N vector of population sizes
##' @param D matrix of distances among all \eqn{ij} pairs
##' @param theta scalar giving the proportionality constant of gravity formula (default = 1)
##' @param omega.1 scalar giving exponential scaling of origin population size (default = 1)
##' @param omega.2 scalar giving exponential scaling of destination population size (default = 1)
##' @param gamma scalar giving the dispersal kernel paramater (default = 1)
##' @param lambda matrix of trip duration decay parameters for each \eqn{ij} route
##' @param alpha model fitting parameter for the ECDF of lambda (default = 1)
##' @param counts logical indicating whether or not to return a count variable by scaling the connectivity matrix by origin population size (\eqn{N_i}) (default = FALSE)
##' 
##' @return a matrix with values between 0 and 1 (if \code{counts = FALSE}) or positive integers (if \code{counts = TRUE})
##' 
##' @author John Giles
##' 
##' @example R/examples/sim_gravity_duration.R
##'
##' @family simulation
##' @family gravity
##' 
##' @export
##' 



sim.gravity.duration <- function(
     N,
     D,
     theta=1,
     omega.1=1,
     omega.2=1,
     gamma=1,
     lambda,    # matrix of decay rate parameters
     alpha=1,
     counts=FALSE
) {
     
     if (!(identical(length(N), dim(D)[1], dim(D)[2], dim(lambda)[1], dim(lambda)[2]))) stop('Check dimensions of input data (N, D, or lambda)')
     if (!(length(c(theta, omega.1, omega.2, gamma)) == 4)) stop('theta, omega, and zeta parameters must be scalars')
     
     message('Initializing simulation matrices')
     
     n.districts <- length(N)
     D.scl <- (D - mean(D, na.rm=T)) / sd(D, na.rm=T)     # scaled distance matrix
     
     diag(lambda) <- 0
     lambda.scl <- scale(lambda)                                   # Estimated mean of relative decay parameter scaled and centered
     lambda.scl.unique <- sort(unique(as.vector(lambda.scl)))
     lambda.scl.unique <- seq(min(lambda.scl.unique), max(lambda.scl.unique), length.out=100) # A set of unique lambda values for integral approximation
     
     if (length(alpha) == 1) alpha <- rep(alpha, n.districts)
     
     x <- f.d <- f.d.lambda <- matrix(NA, n.districts, n.districts)
     
     message('Simulating gravity model with duration')
     for (i in 1:n.districts) {
          
          message(paste('Origin:', i, sep=' '))
          
          for (j in 1:n.districts) {
               
               # Gravity model
               if (i == j) {
                    
                    x[i,j] <- 0
                    
               } else {
                    
                    
                    f.d[i,j] <- (D[i,j]^gamma) + 1e-6
                    
                    # Conditional dispersal kernel
                    f.d.lambda[i,j] <- ( f.d[i,j] * (1 - sum(lambda[,] <= lambda[i,j])/(n.districts^2))^alpha[i]) + 1e-6
                    
                    x[i,j] <- exp(log(theta) + (omega.1*log(N[i]) + omega.2*log(N[j]) - log(f.d.lambda[i,j])))
               }          
          }
          
          x[i,] <- (x[i,]/sum(x[i,]))
          if (counts == TRUE) x[i,] <- round(x[i,]*N[i])
     }
     
     dimnames(x) <- list(origin=dimnames(D)[[1]], destination=dimnames(D)[[2]])
     return(x)
}




##' Build distance matrix from XY coordinates
##' 
##' This function builds the pairwise distance matrix from vectors of XY coordinates and associated names.
##' 
##' @param x vector giving X coordinates
##' @param y vector giving Y coordinates
##' @param id vector of names for each location

##' @return a named matrix of pairwise distances among locations
##' 
##' @author John Giles
##' 
##' @example R/examples/get_distance_matrix.R
##'
##' @family data synthesis
##' 
##' @export
##' 

get.distance.matrix <- function(x,   # x coord
                                y,   # y coord 
                                id   # name associated with each element
) {
     xy <- cbind(x, y)
     window <- spatstat::bounding.box.xy(xy)
     out <- spatstat::pairdist(spatstat::as.ppp(xy, window, check=FALSE))
     dimnames(out) <- list(origin=id, destination=id)
     out[order(dimnames(out)$origin), order(dimnames(out)$destination)]
}

##' Get distance between two points for one observation
##' 
##' Takes X and Y coordinates of two locations and returns cross distance for all entries.
##' 
##' @param xy1 two column matrix of XY coordinates for first group
##' @param xy2 two column matrix of XY coordinates for second group
##' @param id1 optional names for first group
##' @param id2 optional names for second group
##' 
##' @return numeric scalar or matrix
##' 
##' @author John Giles
##' 
##' @example R/examples/get_crossdist.R
##'
##' @family data synthesis
##' 
##' @export
##' 

get.crossdist <- function(xy1,
                          xy2,
                          id1=NULL,
                          id2=NULL
) {
     
     if (!is.null(xy1) | !is.null(xy2)) colnames(xy1) <- colnames(xy2) <- rep(NA, ncol(xy1))
     
     suppressWarnings(
          window <- spatstat::bounding.box.xy(rbind(xy1, xy2))
     )
     
     out <-  spatstat::crossdist(spatstat::as.ppp(xy1, window, check=FALSE),
                                 spatstat::as.ppp(xy2, window, check=FALSE))
     
     dimnames(out) <- list(origin=id1, destination=id2)
     
     if (!is.null(id1)) out <- out[order(dimnames(out)$origin),]
     if (!is.null(id2)) out <- out[,order(dimnames(out)$destination)]
     
     out
}


##' Get sparse mobility matrix
##' 
##' Takes X and Y coordinates of two locations and returns cross distance for all entries.
##' 
##' @param orig vector of origin names
##' @param dest vector of destination names
##' @param value vector of observed values for each origin/destination pair
##' @param missing.obs filler value for missing observations (default=NA)
##' 
##' @return named numeric matrix
##' 
##' @author John Giles
##' 
##' @example R/examples/get_sparse_mob_matrix.R
##'
##' @family data synthesis
##' 
##' @export
##' 

get.sparse.mob.matrix <- function(orig,
                                  dest,
                                  value,
                                  missing.obs='NA'
){
     
     fac <- factor(sort(unique(c(orig, dest))))
     
     m <- formatSpMatrix(
          sparseMatrix(i=as.integer(factor(orig, levels=levels(fac))), 
                       j=as.integer(factor(dest, levels=levels(fac))), 
                       x=value), 
          zero.print=missing.obs
     )
     
     suppressWarnings(class(m) <- "numeric")
     dimnames(m) <- list(origin=levels(fac), destination=levels(fac))
     m[order(dimnames(m)$origin), order(dimnames(m)$destination)]
}


##' Fit gravity model to movement matrix
##' 
##' This function fits gravity model parameters to a supplied movement matrix using Bayesian MCMC inference. The function defines the model and serves as a wrapper for the \code{\link{run.jags}}
##' function in the \code{\link{runjags}} package. Gravity model formula:
##' \deqn{
##'     \theta * ( N_i^\omega_1 N_j^\omega_2 / f(d_ij) )
##' }
##' 
##' @param M named matrix of trip counts among all \eqn{ij} location pairs
##' @param D named matrix of distances among all \eqn{ij} location pairs
##' @param N named vector of population sizes for all locations (either N or both N_orig and N_dest must be supplied)
##' @param N_orig named vector of population sizes for each origin 
##' @param N_dest named vector of population sizes for each destination.
##' @param n.chain number of MCMC sampling chains
##' @param n.adapt number of adaptive iterations
##' @param n.burn number of iterations to discard before sampling of chains begins (burn in)
##' @param n.samp number of iterations to sample each chain
##' @param n.thin interval to thin samples 
##' @param prior a list of priors for model parameters. If NULL (default) the model uses uniformative priors
##' @param parallel logical indicating whether or not to run MCMC chains in parallel or sequentially (default = FALSE)
##' 
##' @return a runjags model object conataining fitted gravity model paramters
##' 
##' @author John Giles
##' 
##' @example R/examples/fit_gravity.R
##'
##' @family model
##' @family gravity
##' 
##' @export
##' 

fit.gravity <- function(
     M,   
     D, 
     N=NULL,
     N_orig=NULL,
     N_dest=NULL,
     n.chain=4,
     n.adapt=1000,
     n.burn=1000,
     n.samp=1000,
     n.thin=1,
     prior=NULL,
     parallel=FALSE
) {
     
     # Check data
     if (all(!is.null(N), is.null(N_orig), is.null(N_dest))) {
          N_dest <- N_orig <- N
     } else if (all(is.null(N), !is.null(N_orig), is.null(N_dest))) {
          N_dest <- N_orig
     } 
     
     if (!(identical(dim(M)[1], dim(D)[1], length(N_orig)))) stop('Dimensions of input data must match')
     if (!(identical(dim(M)[2], dim(D)[2], length(N_dest)))) stop('Dimensions of input data must match')
     
     if ( !(identical(dimnames(M)[[1]], dimnames(D)[[1]])) | !(identical(dimnames(M)[[1]], names(N_orig))) ) {
          stop('Dimension names of input data do not match.')
     }
     
     if ( !(identical(dimnames(M)[[2]], dimnames(D)[[2]])) | !(identical(dimnames(M)[[2]], names(N_dest))) ) {
          stop('Dimension names of input data do not match.')
     }
     
     message(
          paste('::Fitting gravity model for', 
                dim(M)[1], 
                'origins and',
                dim(M)[2], 
                'destinations::',
                sep=' ')
     )
     
     if (!all(unlist(lapply(list(M, N_orig, N_dest), is.integer)))) {
          M[,] <- as.integer(M)
          N_orig[] <- as.integer(N_orig)
          N_dest[] <- as.integer(N_dest)
     }
     
     if (is.null(prior)) { 
          
          message('Using uniformative priors')
          prior <- c(1, 0.05)
          prior <- list(theta=prior,
                        omega_1=prior,
                        omega_2=prior,
                        gamma=prior)
          
     } else {
          
          message('Using supplied informative priors')
     }
     
     jags.data <- list(
          M=M,                     
          D=D,                     
          N_orig=N_orig,
          N_dest=N_dest,
          prior_theta=prior$theta,
          prior_omega_1=prior$omega_1,
          prior_omega_2=prior$omega_2,
          prior_gamma=prior$gamma
     )
     
     jags.model <- "
     model {     
          
          # Poisson likelihood
          for (i in 1:length(N_orig)) {
               for (j in 1:length(N_dest)) {
                    
                    M[i,j] ~ dpois(pi[i,j]*N_orig[i]) 
               }
               
               pi[i,1:length(N_dest)] <- c[i,]/sum(c[i,])
          }
          
          for (i in 1:length(N_orig)) {
               for (j in 1:length(N_dest)) {
                    
                    # Gravity model
                    c[i,j] <- ifelse(
                         i == j, 
                         0,
                         exp(log(theta) + (omega_1*log(N_dest[i]) + omega_2*log(N_orig[j]) - log( f.d[i,j] )))
                    )
                    
                    # Dispersal kernel
                    f.d[i,j] <- D[i,j]^gamma
               }
          }
          
          ### Priors ###
          theta ~ dgamma(prior_theta[1], prior_theta[2])
          omega_1 ~ dgamma(prior_omega_1[1], prior_omega_1[2])
          omega_2 ~ dgamma(prior_omega_2[1], prior_omega_2[2])
          gamma ~ dgamma(prior_gamma[1], prior_gamma[2])
          
     }"
     
     
     params <- c('omega_1', 'omega_2', 'theta', 'gamma')
     
     init.list <- replicate(n.chain, 
                            list(.RNG.name='lecuyer::RngStream',
                                 .RNG.seed= sample(1:1e6, 1)), 
                            simplify=FALSE)
     
     if (parallel == TRUE) {
          
          method <- 'parallel'
          
     } else if (parallel == FALSE) {
          
          method <- 'rjags'
     }
     
     runjags::run.jags(model=jags.model,
                       data=jags.data,
                       monitor=params,
                       n.chains=n.chain,
                       adapt=n.adapt,
                       burnin=n.burn,
                       sample=n.samp,
                       thin=n.thin,
                       inits=init.list,
                       modules=c('lecuyer'),
                       method=method,
                       summarise=FALSE)
}



##' Estimate probability of travelling outside origin
##' 
##' This function fits a hierarchical model that estimates the probability an individual travels outside their home location
##' within the time period of the survey (tau). The model estimates both the overall population-level probability of travel (tau_pop) and 
##' the origin-level probability of travel (tau_i). Further this method is designed for sparse observations that typically result from 
##' travel survey data, where unobserved routes of travel regress to the population mean.
##' 
##' @param V_travel named vector of total number of people that reported travelling outside their home location
##' @param V_tot named vector of the total number of individuals in travel survey for each location
##' @param n.chain number of MCMC sampling chains
##' @param n.adapt number of adaptive iterations
##' @param n.burn number of iterations to discard before sampling of chains begins (burn in)
##' @param n.samp number of iterations to sample each chain
##' @param n.thin interval to thin samples 
##' @param parallel logical indicating whether or not to run MCMC chains in parallel or sequentially (default = FALSE)
##' 
##' @return dataframe giving input data along with estimates of travel probability for each location
##' 
##' @author John Giles
##' 
##' @example R/examples/fit_prob_travel.R
##'
##' @family model
##' @family travel survey
##' 
##' @export
##' 

fit.prob.travel <- function(
     V_travel,
     V_tot,
     n.chain=4,
     n.adapt=1000,
     n.burn=1000,
     n.samp=1000,
     n.thin=1,
     parallel=FALSE
) {
     
     # Check data
     if (!(identical(length(V_travel), length(V_tot)))) stop('Dimensions of input data must match')
     if ( !(identical(names(V_travel), names(V_tot))) ) stop('Dimension names of input data do not match.')
     if (is.null(names(V_travel))) stop('Vectors are not named.')
     
     # Work around for NAs
     na.fix <- any(is.na(V_travel)) | any(is.na(V_tot))
     if (na.fix) {
          sel <- complete.cases(cbind(V_travel, V_tot))
     } else {
          sel <- seq_along(V_travel)
     }
     
     jags.data <- list(
          V_travel=V_travel[sel],
          V_tot=V_tot[sel]
     )
     
     jags.model <- "
     model {     
          
          # Origin-level probability of travel
          for (j in 1:length(V_travel)) {
               
               V_travel[j] ~ dbin(tau[j], V_tot[j])
          }
          
          # Population-level hyper-prior
          tau_pop ~ dbeta(1, 1) 
          
          # Origin-level priors
          for (k in 1:length(V_travel)) {
               
               tau[k] ~ dnorm(tau_pop, 10) T(0,1)
          }
     }"
     
     params <- c('tau_pop', 'tau')
     
     init.list <- replicate(n.chain, 
                            list(.RNG.name='lecuyer::RngStream',
                                 .RNG.seed= sample(1:1e6, 1)), 
                            simplify=FALSE)
     
     if (parallel == TRUE) {
          method <- 'parallel'
     } else if (parallel == FALSE) {
          method <- 'rjags'
     }
     
     mod <- summary(
          runjags::run.jags(model=jags.model,
                            data=jags.data,
                            monitor=params,
                            n.chains=n.chain,
                            adapt=n.adapt,
                            burnin=n.burn,
                            sample=n.samp,
                            thin=n.thin,
                            inits=init.list,
                            modules=c('lecuyer'),
                            method=method,
                            summarise=FALSE)
     )
     
     if (na.fix) {
          
          # Merge with missing obs
          out <- merge(
               data.frame(orig_id=names(V_travel),
                          travel=V_travel,
                          total=V_tot,
                          row.names=NULL),
               data.frame(orig_id=names(V_travel[sel]),
                          travel=V_travel[sel],
                          total=V_tot[sel],
                          mod[-1,],
                          row.names=NULL),
               all=T
          )
          
          # Set missing obs to population mean
          for(i in which(!sel)) out[i, -(1:3)] <- mod['tau_pop',]
          return(out)
          
     } else {
          
          return(
               data.frame(orig_id=names(V_travel),
                          travel=V_travel,
                          total=V_tot,
                          mod[-1,],
                          row.names=NULL)
          )
     }
}
