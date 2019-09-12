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
##' @family spatial
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
##' @family spatial
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
                             sub.samp=NULL     # number of generation to subsamp, if NULL (default) will use all observed generation times in the data
) {
     
     if (is.null(dimnames(d)$duration)) {
          stop("The calc.prop.remain function assumes that the duration values in the data array are NOT aggregated. Check that you have run the mob.data.array function with agg.int=1")
     } 
     
     max.gen <- ceiling(max(as.numeric(dimnames(d)$duration))/gen.t)
     
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
##' This function to calculates the two shape parameters (\eqn{a} and \eqn{b}) for the Beta distribution 
##' using the mean \eqn{\mu} and variance \eqn{\sigma^2} of a variable that between 0 and one 1.
##' 
##' @param m the mean \eqn{\mu} of a random variable between 0 and 1
##' @param v the variance \eqn{\sigma^2} of a random variable between 0 and 1
##' 
##' @return A list containing shape parameters \code{a} and \code{b}
##' 
##' @author John Giles
##'
##' @family simulation
##' 
##' @export
##' 

get.beta.params <- function(m, v) {
     a <- ((1-m) / v - 1/m) * m^2 
     list(a=a, b=a * (1 / m-1))
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
                         x <- rbeta(1, beta.params$a[i,j], beta.params$b[i,j])
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
               
               out[i] <- rbeta(1, beta.params$a[i], beta.params$b[i])
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
##' @param districts Vector of district names
##' @param n.districts Number of total districts
##' @param N Vector giving the population size of each district
##' @param lambda Estimated parameters from trip duration decay model (lambda), processed by the \code{\link{get.param.vals}} function
##' @param pi.duration Estimated parameters from gravity model with duration (pi), processed by the \code{\link{get.param.vals}} function
##' @param pi.basic Estimated parameters from basic gravity model (pi*), processed by the \code{\link{get.param.vals}} function
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
     districts,                 # Vector of district names
     n.districts,               # Number of total districts
     N,                         # Vector giving the population size of each district
     lambda,                    # Decay model parameters (Lambda)
     pi.duration,               # Gravity model with duration
     pi.basic,                  # Basic gravity model
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
     
     if(!(identical(n.districts, length(districts), length(N), nrow(pi.duration$mean), nrow(pi.basic$mean), nrow(lambda$mean), nrow(prop.remain)))) {
          stop("Dimensions of simulation arguments must match.")
     }
     
     if (parallel == TRUE) {
          
          cl <- makePSOCKcluster(n.cores)
          registerDoParallel(cl) 
          if (getDoParRegistered()) print(paste(class(cl)[1], 'with', getDoParWorkers(), 'cores named', getDoParName(), 'was initiated.', sep=' '), quote=FALSE)
     }
     
     out <- foreach(i=1:N.sim1, .combine=sim.combine.dual, .packages=c('hmob', 'abind')) %dopar% {
          
          # Simulate one realization of estimated model parameters
          lambda.hat <- sim.lambda(mu=lambda$mean, sigma=lambda$sd, level='route')
          pi.hat <- sim.pi(mu=pi.duration$mean, level='route')
          pi.hat.basic <- sim.pi(mu=pi.basic$mean, level='route')
          rho.hat <- sim.rho(p=prop.remain, level='route')
          tau.hat <- sim.tau(prop.leave)
          
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
               
               B <- list(tot.inf=sim$tot.inf,
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
               
               R <- list(tot.inf=sim$tot.inf,
                         epi.curve=sim$tsir[,'I',],
                         wait.time=sim$wait.time)
               
               list(B=B, R=R)
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
##' The method uses a simple linear combination of waiting time probabilities, sometimes referred to as the 'linear opinion pool'.
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
##' @family simulation
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

