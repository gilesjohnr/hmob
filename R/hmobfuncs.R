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
##' @family data management
##' 
##' @export
##' 

parse.longform <- function(d,          # expects 'trip_durations_counts_sub_42.txt'
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
##' @family data management
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
##' @family data management
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
##' @family data management
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
##' @family data management
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
##' @param adm administrative level 2 data, expects \code{admin2} data object (NAM_adm2.shp)
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
                     adm=admin2,  # administrative level 2 data (expects NAM_adm2.shp)
                     tol=0.1      # tolerance argument for gSimplify, set to zero for original
){
     
     # District name required for accessing shapefiles slots
     # Get district name if integer ID provided
     if(is.character(a) == FALSE) a <- distID[which(distID[,1] == a),2] 
     
     # Get origin shapefile
     a <- adm[which(adm$NAME_2 == a),]
     
     if (is.null(b)) {
          
          # Plot all districts
          plot(rgeos::gSimplify(adm, tol=tol, topologyPreserve=TRUE), border='grey60', lwd=1.25)
          
          # Add origin
          plot(rgeos::gSimplify(a, tol=tol, topologyPreserve=TRUE), col='grey35', add=T)
          plot(rgeos::gCentroid(a), pch=24, cex=1.75, bg="blue", lwd=2, add=T)
          
     } else {
          
          # Get district name if integer ID provided
          if(is.character(b) == FALSE) b <- distID[which(distID[,1] %in% b),2]
          
          # Get destination shapefile
          b <- adm[which(adm$NAME_2 %in% b),]
          
          # Plot all districts
          plot(rgeos::gSimplify(adm, tol=tol, topologyPreserve=TRUE), border='grey60', lwd=1.25)
          
          # Add origin
          plot(rgeos::gSimplify(a, tol=tol, topologyPreserve=TRUE), col=rgb(0,0,1, alpha=0.3), border='black', add=T)
          plot(rgeos::gCentroid(a), pch=24, cex=1.75, bg="blue", lwd=2, add=T)
          
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

##' Build JAGS data array
##'
##' This function that builds data arrays at the time-level (e.g. month) for use in JAGS. The data arrays contain counts of either 
##' the distance or duration of trips. When 'distance' is selected as the variable, the function returns 
##' a three-dimensional array and when 'duration' is selected, the function returns a four-dimensional array.
##' The difference in array dimensions occurs because the destination dimension must be collapsed in order to
##' perform counts of trip distance, leaving only the origin, time, and distance counts.
##' 
##' @param d Longform data with metadata attached
##' @param time The temporal interal used to construct the array (default = \code{'month'})
##' @param variable Character string giving the response variable; expects either \code{'distance'} or \code{'duration'}
##' @param gen.t The length of generation time (in days) by which to aggregate trip durations
##' @param parallel Logical indicating whether to execute in parallel (default = 1)
##' @param n.cores Number of cores to use when \code{parallel = TRUE} (default = NULL, which uses half available cores)
##' @return A 3-dimensional array when \code{variable = 'distance'}, and a 4-dimensional array when \code{variable = 'duration'}
##' 
##' @author John Giles
##'
##' @family model processing
##' 
##' @export
##' 

jags.data.array <- function(d,                            # data
                            time='month',                 # temporal interval
                            variable='duration',          # character string giving the response variable: 'distance' or 'duration'
                            gen.t=1,                      # length of epidemic generation in days
                            parallel=FALSE,               # whether or not to execute in parallel
                            n.cores=NULL
) {
     
     if (gen.t < 1) stop('Generation time gen.t must be at least 1')
     
     if (parallel == TRUE) {
          
          print("Beginning parallel jags.data.array function")
          
          if (variable == 'distance') {
               
               o <- sort(unique(d$from))                # rows = i
               t <- sort(unique(d[,time]))              # cols = j
               v <- sort(unique(d[,variable]))          # 3D  = k
               
               registerDoParallel(cores=n.cores)
               
               # Initialize array
               out <- array(NA, dim=c(length(o), length(t), ceiling(max(d[,variable]))))
               
               # get counts for observed route distances
               tmp <- foreach(k = v, .combine=function(a, b) abind(a, b, along=3)) %:% 
                    foreach(i = o, .combine='rbind', .multicombine=TRUE) %:% 
                    foreach(j = t, .combine='c', .multicombine=TRUE) %dopar% {
                         
                         sum(d$from == i & d[,time] == j & d[,variable] <= k & d[,variable] > k - 1)
                    }
               
               # populate NA array with observed counts
               for (i in 1:length(o)) {
                    dists <- unique(d[d$from == o[i], variable])
                    for (j in dists) {
                         out[i,,ceiling(j)] <- tmp[i,,which(v == j)]
                    }
               }
               
               dimnames(out) <- list(origin=o, time=t, distance=1:ceiling(max(d[,variable])))
               
          } else if (variable == 'duration') {
               
               print("Variable is duration")
               
               n.gen <- floor(max(d[,variable])/gen.t)
               
               orig <- sort(unique(d$from))                # rows = i origins
               dest <- sort(unique(d$to))                  # cols = j destinations
               t <- sort(unique(d[,time]))                 # 3D = k months
               g <- 1:n.gen                                # 4D  = l generations
               
               registerDoParallel(cores=n.cores)
               
               out <- foreach(l = g, .combine=function(a, b) abind(a, b, along=4)) %:%
                    foreach(k = t, .combine=function(a, b) abind(a, b, along=3)) %:% 
                    foreach(i = orig, .combine='rbind', .multicombine=TRUE) %:% 
                    foreach(j = dest, .combine='c', .multicombine=TRUE) %dopar% {
                         
                         x <- sum(d[d$from == i 
                                    & d$to == j 
                                    & d[,time] == k  
                                    & d[,variable] > (l*gen.t - gen.t) 
                                    & d[,variable] <= l*gen.t, 
                                    'count'], 
                                  na.rm=TRUE)
                         
                         if (x == 0) x <- NA
                         x
                    }
               
               print("Finished foreach parallel part")
               
               dimnames(out) <- list(origin=orig, destination=dest, time=t, generation=1:n.gen)
          }
     }
     
     if (parallel == FALSE) {
          
          print("Beginning sequential jags.data.array")
          
          
          
          if (variable == 'distance') {
               
               orig <- sort(unique(d$from))
               t <- sort(unique(d[,time]))                
               v <- sort(unique(d[,variable])) 
               
               print("Variable is distance")
               
               # Intialize array
               out <- array(numeric(0), 
                            dim=c(max(orig), 
                                  max(t), 
                                  ceiling(max(d[,variable]))))
               
               print("Initialized out array")
               
               # populate NA array with observed counts
               for (i in 1:nrow(d)) {
                    
                    print(paste(i, "of", nrow(d), "---", round((i/nrow(d))*100), "%", sep= " "))
                    
                    sel <- out[d[i, 'from'], 
                               d[i, time], 
                               ceiling(d[i, variable])]
                    
                    if (is.na(sel)) {
                         
                         out[d[i, 'from'], 
                             d[i, time], 
                             ceiling(d[i, variable])] <- d[i, 'count']
                         
                    } else {
                         
                         out[d[i, 'from'], 
                             d[i, time], 
                             ceiling(d[i, variable])] <- sel + d[i, 'count']
                    }
               }
               
               dimnames(out) <- list(origin=1:max(orig), 
                                     time=1:max(t), 
                                     distance=1:ceiling(max(d[,variable])))
               
          } else if (variable == 'duration') {
               
               print("Variable is duration")
               
               n.gen <- floor(max(d[,variable])/gen.t)
               
               orig <- sort(unique(d$from))  
               dest <- sort(unique(d$to))
               t <- sort(unique(d[,time]))                
               g <- 1:n.gen
               
               # Intialize array
               out <- array(numeric(0), 
                            dim=c(max(orig), 
                                  max(dest), 
                                  max(t), 
                                  n.gen))
               
               print("Initialized out array")
               
               # populate NA array with observed counts
               for (i in 1:nrow(d)) {
                    
                    print(paste(i, "of", nrow(d), "---", round((i/nrow(d))*100), "%", sep= " "))
                    
                    if (d[i, variable] > gen.t*n.gen) (next)
                    
                    sel <- out[d[i, 'from'], 
                               d[i, 'to'], 
                               d[i, time],
                               ceiling(d[i,variable]/gen.t)]
                    
                    if (is.na(sel)) {
                         
                         out[d[i, 'from'], 
                             d[i, 'to'], 
                             d[i, time], 
                             ceiling(d[i, variable]/gen.t)] <- d[i, 'count']
                         
                    } else {
                         
                         out[d[i, 'from'], 
                             d[i, 'to'], 
                             d[i, time], 
                             ceiling(d[i, variable]/gen.t)] <- sel + d[i, 'count']
                    }
               }
               
               dimnames(out) <- list(origin=1:max(orig), destination=1:max(dest), time=1:max(t), generation=1:n.gen)
          }
     }
     
     print("Finished jags.data.array")
     return(out)
}

##' Reduce a jags.data.array object to route-level
##' 
##' This function reduces the output from the \code{\link{jags.data.array}} function---which contains data for the
##' month-level---to a route-level data array. The route-level array contains the mean value across all
##' months.
##' 
##' @param x Month-level Output from the \code{\link{jags.data.array}} function
##' @param variable The variable in the data object (expects either \code{'distance'} or \code{'duration'})
##' 
##' @return A 2-dimensional array when \code{variable = 'distance'}, and a 3-dimensional array when \code{variable = 'duration'}
##' 
##' @author John Giles
##'
##' @family model processing
##' 
##' @export
##' 

jags.data.array.route.level <- function(x,              # output from jags.data.array function
                                        variable        # 'distance' or 'duration'
){
     
     if (variable == 'distance') {
          
          x <- apply(distance.array.month.level, 
                     c(1,3), 
                     function(x) as.integer(round(mean(x, na.rm=TRUE))))
          
     } else if (variable == 'duration') {
          
          x <- apply(x, 
                     c(1,2,4), 
                     function(x) as.integer(round(mean(x, na.rm=TRUE))))
          
     }
     
     x[is.nan(x)] <- NA
     return(x)
}

##' Reduce a jags.data.array object to population-level
##' 
##' This function reduces the output from the \code{\link{jags.data.array}} function---which contains data for the
##' month-level---to a vector representing the variable across all routes and months. The population-level vector contains the mean value across all
##' observed values of the variable.
##' 
##' @param x Month-level output from the \code{\link{jags.data.array}} function
##' @param variable The variable in the data object (expects either \code{'distance'} or \code{'duration'})
##' 
##' @return vector
##' 
##' @author John Giles
##'
##' @family model processing
##' 
##' @export
##' 

jags.data.array.pop.level <- function(x,              # output from jags.data.array function
                                      variable        # 'distance' or 'duration'
){
     
     if (variable == 'distance') {
          
          x <- apply(distance.array.month.level, 
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

##' Proportion of individuals remaining for full epidemic generation
##'
##' This function calculates the proportion of individuals that remain a location for
##' all of epidemic generation \eqn{n}. The value is represented by the variable \eqn{p_jt}, which
##' is defined as the effective probability if individuals that stay for a full epidemic generation 
##' when they travel to destination \eqn{j} at time \eqn{t}:\cr
##' \cr
##' \eqn{p_ij} = Pr(remaining for all of \eqn{n^th} epidemic generation | generation time \eqn{g})
##' 
##' @param d 4-dimensional data array produced by the \code{\link{jags.data.array}} function \emph{or} the \code{duration.array.month.level} data object in the \pkg{hmobdata} package
##' @param gen.t the time interval in days used to define the epidemic generation
##' @param n.gen a scalar indicating the $n^th$ generation for which to calculate proportion of individuals remaining for the full epidemic generation
##' 
##' @return A 4-dimensional array with values between 0 and 1
##' 
##' @author John Giles
##'
##' @family simulation
##' 
##' @export
##' 

calc.p <- function(d,       # 4D data array produced by the jags.data.array function
                   gen.t,   # interval used to define the epidemic generation
                   n.gen=1  # a scalar indicating the maximum epidemic generation to calculate rho for
) {
     
     out <- array(NA, dim(d)[1:3])
     
     for (i in 1:dim(d)[1]) {
          for (j in 1:dim(d)[2]) {
               for (t in 1:dim(d)[3]) {
                    
                    if (i == j) (next)
                    x <- d[i,j,t,which(as.numeric(dimnames(d)$duration) > gen.t*(n.gen-1) & 
                                            as.numeric(dimnames(d)$duration) <= gen.t*n.gen)]
                    
                    out[i,j,t] <- (x[!is.na(x)] %*% (seq_along(x)[!is.na(x)]/length(x))) / (sum(x, na.rm=T))
                    
               }
          }
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
##' given the intercept \eqn{\beta} and decay rate \eqn{\lambda} parameters using the functional form:
##' \eqn{N(y) = \beta exp(-\lambda*y)}. If \eqn{y =} duration, the function calculates the number of trips counted for a given duration.
##' 
##' @param beta intercept (baseline number of expected trips at \eqn{y=0})
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

decay.func <- function(beta,       # intercept (baseline number of expected trips at \eqn{y=0})
                       lambda,     # decay rate parameter
                       y           # data value duration (integer representing days)
) {
     beta * exp(-lambda*y)
}