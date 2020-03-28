##' Generalized template for travel survey data
##' 
##' This is a template data frame providing a general structure for travel survey data that integrates with data synthesis and 
##' modeling functions.
##' 
##' @format a data frame with empty columns
##' \describe{
##'   \item{indiv_id}{integer: unique individual identifier}
##'   \item{indiv_age}{numeric: age of participant}
##'   \item{indiv_sex}{logical: gender of perticipant}
##'   \item{indiv_type}{factor: if individual participants belong to different groups}
##'   \item{orig_adm0}{character: name of highest administration level of origin location (country)}
##'   \item{orig_adm1}{character: name of administration level 1 of origin location}
##'   \item{orig_adm2}{character: name of administration level 2 of origin location}
##'   \item{orig_adm3}{character: name of administration level 3 of origin location}
##'   \item{orig_adm4}{character: name of administration level 4 of origin location}
##'   \item{orig_adm5}{character: name of administration level 5 of origin location}
##'   \item{orig_type}{character: administrative type for the origin location (e.g. sub-district, community vs town, or urban vs rural)}
##'   \item{orig_x}{numeric: longitude of origin location in decimal degrees}
##'   \item{orig_y}{numeric: latitude of origin location in decimal degrees}
##'   \item{orig_pop}{numeric: population size of lowest administrative unit for origin location}
##'   \item{dest_adm0}{character: name of highest administration level of destination location (country)}
##'   \item{dest_adm1}{character: name of administration level 1 of destination location}
##'   \item{dest_adm2}{character: name of administration level 2 of destination location}
##'   \item{dest_adm3}{character: name of administration level 3 of destination location}
##'   \item{dest_adm4}{character: name of administration level 4 of destination location}
##'   \item{dest_adm5}{character: name of administration level 5 of destination location}
##'   \item{dest_type}{character: administrative type for the destination location (e.g. sub-district, community vs town, or urban vs rural)}
##'   \item{dest_x}{numeric: longitude of destination location in decimal degrees}
##'   \item{dest_y}{numeric: latitude of destination location in decimal degrees}
##'   \item{dest_pop}{numeric: population size of lowest administrative unit for destination location}
##'   \item{trips}{numeric: total number of trips individual made from origin to destination during time span of travel survey}
##'   }
##'   
##' @author John Giles
##' 
"survey.template"