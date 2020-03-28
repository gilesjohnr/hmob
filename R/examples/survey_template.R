### Travel within the same district ###
trip <- survey.template
n <- 10 # Add some observations
trip[1:n,] <- NA

# Participant info
trip$indiv_id <- 1:n
trip$indiv_age <- round(runif(n, 5, 80))
trip$indiv_sex <- rbinom(n, 1, 0.5)

# Origin info
trip$orig_adm0 <- 'Bangladesh'
trip$orig_adm1 <- 'Dhaka'
trip$orig_adm2 <- 'Dhaka'
trip$orig_adm3 <- 'Manda'
trip$orig_type <- 'Upazila' # Type of admin unit for lowest admin level
trip$orig_x <- rnorm(n, 100, 5)
trip$orig_y <- rnorm(n, 20, 2)
trip$orig_pop <- rpois(n, 10000)

# Destination info
trip$dest_adm0 <- 'Bangladesh'
trip$dest_adm1 <- 'Dhaka'
trip$dest_adm2 <- 'Dhaka'
trip$dest_adm3 <- 'Adabor'
trip$dest_type <- 'Upazila' # Type of admin unit for lowest admin level
trip$dest_x <- rnorm(n, 100, 5)
trip$dest_y <- rnorm(n, 20, 2)
trip$dest_pop <- rpois(n, 5000)

# Number of reported trips
trip$trips <- rpois(n, 10)

head(trip)




### Stays in home location ###
stay <- survey.template
n <- 6 # add some observations
stay[1:n,] <- NA

# Participant info
stay$indiv_id <- 1:n
stay$indiv_age <- round(runif(n, 5, 80))
stay$indiv_sex <- rbinom(n, 1, 0.5)

# Origin info
stay$orig_adm0 <- 'Bangladesh'
stay$orig_adm1 <- 'Dhaka'
stay$orig_adm2 <- 'Dhaka'
stay$orig_adm3 <- 'Manda'
stay$orig_type <- 'Upazila' # Type of admin unit for lowest admin level
stay$orig_x <- rnorm(n, 100, 5)
stay$orig_y <- rnorm(n, 20, 2)
stay$orig_pop <- rpois(n, 10000)

head(stay)
