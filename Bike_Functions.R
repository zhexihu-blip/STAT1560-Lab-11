library(dplyr)

#' Estimate hourly bike arrival rates
#'
#' It first reconstructs station inventories from data and then estimate
#' hourly arrival rates for each possible trip.
#'
#' @param data A data frame containing bike trip information,
#' including start_station, end_station, start_time, and end_time.
#' @param window_start: a POSIXct date-time; 
#' the beginning of the observation window.
#' @param window_end: a POSIXct date-time; the end of the observation window.
#'
#' @return A data frame containing origin, destination, hour,
#' observed_rate, availability, and adjusted arrival rate.
#'
bike<-read_csv("sample_bike.csv")

estimate_arrival_rates <- function(data, window_start, window_end) {

#The recreation of inventory_history from week 10 lab
data<-data %>%
  select(start_station, end_station, start_time, end_time)

data$start_time<-as.POSIXct(data$start_time)
data$end_time<-as.POSIXct(data$end_time)
data$start_station<-as.character(data$start_station)
data$end_station<-as.character(data$end_station)
data$start_date<-as.Date(substr(data$start_time, 1, 10))
data$end_date<-as.Date(substr(data$end_time, 1, 10))


rebalance_add<-data %>%
  filter(start_station=="R") %>%
  mutate(
    station=end_station,
    time=end_time,
    inventory_change=1,
    event_type="R",
    priority=1
  ) %>%
  select(station, time, inventory_change, event_type, priority)

customer_departure<-data %>%
  filter(start_station!="R", end_station!="R") %>%
  mutate(
    station=start_station,
    time=start_time,
    inventory_change=-1,
    event_type="C",
    priority=2
  ) %>%
  select(station, time, inventory_change, event_type, priority)


customer_arrival<-data %>%
  filter(start_station!="R", end_station!="R") %>%
  mutate(
    station=end_station,
    time=end_time,
    inventory_change=1,
    event_type="C",
    priority=3
  ) %>%
  select(station, time, inventory_change, event_type, priority)


rebalance_remove<-data %>%
  filter(end_station=="R") %>%
  mutate(
    station=start_station,
    time=start_time,
    inventory_change=-1,
    event_type="R",
    priority=4
  ) %>%
  select(station, time, inventory_change, event_type, priority)


events<-bind_rows(
  rebalance_add,
  customer_departure,
  customer_arrival,
  rebalance_remove
) %>%
  arrange(time, priority) %>%
  select(station, time, inventory_change, event_type)

stations<-unique(events$station)
inventory<-setNames(rep(0, length(stations)), stations)
events$inventory<-NA_integer_

for (i in 1:nrow(events)){
  station<-events$station[i]
  inventory[station]<-inventory[station]+
    events$inventory_change[i]
  events$inventory[i]<-inventory[station]
}

inventory_history<-events
###########################


#Find inventory of that station right at the window_start
window_start <- as.POSIXct(window_start)
window_end <- as.POSIXct(window_end)

initial_inventory <- inventory_history %>%
  filter(time <= window_start) %>%
  group_by(station) %>%
  slice_max(time, n = 1, with_ties = FALSE) %>%
  select(station, inventory)

initial_inventory <- setNames(
  initial_inventory$inventory,
  initial_inventory$station
)
##########################

customer_trips <- data %>%
  filter(
    !is.na(start_station),
    !is.na(end_station),
    !is.na(start_time),
    start_station != "R",
    end_station != "R",
    start_time >= window_start,
    start_time < window_end
  )
customer_trips$hour <- as.integer(format(customer_trips$start_time, "%H"))

customer_trips <- customer_trips %>%
  group_by(start_station, end_station, hour) %>%
  summarise(n = n(), .groups = "drop")

names(customer_trips)[1:2] <- c("origin", "destination")

days <- as.numeric(
  difftime(window_end, window_start, units = "days")
)

customer_trips$observed_rate <- customer_trips$n / days

possible_trips <- unique(customer_trips[, c("origin", "destination")])

possible_trips_at_each_hour <- merge(
  possible_trips,
  data.frame(hour = 0:23)
)

result <- left_join(
  possible_trips_at_each_hour,
  customer_trips[, c("origin", "destination", "hour",
                 "observed_rate")],
  by = c("origin", "destination", "hour")
)
result$observed_rate[is.na(result$observed_rate)] <- 0

stations <- unique(result$origin)

availability <- data.frame()

for (i in stations) {
  
  station_activities <- events[events$station == i, ]
  
  for (h in 0:23) {
    
    activities_at_hour <- station_activities[
      as.integer(format(station_activities$time, "%H")) == h, ]
    
    if (nrow(activities_at_hour) > 0){
    avg_availability <- mean(activities_at_hour$inventory)
    } else {
    avg_availability<- NA_real_
    }
    
    availability <- rbind(
      availability,
      data.frame(
        origin = i,
        hour = h,
        availability = avg_availability
      )
    )
  }
}
result <- left_join(
  result,
  availability,
  by = c("origin", "hour")
)
result$rate <- NA_real_

available <- !is.na(result$availability) &
  result$availability > 0

result$rate[available] <-
  result$observed_rate[available] /
  result$availability[available]

result <- result[, c(
  "origin", "destination", "hour",
  "observed_rate", "availability", "rate"
)]

result <- arrange(
  result,
  origin,
  destination,
  hour,
  
)

return(result)
}

estimate_arrival_rates(bike,"2022-02-16 00:00:00", "2022-02-22 00:00:00")

test_data <- data.frame(
  start_station = c("R","4"),
  end_station = c("4","10"),
  start_time = c("2022-02-27 10:10:00","2022-02-28 10:30:00"),
  end_time = c("2022-02-28 10:30:00","2022-02-28 11:00:00")
)

estimate_arrival_rates(
  test_data,
  "2022-02-27 00:00:00",
  "2022-03-01 00:00:00"
)





#' Simulate customer arrivals using hourly arrival rates
#'
#' @param rates A data frame containing the origin, destination, hour, 
#' and rate columns.
#' @param start_time: a POSIXct date-time; the beginning of the simulation window.
#'
#' @param end_time: a POSIXct date-time; the end of the simulation window.
#'
#'
#' @return A data frame containing three columns:
#'   origin, destination, and arrival_time.
#'   Each row represents one simulated customer request.
#'
#' @examples
#' simulate_arrivals(rates, start_time, end_time)
#'
simulate_arrivals <- function(rates, start_time, end_time) {
  
  # Convert the simulation times to POSIXct
  start_time <- as.POSIXct(start_time)
  end_time <- as.POSIXct(end_time)
  
  # Create an empty arrival table
  arrivals <- data.frame(
    origin = character(),
    destination = character(),
    arrival_time = as.POSIXct(character())
  )
  
  pairs <- unique(rates[, c("origin", "destination")])
  
  # Loop through each origin-destination pair
  for (i in 1:(nrow(pairs))) {
    
    origin <- pairs$origin[i]
    destination <- pairs$destination[i]
    
    # Get the hourly rates for this pair
    pair_rates <- rates[
      rates$origin == origin &
      rates$destination == destination, ]
    
    # Replace NA rates with zero
    pair_rates$rate[is.na(pair_rates$rate)] <- 0
    
    # Find the maximum hourly rate
    M <- max(pair_rates$rate)
    
    if (M == 0) {
      next
    }

    candidate_times <- as.POSIXct(character())
    candidate_time <- start_time
    
    repeat {
      waiting_time <- rexp(1, rate = M)
      candidate_time <- candidate_time + waiting_time * 3600
      if (candidate_time >= end_time) {
        break
      }
      candidate_times <- c(candidate_times, candidate_time)
    }
    
    for (j in seq_along(candidate_times)) {
      h <- as.integer(format(candidate_times[j], "%H"))
      hourly_rate <- pair_rates$rate[
        which(pair_rates$hour == h)
      ]

      acceptance_probability <- hourly_rate / M
      
      if (runif(1) < acceptance_probability) {
        arrivals <- rbind(
          arrivals,
          data.frame(
            origin = as.character(origin),
            destination = as.character(destination),
            arrival_time = candidate_times[j]
          )
        )
      }
    }
  }
  
  arrivals <- arrivals[
    order(arrivals$origin), ]
  
  return(arrivals)
}
simulate_arrivals(estimate_arrival_rates(bike, 
                                         "2022-02-16 00:00:00", 
                                         "2022-02-22 00:00:00"), 
                  "2022-02-16 00:00:00", 
                  "2022-02-22 00:00:00")

