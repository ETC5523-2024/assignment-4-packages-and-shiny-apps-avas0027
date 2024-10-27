#' Filter NYC Collision Data by Date Range and Borough
#'
#' @param data The NYC collisions dataset.
#' @param start_date The start date as "YYYY-MM-DD".
#' @param end_date The end date as "YYYY-MM-DD".
#' @param borough The borough to filter by (e.g., "BROOKLYN").
#' @return A filtered dataframe.
#' @export
filter_collisions <- function(data, start_date, end_date, borough) {
  data %>%
    filter(DATE >= as.Date(start_date) & DATE <= as.Date(end_date)) %>%
    filter(BOROUGH == borough)
}

# filtered_data <- filter_collisions(nyc_data, "2015-01-01", "2015-12-31", "MANHATTAN")

#' Summarize Collision Counts by Vehicle Type
#'
#' @param data The NYC collisions dataset.
#' @param borough The borough to filter by (e.g., "BROOKLYN").
#' @param start_date The start date as "YYYY-MM-DD".
#' @param end_date The end date as "YYYY-MM-DD".
#' @return A summary dataframe with vehicle types and collision counts.
#' @export
summarize_by_vehicle_type <- function(data, borough, start_date, end_date) {
  data %>%
    filter(DATE >= as.Date(start_date) & DATE <= as.Date(end_date)) %>%
    filter(BOROUGH == borough) %>%
    count(`VEHICLE 1 TYPE`, `VEHICLE 2 TYPE`, sort = TRUE, name = "collision_count") %>%
    arrange(desc(collision_count))
}

# summary_data <- summarize_by_vehicle_type(nyc_data, "QUEENS", "2015-01-01", "2015-12-31")

#' Calculate Monthly Collision Trends
#'
#' @param data The NYC collisions dataset.
#' @param start_date The start date as "YYYY-MM-DD".
#' @param end_date The end date as "YYYY-MM-DD".
#' @return A dataframe with monthly collision counts.
#' @export
monthly_collision_trends <- function(data, start_date, end_date) {
  data %>%
    filter(DATE >= as.Date(start_date) & DATE <= as.Date(end_date)) %>%
    mutate(month = format(DATE, "%Y-%m")) %>%
    group_by(month) %>%
    summarize(total_collisions = n()) %>%
    arrange(month)
}

# monthly_trends <- monthly_collision_trends(nyc_data, "2015-01-01", "2015-12-31")

#' Get Top Contributing Factors for Collisions
#'
#' @param data The NYC collisions dataset.
#' @param start_date The start date as "YYYY-MM-DD".
#' @param end_date The end date as "YYYY-MM-DD".
#' @param top_n Number of top contributing factors to return.
#' @return A dataframe with contributing factors and their counts.
#' @export
top_contributing_factors <- function(data, start_date, end_date, top_n = 5) {
  data %>%
    filter(DATE >= as.Date(start_date) & DATE <= as.Date(end_date)) %>%
    count(`VEHICLE 1 FACTOR`, sort = TRUE, name = "factor_count") %>%
    top_n(top_n, factor_count)
}

# top_factors <- top_contributing_factors(nyc_data, "2015-01-01", "2015-12-31", top_n = 5)




