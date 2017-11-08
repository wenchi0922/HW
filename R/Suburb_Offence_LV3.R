## QUESTION ----
#'SUBURB_OFFENCE_LV3_WENCHIT
#'
#' \code{adl_crime_suburb3} the function takes in Adelaide Crime Data from two suburbs and counts the total offences
#' @param crime_data A data.table object with the following columns:
#'     "date" (POSIXct), "suburb" (chr), "postcode" (chr), "offence_level_1" (chr),
#'     "offence_level_2" (chr), "offence_level_3" (chr), "offence_count" (num).
#' @param offence_description A character string of <What are your expected inputs?>.
#' @param suburbs A two-element character vector. Each element is the name (UPPERCASE)
#'     of an SA suburb.
#' @export
#' @return  A ggplot object showing the correlation in offence count between the two input suburbs.
#' @examples
#' adl_crime_suburb3 <- c("../data/crime-statistics-2012-13.xlsx", "OFFENCES AGAINST THE PERSON" ,suburbs <- c("ADELAIDE", "GOODWOOD")
#'

require(readxl)
require(dplyr)
require(data.table)

adl_crime_suburb3 <- function(crime_data, offence_description, suburbs) {
  require(data.table)
  require(ggplot2)

  # Error catching
  if (length(suburbs) != 2){
    stop("Please enter two suburbs")
  }

  expected_colnames <- c("date", "suburb", "postcode", "offence_level_1", "offence_level_2",
                         "offence_level_3", "offence_count")
  real_name <- names(crime_data)

  #check if the name for the inut table meatches the expected column names
  # I changed the syntac a little to isTRUE on OSX not sure why !all.equal wont work here
  if (isTRUE(all.equal(expected_colnames, real_name))) {
    stop(paste("Input table columns need to match: ",
               paste(expected_colnames, collapse = ", ")))
  }

  # Check that the input suburbs and offence description exist in crime_data
    stop("Please enter valid data")
  }

  # Make a data table for plotting using data.table transformations
  # You will need to filter, summarise and group by
  # Expect cols: "date", "suburb", "total_offence_count"
  plot_data <- crime_data[ suburb %in% c(suburbs[1], suburbs[2]) & offence_level_3 %in% offence_description,
                           list(total_offence_count = sum(offence_count),suburb),
                           by = date]

  # These lines will transform the plot_data structure to allow us to plot
  # correlations. Try them out
  plot_data[, suburb := plyr::mapvalues(suburb, suburbs, c("x", "y"))]

  plot_data <- dcast(plot_data, date ~ suburb, fun = sum,
                     fill = 0, value.var = "total_offence_count")

  # Generate the plot
  ggplot(plot_data, aes(x, y, group=month(date), color=type)) +
    geom_count() +
    labs(x = suburbs,
         y = "Total Offence Count")
}

adl_crime_suburb3("../data/crime-statistics-2012-13.xlsx", "OFFENCES AGAINST THE PERSON" ,suburbs <- c("ADELAIDE", "GOODWOOD"))

