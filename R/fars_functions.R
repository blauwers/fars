#' Reads a CSV file with FARS data.
#'
#' Read Fatality Analysis Reporting System (FARS) CSV file.
#'
#' @param filename Path to FARS CSV file
#' @return A tbl_df of the data in the CSV file for which the path was provided. If the path was invalid, execution
#'    will stop
#' @examples
#' \dontrun{
#' accident_2013 <- fars_read("data/accident_2013.csv.bz2")
#'
#' # stops executing
#' accident_fail <- fars_read("data/this.file.does.not.exist.csv.bz2")
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
        filename <- system.file("extdata", filename, package = "fars")
        if (!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Create a filename
#'
#' Create a filename for a Fatality Analysis Reporting System (FARS) CSV file
#'
#' @param year number indicating the year when the dataset was captured
#' @return Returns a character vector containing the name of the FARS CSV file in the format "accident_<year>.csv.bz2".
#'     Returns "accident_NA.csv.bz2" and a warning if the year argument is a string.
#' @examples
#' filename <- make_filename(2017)
#' bad_filename <- make_filename("a")
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Create a filename format
#'
#' Create a filename for a Fatality Analysis Reporting System (FARS) CSV file
#'
#' @param years A vector of years for which to read the FARS data. See \code{\link{make_filename}}
#' @return Returns a list of tbl_df containing the MONTH column from the FARS data and an additional column for the
#'     year. Returns NULL and a warning for every FARS data file that does not exist.
#' @examples
#' \dontrun{
#' fars_2013_2016 <- fars_read_years(2013:2016)
#'
#' # Returns NULL and a warning
#' fars_2022 <- fars_read_years(2022)
#' }
#' @importFrom dplyr mutate_ select_ '%>%'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, "year" = year) %>%
                                dplyr::select_("MONTH", "year")
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Summarize FARS incidents by year
#'
#' Summarizes the number of incidents in a year by month
#'
#' @param years A vector of years for which to process the FARS data.
#' @return A tibble with rows for MONTH and each year with the total number of accidents. Returns a warning for every
#'     year for which no data exists.
#' @examples
#' \dontrun{
#' fars_summarize_years(2013:2014)
#' }
#' @importFrom dplyr bind_rows group_by_ summarize_ n '%>%'
#' @importFrom tidyr spread_
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_("year", "MONTH") %>%
                dplyr::summarize("n" = n()) %>%
                tidyr::spread("year", "n")
}

#' Generate a state map with FARS data
#'
#' Plots accidents for a given year on a map of the selected state
#'
#' @param state.num State number as used in the FARS dataset.
#' @param year Year for which to plot the accidents.
#' @return Returns NULL. Prints a warning message if there are no accidents to plot (but not really since this can
#'     never happen). Generates an error if the state number is not valid.
#' @examples
#' \dontrun{
#' library(map)
#' fars_map_state(12, 2013)
#' fars_map_state(45, 2014)
#'
#' # generates an error
#' fars_map_state(3, 2013)
#' }
#' @importFrom dplyr filter_
#' @importFrom maps map
#' @importFrom graphics points
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
