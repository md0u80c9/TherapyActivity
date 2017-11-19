#' @export
#' @title summarisedata
#'
#' @description
#' \code{summarisedata} creates a slideshow for an acute team.
#'
#' @details
#' This function takes an imported data table from SSNAP and creates a slideshow of performance
#' over the previous year to inform teams of their performance.
#'
#' @param teamdataframe A source tibble containing the aggregated SSNAP data by month for the team. You should 
#' use the functions in SSNAPInterface to obtain the raw data, and the functions in 
#' SSNAPStats to aggregate the data appropriately.
#' @param nationaldomaindataframe A source tibble containing the national domain results for all teams.
#' @param reportingPeriodStart the date the reporting period will start from
#' @param reportingPeriodEnd the date the reporting period will end
#' @param pathname The pathname for the resulting presentation file(s). The file will be called
#' Team_XXX_MonthlySlideshow_Jan00-Mar00 where XXX is the TeamCode, and Jan00 and Mar00 are the start and end
#' date of the current reporting period respectively.
#' @return The presentation is saved as the proposed filename

#' @author Andrew Hill, \email{andrew.hill@@doctors.org.uk}

summarystats <- function() {

# TEST CODE - used to test the function with sample data. Remove from any production code
  pttable <- readr::read_csv('../STHKPT.csv',
                             col_names = TRUE,
                             readr::cols(hospitalid = readr::col_character(),
                                         name =  readr::col_character(),
                                         ward = readr::col_factor(levels = c('5C', '5D')),
                                         date = readr::col_date(format = "%d/%m/%Y"),
                                         minutes = readr::col_integer(),
                                         unwell = readr::col_logical()
                                         ))
  ottable <- readr::read_csv('../STHKOT.csv',
                             col_names = TRUE,
                             readr::cols(hospitalid = readr::col_character(),
                                         name =  readr::col_character(),
                                         ward = readr::col_factor(levels = c('5C', '5D')),
                                         date = readr::col_date(format = "%d/%m/%Y"),
                                         minutes = readr::col_integer(),
                                         unwell = readr::col_logical()
                             ))
  
  groupedpt <- dplyr::group_by(pttable, hospitalid, name, ward)
  summarypt <- dplyr::summarise(groupedpt, mins = median(minutes), days = sum(minutes > 0), sickdays = sum((minutes == 0) & unwell))

  groupedot <- dplyr::group_by(pttable, hospitalid, name, ward)
  summaryot <- dplyr::summarise(groupedot, mins = median(minutes), days = sum(minutes > 0), sickdays = sum((minutes == 0) & unwell))

  return(summarypt)
}