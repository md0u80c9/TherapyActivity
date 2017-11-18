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

summarisedata <- function() {

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
  
  groupedpt <- dplyr::group_by(pttable, hospitalid, name, ward)
  summarypt <- dplyr::summarise(groupedpt, mins = median(minutes), days = sum(minutes > 0), sickdays = sum((minutes == 0) & unwell))
  
  ptgroupeddate <- dplyr::group_by(pttable, date)
  ptsummarydate <- dplyr::summarise(ptgroupeddate, minsprovided = sum(minutes), need = n() * 45, pcprovided = sum(minutes) / (n() * 45) * 100)
  
  calendarHeat(ptsummarydate$date, ptsummarydate$pcprovided, varname="Percent of physio need met")
  
  ptptson5D = dplyr::select(pttable, hospitalid, ward, date)
  ptptson5D = dplyr::filter(ptptson5D, ward == '5D')
  ptptson5D = dplyr::group_by(ptptson5D, hospitalid)
  ptptson5D <- dplyr::slice(ptptson5D, which.min(date))
  ptptson5D <- dplyr::select(ptptson5D, hospitalid, firstdayon5D = date)
  ptrehabdays <- dplyr::inner_join(pttable, ptptson5D)
  # TODO: If the patient hasn't spent any days on 5D, then we need to set the clock as the last day on 5C + 1 day.
  ptrehabdays <- dplyr::mutate(ptrehabdays,
                               daynumber = as.integer(difftime(date, firstdayon5D, units = 'days')))
#  ptrehabdays <- dplyr::mutate(ptrehabdays, minutes = scales::rescale(minutes))
  ptrehabdays$minutes <- ifelse(ptrehabdays$unwell,
                                NA,
                                ptrehabdays$minutes)
  
  ptrehabdays <- dplyr::select(ptrehabdays, -ward, -date, -firstdayon5D, -unwell)

  p <- ggplot2::ggplot(ptrehabdays,
                       ggplot2::aes(daynumber,
                                    hospitalid)) +
       ggplot2::theme_classic() +
       ggplot2::geom_tile(ggplot2::aes(fill = minutes),
                          colour = "white") +
       ggplot2::scale_fill_gradientn(colours = c("#deebf7", "#9ecae1", "#3182bd"),
                                     limits = c(0, 45),
                                     space = 'Lab',
                                     na.value = 'white') +
#       ggplot2::scale_fill_gradientn(low = "red",
#                                     mid = "yellow",
#                                     high = "green",
#                                     space = 'Lab',
#                                     na.value = 'grey80',
#                                     midpoint = 0.5) +
       ggplot2::geom_vline(xintercept = 0, colour = 'black')

  print(p)
  
  return(summarypt)
}