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

olddailytherapyheatmap <- function() {

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
  
# Cap the minutes at 45 for all the graphical work, and remove days when the patient was unwell.
  pttable$minutes <- ifelse(pttable$minutes > 45,
                            45,
                            pttable$minutes)
  pttable$minutes <- ifelse(pttable$unwell,
                            NA,
                            pttable$minutes)

  ottable$minutes <- ifelse(ottable$minutes > 45,
                            45,
                            pttable$minutes)
  ottable$minutes <- ifelse(ottable$unwell,
                            NA,
                            pttable$minutes)
  
  ptptson5D = dplyr::select(pttable, hospitalid, ward, date)
  ptptson5D = dplyr::filter(ptptson5D, ward == '5D')
  ptptson5D = dplyr::group_by(ptptson5D, hospitalid)
  ptptson5D <- dplyr::slice(ptptson5D, which.min(date))
  ptptson5D <- dplyr::select(ptptson5D, hospitalid, firstdayon5D = date)
  ptrehabdays <- dplyr::inner_join(pttable, ptptson5D)
  ptrehabdays <- dplyr::rename(ptrehabdays,
                               ptminutes = minutes)
  ptrehabdays <- dplyr::mutate(ptrehabdays,
                               scaledptminutes = scales::rescale(ptminutes))
  
  otptson5D = dplyr::select(ottable, hospitalid, ward, date)
  otptson5D = dplyr::filter(otptson5D, ward == '5D')
  otptson5D = dplyr::group_by(otptson5D, hospitalid)
  otptson5D <- dplyr::slice(otptson5D, which.min(date))
  otptson5D <- dplyr::select(otptson5D, hospitalid, firstdayon5D = date)
  otrehabdays <- dplyr::inner_join(ottable, ptptson5D)
  otrehabdays <- dplyr::rename(otrehabdays,
                               otminutes = minutes)
  otrehabdays <- dplyr::mutate(otrehabdays,
                               scaledotminutes = scales::rescale(otminutes))
  

  therapyrehabdays <- dplyr::inner_join(otrehabdays, ptrehabdays)
  # TODO: If the patient hasn't spent any days on 5D, then we need to set the clock as the last day on 5C + 1 day.
  therapyrehabdays <- dplyr::mutate(therapyrehabdays,
                               daynumber = as.integer(difftime(date, firstdayon5D, units = 'days')))

  therapyrehabdays <- dplyr::select(therapyrehabdays, -ward, -date, -firstdayon5D, -unwell)

  p <- ggplot2::ggplot(therapyrehabdays,
                       ggplot2::aes(daynumber,
                                    hospitalid)) +
       ggplot2::theme_classic() +
       ggplot2::geom_tile(ggplot2::aes(fill = scaledptminutes),
                          data = therapyrehabdays,
                          colour = "white") +
       ggplot2::scale_fill_gradient2(low = "#deebf7",
                                     mid = "#9ecae1",
                                     high = "#3182bd",
                                     breaks = c(min(therapyrehabdays$ptminutes), max(therapyrehabdays$ptminutes)),
                                     labels = c("0", "45 minutes"),
                                     space = 'Lab',
                                     na.value = 'white',
                                     guide = 'colourbar') +
    
#      ggplot2::geom_tile(ggplot2::aes(fill = scaledotminutes),
#                       data = therapyrehabdays,
#                       colour = "white") +
    
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