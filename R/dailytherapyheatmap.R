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

dailytherapyheatmap <- function() {

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
       ggplot2::geom_tile(ggplot2::aes(fill = scaledptminutes),
                          data = therapyrehabdays,
                          colour = "white") +
       ggplot2::scale_fill_gradient(low = "#deebf7",
                                    high = "#3182bd",
                                    na.value = NA) +
       ggplot2::scale_x_continuous(breaks = NULL, minor_breaks = NULL) +
       ggplot2::xlab('Days of admission (the vertical line represents the day of transfer from HASU to ASU)') +
#         minor_breaks = seq(-100, + 100, 200)) +
#       ggplot2::scale_x_discrete("", expand = c(0,0)) +
       ggplot2::scale_y_discrete("", expand = c(0,0)) +
    ggplot2::ggtitle('Daily therapy activity broken down by patient') +
    ggplot2::theme(legend.position = 'right',
                   panel.background = ggplot2::element_rect(fill = NA),
                   panel.grid.major = ggplot2::element_line(colour = "black")) +
#                   axis.ticks = ggplot2::element_blank(),
#                   axis.text.x = ggplot2::element_text(angle = 330, hjust = 0)) +
    
       ggplot2::geom_vline(xintercept = 0,
                           colour = 'black')

  print(p)
  
  return(summarypt)
}