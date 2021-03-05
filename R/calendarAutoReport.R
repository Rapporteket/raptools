#' Make calendar of report schedule and daily load
#'
#' @param runDayOfYear Numeric vector of yeardays for which reports are to be
#' run. May well contain repeating values
#' @param pointRangeMax Integer to provide static range of geom_point as
#' [1 pointRangeMax]. If set to 0 (default) the range of current data will be
#' used.
#'
#' @return a (gg)plot object
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' monthlyReport <- seq(from = 1, to = 365, by = 30)
#' weeklyReport <- seq(from = 1, to = 365, by = 7)
#' summerWeeklyReport <- seq(from = 130, to = 250, by = 7)
#' specialReport <- c(summerWeeklyReport, 150:170)
#' rdoy <- c(monthlyReport, weeklyReport, summerWeeklyReport, specialReport)
#' calendarAutoReport(rdoy)

calendarAutoReport <- function(runDayOfYear, pointRangeMax = 0) {
  stopifnot(is.numeric(unlist(runDayOfYear)) | is.null(runDayOfYear))
  stopifnot(is.integer(pointRangeMax) | pointRangeMax == 0)

  startDate <- Sys.Date() - as.numeric(strftime(Sys.Date(), format = "%d")) + 1
  b <- data.frame(datetime = seq(startDate, by = "day", length.out = 365))
  b$year <- as.POSIXlt(b$datetime)$year + 1900
  b$dayOfYear <- as.POSIXlt(b$datetime)$yday + 1
  b$yearMonthName <- strftime(b$datetime, format = "%B %Y")
  b$dayNum <- as.numeric(strftime(b$datetime, format = "%u"))
  b$dayName <- strftime(b$datetime, format = "%a")
  b$weekNum <- as.numeric(strftime(b$datetime, format = "%W"))
  b$monthNum <- as.numeric(strftime(b$datetime, format = "%m"))
  b$monthName <- strftime(b$datetime, format = "%b")
  b$monthDayNum <- strftime(b$datetime, format = "%d")
  b <- b %>%
    dplyr::group_by(.data$yearMonthName) %>%
    dplyr::mutate(weekOfMonth = 1 + .data$weekNum - min(.data$weekNum))
  # make numeric id for yearMontName
  b$ymnId <- as.POSIXlt(b$datetime)$mon

  # make levels of montyear
  # the first of current month
  present <- as.Date(strftime(Sys.Date(), format = "%Y-%m-01"))
  b$yearMonthName <- factor(
    b$yearMonthName,
    levels = strftime(seq(present, by = "month", length.out = 12),
                      format = "%B %Y")
  )

  # make levels to get in right day name order
  aMonday <- as.Date("2018-09-03")
  followingSunday <- as.Date("2018-09-09")
  weekDaySeq <- strftime(seq(aMonday, followingSunday, by = "day"),
                         format = "%a")
  b$dayName <- factor(b$dayName, levels = weekDaySeq)

  # add autoReports dayly count
  # not needed ? b$autoReportCount <- rep(0, dim(b)[1])


  # plot object
  g <- ggplot2::ggplot(
    data = b, ggplot2::aes(x = .data$weekOfMonth, y = .data$dayName,
                           fill = .data$ymnId)) +
    ggplot2::geom_tile(colour = "white", size = .1, alpha = 0.3) +
    ggplot2::facet_wrap(~yearMonthName, scales = "fixed", nrow = 4) +
    ggplot2::geom_text(
      data = b,
      ggplot2::aes(.data$weekOfMonth, .data$dayName,
                   label = .data$monthDayNum),
      colour = "white", size = 2.5, nudge_x = .35, nudge_y = -.2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank()) +
    ggplot2::guides(fill = FALSE) +
    ggplot2::labs(x = "", y = "")

  if (!is.null(runDayOfYear)) {
  # prepare data
  df <- data.frame(dayOfYear = runDayOfYear)

  # count reports for each day
  autoReportCount <- dplyr::count(df, .data$dayOfYear)
  b <- b %>%
    dplyr::left_join(autoReportCount, by = "dayOfYear")
  g <- g + ggplot2::geom_point(data = b %>% dplyr::filter(!is.na(.data$n)),
                               ggplot2::aes(x = .data$weekOfMonth,
                                            y = .data$dayName, size = .data$n),
                               colour = "#FF7260")
  }

  if (pointRangeMax > 0) {
    g <- g + ggplot2::scale_size_continuous(limits = c(1, pointRangeMax))
  }

  return(g)
}
