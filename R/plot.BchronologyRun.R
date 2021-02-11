#' Plot output from Bchronology
#'
#' Plots output from a run of \code{\link{Bchronology}}
#'
#' @param x The object created by \code{\link{Bchronology}}
#' @param dateHeight The height of the dates in the plot (on the same scale as the positions)
#' @param chronCol The colour of the chronology uncertainty ribbon to be plotted
#' @param alpha The credible interval of the chronology run to be plotted. Defaults to 95 percent
#' @param expandX The amount to expand the horizontal axis in case part are missed off the plot. See \code{\link[ggplot2]{expand_limits}} for details
#' @param expandY The amount to expand the vertical axis in case part are missed off the plot. See \code{\link[ggplot2]{expand_limits}} for details
#' @param nudgeX The amount to move the date labels in the x direction. Can be negative. See \code{\link[ggplot2]{geom_text}} for details
#' @param nudgeY The amount to move the date labels in the y direction. Can be negative. See \code{\link[ggplot2]{geom_text}} for details
#' @param dateLabels Whether to label the dates on the vertical axis (default TRUE)
#' @param dateCol The colour of the date labels
#' @param dateLabelSize The size of the date labels
#' @param chronTransparency The amount of transparency for the chronology ribbon
#' @param ageScale Either \code{bp} for years before present, \code{bc} for years BC/AD (BC will be negative), \code{b2k} for years before 2000. Others not supported (yet).
#' @param scaleReverse Whether to reverse the x-axis scale. Defaults to TRUE which works best for dates presented in e.g. years BP
#' @param ... Other arguments to plot (currently ignored)
#'
#' @details Creates a simple plot of the chronology output. The height of the date densities in the plots can be manipulated via the \code{dateHeight} argument which is represented in the same units as the positions/depths provided. More detailed plots can be created by manipulating the Bchronology object as required.
#'
#' @seealso For examples see \code{\link{Bchronology}}. Also \code{\link{BchronCalibrate}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#'
#' @import ggplot2
#' @importFrom ggridges geom_ridgeline
#' @importFrom scales pretty_breaks
#' @importFrom stringr str_pad
#' @importFrom grDevices rgb
#'
#' @export
plot.BchronologyRun <-
  function(x,
           dateHeight = 100,
           dateLabels = TRUE,
           dateLabelSize = 2,
           dateCol = rgb(47 / 255, 79 / 255, 79 / 255, 0.5),
           chronCol = "deepskyblue4",
           chronTransparency = 0.75,
           alpha = 0.95,
           nudgeX = 0,
           nudgeY = 0,
           expandX = if (dateLabels) {
             c(0.1, 0)
           } else {
             c(0, 0)
           },
           expandY = c(0.05, 0),
           ageScale = c("bp", "bc", "b2k"),
           scaleReverse = TRUE,
           ...) {

    # x contains the output from a run of the Bchronology function

    # Scale function for age scales
    ageScale <- match.arg(ageScale, several.ok = FALSE)
    ageScaleFun <- function(z) {
      switch(ageScale,
        bp = z,
        bc = 1950 - z,
        b2k = z + 50
      )
    }

    # Get chronology ranges
    chronRange <- data.frame(
      chronLow = ageScaleFun(apply(x$thetaPredict, 2, "quantile", probs = (1 - alpha) / 2)),
      chronMed = ageScaleFun(apply(x$thetaPredict, 2, "quantile", probs = 0.5)),
      chronHigh = ageScaleFun(apply(x$thetaPredict, 2, "quantile", probs = 1 - (1 - alpha) / 2)),
      positions = x$predictPositions
    )

    # Swap round so we can use geom_ribbon
    ageGrid <- with(chronRange, seq(min(chronLow), max(chronHigh),
      length = nrow(chronRange)
    ))
    chronRangeSwap <- data.frame(
      Age = ageGrid,
      positionLow = with(chronRange, approx(chronLow, positions,
        xout = ageGrid,
        rule = 2
      )$y),
      Position = with(chronRange, approx(chronMed, positions,
        xout = ageGrid,
        rule = 2
      )$y),
      positionHigh = with(chronRange, approx(chronHigh, positions,
        xout = ageGrid,
        rule = 2
      )$y),
      Date = "Bchron",
      densities = NA,
      height = NA
    )

    # Start extracting ages for plots
    allAges <- map_dfr(x$calAges,
      `[`, c("ageGrid", "densities"),
      .id = c("Date")
    ) %>%
      rename(Age = ageGrid) %>%
      mutate(Age = ageScaleFun(.data$Age))
    # scale all the densities to have max value 1
    scaleMax <- function(x) {
      return(x / max(x))
    }
    allAges2 <- allAges %>%
      group_by(.data$Date) %>%
      mutate(densities = scaleMax(.data$densities)) %>%
      filter(.data$densities > 0.01) %>%
      ungroup()
    positionLookUp <- tibble(
      Date = names(x$calAges),
      Position = map_dbl(x$calAges, "positions")
    )
    allAges3 <- left_join(allAges2, positionLookUp, by = "Date") %>%
      mutate(height = .data$densities * dateHeight)
    my_breaks <- pretty(x = ageGrid, n = 10)
    p <- allAges3 %>%
      ggplot(aes_string(
        x = "Age",
        y = "Position",
        height = "height",
        group = "Date"
      )) +
      ggridges::geom_ridgeline(fill = dateCol, colour = dateCol) +
      scale_y_reverse(
        breaks = scales::breaks_pretty(n = 10),
        expand = expandY
      ) +
      theme_bw() +
      scale_x_continuous(
        breaks = my_breaks,
        expand = expandX,
        labels = abs(my_breaks),
        trans = ifelse(scaleReverse, "reverse", "identity")
      ) +
      geom_ribbon(
        data = chronRangeSwap,
        aes_string(
          x = "Age",
          ymin = "positionLow",
          ymax = "positionHigh"
        ),
        colour = chronCol,
        fill = chronCol,
        alpha = chronTransparency
      ) +
      geom_line(
        data = chronRangeSwap,
        aes_string(x = "Age", y = "Position"),
        linetype = 1
      )

    if (dateLabels) {
      newData <- allAges3 %>%
        group_by(.data$Date) %>%
        summarise_all("mean") %>%
        mutate(
          Position = Position - 0.5 * dateHeight,
          Date = stringr::str_pad(.data$Date,
            width = max(nchar(.data$Date)),
            side = "right"
          )
        )
      p <- p + geom_text(
        data = newData,
        aes_string(label = "Date"),
        check_overlap = TRUE,
        vjust = 0.5,
        hjust = "right",
        nudge_x = nudgeX,
        nudge_y = nudgeY,
        size = dateLabelSize
      )
    }
    p
  }
