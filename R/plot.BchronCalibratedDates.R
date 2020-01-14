#' Plot calibrated dates from a BchronCalibrate run
#'
#' Plots calibrated radiocarbon dates from a \code{\link{BchronCalibrate}} run. Has options to plot on a position (usually depth) scale if supplied with the original run
#'
#' @param x Output from \code{\link{BchronCalibrate}}
#' @param withPositions Whether to plot with positions (i.e. using the position values as the y axis). By detault TRUE if \code{x} has more than one date and contains positions
#' @param dateHeight The height of the dates in the plot in the same units as the position values. Only relevant if \code{withPositions=TRUE}.
#' @param dateLabels Whether to add the names of the dates to the left of them. Default TRUE
#' @param fillCol A colour to fill the date densities when \code{withPositions} is TRUE, or HDR ranges when it is FALSE
#' @param withHDR Whether to plot the 95\% highest density region values
#' @param ageScale Either \code{bp} for years before present, \code{bc} for years BC/AD (BC will be negative), \code{b2k} for years before 2000. Others not supported (yet).
#' @param scaleReverse Whether to reverse the x-axis scale. Defaults to TRUE which works best for dates presented in e.g. years BP
#' @param ... Other arguments to plot (currently ignored)
#'
#' @details These plots are intended to be pretty basic and used simply for quick information. Users are encouraged to learn the R plotting features to produce publication quality graphics
#'
#' @seealso \code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom purrr map_dfr map_dbl
#'
#' @export
plot.BchronCalibratedDates =
function(x,
         withPositions=ifelse(length(x)>1 & 
                                !is.null(x[[1]]$positions),
                              TRUE,FALSE),
         dateHeight = 100,
         dateLabels = TRUE,
         fillCol = "darkslategray",
         withHDR = TRUE,
         ageScale = c('bp', 'bc', 'b2k'),
         scaleReverse = TRUE,
         ...) {
  
  # Scale function for age scales
  ageScale = match.arg(ageScale, several.ok = FALSE)
  ageScaleFun = function(z) switch(ageScale,
                                   bp = z,
                                   bc = 1950 - z,
                                   b2k = z + 50)
  
  # First plot for individual dates
  if(length(x)==1) {
    df = data.frame(
      Age = ageScaleFun(x[[1]]$ageGrid),
      Density = x[[1]]$densities
    )
    my_breaks = pretty(x = df$Age, n = 10)
    p = ggplot(df, aes_string(x = "Age", y = "Density")) + 
      geom_line() + 
      theme_bw() + 
      scale_x_continuous(breaks = my_breaks,
                         labels = abs(my_breaks),
                         trans = ifelse(scaleReverse,'reverse','identity')) +
      ggtitle(names(x)[1])
    if(withHDR) {
      my_hdr = hdr(x[[1]])
      hdr_list = vector('list', length(my_hdr))
      for(j in 1:length(my_hdr)) {
        x_seq = ageScaleFun(seq(my_hdr[[j]][1], my_hdr[[j]][2], by = 1))
        y_lookup = match(x_seq, df$Age)
        y_seq = df$Density[y_lookup]
        hdr_list[[j]] = data.frame(Age = c(ageScaleFun(my_hdr[[j]][1]), 
                                           x_seq, 
                                           ageScaleFun(my_hdr[[j]][2])), 
                                   Density = c(0, y_seq, 0),
                                   hdr = j)
      }
      hdr_df = do.call(rbind, hdr_list)
      p = p + geom_polygon(data = hdr_df, fill = fillCol)
    }
  }

  # Now for multiple dates without depths
  if(length(x)>1 & withPositions==FALSE) {
    p = vector('list', length(x))
    for(i in 1:length(x)) {
      df = data.frame(
        Age = ageScaleFun(x[[i]]$ageGrid),
        Density = x[[i]]$densities
      )
      my_breaks = pretty(x = df$Age, n = 10)
      p[[i]] = ggplot(df, aes_string(x = "Age", y = "Density")) + 
        geom_line() +
        scale_x_continuous(breaks = my_breaks,
                           labels = abs(my_breaks),
                           trans = ifelse(scaleReverse,'reverse','identity')) +
        theme_bw() + 
        ggtitle(names(x)[i])
      if(withHDR) {
        my_hdr = hdr(x[[i]])
        hdr_list = vector('list', length(my_hdr))
        for(j in 1:length(my_hdr)) {
          x_seq = ageScaleFun(seq(my_hdr[[j]][1], my_hdr[[j]][2], by = 1))
          y_lookup = match(x_seq, df$Age)
          y_seq = df$Density[y_lookup]
          hdr_list[[j]] = data.frame(Age = c(ageScaleFun(my_hdr[[j]][1]), 
                                             x_seq, 
                                             ageScaleFun(my_hdr[[j]][2])), 
                                     Density = c(0, y_seq, 0),
                                     hdr = j)
        }
        hdr_df = do.call(rbind, hdr_list)
        p[[i]] = p[[i]] + geom_polygon(data = hdr_df, fill = fillCol)
      }
    }
  }

  # Finally for multiple dates with depths
  if(length(x)>1 & withPositions==TRUE) {
    allAges = purrr::map_dfr(x, `[`, c("ageGrid", "densities"), .id = c('Date')) %>% 
      dplyr::rename(Age = .data$ageGrid)
    # scale all the densities to have max value 1
    scaleMax = function(x) return(x/max(x))
    allAges2 = allAges %>% group_by(.data$Date) %>% 
      mutate(densities = scaleMax(.data$densities)) %>% 
      ungroup()
    positionLookUp = tibble(Date = names(x),
                            Position = purrr::map_dbl(x, 'positions'))
    allAges3 = left_join(allAges2, positionLookUp, by = 'Date') %>% 
      mutate(height = .data$densities*dateHeight,
             Age = ageScaleFun(.data$Age))
    expand_x = if(dateLabels) { c(0.2,0) } else { c(0, 0) }
    expand_y = c(0.1, 0)
    my_breaks = pretty(x = allAges3$Age, n = 10)
    p = allAges3 %>% 
      ggplot(aes_string(x = "Age", 
                 y = "Position",
                 height = "height",
                 group = "Date")) +
     ggridges::geom_ridgeline(fill = fillCol, colour = fillCol) +
      scale_y_reverse(breaks = scales::pretty_breaks(n = 10),
                      expand = expand_y) +
      theme_bw() +
      scale_x_continuous(breaks = my_breaks,
                         expand = expand_x,
                         labels = abs(my_breaks),
                         trans = ifelse(scaleReverse,'reverse','identity'))
    if(dateLabels) {
      p = p + geom_text(data = allAges3 %>% 
                          group_by(.data$Date) %>% 
                          summarise_all('mean') %>% 
                          mutate(Position = Position - 0.5*dateHeight),
                        aes_string(label="Date"),
                        check_overlap = TRUE,
                        vjust = 0.5, hjust = 1.5)
    }
    
  }
  p
}
