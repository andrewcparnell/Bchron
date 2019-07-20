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
#'
#' @details These plots are intended to be pretty basic and used simply for quick information. Users are encouraged to learn the R plotting features to produce publication quality graphics
#'
#' @seealso \code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#'
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
         ggargs = NULL,
         showPlot = TRUE) {

  # First plot for individual dates
  if(length(x)==1) {
    df = data.frame(
      Age = x[[1]]$ageGrid,
      Density = x[[1]]$densities
    )
    
    p = ggplot(df, aes(x = Age, y = Density)) + 
      geom_line() + 
      theme_bw() + 
      ggtitle(names(x)[1])
    if(withHDR) {
      my_hdr = hdr(x[[1]])
      hdr_list = vector('list', length(my_hdr))
      for(j in 1:length(my_hdr)) {
        x_seq = seq(my_hdr[[j]][1], my_hdr[[j]][2], by = 1)
        y_lookup = match(x_seq, df$Age)
        y_seq = df$Density[y_lookup]
        hdr_list[[j]] = data.frame(Age = c(my_hdr[[j]][1], 
                                           x_seq, my_hdr[[j]][2]), 
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
        Age = x[[i]]$ageGrid,
        Density = x[[i]]$densities
      )
      p[[i]] = ggplot(df, aes(x = Age, y = Density)) + 
        geom_line() + 
        theme_bw() + 
        ggtitle(names(x)[i])
      if(withHDR) {
        my_hdr = hdr(x[[i]])
        hdr_list = vector('list', length(my_hdr))
        for(j in 1:length(my_hdr)) {
          x_seq = seq(my_hdr[[j]][1], my_hdr[[j]][2], by = 1)
          y_lookup = match(x_seq, df$Age)
          y_seq = df$Density[y_lookup]
          hdr_list[[j]] = data.frame(Age = c(my_hdr[[j]][1], 
                                             x_seq, my_hdr[[j]][2]), 
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
    allAges = map_dfr(x, `[`, c("ageGrid", "densities"), .id = c('Date')) %>% 
      rename(Age = ageGrid)
    # scale all the densities to have max value 1
    scaleMax = function(x) return(x/max(x))
    allAges2 = allAges %>% group_by(Date) %>% 
      mutate(densities = scaleMax(densities)) %>% 
      ungroup()
    positionLookUp = tibble(Date = names(x),
                            Position = map_dbl(x, 'positions'))
    allAges3 = left_join(allAges2, positionLookUp, by = 'Date')
    expand_x = if(dateLabels) { c(0.2,0) } else { c(0, 0) }
    expand_y = c(0.1, 0)
    p = allAges3 %>% 
      ggplot(aes(x = Age, 
                 y = Position,
                 height = densities*dateHeight,
                 group = Date)) +
      geom_ridgeline(fill = fillCol, colour = fillCol) +
      scale_y_reverse(breaks = scales::pretty_breaks(n = 10),
                      expand = expand_y) +
      theme_bw() +
      scale_x_reverse(breaks = scales::pretty_breaks(n = 10),
                      expand = expand_x)
    if(dateLabels) {
      p = p + geom_text(data = allAges3 %>% 
                          group_by(Date) %>% 
                          summarise_all('mean') %>% 
                          mutate(Position = Position - 0.5*dateHeight),
                        aes(label=Date),
                        check_overlap = TRUE,
                        vjust = 0.5, hjust = 1.5)
    }
    
  }
  p
}
