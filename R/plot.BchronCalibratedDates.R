#' Plot calibrated dates from a BchronCalibrate run
#'
#' Plots calibrated radiocarbon dates from a \code{\link{BchronCalibrate}} run. Has options to plot on a position (usually depth) scale if supplied with the original run
#'
#' @param x Output from \code{\link{BchronCalibrate}}
#' @param date Either numbers or date names to plot (only used if multiple dates have been calibrated)
#' @param withPositions Whether to plot with positions (i.e. using the position values as the y axis). By default TRUE if \code{x} has more than one date and contains positions
#' @param includeCal Whether to plot the date alongside the calibration curve and the normally distributed uncalibrated date. 
#' @param dateHeight The height of the dates in the plot in the same units as the position values. Only relevant if \code{withPositions=TRUE}.
#' @param dateLabels Whether to add the names of the dates to the left of them. Default TRUE
#' @param fillCol A colour to fill the date densities when \code{withPositions} is TRUE, or HDR ranges when it is FALSE
#' @param withHDR Whether to plot the 95\% highest density region values
#' @param ageScale Either \code{bp} for years before present, \code{bc} for years BC/AD (BC will be negative), \code{b2k} for years before 2000. Others not supported (yet).
#' @param scaleReverse Whether to reverse the x-axis scale. Defaults to TRUE which works best for dates presented in e.g. years BP
#' @param pathToCalCurves The Bchron path to calibration curves. Defaults to the package location might need to be set to another folder if user defined calibration curves are being used
#' @param ... Other arguments to plot (currently ignored)
#'
#' @details These plots are intended to be pretty basic and used simply for quick information. Users are encouraged to learn the R plotting features to produce publication quality graphics
#'
#' @seealso \code{\link{BchronCalibrate}}, \code{\link{Bchronology}}, \code{\link{BchronRSL}}, \code{\link{BchronDensity}}, \code{\link{BchronDensityFast}}
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom purrr map_dfr map_dbl
#' @importFrom stats dnorm
#'
#' @export
plot.BchronCalibratedDates =
  function(x,
           date = NULL,
           withPositions=ifelse(length(x)>1 & 
                                  !is.null(x[[1]]$positions),
                                TRUE,FALSE),
           includeCal = FALSE,
           dateHeight = 100,
           dateLabels = TRUE,
           fillCol = "darkslategray",
           withHDR = TRUE,
           ageScale = c('bp', 'bc', 'b2k'),
           scaleReverse = TRUE,
           pathToCalCurves = system.file('data',
                                         package='Bchron'),
           ...) {
    
    # Check for dodgy options
    if(includeCal & withPositions) stop("Both includeCal and withPositions cannot be TRUE")
    
    # Extract out which date to plot if given
    if(!is.null(date)) {
      n_dates = length(date)
      
      # Stop if not all the dates are character or numeric
      if(!all(is.numeric(date))) {
        which_dates = match(date,names(x))
        if(any(is.na(which_dates))) stop("Some provided dates do not match date names in calibrated dates object")
      } else if(any(is.na(match(1:length(date), 1:length(x))))) {
        stop("Some date numbers out of range")
      }
      
      x_new = vector('list', length = n_dates)
      for(j in 1:n_dates) {
        x_new[[j]] = x[[date[j]]]
        if(is.numeric(date[j])) {
          names(x_new)[j] = names(x)[[date[j]]]  
        } else {
          names(x_new)[j] = names(x)[[which_dates[j]]]
        }
      }
      x = x_new
    }
      
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
      
      if(includeCal) {
        
        # Include the calibration curve
        calCurveFile = paste(pathToCalCurves,'/',x[[1]]$calCurves,
                             '.rda',sep='')
        if(!file.exists(calCurveFile)) stop(paste('Calibration curve file',calCurveFile,'not found'))
        calLocation = load(calCurveFile)
        calCurve = get(calLocation)
        calCurveUse = subset(calCurve,
                             calCurve$V1 > min(x[[1]]$ageGrid) &
                               calCurve$V1 < max(x[[1]]$ageGrid))
        df$Density2 = df$Density/max(df$Density)*dateHeight + min(calCurveUse$V2)
        
        c14ageGrid = seq(x[[1]]$ages - 3 * x[[1]]$ageSds, 
                         x[[1]]$ages + 3 * x[[1]]$ageSds, 
                         by = 1)
        c14density = stats::dnorm(c14ageGrid, 
                                  mean = x[[1]]$ages,
                                  sd = x[[1]]$ageSds)
        edge_val = ifelse(scaleReverse,max(calCurveUse$V1),
                          min(calCurveUse$V1))
        mult_val = ifelse(scaleReverse, -1, 1)
        df_14C = data.frame(age = c14ageGrid,
                            dens = mult_val*c14density/max(c14density)*
                              dateHeight + edge_val)
        
        p = ggplot(calCurveUse, aes_string(x = "V1", y = "V2")) + 
          geom_line() + 
          theme_bw() + 
          scale_x_continuous(breaks = my_breaks,
                             labels = abs(my_breaks),
                             trans = ifelse(scaleReverse,
                                            'reverse',
                                            'identity')) +
          ggtitle(names(x)[1]) + 
          geom_polygon(data = df, aes_string(x = "Age", y = "Density2"), 
                       fill = fillCol) + 
          geom_polygon(data = df_14C, aes_string(x = "dens", y = "age"),
                       fill = fillCol) + 
          labs(x = 'Cal Age', y = '14C Age')
      } else if(withHDR) {
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
        
        if(includeCal) {
          # Include the calibration curve
          calCurveFile = paste(pathToCalCurves,'/',x[[i]]$calCurves,
                               '.rda',sep='')
          if(!file.exists(calCurveFile)) stop(paste('Calibration curve file',calCurveFile,'not found'))
          calLocation = load(calCurveFile)
          calCurve = get(calLocation)
          calCurveUse = subset(calCurve,
                               calCurve$V1 > min(x[[i]]$ageGrid) &
                                 calCurve$V1 < max(x[[i]]$ageGrid))
          df$Density2 = df$Density/max(df$Density)*dateHeight + min(calCurveUse$V2)
          
          c14ageGrid = seq(x[[i]]$ages - 3 * x[[i]]$ageSds, 
                           x[[i]]$ages + 3 * x[[i]]$ageSds, 
                           by = 1)
          c14density = dnorm(c14ageGrid, 
                             mean = x[[i]]$ages,
                             sd = x[[i]]$ageSds)
          edge_val = ifelse(scaleReverse,max(calCurveUse$V1),
                            min(calCurveUse$V1))
          mult_val = ifelse(scaleReverse, -1, 1)
          df_14C = data.frame(age = c14ageGrid,
                              dens = mult_val*c14density/max(c14density)*
                                dateHeight + edge_val)
          
          p[[i]] = ggplot(calCurveUse, aes_string(x = "V1", y = "V2")) + 
            geom_line() + 
            theme_bw() + 
            scale_x_continuous(breaks = my_breaks,
                               labels = abs(my_breaks),
                               trans = ifelse(scaleReverse,
                                              'reverse',
                                              'identity')) +
            ggtitle(names(x)[i]) + 
            geom_polygon(data = df, aes_string(x = "Age", y = "Density2"), 
                         fill = fillCol) + 
            geom_polygon(data = df_14C, aes_string(x = "dens", y = "age"),
                         fill = fillCol) + 
            labs(x = 'Cal Age', y = '14C Age')
        } else if(withHDR) {
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
