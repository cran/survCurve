#' Setup for a plot for survival data without the curves.
#'
#' This function draws the confident interval area of one group of a survfit 
#' object. For Competitive risk analysis, the event number needs to be 
#' specified. 
#' To get a nice graph, the function should be runned after one made an empty 
#' plot, and before drawing the actual curves (Thus the area is underlining
#' to the curves). If two curves are drawn which confident intervals overlap
#' transparency should be added to the colors.
#' 
#' @param main Title of the plot, Default is "".
#' @param title.xaxis Title of the x-axis. Default is "".
#' @param title.yaxis Title of the y-axis. Default is "".
#' @param xmin Minimum for the x (Time) - axis. Default is 0.
#' @param xmax Maximum of the x (time) axis. No default, must be specified.
#' @param ymin Minimum of the y-axis. Default is -0.02
#' @param ymax Maximum of the y-axis. Default is 1.02
#' @param ypercent Specifies if the Unit of the y-axis is ratio (usually 0-1)
#' or percentage (0-100), Default is TRUE (0-100). Equals yscale=100.
#' @param title.nrAtRisk Label for the number at risk region. Default is
#' "number at risk", other meaningful value is "patients at risk", or  
#' translations in any language for example. Not plotted if space.nrAtRisk is 0
#' @param space.nrAtRisk Space (usually around 0.2-0.5) below the plot to draw
#' the values of number at risk. Default is 0.
#' @param interval.xaxis Interval at which the ticks of the x-axis are drawn.
#' Default depends on the size of the plot.
#' @param interval.yaxis Interval at which the ticks of the y-axis are drawn.
#' Default depends on the size of the plot.
#' @param las.xaxis Orientation of the labels of the x-axis. Default is 1
#' (horizontal).
#' @param las.yaxis Orientation of the labels of the y-axis. Default is 1
#' (horizontal).
#' @param font.xaxis Font-type for the labels of the x-axis. Default is 1.
#' @param font.yaxis Font-type for the labels of the y-axis. Default is 1.
#' @param cex.xaxis Font-size for the labels of the x-axis. Default is 1.
#' @param cex.yaxis Font-size for the labels of the y-axis. Default is 1.
#' @param points.xaxis Exact position of the ticks of the x-axis. Overwrites
#' the values of interval.xaxis. Usually not required.
#' @param points.yaxis Exact position of the ticks of the y-axis. Overwrites
#' the values of interval.yaxis. Usually not required.
#' @param labels.xaxis Label for the ticks of the x-axis. Only valid if 
#' points are specified. Must be same length like points.
#' @param labels.yaxis Label for the ticks of the y-axis. Only valid if 
#' points are specified. Must be same length like points.
#' @param font.nrAtRiskTitle Font type of the title of the nr-at-Risk Space
#' Default is 1.
#' @param cex.nrAtRiskTitle Font size of the title of the nr-at-Risk Space
#' Default is 1.
#' @return Draws an empty plot optimized for survival-curves.
#' @import graphics
#' @examples
#'   require(survival)
#'   aml_model <- with(aml, survfit(Surv(time, status)~x))
#'   col1 <- adjustcolor("red",0.2); col2 <- adjustcolor("blue",0.2)
#'   survPlot(xmax=50, space.nrAtRisk=0.32)
#'   confIntArea(aml_model, col=col1, group=1)
#'   confIntArea(aml_model, col=col2, group=2)
#'   survCurve(aml_model, group=1)
#'   survCurve(aml_model, group=2, lty=2)
#'   nrAtRisk(aml_model, group=1, y=-0.17, bgcol.flag=col1, label="maintain")
#'   nrAtRisk(aml_model, group=2, y=-0.24, bgcol.flag=col2, lty.flag=2, label="non-maint.")
#' @export
survPlot <- function(main="", #ok
                     title.xaxis="", #ok
                     title.yaxis="", #ok
                     xmin=0, #ok
                     xmax,   #ok
                     ymin=-0.02, #ok
                     ymax=1.02,  #ok
                     ypercent=TRUE, #ok
                     title.nrAtRisk="number at risk", #ok
                     space.nrAtRisk=0, #ok
                     # ----------------------------------------------
                     interval.xaxis,   #ok
                     interval.yaxis,#ok
                     las.xaxis=1, #ok
                     las.yaxis=1, #ok
                     font.xaxis=1,#ok
                     font.yaxis=1,#ok
                     cex.xaxis=1, #ok
                     cex.yaxis=1, #ok
                     points.xaxis,#ok
                     points.yaxis,#ok
                     labels.xaxis,#ok
                     labels.yaxis,#ok
                     font.nrAtRiskTitle=1,#ok
                     cex.nrAtRiskTitle=1  #ok
                     ){

  mkInterval <- function(min,max){
    diff <- max-min
    if( diff < 0.06 ) return(diff/5)
    if( diff < 0.4  ) return(0.05)
    if( diff < 0.9  ) return(0.1)
    if( diff < 2    ) return(0.2)
    if( diff < 11   ) return(1  )
    if( diff <= 15  ) return(2)
    if( diff < 46   ) return(5)
    if( diff < 80   ) return(10)
    if( diff < 140  ) return(20)
    return(diff/5)
  }

  x_axis <- list(type      = "x",
                 interval = ifelse(missing(interval.xaxis),NA,interval.xaxis),
                 points   = ifelse(missing(points.xaxis),NA,points.xaxis),
                 labels   = ifelse(missing(labels.xaxis),NA,labels.xaxis),
                 las      = las.xaxis,
                 font     = font.xaxis,
                 cex      = cex.xaxis,
                 min      = xmin,
                 max      = xmax)
  y_axis <- list(type      = "y",
                 interval = ifelse(missing(interval.yaxis),NA,interval.yaxis),
                 points   = ifelse(missing(points.yaxis),NA,points.yaxis),
                 labels   = ifelse(missing(labels.yaxis),NA,labels.yaxis),
                 las      = las.yaxis,
                 font     = font.yaxis,
                 cex      = cex.yaxis,
                 min      = ymin,
                 max      = ymax)

  calculate_axis_data <- function(axis){
    within(axis,{
             type <- type #required to pass R CMD check for CRAN
      if(is.na(interval))
        interval <- mkInterval(min, max)
      if(is.na(points)){
        n_steps <- ceiling(max/interval)
        points <- c(interval*c(0:n_steps))
        points <- points[points>=min]
        if(!is.na(labels))
          stop("if labels.", type, "axis is spcified, points.",
                     type ,"axis is required")
        labels   <- points*ifelse(ypercent & type=="y", 100, 1)
      }else{
        if(!is.na(labels)){
          if(length(labels)!=length(points))
            stop("labels.", type, "axis and ponts.",
                       type, "axis must have same length")
        }else{
          labels   <- points*ifelse(ypercent & type=="y", 100, 1)
        }
      }
      points <- c(min-(10+ifelse(type=="y", space.nrAtRisk,0)),
                       points,max+10)
      labels   <- c("",labels,"")
    })
  }

  x_axis <- calculate_axis_data(x_axis)
  y_axis <- calculate_axis_data(y_axis)

  plot(NA,
       main=main,
       xlab=title.xaxis,
       ylab=title.yaxis,
       xlim=c(xmin,xmax),
       ylim=c(ymin-space.nrAtRisk,ymax),
       xaxt="n", yaxt="n",
       xaxs="i", yaxs="i"
      )

  if(space.nrAtRisk!=0){
    strStart <- xmin + (xmax-xmin)*0.02
    strEnd   <- strStart + strwidth(title.nrAtRisk, cex=cex.nrAtRiskTitle)
    ticks_sep <- with(x_axis, points[points < strStart | points > strEnd ])
    axis(1, at = ticks_sep, pos=ymin, labels=NA)
    text(x      = strStart,
         y      = ymin - 1*strheight(title.nrAtRisk,
                                     cex=cex.nrAtRiskTitle), 
         labels = title.nrAtRisk,
         font   = font.nrAtRiskTitle,
         cex    = cex.nrAtRiskTitle,
         adj    = 0 )
  }

  for(i in c(1,2)){
    with(list(x_axis, y_axis)[[i]],
      axis(i,
           cex.axis = cex,
           las      = las,
           font     = font,
           at       = points,
           labels   = labels
          ))
  }
}
