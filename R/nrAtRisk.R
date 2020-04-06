#' Adds number at risk of a survival model to a plot.
#'
#' This function adds the number at risk of one group (strata) of
#' a survfit object to an existing plot. If label is specified, it draws
#' a label for the group right to the number at risks. If any elements of
#' the "flag" is specified, it also draws a small identifier, with which 
#' the corresponding curves / confidence intervals of the plot can be
#' identified near to the label.
#' 
#' @param x A survfit (survival-package) or a Cuminc (mstate-package) object.
#' @param group The number of the group (=strata) of which the confidence interval 
#' should be plotted. If the survfit-object has only one strata, this
#' argument can be omitted.
#' @param ypos A numeric value for the position at the y-axis.
#' @param times An optional vector of numeric values specifying at which times
#' (x-axis) the number at risk are calculated and plotted. If not specified, 
#' the defaults depends on "interval.times" value if available, or
#' the size of the plot.
#' @param interval.times An optional numeric value which specifies the interval
#' at which the number at risk values are plotted. Is overwritten by times. If 
#' not specified, the value depends on the size of the plot.
#' @param zero.adjust A logical value. If true, the number at risk at time 0 is not
#' plotted at the precise position, but slightly adjusted to the left to prevent
#' the value to be cut-off by the plot margins. Also, the value at x=0 is plotted 
#' if true. Default is TRUE.
#' @param zero.value A numeric value or string that overwrites the nr at
#' risk value at x=0 if specified (only if zero.adjust is TRUE).
#' @param label String for the group name.
#' @param font.text Font of the text (nr at risk and label). Default is 1.
#' @param col.text Colour of the text (nr at risk and label). Default is "black".
#' @param cex.text Font-size of the text (nr at risk and label). Default is 1.
#' @param font.nr Overwrites font.text for the number-part.
#' @param col.nr Overwrites col.text for the number-part.
#' @param cex.nr Overwrites cex.text for the number-part.
#' @param font.lab Overwrites font.text for the label-part.
#' @param col.lab Overwrites col.text for the label-part.
#' @param cex.lab Overwrites cex.text for the label-part.
#' @param xpos.lab x-position of the label, default is near the right border
#' of the plot.
#' @param bgcol.flag Background-color of the flag (corresponding to the 
#' color of the confidence interval in the plot). No color if not value is
#' specified.
#' @param lwd.flag Line-wide of the flag. Value is 1 if not specified.
#' @param lty.flag Line-type of the flag. Value is 1 if not specified.
#' @param lncol.flag color of the line of the flag. Default is "black".
#' @param xlim.flag Vector with two values, defining the beginning and end of the 
#' flag on the x-axis. Default depends on the size of the plot.
#' @return Draws the number at risk to an existing plot.
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
nrAtRisk <- function(x, #ok
                     group, #ok
                     ypos = 0.08, #ok
                     times, #ok
                     interval.times,
                     zero.adjust = TRUE, #ok
                     zero.value, #ok
                     font.text=1, #ok
                     cex.text = 1, #ok
                     col.text = "black", #ok
                     cex.nr, #ok
                     col.nr, #ok
                     font.nr, #ok
                     label, #ok
                     xpos.lab, #ok
                     cex.lab, #ok
                     col.lab, #ok
                     font.lab, #ok
                     lty.flag, #ok
                     lwd.flag, #ok
                     bgcol.flag, #ok
                     lncol.flag, #ok
                     xlim.flag #ok
                     ){
  # read input ------------
  if("Cuminc" %in% class(x))
    x <- attr(x,"survfit")

  has.flag <- !missing(lncol.flag)|!missing(lty.flag)|
              !missing(lwd.flag)|!missing(bgcol.flag)
  has.lab  <- !missing(label)
  has.zero <- zero.adjust

  # function definintion --

  ArgOrDefault <- function(arg, default=NA){
    miss_call <- substitute(missing(arg))
    if(eval(miss_call, envir=parent.env(environment())))  return(default)
    return(arg)}

  optimalIntervalLength <- function(min,max){
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

  nrRiskAtCustomTimes <- function(times, times_model, n_risk_model){
    if(length(times)==0) return(NULL)
    largerT_indexes <- which(times_model>times[1])
    n_risk <- ifelse(length(largerT_indexes)>0,
                     n_risk_model[min(largerT_indexes)],
                     NA)
    c(n_risk, nrRiskAtCustomTimes(times[-1], times_model, n_risk_model))
  }

  # defaults % calculation-
  survdata <- extractOneGroupOneEvent(x, 
                                      group=ifelse(missing(group),NA,group),
                                      event=1,
                                      firstRow=F)
  plot_xmin <- par("usr")[1] 
  plot_xmax <- par("usr")[2] 
  plot_width <- plot_xmax-plot_xmin

  flag <- list( 
          xlim   = ArgOrDefault(xlim.flag, plot_xmin + c(0.82, 0.86)*plot_width),
          lty    = ArgOrDefault(lty.flag, 1),
          lwd    = ArgOrDefault(lwd.flag, 1),
          bgcol  = ArgOrDefault(bgcol.flag, NA),
          lncol  = ArgOrDefault(lncol.flag, "black")  )

  lab <- list(
          label  = ArgOrDefault(label, ""),
          xpos   = ArgOrDefault(xpos.lab, flag$xlim[2]+plot_width/200),
          font   = ArgOrDefault(font.lab, font.text),
          cex    = ArgOrDefault(cex.lab, cex.text),
          col    = ArgOrDefault(col.lab, col.text)   )

  group <- ArgOrDefault(group, 1) # because extractOneGroupOneEvent 
                                  # already handeld invalid cases
  numbers  <- list(
          times    = ArgOrDefault(times, NA),
          font     = ArgOrDefault(font.nr, font.text),
          cex      = ArgOrDefault(cex.nr, cex.text),
          col      = ArgOrDefault(col.nr, col.text),
          interval = ArgOrDefault(interval.times,
                                  optimalIntervalLength(plot_xmin,plot_xmax)),
          zeroVal  = ArgOrDefault(zero.value, x$n[group]) )

  numbers  <- within(numbers,{ 
      if(is.na(times)){
          times     <- c(numbers$interval*c(0:ceiling(plot_xmax/numbers$interval)))
        if(has.flag)
          times     <- times[which(times<flag$xlim[1]-plot_width/100)]
        if(has.lab)
          times     <- times[which(times<lab$xpos-plot_width/100)]
      }
          n.risk    <- nrRiskAtCustomTimes(times, survdata$time,
                                           survdata$n.risk)
      if(has.zero & plot_xmin <= 0 & plot_xmax > 0){
          nr_width  <- strwidth(numbers$zeroVal, cex=numbers$cex, font=numbers$font)
          n.risk    <- n.risk[which(times!=0)]
          times     <- times[which(times!=0)]
          zeroPos   <- if(plot_xmin > -nr_width/2){
                           plot_xmin+0.5*nr_width+plot_width/200
                         }else{0}
          times     <- c(zeroPos, times)
          n.risk    <- c(numbers$zeroVal, n.risk)
      }
     })

  # draw ------------------------
  with(numbers, for(i in 1:length(times))
           if(!is.na(n.risk[i]))
             text(times[i], ypos, n.risk[i], cex=cex, col=col, font=font)
      )

  if(has.flag) with(flag,{if(!is.na(bgcol)){
                             polygon(c(xlim[c(1,2,2,1)]),
                                     c(ypos+c(1, 1, -1, -1)*strheight("I")/2), 
                                     col=bgcol, border=bgcol)
                           }
                           segments(xlim[1], ypos, xlim[2], ypos, lty=lty,
                                    lwd=lwd, col=lncol)
                         }
                   )

  if(has.lab) with(lab,text(xpos, y=ypos, label=label, adj=0,
                            col=col, cex=cex, font=font)
                  )
}

#' Adds number at risk of a survival model to a plot.
#' Adds label to plot.
#'
#' This function adds a text label to the plot, which is preceded by a 
#' small identifier, similar to the function nrAtRisk.
#' 
#' @param text String, content of the label.
#' @param x A numeric value for the position at the x-axis.
#' @param y A numeric value for the position at the y-axis.
#' @param font Font of the label.
#' @param col.text Color of the label.
#' @param len.flag Length of the flag.
#' @param cex Font size of the label
#' @param bgcol.flag Background-color of the flag (corresponding to the 
#' color of the confidence interval in the plot). No color if not value is
#' specified.
#' @param lwd.flag Line-wide of the flag. Value is 1 if not spezified.
#' @param lty.flag Line-typ of the flag. Value is 1 if not spezified.
#' @param lncol.flag Color of the line of the flag. Default is "black".
#' @return Draws the number at risk to an existing plot.
#' @examples
#'   require(survival)
#'   aml_model <- with(aml, survfit(Surv(time, status)~x))
#'   col1 <- adjustcolor("red",0.2); col2 <- adjustcolor("blue",0.2)
#'   survPlot(xmax=50)
#'   confIntArea(aml_model, col=col1, group=1)
#'   confIntArea(aml_model, col=col2, group=2)
#'   survCurve(aml_model, group=1)
#'   survCurve(aml_model, group=2, lty=2)
#'   survLable("maintain", 1, 0.2, bgcol.flag=col1)
#'   survLable("non-maint.", 1, 0.1, bgcol.flag=col2, lty.flag=2)
#' @export
survLable <- function(
                     text,
                     x,
                     y,
                     font=1,
                     cex = 1, 
                     col.text = "black",
                     len.flag ,
                     lty.flag=1,
                     lwd.flag=1, 
                     bgcol.flag,
                     lncol.flag="black"
                     ){
  plot_xmin <- par("usr")[1] 
  plot_xmax <- par("usr")[2] 
  plot_width <- plot_xmax-plot_xmin

  if(missing(x)) stop("x-value required")
  if(missing(y)) stop("y-value required")
  ypos <- y

  flag    <- list(lty      = lty.flag,
                  lwd      = lwd.flag,
                  bgcol    = bgcol.flag,
                  lncol    = lncol.flag)

  lab     <- list(label    = ifelse(!missing(text),text,""),
                  cex      = cex,
                  col      = col.text,
                  font     = font)

len.flag <- ifelse(!missing(len.flag),len.flag,0.05*plot_width)

  flag <- within(flag,{ 
          xlim      <- c(x, x + len.flag)
     })
  lab <- within(lab,{ 
          xpos      <- flag$xlim[2]+plot_width/200
     })

  # draw ------------------------
  with(flag,{if(!is.na(bgcol)){
                polygon(c(xlim[c(1,2,2,1)]),
                        c(ypos+c(1, 1, -1, -1)*strheight("I")/2), 
                        col=bgcol, border=bgcol)
              }
              segments(xlim[1], ypos, xlim[2], ypos, lty=lty,
                       lwd=lwd, col=lncol)
            }
      )

  with(lab,text(xpos, y=ypos, label=label, adj=0,
                col=col, cex=cex, font=font)
      )
}
