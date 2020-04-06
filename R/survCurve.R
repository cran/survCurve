#' Draws a survival curve for one group and one event.
#'
#' This function draws the confident interval area of one group of a survfit 
#' object. For Competitive risk analysis, the event number needs to be 
#' specified. 
#' 
#' @include oneGroupOneEvent.R
#' @param x A survfit or a Cuminc object.
#' @param group Number of the chosen group. If the model-object has only one
#' Strata (Group), this parameter can be omitted.
#' @param event If the model-object is a multistate-model, the event-type
#' needs to be specified.
#' @param conf.int The confidence-interval is plotted as lines if TRUE. 
#' Default is FALSE
#' @param mark.time The times of censoring are marked if TRUE. Default
#' is FALSE
#' @param col Color of the line. Default is "black".
#' @param lty Line-type of the line. Default is 1.
#' @param lwd Line-wide of the line. Default is 1.
#' @param cex.markTime Size of the marks for censoring. Default is 1.
#' @param pch.markTime Character of the marks for censoring. Default is
#' 1 (stroke).
#' @param col.confInt Color of the line for the confidence interval. Default is "black".
#' @param lty.confInt Line-type of the line for the confidence interval. Default is 2.
#' @param lwd.confInt Line-wide of the line for the confidence interval. Default is 1.
#' @param invert Inverts the curve if TRUE. Default is FALSE.
#' @return Draws the survival curve for one group / one event.
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

survCurve <-
function (x,
          group,
          event,
          conf.int=FALSE,
          mark.time = FALSE,
          col = "black",
          lty = 1,
          lwd = 1,
          cex.markTime = 1,
          pch.markTime = 3,
          col.confInt = 1,
          lty.confInt = 2, 
          lwd.confInt = 1,
          invert = FALSE
          ) 
{
  if("Cuminc" %in% class(x))
    x <- attr(x,"survfit")

  optimizeForCurve <-function(x,y){
    firstOfEqualValues <-function(v){
      nInGroups <- rle(v)$lengths
      c(1, 1 + cumsum(nInGroups[-length(nInGroups)]))
    }
    optimizedPoints <- c(firstOfEqualValues(y),length(y)) 
    list(x = x[optimizedPoints], y = y[optimizedPoints])
  }

  survdata <- extractOneGroupOneEvent(x, 
                                  group=ifelse(missing(group),NA,group),
                                  event=ifelse(missing(event),NA,event),
                                 )

  if(invert==TRUE){
    fun <- function(x) 1-x
  }else{
    fun <- function(x) x
  }
  
  L <- nrow(survdata)
  modif_survdata <- within(survdata,{
                           lower[L] <- lower[L-1]
                           upper[L] <- upper[L-1]
                           estimate <- fun(estimate)
                           lower <- fun(lower)
                           upper <- fun(upper)
                           })
  with(modif_survdata,
    { lines(optimizeForCurve(x=time, y=estimate),
            type="s", lty = lty, col=col, lwd=lwd)
      if(conf.int){
        lines(optimizeForCurve(x=time, y=lower),
              type="s", lty = lty.confInt, col=col.confInt, lwd=lwd.confInt)
        lines(optimizeForCurve(x=time, y=upper),
              type="s", lty = lty.confInt, col=col.confInt, lwd=lwd.confInt)
      }
    }
  )
}
