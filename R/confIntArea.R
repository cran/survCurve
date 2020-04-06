#' Adds a confident interval area of a survival model to a plot.
#'
#' This function adds the confident interval area of one group (strata) of
#' a survfit object to an existing plot. For competitive risk models, it
#' draws the confidence interval of one group and one event; the event-number
#' needs to be specified. If two overlapping confidence intervals are drawn 
#' (by two function calls) in one plot, the use of transparent color is
#' recommended, for example "adjustcolor("red",0.1).

#' 
#' @param x A survfit (survival-package) or a Cuminc (mstate-package) object.
#' @param group Number of the group (=strata) of which the confidence interval 
#' should be plotted. If the survfit-object has only one strata, this
#' parameter can be omitted.
#' @param event If the model-object is a multistate-model, the number of the
#' event-type needs to be specified.
#' @param col Color of the confident interval area. Default is "grey". A
#' transparent value is recommended, for example "adjustcolor("red",0.1).
#' @param invert Inverts the area if TRUE, default is FALSE.
#' @return Draws an area for the confidence interval.
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
confIntArea <- function(x, group, event, col="grey", invert=FALSE){
  if("Cuminc" %in% class(x))
      x <- attr(x,"survfit")

  survdata <- extractOneGroupOneEvent(x, 
                                  group=ifelse(missing(group),NA,group),
                                  event=ifelse(missing(event),NA,event)
                                 )

  if(invert==TRUE){
    fun <- function(x) 1-x
  }else{
    fun <- function(x) x
  }

  modif_survdata <- within(survdata,{
                           estimate <- fun(estimate)
                           upper    <- fun(upper)
                           lower    <- fun(lower)
                           })

  drawCIArea <- function(t, u, l){
    L <- length(t)
    t_rep <- c(t[1], rep(t[c(-1,-L)], each = 2), t[L])
    u_rep <- rep(u[-L], each = 2)
    l_rep <- rep(l[-L], each = 2)
    polygon(c(t_rep,rev(t_rep)),c(u_rep,rev(l_rep)), border = NA,col=col)
  }

  with(modif_survdata,
    drawCIArea(time, upper, lower)
  )
}
