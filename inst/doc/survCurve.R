## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(survival)
library(mstate)
library(survCurve)

## ----eval=FALSE----------------------------------------------------------
#  plot(model,options)

## ----eval=FALSE----------------------------------------------------------
#  survPlot(options)
#  confIntArea(model,group,event,options)
#  confIntArea(model,group,event,options)
#  survCurve(model,group,event,options)
#  survCurve(model,group,event,options)
#  nrAtRisk(model,group,options)
#  nrAtRisk(model,group,options)

## ----eval=FALSE----------------------------------------------------------
#  survPlot(options)
#  confIntArea(model,options)
#  survCurve(model,options)
#  nrAtRisk(model,options)

## ----fig.height=5, fig.width=5-------------------------------------------
survPlot(xmax=10,space.nrAtRisk=0.2)

## ----eval=FALSE----------------------------------------------------------
#  survPlot(xmax=..., space.nrAtRisk=0.35, ...)
#  survCurve(model, group=1 lty=1, ...)
#  survCurve(model, group=2 lty=2, ...)
#  nrAtRisk(model, y=-0.15, group=1, lty.flag=1, label="Group A", ...)
#  nrAtRisk(model, y=-0.25, group=2, lty.flag=2, label="Group B", ...)

## ----fig.height=7, fig.width=7-------------------------------------------
model <- survfit(Surv(aml$time,aml$status)~aml$x)

col1 <- adjustcolor("red",0.15)
col2 <- adjustcolor("blue",0.15)

survPlot(main="Survival- AML",
         xmax=50,
         space.nrAtRisk=0.20,
         title.xaxis="months after event",
         title.yaxis="                   (%)",
         title.nrAtRisk="patients at risk",
         font.xaxis=2, font.yaxis=2)

confIntArea(model, col=col1, group=1)
confIntArea(model, col=col2, group=2)

survCurve(model, lty=1, group=1) 
survCurve(model, lty=2, group=2) 

nrAtRisk(model, group=1,
         label="Maint.",
         bgcol.flag=col1,
         lty=1,
         ypos=-0.10)
nrAtRisk(model, group=2,
         label="non-Maint.",
         bgcol.flag=col2,
         lty=2,
         ypos=-0.17)


## ----fig.height=5, fig.width=5-------------------------------------------
model <- survfit(Surv(aml$time,aml$status)~0)

col <- "grey"

survPlot(main="Survival- AML",
         xmax=55,
         title.xaxis="months after event",
         title.yaxis="                   (%)"
         )

confIntArea(model, col=col)

survCurve(model) 

nrAtRisk(model)


## ----fig.height=5, fig.width=5-------------------------------------------
data(aidssi)
model <- Cuminc(time="time", status= "status", failcodes=c(1,2), data=aidssi)
col1 <- adjustcolor("red",0.15)
col2 <- adjustcolor("blue",0.15)

survPlot(main="Disease progression after HIV-infection",
         xmax=14,
         ymax=0.65,
         space.nrAtRisk=0.12,
         title.xaxis="years after hiv-infection",
         title.yaxis="               incidence (%)",
         title.nrAtRisk="patients at risk",
         font.xaxis=2, font.yaxis=2)

confIntArea(model, col=col1, event=1)
confIntArea(model, col=col2, event=2)

survCurve(model, event=1, lty=3)
survCurve(model, event=2, lty=2)

nrAtRisk(model, ypos=-0.1,cex.nr=0.8)

survLable(x=1, y=0.6, text="AIDS",bgcol.flag=col1,lty=3)
survLable(x=1, y=0.50, text="SI-Apperance",bgcol.flag=col2,lty=2)

## ----fig.height=5, fig.width=7.5-----------------------------------------
data(aidssi)
par_backup <- par(mfrow=c(1,2))
model2 <- Cuminc(time="time", status= "status", group="ccr5", failcodes=c(1,2), data=aidssi)
col1 <- adjustcolor("red",0.15)
col2 <- adjustcolor("blue",0.15)

survPlot(main="ccr5 ww\n(2x wild allele)",
         xmax=14,
         ymax=0.65,
         space.nrAtRisk=0.12,
         title.xaxis="years after hiv-infection",
         title.yaxis="               incidence (%)",
         title.nrAtRisk="patients at risk",
         font.xaxis=2, font.yaxis=2)
confIntArea(model2, col=col1, group=1, event=1)
confIntArea(model2, col=col2, group=1, event=2)
survCurve(model2, group=1, event=1, lty=3)
survCurve(model2, group=1, event=2, lty=2)
nrAtRisk(model2, group=1, ypos=-0.1,cex.nr=0.8)
survLable(x=1, y=0.6, text="AIDS",bgcol.flag=col1,lty=3)
survLable(x=1, y=0.50, text="SI-Apperance",bgcol.flag=col2,lty=2)
#
survPlot(main="ccr5 wm\n(1 wild, 1 mutant allele )",
         xmax=14,
         ymax=0.65,
         space.nrAtRisk=0.12,
         title.xaxis="years after hiv-infection",
         title.yaxis="               incidence (%)",
         title.nrAtRisk="patients at risk",
         font.xaxis=2, font.yaxis=2)
confIntArea(model2, col=col1, group=2, event=1)
confIntArea(model2, col=col2, group=2, event=2)
survCurve(model2, group=2, event=1, lty=3)
survCurve(model2, group=2, event=2, lty=2)
nrAtRisk(model2, group=2, ypos=-0.1,cex.nr=0.8)
survLable(x=1, y=0.6, text="AIDS",bgcol.flag=col1,lty=3)
survLable(x=1, y=0.50, text="SI-Apperance",bgcol.flag=col2,lty=2)
par(par_backup)


