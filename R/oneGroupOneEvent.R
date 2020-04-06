#' Extracts one group and one event of a survfit model as a data frame.
#'
#' This function is a helper function for the package and is not exported.
#' 
#' @param model A survfit object.
#' @param group Number of the chosen group. If the model-object has only one
#' Strata (Group), this parameter can be NA.
#' @param event If the model-object is a multistate-model, the event-type
#' needs to be specified, otherwise it can be NA.
#' @param firstRow Typically, a survfit-model does not include data of time=0,
#' if true this function tries to add a column time=0 as a first row.
#' @return Returns a data frame with the times, estimate, upper and lower CI and
#' the n.Risk of one group and one event of a Kaplan Meier estimator or a 
#' competitive risk analysis

extractOneGroupOneEvent <- function(model, group=NA, event=NA, firstRow=TRUE){

  is_multistate  <- inherits(model, "survfitms")
  is_multistrata <- !is.null(model$strata)

  nr_of_events <- function(model){
    ifelse(is_multistate, ncol(model$pstate), 1)
  }

  nr_of_groups <- function(model){
    ifelse(is_multistrata, length(model$strata), 1)
  }

  if (is.na(event) & nr_of_events(model)==1 ) 
    event <- 1
  if (is.na(event) & nr_of_events(model)>1 ) 
    stop("envent-number needs to be specified", call.=FALSE)

  if (is.na(group) & nr_of_groups(model)==1 ) 
    group <- 1
  if (is.na(group) & nr_of_groups(model)>1 ) 
    stop("group-number needs to be specified", call.=FALSE)

  if (event > nr_of_events(model)) 
    stop("selected event not avialable, max event =",
               nr_of_events(model), call.=F)
  if(group > nr_of_groups(model)) 
    stop("selected group not avialable, max group =",
               nr_of_groups(model), call.=F)

  selectEvent <- function(modelElement, type="other"){
    if(is_multistate){
        if(type=="other") return(modelElement[,event+1])
        if(type=="n.risk") return(modelElement[,1])
    }else{
      return(modelElement)
    }
  }

  firstOfStrata <- ifelse(group==1, 1, cumsum(model$strata)[group-1]+1)
  lastOfStrata  <- ifelse(is_multistrata,
                          cumsum(model$strata)[group], length(model$time))
  members <- firstOfStrata:lastOfStrata 
  default.start <- ifelse(is_multistate, 0,1)
  rbind(
    if(firstRow){
      data.frame(time     = 0,
                 estimate = default.start,
                 lower    = default.start,
                 upper    = default.start,
                 n.risk   = model$n.risk[members][1])
    },
    data.frame(  time     = model$time[members],
                 estimate = if(is_multistate){
                              selectEvent(model$pstate)[members]
                            }else{
                              selectEvent(model$surv)[members]
                            },
                 lower    = selectEvent(model$lower)[members],
                 upper    = selectEvent(model$upper)[members],
                 n.risk   = selectEvent(model$n.risk,type="n.risk")[members])
  )
}
