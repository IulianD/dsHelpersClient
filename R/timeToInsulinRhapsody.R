#' @title  Time to insulin
#' @description Implements the criterion for the clinical endpoint for time to insulin, namely the earlier of:
#'
#' 1) starting sustained (more than 6 months by default) insulin treatment
#'
#' 2) 2 or more HBA1C measurements higher than 8.5\% (default), more than 3 months apart (default) when on 2 or more non-insulin therapies
#' @param symbol the name of the new dataframe
#' @param treatment.df the name of the treatment dataframe (CM table)
#' @param lab.df the name of the lab measures dataframe (LB table)
#' @param diag.df the name of the diagnosis dataframe (MH table)
#' @param treatments a list with exactly one named element (the default name CMCAT is the treatment column name in treatment.df  and
#' should be correct in most cases ). The element must be a vector with one ore more treatments present in treatment.df$CMCAT (again, the default should suffice)
#' @param trt.date.col the name of the date column in treatment.df. The default (CMDTC) should be OK in all cohorts with the exception of dcs where the name is CMSTDTC
#' @param lab.date.col the name of the date column in lab.df. The default (LBDTC) should be OK in all cohorts.
#' @param diag.date.col the name of the date column in diag.df. The default (MHDTC) should be OK in all cohorts.
#' @param occur.col a list with exactly one element whose name must match the name fo the occurence column in treatment.df ('CMOCCUR' should be fine everywhere).
#' The element must be a list with exactly 2 elements, the encoding of the 'yes' and 'no' values. Again the defaults should be fine.
#' If in doubt, ds.levels(treatment.df$CMOCCUR) or even better ds2.show.factors(treatment.df) should give the necessary info.
#' @param diag.occur.col a list exactly like the one above, this time with the column name and the levels from diag.df
#' @param diag.visit a character, what visit should we consider for diagnosis.
#' @param diagnosis a character, name of the diagnosis, the default should be fine for all cohorts
#' @param min.treatment.days how many days on insulin (criteria 1 above)
#' @param labtest a list with exactly one element containing the name of the column pointing to exactly one value - the name of the test (HBA1C in most cases)
#' @param min.measurement a list with exactly one element containing exactly one value: name of the column in lab.df pointing to the minimum value (default 77 ~ 8.5\% from the second criterion above)
#' @param measure a list with exactly one element containing exactly one value: name of the unit column in lab.df pointing to the unit used (default mmol/m)
#' @param min.days.apart an integer, how many days apart the measurements of HBA1C (second criterion above)
#' @param criteria a character in c('both', 'first', 'second'). Which of the criteria above should we apply?
#' @param async same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return It doesn't return anything,  it creates a filtered dataframe on the remote node with the columns SUBJID, DAYS_TO_INSULIN, CASE
#' (the CASE column is always 1 and it serves to further modeling with clogit)
#'
#' @export
#'
dssTimeToInsulin <- function(symbol, treatment.df , lab.df , diag.df ,  treatments = list(CMCAT = "INSULINS AND ANALOGUES"), trt.date.col = 'CMDTC', lab.date.col = 'LBDTC', diag.date.col = 'MHDTC',
                                occur.col  =  list(CMOCCUR = list(yes = 'Y', no = 'N')),  diag.occur.col = list(MHOCCUR = list(yes = 'Y', no = 'N')), diag.visit = 'DIAGNOSIS',
                                diagnosis = list(MHTERM = "TYPE 2 DIABETES"), min.treatment.days = 180,
                                labtest = list(LBTESTCD = 'HBA1C'), min.measurement = list(LBORRES = 77), measure = list(LBORRESU = "mmol/m"), min.days.apart = 90,
                                criteria = 'both', async = TRUE, datasources = NULL){
  if(is.null(datasources)){
    datasources <- dssSwissKnifeClient::dsBaseClient_findLoginObjects()
  }


  actual.args <- as.list(match.call())[-1]
  arglist <- RCurl::merge.list(actual.args,formals())
  arglist[c('symbol', 'async', 'datasources')] <- NULL # we don't need to send these ones
  arglist <- sapply(arglist, eval, USE.NAMES = TRUE, simplify = FALSE)
  arglist <- dssSwissKnifeClient:::.encode.arg(arglist)
  cally <- paste0('timeToInsulin("', arglist, '")')
  datashield.assign(datasources, symbol = symbol, value = as.symbol(cally), async = async)

}

#' @title  Monotherapy no insulin
#' @description Implements the controls for time to insulin, defined as a diet or monotherapy without insulin treatment at 5 years (default) from diagnosis
#' @param symbol the name of the new dataframe
#' @param treatment.df the name of the treatment dataframe (CM table)
#' @param diag.df the name of the diagnosis dataframe (MH table)
#' @param insulin.treatments  encoding of the insulin treatments to be excluded. A list with exactly one named element (the default name CMCAT is the treatment column name in treatment.df  and
#' should be correct in most cases ). The element must be a vector with one ore more treatments present in treatment.df$CMCAT (again, the default should suffice)
#' @param trt.date.col the name of the date column in treatment.df. The default (CMDTC) should be OK in all cohorts with the exception of dcs where the name is CMSTDTC
#' @param diag.date.col the name of the date column in diag.df. The default (MHDTC) should be OK in all cohorts.
#' @param occur.col a list with exactly one element whose name must match the name fo the occurence column in treatment.df ('CMOCCUR' should be fine everywhere).
#' The element must be a list with exactly 2 elements, the encoding of the 'yes' and 'no' values. Again the defaults should be fine.
#' If in doubt, ds.levels(treatment.df$CMOCCUR) or even better ds2.show.factors(treatment.df) should give the necessary info.
#' @param diag.occur.col a list exactly like the one above, this time with the column name and the levels from diag.df
#' @param diag.visit a character, what visit should we consider for diagnosis.
#' @param diagnosis a character, name of the diagnosis, the default should be fine for all cohorts
#' @param years.after.diagnosis a number what is the cutoff point? (default 5 years)
#' @param days.offset a number, how far to look around the cutoff point, in days, default one month
#' @param async same as in datashield.assign
#' @param datasources same as in datashield.assign
#' @return It doesn't return anything,  it creates a filtered dataframe on the remote node with the columns SUBJID, DIFF_DAYS, CASE
#' (the CASE column is always 0 and it serves to further modeling with clogit)
#'
#' @export
#'
ds2MonotherapyNoInsulin <- function(symbol, treatment.df, diag.df ,  insulin.treatments = list(CMCAT = "INSULINS AND ANALOGUES"), trt.date.col = 'CMDTC', diag.date.col = 'MHDTC',
                                       occur.col  =  list(CMOCCUR = list(yes = 'Y', no = 'N')),  diag.occur.col = list(MHOCCUR = list(yes = 'Y', no = 'N')), diag.visit = 'DIAGNOSIS',
                                       diagnosis = list(MHTERM = "TYPE 2 DIABETES"), years.after.diagnosis = 5, days.offset = 30,
                                       async = TRUE,  datasources = NULL){
  if(is.null(datasources)){
    datasources <- dssSwissKnifeClient::dsBaseClient_findLoginObjects()
  }


  actual.args <- as.list(match.call())[-1]
  arglist <- RCurl::merge.list(actual.args,formals())
  arglist[c('symbol', 'async', 'datasources')] <- NULL # we don't need to send these ones
  arglist <- sapply(arglist, eval, USE.NAMES = TRUE, simplify = FALSE)
  arglist <- dssSwissKnifeClient:::.encode.arg(arglist)
  cally <- paste0('monotherapyNoInsulin("', arglist, '")')
  datashield.assign(datasources, symbol = symbol, value = as.symbol(cally), async = async)
}
