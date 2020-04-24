
#' @title Extends datashield.login from package dsSwissKnife
#' @description Executes the login then redefines the data.frame function and sets the name of the server in the respective remote sessions
#' @param ...  the parameters for dsSwissKnifeClient::datashield.login (logins dataframe, etc).
#' See the documentation for that function for details.
#' @details Some functions in the server dssHelpers package need  modifications, namely:
#' * The remote server session needs to know its own server name. This functions sets this name in a hidden object called .whoami
#' * Datashield and dssSwissKnife need strings (as well as dates) loaded as factors and not chars (or dates). This function redefines
#' the data.frame function on all the remote sessions in order to achieve this.
#' @return a vector containing the names of all the establised connections (real and fake)
#'
#'@export

datashield.login <- function(...){
  op <- dsSwissKnifeClient::datashield.login(...)
  sapply(op, function(x){
    expr <- paste0('c("',x$name,'")')
    datashield.assign(x, '.whoami', as.symbol(expr), async = TRUE, wait = FALSE) # poll later
    datashield.aggregate(x, quote(setDFdefinition(TRUE)), async = TRUE, wait = FALSE) # poll later
  })
# now poll:
  dsSwissKnifeClient::dssWaitForAsyncs(op, 2)
  op
}



