
#' @title Extends datashield.login
#' @description Executes the login then redefines the data.frame function and sets the name of the server in the respective remote sessions
#' @param ...  the parameters for datashield.login (logins dataframe, etc).
#' See the documentation for that function for details.
#' @details Some functions in the server need  modifications, namely:
#' * The remote server session needs to know its own server name. This functions sets this name in a hidden object called .whoami
#' * Datashield and dssSwissKnife need strings (as well as dates) loaded as factors and not chars (or dates). This function redefines
#' the data.frame function on all the remote sessions in order to achieve this.
#' @return a vector containing the names of all the establised connections (real and fake)
#'
#'@export

datashield.login <- function(...){
  op <- DSI::datashield.login(...)
  sapply(op, function(x){
    expr <- paste0('c("',x@name,'")')
    #datashield.assign.expr(x, 'whoami', quote(expr), async = TRUE)
    datashield.aggregate(x, as.symbol(paste0('setDFdefinition(TRUE,"',x@name, '")')), async = TRUE)
  })

  op
}



