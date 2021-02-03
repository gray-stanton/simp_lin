#' Fit simple linear regression model
#' 
#' @param x Vector of covariate observations
#' @param y Vector of response observations
#' @return List of model components
#' @export
simp_lin_R  <- function(x, y){
  if(!is.vector(x) || !is.vector(y)){
    stop("x and y must be vectors!")
  }
  if(!is.numeric(x) || !is.numeric(y)){
    stop("x and y must be numeric!")
  }
  if(length(x) != length(y)){
    stop("x and y must have the same length!")
  }
  if(length(x) == 0){
    stop("There must be at least one observation!")
  }
  if(var(x) == 0){
    stop("x cannot be constant")
  }
  output <- simp_lin_cpp(x, y)
  return(output)
}

#' Simulate a dataset and run simple linear regression
#'
#' @param n "Number of observations to simulate"
#' @param method "Which regression function to use, either 'lm' or 'simplin'
#' @param id "run id number
#' @param outputprefix "prefix of output file name"
#' @param writeout "Whether or not to write a summary of the results to a csv file"
#' @export
simulate <- function(n, method="lm", id=0, outputprefix="run_", writeout=TRUE){
  inittime <- proc.time()[3]
  x <- rnorm(n, 0, 1)
  err <- rnorm(n, 0, 1)
  y <- 1 + -1*x + err
  if(method=="lm"){
    fit <- lm(y ~ x)
    sumfit <- summary(fit)
    cis <- confint(fit)
    output <- list(b0=sumfit$coefficients[1,1],
                   b1=sumfit$coefficients[2,1],
                   b0_se=sumfit$coefficients[1,2],
                   b1_se=sumfit$coefficients[2,2],
                   b0_ci=c(cis[1,1], cis[1,2]),
                   b1_ci=c(cis[2,1], cis[2,2]),
                   resids=resid(fit),
                   yhat=fitted(fit))
  }
  else{
    output <- simp_lin_R(x, y)
  }
  elapsed_time <- proc.time()[3] - inittime
  outdf <- data.frame(method=method, id=id, runtime=elapsed_time,
                      b0=output[["b0"]], b1=output[["b1"]],
                      b0_lower=output[["b0_ci"]][1], b0_upper= output[["b0_ci"]][2],
                      b1_lower=output[["b1_ci"]][1], b1_upper= output[["b1_ci"]][2],
                      pred_mse = mean((output[["yhat"]] - output[["resids"]])^2)
  )
  if(writeout){
    outfile <- paste0("./", outputprefix, id, ".csv")
    write.csv(outdf, outfile)
    return(outdf)
  } else{
    return(outdf)
  }

}