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