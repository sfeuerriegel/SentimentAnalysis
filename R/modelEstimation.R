#' Lasso estimation
#' 
#' Function estimates coefficients based on LASSO regularization.
#' @param x An object of type \code{\link[tm]{DocumentTermMatrix}}.
#' @param response Response variable including the given gold standard. 
#' @param control (optional) A list of parameters defining the LASSO model as follows:
#' \itemize{
#'  \item{"s"}{Value of the parameter lambda at which the LASSO is evaluated. Default
#' is \code{s="lambda.1se"} which takes the calculated minimum value for \eqn{\lambda} 
#' and then subtracts one standard error in order to avoid overfitting. This often
#' results in a better performance than using the minimum value itself given by 
#' \code{lambda="lambda.min"}.}
#'  \item{"family"}{Distribution for response variable. Default is \code{family="gaussian"}.
#' For non-negative counts, use \code{family="poisson"}. For binary variables
#' \code{family="binomial"}. See \code{\link[glmnet]{glmnet}} for further details.}
#'  \item{"grouped"} {Determines whether grouped LASSO is used (with default \code{FALSE}).}
#' }
#' @param ... Additional parameters passed to function for \code{\link[glmnet]{glmnet}}.
#' @return Result is a list with coefficients, coefficient names and the model intercept.
#' @importFrom stats coef
#' @export
lassoEstimation <- function(x, response, control = list(alpha=1, s="lambda.min", family="gaussian", grouped=FALSE), ...) {
  control$alpha <- 1
  
  if (!"s" %in% names(control)) {
    control$s <- "lambda.min"
  }
  if (!"family" %in% names(control)) {
    control$family <- "gaussian"
  }
  if (!"grouped" %in% names(control)) {
    control$grouped <- FALSE
  }
  
  return(enetEstimation(x, response, control))
}

#' Ridge estimation
#' 
#' Function estimates coefficients based on ridge regularization.
#' @param x An object of type \code{\link[tm]{DocumentTermMatrix}}.
#' @param response Response variable including the given gold standard. 
#' @param control (optional) A list of parameters defining the model as follows:
#' \itemize{
#'  \item{"s"}{Value of the parameter lambda at which the ridge is evaluated. Default
#' is \code{s="lambda.1se"} which takes the calculated minimum value for \eqn{\lambda} 
#' and then subtracts one standard error in order to avoid overfitting. This often
#' results in a better performance than using the minimum value itself given by 
#' \code{lambda="lambda.min"}.}
#'  \item{"family"}{Distribution for response variable. Default is \code{family="gaussian"}.
#' For non-negative counts, use \code{family="poisson"}. For binary variables
#' \code{family="binomial"}. See \code{\link[glmnet]{glmnet}} for further details.}
#'  \item{"grouped"} {Determines whether grouped function is used (with default \code{FALSE}).}
#' }
#' @param ... Additional parameters passed to function for \code{\link[glmnet]{glmnet}}.
#' @return Result is a list with coefficients, coefficient names and the model intercept.
#' @importFrom stats coef
#' @export
ridgeEstimation <- function(x, response, control = list(s="lambda.min", family="gaussian", grouped=FALSE), ...) {
  control$alpha <- 0

  if (!"s" %in% names(control)) {
    control$s <- "lambda.min"
  }
  if (!"family" %in% names(control)) {
    control$family <- "gaussian"
  }
  if (!"grouped" %in% names(control)) {
    control$grouped <- FALSE
  }
  
  return(enetEstimation(x, response, control))
}

#' Elastic net estimation
#' 
#' Function estimates coefficients based on elastic net regularization.
#' @param x An object of type \code{\link[tm]{DocumentTermMatrix}}.
#' @param response Response variable including the given gold standard. 
#' @param control (optional) A list of parameters defining the model as follows:
#' \itemize{
#'  \item{"alpha"}{Abstraction parameter for switching between LASSO 
#'  and ridge regularization (with default \code{alpha=0.5}). 
#'  Best option is to loop over this parameter and  test different alternatives.}
#'  \item{"s"}{Value of the parameter lambda at which the elastic net is evaluated. Default
#' is \code{s="lambda.1se"} which takes the calculated minimum value for \eqn{\lambda} 
#' and then subtracts one standard error in order to avoid overfitting. This often
#' results in a better performance than using the minimum value itself given by 
#' \code{lambda="lambda.min"}.}
#'  \item{"family"}{Distribution for response variable. Default is \code{family="gaussian"}.
#' For non-negative counts, use \code{family="poisson"}. For binary variables
#' \code{family="binomial"}. See \code{\link[glmnet]{glmnet}} for further details.}
#'  \item{"grouped"} {Determines whether grouped function is used (with default \code{FALSE}).}
#' }
#' @param ... Additional parameters passed to function for \code{\link[glmnet]{glmnet}}.
#' @return Result is a list with coefficients, coefficient names and the model intercept.
#' @importFrom stats coef
#' @export
enetEstimation <- function(x, response, control = list(alpha=0.5, s="lambda.min", family="gaussian", grouped=FALSE), ...) {
  if (!"alpha" %in% names(control)) {
    control$alpha <- 0.5
  }
  if (!"s" %in% names(control)) {
    control$s <- "lambda.min"
  }
  if (!"family" %in% names(control)) {
    control$family <- "gaussian"
  }
  if (!"grouped" %in% names(control)) {
    control$grouped <- FALSE
  }
  
  cv.enet <- glmnet::cv.glmnet(as.matrix(x), response, alpha=control$alpha, family=control$family, grouped=control$grouped, ...)
  coefs <- coef(cv.enet, s=control$s)
  
  scoreNames <- coefs@Dimnames[[1]][setdiff(coefs@i+1, 1)]
  scores <- coefs@x
  if (length(coefs@i) > 0 && coefs@i[1]==0) {
    scores <- scores[-1]
  }
  intercept <- ifelse(length(coefs@i) > 0 && coefs@i[1]==0,
                      coefs@x[1],
                      0)
  out <- list(ScoreNames = scoreNames, 
              Scores = scores, 
              Intercept = intercept)
  return(out)
}

#' Ordinary least squares estimation
#' 
#' Function estimates coefficients based on ordinary least squares.
#' @param x An object of type \code{\link[tm]{DocumentTermMatrix}}.
#' @param response Response variable including the given gold standard. 
#' @param control (optional) A list of parameters (not used).
#' @param ... Additional parameters (not used).
#' @return Result is a list with coefficients, coefficient names and the model intercept.
#' @importFrom stats coef lm
#' @export
lmEstimation <- function(x, response, control = list(), ...) {
  lm <- lm(response ~ as.matrix(x), ...)
  coefs <- coef(lm)
  
  scoreNames <- colnames(x)
  
  scores <- as.numeric(coefs)[-1]
  intercept <- as.numeric(coefs[1])
  
  out <- list(ScoreNames = scoreNames, 
              Scores = scores, 
              Intercept = intercept)
  return(out)
}

#' Estimation via generalized least squares
#' 
#' Function estimates coefficients based on generalized least squares.
#' @param x An object of type \code{\link[tm]{DocumentTermMatrix}}.
#' @param response Response variable including the given gold standard. 
#' @param control (optional) A list of parameters defining the model as follows:
#' \itemize{
#'  \item{"family"}{Distribution for response variable. Default is \code{family="gaussian"}.
#' For non-negative counts, use \code{family="poisson"}. For binary variables
#' \code{family="binomial"}. See \code{\link{glm}} for further details.}
#' }
#' @param ... Additional parameters passed to function for \code{\link{glm}}.
#' @return Result is a list with coefficients, coefficient names and the model intercept.

#' @return Result is a list with coefficients, coefficient names and the model intercept.
#' @importFrom stats coef glm
#' @export
glmEstimation <- function(x, response, control = list(family = "gaussian"), ...) {
  if (!"family" %in% names(control)) {
    control$family <- "gaussian"
  }  
  
  lm <- glm(response ~ as.matrix(x), family = control$family, ...)
  coefs <- coef(lm)
  
  scoreNames <- colnames(x)[-1]
  
  scores <- as.numeric(coefs)[-1]
  intercept <- as.numeric(coefs[1])
  
  out <- list(ScoreNames = scoreNames, 
              Scores = scores, 
              Intercept = intercept)
  return(out)
}

#' Spike-and-slab estimation
#'
#' Function estimates coefficients based on spike-and-slab regression.
#' @param x An object of type \code{\link[tm]{DocumentTermMatrix}}.
#' @param response Response variable including the given gold standard. 
#' @param control (optional) A list of parameters defining the LASSO model. Default is\code{n.iter1=500} and \code{n.iter2=500}.
#' See \code{\link[spikeslab]{spikeslab}} for details.
#' @param ... Additional parameters passed to function for \code{\link[spikeslab]{spikeslab}}.
#' @return Result is a list with coefficients, coefficient names and the model intercept.
#' @export
spikeslabEstimation <- function(x, response, control = list(n.iter1 = 500, n.iter2 = 500), ...) {
  if (!"n.iter1" %in% names(control)) {
    control$n.iter1 <- 500
  }
  if (!"n.iter2" %in% names(control)) {
    control$n.iter2 <- 500
  }
  
  spikeslab.mod <- spikeslab::spikeslab(response ~ ., as.data.frame(as.matrix(x)), 
                                        n.iter1 = control$n.iter1, n.iter2 = control$n.iter2, verbose = F, ...)
  coefs <- spikeslab.mod$gnet.scale[spikeslab.mod$gnet.scale != 0]
  
  out <- list(ScoreNames = names(coefs), 
              Scores = unname(coefs), 
              Intercept = attributes(spikeslab.mod$terms)$intercept)
  return(out)
}

#' Estimation method
#'
#' Decides upon a estimation method for dictionary generation. Input is a name for the estimation method, output is the corresponding function object.
#'
#' @param type A string denoting the estimation method. Allowed values are \code{lasso}, \code{ridge},
#' \code{enet}, \code{lm}, \code{glm} or \code{spikeslab}.
#' @return Function that implements the specific estimation method.
#' @export
lookupEstimationMethod <- function(type) {
  if (type == "lasso") {
    return(lassoEstimation)
  } else if (type == "ridge") {
    return(ridgeEstimation)
  } else if (type == "enet") {
    return(enetEstimation)
  } else if (type == "lm") {
    return(lmEstimation)
  } else if (type == "glm") {
    return(glmEstimation)
  } else if (type == "spikeslab") {
    return(spikeslabEstimation)
  }
  
  stop("Name of estimated method not recognized. Corresponding argument has an invalid value.")
}
