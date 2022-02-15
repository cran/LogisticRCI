## incomplete beta function
ibeta <- Vectorize(function(x, a = 2/3, b = 2/3) {
  pbeta(x, a, b) * beta(a, b)
}, "x")

## Logistic-RCI
RCI_binomial <- function(obj) {
  fam <- obj$family$family
  link <- obj$family$link
  if(fam != "binomial" | link != "logit") {
    stop("Only implemented for binomial models with a logit link.")
  }
  X <- model.matrix(obj)
  disp <- summary(obj)$dispersion
  beta <- coef(obj)
  m <- obj$prior.weights
  p <- obj$y
  p_hat <- plogis(as.numeric(X %*% beta))
  numerator <- ibeta(p) - ibeta(p_hat - (1 - 2 * p_hat)/(6 * m))
  denominator <- (p_hat * (1 - p_hat))^(-1/6)
  score <- sqrt(m) * numerator/denominator
  return(score)
}

## Linear-RCI
RCI_linear <- function(obj) {
  m <- model.matrix(obj)
  numerator <- residuals(obj, type = "response")
  sigma <- summary(obj)$sigma
  h <- influence(obj)$h
  denominator <- sigma
  score <- numerator/denominator
  return(score)
}

## Wrapper
RCI <- function(obj) {
  if(class(obj)[1] == "glm") {
    return(RCI_binomial(obj))
  } else if(class(obj)[1] == "lm") {
    return(RCI_linear(obj))
  } else {
    stop("Only implemented for lm and binomial glm objects.") 
  }
}