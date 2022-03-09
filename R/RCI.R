## incomplete beta function
ibeta <- Vectorize(function(x, a = 2/3, b = 2/3) {
  pbeta(x, a, b) * beta(a, b)
}, "x")

## Logistic-RCI
RCI_binomial <- function(model) {
  fam <- model$family$family
  link <- model$family$link
  if(fam != "binomial" | link != "logit") {
    stop("Only implemented for binomial models with a logit link.")
  }
  X <- model.matrix(model)
  disp <- summary(model)$dispersion
  beta <- coef(model)
  m <- model$prior.weights
  p <- model$y
  p_hat <- plogis(as.numeric(X %*% beta))
  numerator <- ibeta(p) - ibeta(p_hat - (1 - 2 * p_hat)/(6 * m))
  denominator <- (p_hat * (1 - p_hat))^(-1/6)
  score <- sqrt(m) * numerator/denominator
  return(score)
}

## Linear-RCI
RCI_linear <- function(model) {
  m <- model.matrix(model)
  numerator <- residuals(model, type = "response")
  sigma <- summary(model)$sigma
  h <- influence(model)$h
  denominator <- sigma
  score <- numerator/denominator
  return(score)
}

## Wrapper
RCI <- function(model) {
  if(class(model)[1] == "glm") {
    return(RCI_binomial(model))
  } else if(class(model)[1] == "lm") {
    return(RCI_linear(model))
  } else {
    stop("Only implemented for lm and binomial glm objects.") 
  }
}

## New patients
RCI_binomial_newpatient <- function(model, new) {
  fam <- model$family$family
  link <- model$family$link
  if(fam != "binomial" | link != "logit") {
    stop("Only implemented for binomial models with a logit link.")
  }
  disp <- summary(model)$dispersion
  if(is.null(new$total)) {
    m <- model$prior.weights[1]
  } else {
    m <- new$total 
  }
  p <- as.numeric(new[,all.vars(formula(model))[1]])/m
  p_hat <- predict(model, new, type = "response")
  numerator <- ibeta(p) - ibeta(p_hat - (1 - 2 * p_hat)/(6 * m))
  denominator <- (p_hat * (1 - p_hat))^(-1/6)
  score <- sqrt(m) * numerator/denominator
  return(as.numeric(score))
}

RCI_linear_newpatient <- function(model, new) {
  yhat <- predict(model, new)
  y <- as.numeric(new[,all.vars(formula(model))[1]])
  numerator <- y - yhat
  sigma <- summary(model)$sigma
  h <- influence(model)$h
  denominator <- sigma
  score <- numerator/denominator
  return(as.numeric(score))
}

RCI_newpatient <- function(model, new) {
  if(class(model)[1] == "glm") {
    return(RCI_binomial_newpatient(model, new))
  } else if(class(model)[1] == "lm") {
    return(RCI_linear_newpatient(model, new))
  } else {
    stop("Only implemented for lm and binomial glm objects.") 
  }
}