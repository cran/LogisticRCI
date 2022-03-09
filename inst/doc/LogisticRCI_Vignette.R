## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(LogisticRCI)

## ---- eval = FALSE------------------------------------------------------------
#  install.packages("LogisticRCI")
#  library("LogisticRCI")

## -----------------------------------------------------------------------------
data("RCI_sample_data")
head(RCI_sample_data)

## -----------------------------------------------------------------------------
linear_fit <- lm(score ~ baseline + age + gender + education,
                 data = RCI_sample_data)

logistic_fit <- glm(cbind(score, 15 - score) ~ baseline + age + gender + education,
                    family = binomial,
                    data = RCI_sample_data)

## -----------------------------------------------------------------------------
anova(linear_fit)

anova(logistic_fit, test = "Chisq")

## -----------------------------------------------------------------------------
linear_RCI <- RCI(linear_fit)

logistic_RCI <- RCI(logistic_fit)

## -----------------------------------------------------------------------------
shapiro.test(logistic_RCI)

## -----------------------------------------------------------------------------
sum(logistic_RCI < -1.64)

## -----------------------------------------------------------------------------
which(logistic_RCI < -1.64)

## -----------------------------------------------------------------------------
new_patient <- data.frame("age" = 68,
                          "gender" = "male",
                          "score" = 9,
                          "baseline" = 11,
                          "education" = 12)

## -----------------------------------------------------------------------------
RCI_newpatient(model = linear_fit, new = new_patient)
RCI_newpatient(model = logistic_fit, new = new_patient)

