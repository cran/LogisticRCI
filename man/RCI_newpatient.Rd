\name{RCI_newpatient}
\alias{RCI_newpatient}
\encoding{UTF-8}
\title{
Calculate the Linear or Logistic Regression-Based Reliable Change Index (RCI) for a New Patient Based on a Fitted Model
}
\description{
This function calculates the RCI for a new patient based on a fitted \code{lm} or binomial \code{glm} model object.
}
\usage{
RCI_newpatient(model, new)
}
\arguments{
  \item{model}{An \code{lm} or binomial \code{glm} object.}
  \item{new}{A data frame with data for the new patient.}
}

\details{ 
This function takes a fitted model object and new patient data as input and computes either the linear (for \code{lm} objects) or logistic (for binomial \code{glm}) regression-based reliable change index. The names of the variables in the new patient data have to match the names of the predictors and response variable for the fitted model.
}
\value{
The function returns a numeric vector.
}
\references{
Moral, R.A., Diaz-Orueta, U., Oltra-Cucarella, J. (preprint) Logistic versus linear regression-based Reliable Change Index: implications for clinical studies with diverse sample sizes. DOI: 10.31234/osf.io/gq7az
}
\author{
Rafael A. Moral, Unai Diaz-Orueta and Javier Oltra-Cucarella.
} 
\examples{
data(RCI_sample_data)

## fitting models to sample
linear_fit <- lm(score ~ baseline + age + gender + education,
                 data = RCI_sample_data)

logistic_fit <- glm(cbind(score, 15 - score) ~ baseline + age + gender + education,
                    family = binomial,
                    data = RCI_sample_data)

## new patient data
new_patient <- data.frame("age" = 68,
                          "gender" = "male",
                          "score" = 9,
                          "baseline" = 11,
                          "education" = 12)

## calculating RCI for new patient without refitting model
RCI_newpatient(model = linear_fit, new = new_patient)
RCI_newpatient(model = logistic_fit, new = new_patient)
}