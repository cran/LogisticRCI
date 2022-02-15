\name{RCI}
\alias{RCI}
\encoding{UTF-8}
\title{
Calculate the Linear or Logistic Regression-Based Reliable Change Index (RCI)
}
\description{
This function calculates the RCI for \code{lm} and binomial \code{glm} objects..
}
\usage{
RCI(obj)
}
\arguments{
  \item{obj}{An \code{lm} or binomial \code{glm} object.}
}

\details{ 
This function takes a fitted model object as input and computes either the linear (for \code{lm} objects) or logistic (for binomial \code{glm}) regression-based reliable change index for each observation.
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

linear_fit <- lm(score ~ baseline + age + gender + education_level,
                 data = RCI_sample_data)

logistic_fit <- glm(cbind(score, 15 - score) ~ baseline + age + gender + education_level,
                    family = binomial,
                    data = RCI_sample_data)

linear_RCI <- RCI(linear_fit)
logistic_RCI <- RCI(logistic_fit)

plot(linear_RCI, logistic_RCI)
}