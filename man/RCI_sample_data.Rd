\name{RCI_sample_data}
\alias{RCI_sample_data}
\encoding{UTF-8}
\docType{data}
\title{
Sample Data for RCI Calculation
} 
\description{
This dataset is a random sample of 200 patients from a study on cognitive decline. No patient is identifiable. Random noise was added to the age, baseline and score variables, and the education level variable was generalised.
}
\usage{data("RCI_sample_data")}
\format{
  A data frame with 200 observations on the following 5 variables:
  \describe{
    \item{\code{age}}{The patient's age.}
    \item{\code{gender}}{A factor with two levels: "male" or "female".}
    \item{\code{score}}{The score obtained after 6 months.}
    \item{\code{baseline}}{The score obtained at the start of the study.}
    \item{\code{education_level}}{A factor with 4 levels: "4 to 7 years", "8 to 11 years", "12 to 15 years", and "16 to 20 years".}
  }
}


\examples{
data(RCI_sample_data)
}
\keyword{datasets}
