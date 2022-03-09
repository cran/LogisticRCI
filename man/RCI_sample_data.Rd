\name{RCI_sample_data}
\alias{RCI_sample_data}
\encoding{UTF-8}
\docType{data}
\title{
Sample Data for RCI Calculation
} 
\description{
This dataset is a simulated sample of 100 patients from a study on cognitive decline.
}
\usage{data("RCI_sample_data")}
\format{
  A data frame with 100 observations on the following 5 variables:
  \describe{
    \item{\code{age}}{The patient's age.}
    \item{\code{gender}}{A factor with two levels: "male" or "female".}
    \item{\code{score}}{The score obtained after 6 months.}
    \item{\code{baseline}}{The score obtained at the start of the study.}
    \item{\code{education}}{Number of years of education.}
  }
}


\examples{
data(RCI_sample_data)
}
\keyword{datasets}
