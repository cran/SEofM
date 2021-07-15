\name{SEM}
\alias{SEM}
\title{Standard Error of Measurement}
\description{
To calculate the standard error of measurement (SEM) to assess the observer variability.
}
\usage{
SEM(subject,measurement,observer,value)
}
\arguments{
  \item{subject}{the index of the subjects, e.g., 1,2,3...;1,2,3...}
  \item{measurement}{the index of the measurements, e.g., 1,1,1...;2,2,2...}
  \item{observer}{the index of the observers, e.g., 1,1,1...;2,2,2...}
  \item{value}{the value of the subjects estimated by the observers using the measurements}
}
\value{
  \item{SEMintra}{SEM for intra-observer variation}
  \item{SEMinter.fixed}{SEM for inter-observer variation}
  \item{SEMinter.random}{SEM for inter-observer variation, which is almost always used than SEMinter.fixed}
}
\note{
Please feel free to contact us, if you have any advice and find any bug!

Reference:

1. Zoran B. Popović, James D. Thomas (2017) Assessing observer variability: a user’s guide, Cardiovascular Diagnosis and Therapy, 7(3): 317-324, DOI: 10.21037/cdt.2017.03.12

Update:

Version 0.1.0: The first version.

}
\examples{
data(SEMSample)
value=SEMSample$value
observer=SEMSample$observer
subject=SEMSample$patient
measurement=SEMSample$measurement
SEM(subject,measurement,observer,value)
}
