\name{nifti.image.write}
\alias{nifti.image.write}
\title{Write data to NIfTI (Analyze) files}
\description{
  This function writes volume data to a NIfTI file. The filename for the header/image file is determined from the nifti object and can be set by using the \code{\link{nifti.set.filenames}} function.
}
\usage{
nifti.image.write(nim) 
}
\arguments{
  \item{nim}{the nifti object}
}
\references{
  \url{http://nifti.nimh.nih.gov}
  \url{http://niftilib.sourceforge.net}
}
\author{
  Oliver Granert <o.granert <at> neurologie.uni-kiel.de>
}

\seealso{
  \code{\link{nifti.set.filenames}},
  \code{\link{nifti.image.read}},
  \code{\link{nifti.image.new}}
}

\examples{
\dontrun{
nim <- nifti.image.read(file.choose())
nifti.image.write(nim)
}
}
\keyword{IO}
