\name{mmds.plot}
\Rdversion{1.1}
\alias{mmds.plot}
\title{
Plots a summary of the mmds results
}
\description{
Displays one scree plot and three scatter plots of \code{\link{mmds}} results.
}
\usage{
mmds.plot(x, new.plot = TRUE, pdf.file = NULL)
}
\arguments{
  \item{x}{
  an object of class 'mmds', obtained from \code{\link{mmds}}.
}
  \item{new.plot}{
  a boolean indicating whether a new graphical device should be created (TRUE) or not (FALSE). Default is TRUE.
}
  \item{pdf.file}{
  a string indicating the name and directory of the pdf graph outfile. Default is NULL. If this parameter is not
NULL, the parameter \code{new.plot} is inactivated.
}

}
\details{
\code{mmds.plot} is a wrapper calling of both \code{\link{scree.plot}} and \code{\link{mmds.2D.plot}}.
It produces a 2x2 plot with one scree plot of the relative eigenvalues, in the upper left, and three scatter plots. The three scatter plots are generated as follows:
\itemize{
\item scatter plot of the elements on the first and second components in the upper right.
\item scatter plot of the elements on the first and third components in the lower left.
\item scatter plot of the elements on the second and third components in the lower right.
}
If object \code{x} contains supplementary elements, they are also projected onto the three scatter plots.
The active and supplementary elements are represented by dots and crosses, respectively.
The \code{color.group} function may be used before calling \code{mmds.plot} to color elements by user-provided groups. 
}
\value{
Produces a summary plot of the MDS analysis on the same active graphical device.
}
\note{
The scatter plots can display supplementary objects if their coordinates are present in \code{x} input.
}
\author{
Julien Pele and Jean-Michel Becu
}
\seealso{
\code{plot.pca} function in \code{bio3d} package.
}
\examples{
# summary plot of the MDS analysis of human GPCRs with projection of GPCRs from
# D. melanogaster as supplementary elements:
data(gpcr)
mmds.plot(gpcr$mmds)
}
\keyword{plot}

