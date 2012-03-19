\name{scree.plot}
\Rdversion{1.1}
\alias{scree.plot}
\title{
Plots the eingenvalues of an MDS analysis
}
\description{
Displays a bar plot of the eigenvalues obtained by MDS
}
\usage{
scree.plot(x, lab = FALSE, title = NULL, xlim = NULL,
    ylim = NULL, new.plot = TRUE, pdf.file = NULL)
}
\arguments{
  \item{x}{
  a numeric vector representing the raw or the relative eigenvalues of each component.
}
  \item{lab}{
  a boolean indicating whether bar labels should be displayed (TRUE) or not (FALSE). Default is FALSE.
}
  \item{title}{
  a character string representing the title of the plot. Default is "Scree plot".  
}
  \item{xlim}{
  a numeric vector representing the range of the x values. Default is full range.  
}
  \item{ylim}{
  a numeric vector representing the range of the y values. Default is full range.  
}
  \item{new.plot}{
  a boolean indicating whether a new graphical device should be created (TRUE) or not (FALSE). Default is TRUE.
}
  \item{pdf.file}{
  a string indicating the name and directory of the pdf graph outfile. Default is NULL. If this parameter is not NULL, the parameter \code{new.plot} is inactivated.
}

}
\details{
A scree plot is a method for determining the optimal number of components useful to describe the data in the context of metric MultiDimensional Scaling (MDS). The scree plot is an histogram showing the eigenvalues of each component. The relative eigenvalues express the ratio of each eigenvalue to the sum of the eigenvalues. The relative eigenvalue of a component gives the proportion of the data variance explained by this component.

The aim is to evaluate the number of components required to capture most information contained in the data. In a scree plot, the relative eigenvalues decrease when the component number increases. The 'elbow' of the plot determines the optimal number of components to describe the data (usually the components before the 'elbow').
}
\value{
Produces a bar plot on the active graphical device.
}
\note{
The scree plot is not an exclusive method to determine the optimal number of components. 
A shepard plot, which is a scatterplot of the scaled MDS distances against the original distance data, can be
another solution. See \code{shepard} function from \code{MASS} package.
}
\author{
Julien Pele
}
\seealso{
\code{plot.pca.scree} function from \code{bio3d} package.\cr
\code{goodness.metaMDS} function from \code{vegan} package.
}
\examples{
# displaying the scree plot of the MDS analysis of human GPCRs
data(gpcr)
active <- gpcr$dif$sapiens.sapiens
mmds1 <- mmds(active, pc = 5)
scree.plot(mmds1$eigen.perc, lab = TRUE, title = "Scree plot of metric MDS")
}
\keyword{plot}
