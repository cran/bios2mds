\name{mmds.3D.plot}
\Rdversion{1.1}
\alias{mmds.3D.plot}
\title{
Display the mmds coordinates onto a 3D space
}
\description{
Displays a 3D plot of the active elements and, if present, of supplementary elements, after a metric MDS analysis.
}
\usage{
mmds.3D.plot(x, project = NULL, title = NULL, axis = c(1:3), 
active.type = "s", sup.type = "p", active.size = 2, radius = 0.005,
sup.size = 10, active.col = x$col[,3], sup.col = project$col[,3], 
box = TRUE, axes = TRUE, new.plot = TRUE, label = TRUE,
xlim = NULL, ylim = NULL, zlim = NULL, box.lwd = 2,
box.antialias = TRUE, ...)
}
\arguments{
  \item{x}{
  an object of class 'mmds', obtained from \code{\link{mmds}} function.
}
  \item{title}{
  a string of characters representing the title of the plot. Default is "Metric MDS".  
}
  \item{project}{
  an object of class 'project', obtained from \code{\link{mmds.project}} function, or a list of object of class 'project'.
}
  \item{axis}{
  a numeric vector of length three representing the principal components displayed on the plot. Default is c(1:3). 
}
  \item{active.type}{
  an character indicating the symbol of active elements. This should be one of "s" for spheres, "p" for points, "l" for lines, "h" for line segments from z=0 and "n" for none. Default is "s", corresponding to spheres.
}
  \item{sup.type}{
  an character indicating the symbol of supplementary elements. This should be one of "s" for spheres, "p" for points, "l" for lines, "h" for line segments from z=0 and "n" for none. Default is "p", corresponding to points.
}
  \item{active.col}{
  a string of characters or character vector representing the color(s) of the active elements. Default is x$col[,3]. 
  It corresponds either to the user-provided colors if the \code{col.group} function has been used previously, 
  or to black (filled automaticaly with the \code{mmds} function). 
}
  \item{sup.col}{
  a string or character vector representing the color(s) of supplementary elements. Default is project$col[,3]. 
  It corresponds either to the user-provided colors if the \code{col.group} function has been used previously, 
  or to magenta (filled automaticaly with the \code{mmds} function)
}
  \item{active.size}{
  a numeric value indicating the size of active symbols. Default is 2.
}
  \item{sup.size}{
  a numeric value indicating the size of supplementary symbols. Default is 20. 
}
  \item{box}{
  a boolean indicating whether the box should be displayed (TRUE) or not (FALSE). Default is TRUE.
}
  \item{axes}{
  a boolean indicating whether axes should be displayed (TRUE) or not (FALSE). Default is TRUE. 
}
  \item{radius}{
  a numeric value indicating the radius of spheres symbols only. If x.type equal to "s" the x.size parameter was inactivated. Default is 0.01.
}
  \item{new.plot}{
  a boolean indicating whether a new 3D plot create/replace active 3D device (TRUE) or not to insert in it (FALSE). Default is TRUE.
}
  \item{label}{
  a boolean indicating whether the label axes should be displayed (TRUE) or not (FALSE). Default is TRUE.
}
  \item{xlim}{
  a numeric vector representing the range for the x values. Default is full range.
}
  \item{ylim}{
  a numeric vector representing the range for the y values. Default is full range.
}
  \item{zlim}{
  a numeric vector representing the range for the z values. Default is full range.
}
  \item{box.lwd}{
  a numeric value indicating the width of box and axes lines. Default is 2.
}
  \item{box.antialias}{
  a boolean specifying if box and axes lines should be antiliased. Default is TRUE.
}
  \item{...}{
  additional parameters which will be passed to \code{par3d}, \code{material3d} and \code{decorate3d} of \code{rgl} package.
}

}
\details{
If \code{mmds.3D.plot} is used after the \code{col.group} function, 
the elements are colored by the color scheme provided in the .csv file (see \code{col.group} for details).
If the \code{col.group} function has not been used, the default colors are black and magenta for active and supplementary elements.

\code{mmds.3D.plot} helps identify patterns in data and compare active and supplementary elements.

}
\value{
Produces a 3D plot on graphical device.
}
\author{
Jean-Michel Becu
}
\note{
\code{mmds.3D.plot} requires \code{plot3D} function from \code{rgl} package.
See \code{rgl} documentation to supplementary function like \code{snapshot3D} to save image file of 3D device.
}
\examples{
# 3D plot of human GPCRs onto the first three axes obained from MDS analysis
# with projection of GPCRs from D. melanogaster as supplementary elements:
data(gpcr)
mmds.3D.plot(gpcr$mmds$sapiens.active,active.type="p",label=FALSE,lit=FALSE,
point_antialias=TRUE,box.lwd=3,sup.size=4.3,active.size=4.3)
bbox3d(shininess=0.5)

}
\keyword{plot, 3D}
