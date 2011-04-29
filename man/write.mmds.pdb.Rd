\name{write.mmds.pdb}
\Rdversion{1.1}
\alias{write.mmds.pdb}
\title{
Writes PDB file}
\description{
Writes MDS coordinates in the Protein Data Bank format for visualization with a molecular graphics viewer.
}
\usage{
write.mmds.pdb(x, axis = c(1, 2, 3), file.pdb = "R.pdb")
}
\arguments{
  \item{x}{
  an object of class 'mmds', obtained from \code{\link{mmds}} function.
}
  \item{axis}{
  a numeric vector of length three the principal components to be displayed. Default is c(1, 2, 3). 
}
  \item{file.pdb}{
  a string of characters indicating the output PDB file name.
}
}
\details{
The elements can be visualized in three dimensions (3D) with a molecular viewer as Pymol or Rasmol. 
If \code{x} contains active and supplementary elements, the active and supplementary elements are numbered from 1 and from 5001, respectively. If group is not NULL, the assignment of an element to a group is indicated by the chain name from \emph{A} for the first group to \emph{Z} when the maximum number of groups, 26, is reached. 
}
\value{
Produces a PDB file from the MDS coordinates, with the elements numbered in the order of the MSA file and the groups corresponding to the chain numbers.
}
\references{
\url{http://www.wwpdb.org/docs.html}
}
\author{
Julien Pele and Jean-Michel Becu
}
\seealso{
\code{write.pdb} function from \code{bio3d} package.
}
\examples{
# writing the first three MDS coordinates of human GPCRs in a PDB file 
data(gpcr)
write.mmds.pdb(gpcr$mmds)
}
\keyword{write}

