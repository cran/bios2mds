\name{mmds}
\Rdversion{1.1}
\alias{mmds}
\title{
Metric multidimensional scaling
}
\description{
Performs metric MultiDimensional Scaling (MDS) analysis of active elements.
}
\usage{
mmds(active, pc = 3, group.file = NULL)
}
\arguments{
  \item{active}{
  a numeric matrix of distances between active elements.
  }
  \item{pc}{
  a numeric value indicating the number of principal components to be saved. Default is 3.
  }
  \item{group.file}{
  a string of characters to indicate the file name assigning groups and colors to each active element.
  }
}
\details{
Metric multidimensional scaling is a statistical analysis technique aimed at analyzing a matrix of distances between 'active' elements. MDS maps the elements onto a low dimensional space (usually 2D or 3D),  In this space, elements are represented by points whose respective distances best approximate the initial distance. 

\code{active} must have some characteristics:

\itemize{
\item \code{active} represents the matrix of pairwise distances between active elements. The active matrix must be symmetric (square and equals to its transpose). The distances on the main diagonal must be equal to 0. The distances do not have to be Euclidean. They can just express a difference or dissimilarity score, with a 0 value between same elements and a positive value between different elements.
}
The method for the computation of metric MDS is described by Abdi (2007). Briefly, if \emph{N} is the number of active sequences and \emph{D} is the \emph{N} by \emph{N} matrix of the squared distances computed from the active matrix, the \code{mmds} function performs the following steps:

\describe{
  \item{(1)}{
  Transforms \bold{D} into a cross-product matrix \bold{S}:
  \deqn{\bold{S} = -0.5 [\bold{I} - \bold{m}^{T}\bold{1}] \times \bold{D} \times [\bold{I} - \bold{m}^{T}\bold{1}]^{T}}
  where \bold{I} is the \emph{N} by \emph{N} identity matrix, \bold{m} is the \emph{N} by \emph{1} matrix mass, where each mass equal to \emph{\eqn{\frac{1}{N}}} and \bold{1} is an \emph{N} by \emph{N} matrix of ones.
  }
  \item{(2)}{
  Transforms \bold{\emph{S}} into a factor matrix \bold{\emph{F}}:
  \deqn{\bold{F} = \bold{M}^{\frac{-1}{2}}\bold{U\Lambda}^{\frac{1}{2}}}
  where \bold{M} is the diag\{\bold{m}\}, \bold{\emph{U}} is the eigenvector matrix and \emph{\eqn{\bold{\Lambda}}} is the diagonal matrix of the eigenvalues, such as \bold{\emph{S}} = \emph{\eqn{\bold{{U{\Lambda}U}^T}}}, where \emph{\eqn{\bold{^T}}} denotes the transposition operation.
  }
}
The eigenvectors of \bold{\emph{S}}, also called principal components (whose number is smaller or
equal to \bold{\emph{N}}), form the active space. \bold{\emph{F}} gives the coordinates of the
active elements in this space.
}
\value{
  A object of class 'mmds', which is a named list of five elements:
  \item{eigen}{
  a numeric vector of the eigenvalues.
  }
  \item{eigen.perc}{
  a numeric vector of the relative eigenvalues (eigenvalues divided by the sum of the absolute eigenvalues).
  }
  \item{coord}{
  a named numeric matrix representing the coordinates of active elements.
  }
  \item{group}{
  a named string matrix representing the differents groups of elements and the associate color (Default is 'NoGroup' and 'black').
  }
  \item{col}{
  a named string matrix representing, foreach named elements, the associate group and color (Default is 'NoGroup' and 'black').
  }
  \item{source}{
  a named list with 2 elements, the matrix (\bold{D}) of squared distance and the vector of mass \bold{m}, that will used for projection of supplementary elements, if required, with the \code{mmds.project} function.
}

}
\note{
If \code{active} do not contain names:\cr
A tag "A" followed by an incremented number names the rows and the columns of \code{active}.\cr 
}
\author{
Julien Pele and Jean-Michel Becu
}
\references{
Abdi H (2007) Metric multidimensional scaling. In N.J. Salkind (Ed.):
\emph{Encyclopedia of Measurement and Statistics.} Thousand Oaks (CA): Sage. pp. 598-605.

For further information on multidimentional scaling:

Takane Y, Jung S and Oshima-Takane Y (2009) Multidimensional scaling, in \emph{Handbook
of quantitative methods in psychology}, eds Millsap R, Maydeu-Olivares A (Sage
Publications, London) pp. 219-242.

Borg I and Groenen PJF (2005) Modern multidimensional scaling.
New York : Springer.

Gower JC (1967) A comparison of some methods of cluster analysis.
\emph{Biometrics} \bold{23}:623-637.

ToRgerson WS (1958) Theory and methods of scaling.
New York : Wiley.

}


\seealso{
\code{cmdscale} function from \code{stats} package.\cr
\code{dudi.pco}, \code{suprow} and \code{supcol} functions from \code{ade4} package.\cr
\code{PCA} function from \code{FactoMineR} package.
}
\examples{
# performing metric MDS of human GPCRs with projection of
# GPCRs from D. melanogaster as supplementary elements:
data(gpcr)
active <- gpcr$dif$sapiens.sapiens
mmds1 <- mmds(active = active)
mmds1$active.coord
}
\keyword{exploratory analysis}
