\name{mmds}
\Rdversion{1.1}
\alias{mmds}
\title{
Metric multidimensional scaling
}
\description{
Performs metric MultiDimensional Scaling (MDS) analysis of active elements and projects supplementary elements onto the active space defined by active elements.
}
\usage{
mmds(active, sup = NULL, pc = 3)
}
\arguments{
  \item{active}{
  a numeric matrix of distances between active elements.
}
  \item{sup}{
  a numeric matrix of distances between supplementary and active elements. Default is NULL.
}
  \item{pc}{
  a numeric value indicating the number of principal components to be saved. Default is 3.
}
}
\details{
Metric multidimensional scaling is a statistical analysis technique aimed at analyzing a matrix of distances between 'active' elements. MDS maps the elements onto a low dimensional space (usually 2D or 3D),  In this space, elements are represented by points whose respective distances best approximate the initial distance. 
In addition, after the metric MDS analysis of active elements, the \code{mmds} function allows projecting supplementary elements onto the active space in the context of the R environment. The active space is defined only by the MDS analysis of the active elements. The position of supplementary elements onto the active space depends only on their distances to active elements. 

\code{active} and \code{sup} (if present) must have some characteristics:

\itemize{
\item \code{active} represents the matrix of pairwise distances between active elements. The active matrix must be symmetric (square and equals to its transpose). The distances on the main diagonal must be equal to 0. The distances do not have to be Euclidean. They can just express a difference or dissimilarity score, with a 0 value between same elements and a positive value between different elements.
\item \code{sup} represents the matrix of pairwise distances between supplementary (rows) and active (columns) elements and does not have to be symmetric. The number of supplementary elements may be lower or higher than the number of active objects. The names of supplementary elements must be placed in the left column of \code{sup}.
}
The method for the computation of metric MDS projection of supplementary data is described by Abdi (2007). Briefly, if \emph{N} is the number of active sequences and \emph{D} is the \emph{N} by \emph{N} matrix of the squared distances computed from the active matrix, the \code{mmds} function performs the following steps:

\describe{
  \item{(1)}{
  Transforms \bold{D} into a cross-product matrix \bold{S}:
  \deqn{\bold{S} = -0.5 [\bold{I} - (\bold{1}/N)\bold{1}] \times \bold{D} \times [\bold{I} - (\bold{1}/N)\bold{1}],}
  where \bold{I} is the \emph{N} by \emph{N} identity matrix and \bold{1} is an \emph{N} by \emph{N}
  matrix of ones.
  }
  \item{(2)}{
  Transforms \bold{\emph{S}} into a factor matrix \bold{\emph{F}}:
  \deqn{\bold{F} = \bold{U\Lambda}^{\frac{1}{2}},}
  where \bold{\emph{U}} is the eigenvector matrix and \emph{\eqn{\bold{\Lambda}}} is the diagonal matrix of the
  eigenvalues, such as \bold{\emph{S}} = \emph{\eqn{\bold{{U{\Lambda}U}^T}}}, where \emph{\eqn{\bold{^T}}} denotes the
  transposition operation.
  }
}
The eigenvectors of \bold{\emph{S}}, also called principal components (whose number is smaller or
equal to \bold{\emph{N}}), form the active space. \bold{\emph{F}} gives the coordinates of the
active elements in this space.

If a \code{sup} matrix is given, the supplementary elements are projected onto the active space as described below.
If \bold{\eqn{N_{sup}}} is the number of supplementary sequences and if \bold{\eqn{D_{sup}}} is the \bold{\eqn{N_{sup}}} by \emph{N}
matrix of the squared distances, the \code{mmds} function performs the following steps :
\describe{
  \item{(3)}{
  Transforms \eqn{\bold{D_{sup}}} into a cross-product matrix \eqn{\bold{S_{sup}}}:
  \deqn{\bold{S_{sup}} = -0.5[\bold{I} - (\bold{1}/\bold{N})\bold{1}] \times [\bold{{D_{sup}}^T} - (\bold{1}/\bold{N})\bold{D}\bold{1_{sup}}],}
  where \eqn{\bold{1_{sup}}} is an \eqn{\bold{N_{sup}}} by \bold{N} matrix of ones.
  }
  \item{(4)}{
  Transforms \eqn{\bold{S_{sup}}} into a factor matrix \eqn{\bold{F_{sup}}}:
  \deqn{\bold{F_{sup}} = \bold{{S_{sup}}^T}\bold{F}\bold{\Lambda}^{-1}.}
  \eqn{\bold{F_{sup}}} gives the coordinates of the supplementary elements in the active space.
  }
}
}
\value{
  A object of class 'mmds', which is a named list of four elements:
  \item{eigen}{
  a numeric vector of the eigenvalues.
  }
  \item{eigen.perc}{
  a numeric vector of the relative eigenvalues (eigenvalues divided by the sum of the absolute eigenvalues).
  }
  \item{active.coord}{
  a named numeric matrix representing the coordinates of active elements.
  }
  \item{sup.coord}{
  a named numeric matrix representing the coordinates of supplementary elements on active elements.
  }
}
\note{
If \code{active} and/or \code{sup} do not contain names:\cr
A tag "A" followed by an incremented number names the rows and the columns of \code{active}.\cr 
A tag "S" followed by an incremented number names the rows of \code{sup}.\cr
The columns of \code{sup} are named as the rows of \code{active}. 
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

For further reading on projection of supplementary elements:

Gower JC (1968) adding a point to vector diagrams in multivariate analysis. \emph{Biometrika} \bold{55}:582-585.

Trosset MW, Pribe CE (2008) The out-of-sample problem for classical multidimensional scaling. \emph{Computational statistics & Data analysis} \bold{52}:4635-4642.

Pele J, Abdi H, Moreau M, Thybert D and Chabbert M (2011) Multidimensional scaling reveals the main evolutionary pathways of class A G-protein-coupled receptors. \emph{PloS ONE} \bold{6}:e19094.
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
sup <- gpcr$dif$melanogaster.sapiens
mmds1 <- mmds(active = active, sup = sup)
mmds1$active.coord
}
\keyword{exploratory analysis}
