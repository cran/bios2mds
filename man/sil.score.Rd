\name{sil.score}
\Rdversion{1.1}
\alias{sil.score}
\title{
Silhouette score
}
\description{
Computes silhouette scores for multiple runs of K-means clustering.
}
\usage{
sil.score(mat, nb.clus = c(2:13), nb.run = 100, iter.max = 1000,
method = "euclidean")
}
\arguments{
  \item{mat}{
  a numeric matrix representing the coordinates of the elements.
}
  \item{nb.clus}{
  a numeric vector indicating the range of the numbers of clusters. Default is c(2:13). 
}
  \item{nb.run}{
  a numeric value indicating the number of runs. Default is 100. 
}
  \item{iter.max}{
  a numeric value indicating the maximum number of iterations for K-means clustering. Default is 1000. 
}
  \item{method}{
  a string of characters to determine the distance measure. This should be one of "euclidean" , "maximum",
  "manhattan", "canberra" or "binary". Default is "euclidean". 
}
}
\details{
Silhouettes are a general graphical aid for interpretation and validation of cluster analysis.
This technique is available through the \code{silhouette} function (\code{cluster} package). In order to
calculate silhouettes, two types of data are needed:
\itemize{
\item the collection of all distances between objects. These distances are obtained from
application of \code{dist} function on the coordinates of the elements in \code{mat} with argument \code{method}.
\item the partition obtained by the application of a clustering technique. In \code{sil.score}
context, the partition is obtained from the \code{Kmeans} function (\code{amap} package) with argument
\code{method} which indicates the cluster to which each element is assigned.
}

For each element, a silhouette value is calculated and evaluates the degree of confidence in the assignment of the element:
\itemize{
\item well-clustered elements have a score near 1,
\item poorly-clustered elements have a score near -1.
}

Thus, silhouettes indicates the objects that are well or poorly clustered. To summarize the results, for each cluster, the silhouettes values can be displayed as an \bold{average silhouette width}, which is the mean of silhouettes for all the elements assigned to this cluster. Finally, the \bold{overall average silhouette} width is the mean of average silhouette widths of the different clusters.

Silhouette values offer the advantage that they depend only on the partition of the elements. As a consequence, silhouettes can be used to compare the output of the same clustering algorithm applied
to the same data but for different numbers of clusters. A range of numbers of clusters can be tested, with the \code{nb.clus} argument. The optimal number of clusters is reached for the maximum of the overall
silhouette width. This means that the clustering algorithm reaches a strong clustering structure.
However, for a given number of clusters, the cluster assignment obtained by different K-means runs can be different because the K-means procedure assigns random initial centroids for each run. It may be necessary to run the K-means procedure several times, with the nb.run argument, to evaluate the uncertainty of the results. In that case, for each number of clusters, the mean of the overall average silhouettes for \code{nb.run} K-means runs is calculated. The maximum of this core gives the optimal number of clusters.
}
\value{
A named numeric vector representing the silhouette scores for each number of clusters.
}
\note{
\code{sil.score} requires \code{Kmeans} and \code{silhouette} functions from \code{amap} and
\code{cluster} packages, respectively.
}
\author{
Julien Pele
}
\references{
Rousseeuw PJ (1987) Silhouettes: A Graphical Aid to the Interpretation and Validation of
Cluster Analysis. \emph{Journal of Computational and Applied Mathematics}, \bold{20}:53-65.

Lovmar L, Ahlford A, Jonsson M and Syvanen AC (2005) Silhouette scores for assessment
of SNP genotype clusters. \emph{BMC Genomics}, \bold{6}:35.

Guy B, Vasyl P, Susmita D and Somnath D (2008) clValid: An R Package for Cluster Validation.
\emph{Journal of Statistical Software}, \bold{25}.
}
\seealso{
\code{connectivity} and \code{dunn} functions from \code{clValid} package.\cr
\code{silhouette} function from \code{cluster} package.
}
\examples{
# calculating silhouette scores for K-means clustering of human GPCRs:
data(gpcr)
active <- gpcr$dif$sapiens.sapiens
mds <- mmds(active)
sil.score1 <- sil.score(mds$active.coord, nb.clus = c(2:10),
 nb.run = 100, iter.max = 100)
barplot(sil.score1)
}
\keyword{clustering}

