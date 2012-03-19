\name{kmeans.run}
\Rdversion{1.1}
\alias{kmeans.run}
\title{
Multiple runs of K-means analysis
}
\description{
Performs multiple runs of K-means clustering and analyzes data.
}
\usage{
kmeans.run(mat, nb.clus = 2, nb.run = 1000, iter.max = 10000,
method = "euclidean")
}
\arguments{
  \item{mat}{
  a numeric matrix representing the coordinates of the elements after metric MDS analysis.
}
  \item{nb.clus}{
  a numeric value indicating the number of clusters. Default is 2. 
}
  \item{nb.run}{
  a numeric value indicating the number of runs. Default is 1000.
}
  \item{iter.max}{
  a numeric value indicating the maximum number of iterations for K-means. Default is 10000. 
}
  \item{method}{
  a string of characters to determine the distance to be used. This should be one of "euclidean", "maximum",
  "manhattan", "canberra", "binary", "pearson", "correlation", "spearman" or "kendall". Default is "euclidean".
}
}
\details{
The aim of K-means clustering is the partition of elements into a user-provided number of clusters. Several runs of K-means analysis on the same data may return different cluster assignments because the K-means procedure attributes random initial centroids for each run. The robustness of an assignment depends on its reproducibility. 

The function \code{matchClasses} from the \code{e1071} package is used to compare the cluster assignments of the different runs and returns a score of agreement between them. The most frequent clustering solution is selected and used as a reference to assess the reproducibility of the analysis.

\code{kmeans.run} returns two lists. In either list, the clusters refer to those observed in the most frequent solution. The first list provides, for each element, the relative ratio of its assignment to each cluster in the different runs. The second list provides, for each cluster, the list of the assigned elements along with the relative assignment to this cluster in the different runs. 
}
\value{
 A object of class 'kmean', which is a named list of two elements
  \item{elements}{
  a named list of elements with the relative assignment of each element to each cluster.
  }
  \item{clusters}{
  a named list of clusters with the elements assigned to this cluster in the most frequent solution and their relative assignment to this cluster in multiple runs. 
  }
}
\author{
Julien Pele
}
\note{
During the K-means procedure, an empty cluster can be obtained if no objects are allocated to the
cluster. In \code{kmeans.run}, runs with empty clusters are discarded. 

\code{kmeans.run} requires \code{Kmeans} and \code{matchClasses} functions from \code{amap}
and \code{e1071} packages, respectively.
}
\examples{
# Clustering human GPCRs in 4 groups with 100 runs of K-means
data(gpcr)
coord <- gpcr$mmds$sapiens.active$coord
kmeans.run1 <- kmeans.run(coord, nb.clus = 4, nb.run = 100)
kmeans.run1$clusters
kmeans.run1$elements
}
\keyword{clustering}

