\name{extract.cluster}
\Rdversion{1.1}
\alias{extract.cluster}
\title{
Extraction of clusters alignments
}
\description{
Extracts the multiple sequence alignement of each cluster after K-means clustering.
}
\usage{
extract.cluster(x, align)
}
\arguments{
  \item{x}{
  an object of class 'kmean', obtained from \code{\link{kmeans.run}} function.
}
  \item{align}{
  an object of class 'align', obtained from \code{\link{import.fasta}} or \code{\link{import.msf}} function.  
}
}
\details{
  Extraction of the MSA of each cluster.
}
\value{
A named list of 'align' objects.
}
\author{
Jean-Michel Becu
}

\examples{
# Clustering human GPCRs in 4 groups with 100 runs of K-means 
# and extraction of the alignment of each cluster
aln <- import.fasta(system.file("msa/human_gpcr.fa", package = "bios2mds"))
data(gpcr)
kmeans <- kmeans.run(gpcr$mmds$sapiens.active$coord, nb.clus = 4, nb.run = 100)
clusAlign <- extract.cluster(kmeans,aln)

}
\keyword{extraction}

