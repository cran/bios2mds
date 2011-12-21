\name{mat.dif}
\Rdversion{1.1}
\alias{mat.dif}
\title{
Matrices of difference scores between sequences 
}
\description{
Computes a matrix providing the distances based on the difference scores between sequences from two multiple sequence alignments.
}
\usage{
mat.dif(align1, align2, gap = FALSE, aa.strict = FALSE, sqrt = FALSE)
}
\arguments{
  \item{align1}{
  a list of character vectors representing a first multiple sequence aligment.
}
  \item{align2}{
  a list of character vectors representing a second multiple sequence aligment.
}
  \item{gap}{
  a logical value indicating whether gap character should be taken as supplementary symbol
  (TRUE) or not (FALSE). Default is FALSE.
}
  \item{aa.strict}{
  a logical value indicating whether only strict amino acids should be taken into account (TRUE)
  or not (FALSE). To be used only for amino acid sequences. Default is FALSE. 
}
  \item{sqrt}{
  a logical value indicating whether the distance should be equal to the squared root of the difference score (TRUE) or not
  (FALSE). Default is FALSE.
}
}
\details{
If \code{align1} and \code{align2} are identical, \code{mat.dif} computes the symetrical matrix of distances between each sequence of the alignment.

Before using \code{mat.dif}, users must check the alignment of sequences within \code{align1} and \code{align2} and between \code{align1} and \code{align2}.

}
\value{
A named numeric matrix providing the difference-based distances between each pair of sequences from \code{align1} and \code{align2}. The number of rows and columns is identical to the number of sequences in \code{align1} and \code{align2}, respectively.
}
\author{
Julien Pele and Jean-Michel Becu
}
\seealso{
\code{identity} function from \code{bio3d} package.
}
\examples{
# calculating the matrix of distances based on the difference scores
# between GPCRs sample from H. sapiens and D. melanogaster:
aln_human <- import.fasta(system.file("msa/human_gpcr.fa", package = "bios2mds"))
aln_drome <- import.fasta(system.file("msa/drome_gpcr.fa", package = "bios2mds"))
mat.dif1 <- mat.dif(aln_human[1:5], aln_drome[1:5])
mat.dif1
}
\keyword{distance}

