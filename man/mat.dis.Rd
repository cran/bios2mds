\name{mat.dis}
\Rdversion{1.1}
\alias{mat.dis}
\title{
Matrices of dissimilarity scores between amino acid sequences
}
\description{
Computes a matrix providing the distances based on dissimilarity scores between sequences from two multiple sequence alignments.
}
\usage{
mat.dis(align1, align2, sub.mat.id = "PAM250", sqrt=FALSE)
}
\arguments{
  \item{align1}{
  a list of character vectors representing a first multiple sequence aligment.
}
  \item{align2}{
  a list of character vectors representing a second multiple sequence aligment.
}
  \item{sub.mat.id}{
  a string of characters indicating the amino acid substitution matrix used for calculation 
of the dissimilarity score. This should be one of "PAM40", "PAM80", "PAM120", "PAM160", "PAM250", "BLOSUM30", "BLOSUM45", "BLOSUM62", "BLOSUM80", "GONNET", "JTT", "JTT_TM" and "PHAT". 
The supported substitution matrices are in \code{\link{sub.mat}}. Default is PAM250.
}
  \item{sqrt}{
  a logical value indicating whether the distance should be equal to the squared root of the difference score (TRUE) or not (FALSE). Default is FALSE.
}
}

\details{
The dissimilarity score between a sequence \emph{i} from \code{align1} and a sequence \emph{j} from \code{align2} is calculated with an amino acid substitution matrix from \code{sub.mat}. 

If \code{align1} and \code{align2} are identical, \code{mat.dis} computes the symetrical matrix of distances between each sequence of the alignment.

Before using \code{mat.dis}, users must check the alignment of sequences within \code{align1} and \code{align2} and between \code{align1} and \code{align2}.

}
\value{
A named numeric matrix providing the dissimilarity-based distances between each pair of sequences from \code{align1} and \code{align2}, based on the substitution matrix \code{sub.mat.id}. The number of rows and columns is identical to the number of sequences in \code{align1} and \code{align2}, respectively.
}
\author{
Julien Pele and Jean-Michel Becu
}
\examples{
# calculating dissimilarity distances between GPCR sequences sample from 
#H. sapiens and D. melanogaster, based on the PAM250 matrix:
aln_human <- import.fasta(system.file("msa/human_gpcr.fa", package = "bios2mds"))
aln_drome <- import.fasta(system.file("msa/drome_gpcr.fa", package = "bios2mds"))
mat.dis1 <- mat.dis(aln_human[1:5], aln_drome[1:5])
mat.dis1

# calculating dissimilarity distances between GPCRs sequences sample from 
#H. sapiens and D. melanogaster, based on the BLOSUM45 matrix:
aln_human <- import.fasta(system.file("msa/human_gpcr.fa", package = "bios2mds"))
aln_drome <- import.fasta(system.file("msa/drome_gpcr.fa", package = "bios2mds"))
mat.dis1 <- mat.dis(aln_human[1:5], aln_drome[1:5], sub.mat.id = "BLOSUM45")
mat.dis1
}
\keyword{distance}

