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
mat.dis(align1, align2, sub.mat.id = "PAM250", gap = NULL)
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
of the dissimilarity score. This should be one of "PAM40", "PAM120", "PAM250", "BLOSUM45", "BLOSUM62", "BLOSUM80", "PAM250TM" and "PHAT". 
The supported substitution matrices are in \code{\link{sub.mat}}. Default is PAM250.
}
  \item{gap}{
 a numeric vector of length two, indicating the penalty for initiating and extending a gap. Default is NULL (gap ignored). 
}
}

\details{
The dissimilarity score between a sequence \emph{i} from \code{align1} and a sequence \emph{j} from \code{align2} is calculated by the \code{\link{dis}} function with an amino acid substitution matrix from \code{sub.mat}. 
To build the matrix of dissimilarity-based distances, \code{\link{mat.dis}} first compares \code{align1} and \code{align2}, then build the matrix:
\itemize{
\item if \code{align1} and \code{align2} are identical (see identical function from base package), the matrix of distances is symmetric (and necessarily square). Thus, \code{\link{mat.dis}} will only calculate dissimilarity scores for each pair \emph{(i, j)} with \emph{i} < \emph{j}. Its main diagonal will contain 0 values. The matrix is equivalent to the matrix of dissimilarity between sequences from one multiple alignment file. 
\item if \code{align1} and \code{align2} are different, the matrix of dissimilarity is not square. \code{\link{mat.dis}} will calculate the dissimilarity scores for every pair \emph{(i, j)} from \code{align1} and \code{align2}.
}
}
\value{
A named numeric matrix providing the dissimilarity-based distances between each pair of sequences from \code{align1} and \code{align2}, based on the substitution matrix \code{sub.mat.id}. The number of rows and columns is identical to the number of sequences in \code{align1} and \code{align2}, respectively.
}
\author{
Julien Pele
}
\note{
\code{mat.dis} requires \code{combinations} function from \code{gtools} package.
}
\examples{
# calculating dissimilarity distances between GPCR sequences sample from 
#H. sapiens and D. melanogaster, based on the PAM250 matrix:
aln_human <- read.fasta(system.file("msa/human_gpcr.fa", package = "bios2mds"))
aln_drome <- read.fasta(system.file("msa/drome_gpcr.fa", package = "bios2mds"))
mat.dis1 <- mat.dis(aln_human[1:5], aln_drome[1:5])
mat.dis1

# calculating dissimilarity distances between GPCRs sequences sample from 
#H. sapiens and D. melanogaster, based on the BLOSUM45 matrix:
aln_human <- read.fasta(system.file("msa/human_gpcr.fa", package = "bios2mds"))
aln_drome <- read.fasta(system.file("msa/drome_gpcr.fa", package = "bios2mds"))
mat.dis1 <- mat.dis(aln_human[1:5], aln_drome[1:5], sub.mat.id = "BLOSUM45")
mat.dis1
}
\keyword{distance}

