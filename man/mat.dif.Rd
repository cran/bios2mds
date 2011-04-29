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
mat.dif(align1, align2, gap = FALSE, aa.strict = FALSE)
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
}
\details{
The difference score between a sequence \emph{i} from \code{align1} and a sequence \emph{j} from \code{align2} is calculated by the function \code{\link{dif}}. 
To build the matrix of difference-based distances, \code{mat.dif} first compares \code{align1} and \code{align2}, then build the matrix:
\itemize{
\item if \code{align1} and \code{align2} are identical (see identical function from base package), the matrix of difference scores is symmetric (and necessarily square). Thus, \code{mat.dif} will only calculate difference scores for each pair \emph{(i, j)} with \emph{i} < \emph{j}. Its main diagonal will contain 0 values. The matrix is equivalent to the matrix of difference scores between sequences from a single multiple sequence alignment file. 
\item if \code{align1} and \code{align2} are different, the matrix of difference scores is not square. \code{mat.dif} will calculate the difference score for every pair \emph{(i, j)} from \code{align1} and \code{align2}.
}

Before using \code{mat.dif}, users must check the alignment of sequences within \code{align1} and \code{align2} and between \code{align1} and \code{align2}.

}
\value{
A named numeric matrix providing the difference-based distances between each pair of sequences from \code{align1} and \code{align2}. The number of rows and columns is identical to the number of sequences in \code{align1} and \code{align2}, respectively.
}
\author{
Julien Pele
}
\note{
\code{mat.dif} requires \code{combinations} function from \code{gtools} package.
}
\seealso{
\code{identity} function from \code{bio3d} package.
}
\examples{
# calculating the matrix of distances based on the difference scores
# between GPCRs sample from H. sapiens and D. melanogaster:
aln_human <- read.fasta(system.file("msa/human_gpcr.fa", package = "bios2mds"))
aln_drome <- read.fasta(system.file("msa/drome_gpcr.fa", package = "bios2mds"))
mat.dif1 <- mat.dif(aln_human[1:5], aln_drome[1:5])
mat.dif1
}
\keyword{distance}

