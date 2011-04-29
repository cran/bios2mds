\name{dis}
\Rdversion{1.1}
\alias{dis}
\title{
Dissimilarity score
}
\description{
Computes the dissimilarity score between two aligned amino acid sequences, based on substitution matrices.
}
\usage{
dis(seq1, seq2, sub.mat.id = "PAM250", gap = NULL)
}
\arguments{
  \item{seq1}{
  a character vector representing a first amino acid sequence.
}
  \item{seq2}{
  a character vector representing a second amino acid sequence.
}
  \item{sub.mat.id}{
  a string of characters indicating the amino acid substitution matrix to be taken into 
  account for calculation. This should be one of "PAM40", "PAM120", "PAM250", "BLOSUM45", "BLOSUM62", "BLOSUM80", "PAM250TM" and "PHAT". Default is PAM250. See \code{\link{sub.mat}}. 
}
  \item{gap}{
  a numeric vector of length 2, indicating the penalty for initiating and extending a strand of two gaps (gap ignored, default).
}
}
\details{
Grishin and Grishin (2002) developped a method to calculate the similarity score with amino acid
substitution matrices.

Let \bold{\emph{s}} be an amino acid substitution matrix with elements \emph{s(a, b)},
let \bold{\emph{A}} be an alignment of \emph{n} sequences, \emph{\eqn{A_{ik}}} is a symbol (amino acid
or gap: '-') in the site \emph{k} of the sequence \emph{i}. For each pair of sequences \emph{i} and
\emph{j} from \bold{\emph{A}}, the following equations\bold{(1)}, \bold{(2)}, \bold{(3)} and \bold{(4)} are calculated as follows:

\describe{
  \item{(1)}{
  \emph{\eqn{S_{ij}}}, called score per site, is obtained as:
  \deqn{S_{ij} = \sum_{k \in K_{ij}}s(A_{ik}, A_{jk})/l(K_{ij})}
  where \eqn{K_{ik}} is the set of sites \emph{k} such that \eqn{A_{ik}} \eqn{!=} '-' and \eqn{A_{jk}} \eqn{!=} '-'
  and \emph{\eqn{l(K_{ij})}} is the number of elements in \eqn{K_{ij}}.
  }
  \item{(2)}{
  \emph{\eqn{T_{ij}}}, called average upper limit of the score per site, is obtained as:
  \deqn{T_{ij} = 0.5 \sum_{k \in K_{ij}}(s(A_{ik}, A_{ik}) + s(A_{jk}, A_{jk}))/l(K_{ij})}
  }
  \item{(3)}{
  \emph{\eqn{S^{rand}_{ij}}}, called score per site expected from random sequences, is obtained as:
  \deqn{S^{rand}_{ij} = \sum^{20}_{a=1}\sum^{20}_{b=1}f^i_j(a)f^j_i(b)s(a, b)}
  where \eqn{f^i_j(a)} is the frequency of amino acid '\emph{a}' in \eqn{i^{th}} protein sequence of \bold{\emph{A}}
  over all sites in \emph{\eqn{K_{ij}}}.
  }
  \item{(4)}{
  \emph{\eqn{V_{ij}}}, called normalized score (Feng and Doolittle, 1997), is obtained as:
  \deqn{V_{ij} = \frac{S_{ij} - S^{rand}_{ij}}{T_{ij} - S^{rand}_{ij}}}
  }
The normalized score \emph{\eqn{V_{ij}}} ranges from 0 (for random sequences) to 1 (for identical
sequences). However, for very divergent sequences, \emph{\eqn{V_{ij}}} can become negative due to
statistical errors. In this case, \code{dis} attributes 0 to negative scores.

The dissimilarity score \emph{\eqn{D_{ij}}} between sequences \emph{i} and \emph{j} is obtained from the similarity score as:

\deqn{D_{ij} = V_{ij} - 1}
}
}
\value{
A single numeric value representing the dissimilarity score.
}
\note{
The computation time for \code{dis} is higher than for \code{\link{dif}} because of the increased algorithm complexity.
}
\author{
Julien Pele
}
\references{
Grishin VN and Grishin NV (2002) Euclidian space and grouping of biological objects.
\emph{Bioinformatics} \bold{18}:1523-1534.

Feng DF and Doolittle RF (1997) Converting amino acid alignment scores into measures of
evolutionary time: a simulation study of various relationships. \emph{J Mol Evol}  
\bold{44}:361-370.
}


\examples{
# calculating dis between the sequences of CLTR1_HUMAN and CLTR2_HUMAN:
aln <- read.fasta(system.file("msa/human_gpcr.fa", package = "bios2mds"))
dis <- dis(aln$CLTR1_HUMAN, aln$CLTR2_HUMAN)
dis
}
\keyword{distance}

