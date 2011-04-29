\name{dif}
\Rdversion{1.1}
\alias{dif}
\title{
Difference score
}
\description{
Measures the difference score between two aligned amino acid or nucleotide sequences.
}
\usage{
dif(seq1, seq2, gap = FALSE, aa.strict = FALSE)
}
\arguments{
  \item{seq1}{
  a character vector representing a first sequence.
}
  \item{seq2}{
  a character vector representing a second sequence.
}
  \item{gap}{
  a boolean indicating whether the gap character should be taken as a supplementary symbol
  (TRUE) or not (FALSE). Default is FALSE.
}
  \item{aa.strict}{
  a boolean indicating whether only strict amino acids should be taken into account (TRUE)
  or not (FALSE). Default is FALSE. 
}
}
\details{
The difference score between two aligned sequences is given by the proportion of sites that differs and is equivalent to \eqn{1 - {PID}} (percent identity). 
\code{dif} is given by the number of aligned positions (sites) whose symbols differ, divided by the number of aligned positions. \code{dif} is equivalent to the \emph{p} distance defined by Nei and Zhang (2006).
In \code{dif}, positions with at least one gap can be excluded (gap = FALSE). When gaps are taken as a supplementary symbol (gap = TRUE), sites with gaps in both sequences are excluded.

From Nei and Zhang (2006), the \emph{p} distance, which is the proportion of sites that differ between
two sequences, is estimated by:

\deqn{{p} = \frac{n_d}{n},}

where \emph{n} is the number of sites and \emph{\eqn{n_d}} is the number of sites with different symbols.

The difference score ranges from 0, for identical sequences, to 1, for completely 
different sequences.
}
\value{
A single numeric value representing the difference score.
}

\author{
Julien Pele
}
\references{
May AC (2004) Percent sequence identity: the need to be explicit. \emph{Structure}
\bold{12}:737-738.

Nei M and Zhang J (2006) Evolutionary Distance: Estimation.
\emph{Encyclopedia of Life Sciences}.

Nei M and Kumar S (2000) Molecular Evolution and Phylogenetics.
\emph{Oxford University Press}, New York.
}
\examples{
# calculating the difference score between the sequences 
# of CLTR1_HUMAN and CLTR2_HUMAN:
aln <- read.fasta(system.file("msa/human_gpcr.fa", package = "bios2mds"))
dif <- dif(aln$CLTR1_HUMAN, aln$CLTR2_HUMAN)
dif
}
\keyword{distance}

