\name{export.fasta}
\Rdversion{1.1}
\alias{export.fasta}
\title{
Writes a MSA file in FASTA format
}
\description{
Writes a multiple sequence alignment (MSA) file in FASTA format.
}
\usage{
export.fasta(x, outfile = "alignment.fa", ncol = 60, open = "w")
}
\arguments{
  \item{x}{
  an object of class 'align', obtained from \code{\link{import.fasta}} or \code{\link{import.msf}}, or an element list from the \code{\link{extract.cluster}} function return.
  }
  \item{outfile}{
  a string of characters or string vector to indicate the name of the MSA file(s) to be written. If x is an object of class 'align', default is "alignment.fa". If x is an element list, for each element in x, default is the element name, followed by the ".alignement.fa" extension.
}
  \item{ncol}{
  an integer value indicating the number of characters per line for the sequences in outfile.Default is 60. 
}
  \item{open}{
  a character value indicating the opening mode for outfile. This should be either of "w" to write into
  a new file, or "a" to append at the end of an already existing file. Default is "w".
}
}
\details{
Initially, FASTA (for FAST-ALL) was the input format of the FASTA program, used for protein comparison and searching in databases.
Presently, FASTA format is a standard format for biological sequences.
  
The FASTA formatted file of a single sequence displays:
\itemize{
\item a single-line description beginning with a greater-than (>) symbol. The following word is the identifier.
\item followed by any number of lines, representing biological sequence.
}
For multiple alignments, the FASTA formatted sequences are concatenated to create a multiple
FASTA format.
}
\value{
Produces a FASTA file for an 'align' object or a FASTA file for each cluster in list.
}
\author{
Jean-Michel Becu
}
\note{
For further information about FASTA format, see: \url{http://www.ncbi.nlm.nih.gov/BLAST/fasta.shtml}

}
\seealso{
\code{write.fasta} function from \code{seqinr} package.\cr
}
\examples{
# reading of the multiple sequence alignment of human GPCRS in FASTA format:
wd <- tempdir()
#wd <- getwd()
file1 <- file.path(wd,"alignment.fa")
aln <- import.fasta(system.file("msa/human_gpcr.fa", package = "bios2mds"))
export.fasta(aln,file1)
}

