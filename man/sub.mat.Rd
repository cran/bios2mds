\name{sub.mat}
\Rdversion{1.1}
\alias{sub.mat}
\docType{data}
\title{
Amino acid substitution matrices
}
\description{
Contains eight amino acid substitution matrices, imported from version 9.1 of the aaindex2 database
and PAM matrix calculator of Wageningen Bioinformatics Webportal.
}
\usage{data(sub.mat)}
\format{
A named list with eight elements corresponding to a 20 by 20 named matrix. Rows and columns names
correspond to the twenty strict amino acids.
}
\details{
\describe{
  \item{PAM40}{
  matrix was produced by "pam" Version 1.0.7 
  }
  \item{PAM120}{
  matrix was produced by "pam" Version 1.0.7
  }
  \item{PAM160}{
  matrix was produced by "pam" Version 1.0.7 
  }
  \item{PAM250}{
  log odds matrix for 250 PAMs (Dayhoff et al., 1978)
  }
  \item{BLOSUM30}{
  substitution matrix (Henikoff-Henikoff, 1992)
  }
  \item{BLOSUM45}{
  substitution matrix (Henikoff-Henikoff, 1992)
  }
  \item{BLOSUM62}{
  substitution matrix (Henikoff-Henikoff, 1992)
  }
  \item{BLOSUM80}{
  substitution matrix (Henikoff-Henikoff, 1992)
  }
  \item{GONNET}{
  substitution matrix (GONNET et al., 1992)
  }
  \item{JTT}{
  substitution matrix (Jones et al., 1992)
  }
  \item{JTT_TM}{
  transmembrane protein exchange matrix (Jones et al., 1994)
  }
  \item{PHAT}{
  substitution matrix built from hydrophobic and transmembrane regions of the Blocks database (Ng et al., 2000)
  }
}
}
\source{
The matrices were downloaded from the AAindex database at
\url{http://www.genome.jp/aaindex} or were calculated on the PAM server at \url{http://www.bioinformatics.nl/tools/pam.html}.
}

\references{
Kawashima S and Kanehisa M (2000) AAindex: amino acid index 
database. \emph{Nucleic Acids Res} \bold{28}:374.

Dayhoff MO, Schwartz R and Orcutt BC (1978) A model of Evolutionary Change in Proteins. 
Atlas of protein sequence and structure (volume 5, supplement 3 ed.). Nat. Biomed. Res. Found.. pp. 345-358. 

Henikoff S and Henikoff JG (1992) Amino acid substitution matrices from protein blocks.
\emph{Proc Natl Acad Sci U S A} \bold{89}:10915-9. 

Jones DT, Taylor WR and Thornton JM (1992) The rapid generation of mutation data matrices
from protein sequences. \emph{Comput Appl. Biosci} \bold{8}:275-282. 

Jones DT, Taylor WR and Thornton JM (1994) A mutation data matrix for transmembrane
proteins. \emph{FEBS Lett} \bold{339}:269-75. 

Ng PC, Henikoff JG and Henikoff S (2000) PHAT: a transmembrane-specific substitution
matrix. Predicted hydrophobic and transmembrane. \emph{Bioinformatics} \bold{16}:760-6.

Gonnet GH, Cohen MA and Benner SA (1992) Exhaustive matching of the entire protein sequence database.
\emph{Science} \bold{256}:1443-1445.

}
\examples{
# loading
data(sub.mat)

# displaying PAM40:
sub.mat$PAM40
}
\keyword{datasets}
