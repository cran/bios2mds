\name{gpcr}
\Rdversion{1.1}
\alias{gpcr}
\docType{data}
\title{
Pre-analyzed G-Protein-Coupled Receptor (GPCR) data set
}
\description{
This data set was obtained by the \code{bios2mds} analysis of the two multiple sequence alignment files provided in /msa. The MSA files were prepared as previously described by Deville et al. (2009) and updated with the July 2009 release of Uniprot (\url{http://www.uniprot.org}). They correspond to non-redundant sets of non-olfactory class A G-protein-coupled receptors (GPCRs) from \emph{H. sapiens} and \emph{D. melanogaster} (283 and 59 sequences, respectively). The data sets from \emph{H. sapiens} and \emph{D. melanogaster} constitute the active and supplementary data sets, respectively. 
}
\usage{data(gpcr)}
\format{
\code{gpcr} is a named list of three elements:
\describe{
  \item{dif}{
  a named list containing two distance matrices calculated from the \code{\link{mat.dif}} function (distances based on difference scores) with default parameters:
  \describe{
  \item{sapiens.sapiens}{ a 283 by 283 matrix of difference scores between the 283 aligned sequences of human GPCRs.}
  \item{melanogaster.sapiens}{ a 59 by 283 matrix of difference scores between the aligned sequences of GPCRs from \emph{H. sapiens} and \emph{D. melanogaster} (283 and 59 sequences, respectively)..}
  }
  }
}
\describe{
  \item{dis}{
  a named list containing sixteen distance matrices calculated from the \code{\link{mat.dis}}
  function (distances based on dissimilarity scores) for the eight substitution matrices in \code{\link{sub.mat}} (other parameters by default):
  \describe{
  	\item{sapiens.sapiens$*}{ are 283 by 283 matrices of dissimilarity scores between the 283 aligned sequences of human GPCRs.
  	\describe{
 	 	\item{sapiens.sapiens$PAM40}{ is calculated with PAM40.}
  		\item{sapiens.sapiens$PAM120}{ is calculated with PAM120.}
  		\item{sapiens.sapiens$PAM250}{ is calculated with PAM250.}
  		\item{sapiens.sapiens$BLOSUM45}{ is calculated with BLOSUM45.}
  		\item{sapiens.sapiens$BLOSUM62}{ is calculated with BLOSUM62.}
  		\item{sapiens.sapiens$BLOSUM80}{ is calculated with BLOSUM80.}
  		\item{sapiens.sapiens$PAM250TM}{ is calculated with PAM250TM.}
  		\item{sapiens.sapiens$PHAT}{ is calculated with PHAT.}
  		}
  	}

  	\item{melanogaster.sapiens$*}{ are 59 by 283 matrices of dissimilarity scores between the 283 aligned sequences from \emph{H. sapiens} and \emph{D. melanogaster}
  	\describe{
  		\item{melanogaster.sapiens$PAM40}{ is calculated with PAM40.}
  		\item{melanogaster.sapiens$PAM120}{ is calculated with PAM120.}
  		\item{melanogaster.sapiens$PAM250}{ is calculated with PAM250.}
  		\item{melanogaster.sapiens$BLOSUM45}{ is calculated with BLOSUM45.}
  		\item{melanogaster.sapiens$BLOSUM62}{ is calculated with BLOSUM62.}
  		\item{melanogaster.sapiens$BLOSUM80}{ is calculated with BLOSUM80.}
  		\item{melanogaster.sapiens$PAM250TM}{ is calculated with PAM250TM.}
  		\item{melanogaster.sapiens$PHAT}{ is calculated with PHAT.}
  		}
  	}
	}
}
}
\describe{
\item{mmds}{
  a typical example of metric MDS analysis with the \code{\link{mmds}} function of \code{bios2mds} using GPCRs from \emph{H. sapiens} as active data and GPCRs from \emph{D. melanogaster} as supplementary data with an example of clustering.
  }
}
}
\source{
Deville J, Rey J and Chabbert M (2009) An indel in transmembrane helix 2 helps to trace
the molecular evolution of class A G-protein-coupled receptors. \emph{J Mol Evol} \bold{68}: 475-
489.
}

\examples{
# loading gpcr
data(gpcr)

# displaying the matrix of differences scores between GPCRs sequences
# from H. sapiens
gpcr$dif$sapiens.sapiens

# displaying the matrix of dissimilarity scores between GPCRs sequences
# from H. sapiens
gpcr$dis$sapiens.sapiens$PAM250

# displaying the matrix of dissimilarity scores between GPCRs sequences
# from H. sapiens and D. Melanogaster calculated with the BLOSUM45 matrix
gpcr$dis$melanogaster.sapiens$BLOSUM45

# displaying mmds analysis of the MSA of GPCRs from H. sapiens
# and D. Melanogaster
gpcr$mmds
}
\keyword{datasets}
