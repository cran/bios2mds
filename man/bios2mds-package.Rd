\name{bios2mds-package}
\Rdversion{1.1}
\alias{bios2mds-package}
\alias{bios2mds}
\docType{package}
\title{
From BIOlogical Sequences to MultiDimensional Scaling
}
\description{
The \code{bios2mds} package is developed in the Bioinformatics team at Integrated
Neurovascular Biology Laboratory, UMR CNRS 6214 / INSERM 771, University of Angers - FRANCE.

This package is dedicated to the analysis of biological sequences by metric MultiDimensional Scaling (MDS) with projection of supplementary data. It contains functions for reading multiple sequence alignment (MSA) files, calculating distance matrices from MSA files, performing MDS analysis and visualizing results. The MDS analysis and visualization tools can be applied to any kind of data.

The main functionalities of \code{bios2mds} are summarized below:
\describe{
  \item{(1) BUILDING DISTANCE MATRICES FROM MULTIPLE SEQUENCE ALIGNMENTS : }{
Several functions allow users to read multiple sequence alignment files and to compute matrices of distances between these sequences:
    \itemize{
    \item \code{\link{import.fasta}}: reads a multiple sequence alignment in FASTA format.
    \item \code{\link{import.msf}}: reads a multiple sequence alignment in MSF format.
    \item \code{\link{mat.dif}}: computes a matrix of pairwise distances between sequences based on sequence difference.
    \item \code{\link{mat.dis}}: computes a matrix of pairwise distances between sequences based on sequence dissimilarity.
    }
  }
  \item{(2) MULTIDIMENSIONAL SCALING : }{
  A function performs metric MDS analysis of a distance matrix between active elements with the option of projecting supplementary elements onto the active space.
    \itemize{
    \item \code{\link{mmds}}: performs metric multidimensional scaling.
    \item \code{\link{mmds.project}}: performs projection of supplementary elements onto the active space.
    }
  }
  \item{(3) GRAPHICAL TOOLS : }{
  Several functions are proposed to visualize results of metric MDS analysis:
    \itemize{
    \item \code{\link{scree.plot}}: draws the scree plot of eigenvalues.
    \item \code{\link{mmds.2D.plot}}: draws a scatter plot of the MDS coordinates. 
    \item \code{\link{mmds.2D.multi}}: draws a scatter plot of the MDS coordinates with projection of multiple groups of supplementary element. 
    \item \code{\link{mmds.3D.plot}}: Displays a 3D plot of the MDS coordinates. 
    \item \code{\link{mmds.plot}}: wrapper function that draws the scree plot and three scatter plots of MDS coordinates.
    \item \code{\link{col.group}}: colors scatter plots with user provided groupings and colors.
    \item \code{\link{write.mmds.pdb}}: writes MDS coordinates in a PDB formatted file for 3D visualisation.
    }
  }
  \item{(4) CLUSTER ANALYSIS : }{
  Several functions allow users to perform data clustering and to assess the clustering robustness:
    \itemize{
    \item \code{\link{kmeans.run}}: performs multiple runs of K-means clustering and analyzes clusters
    \item \code{\link{sil.score}}: calculates the silhouette score from multiple K-means runs to determine the optimal number of clusters.
    \item \code{\link{extract.cluster}}: extraction of clusters alignments
    }
  }
  \item{(5) DATASETS : }{
  Two raw datasets are proposed to test the functionalities of \code{bios2mds}. They correspond to the multiple sequence alignments of GPCRs from \emph{H. sapiens} and \emph{D. melanogaster} in .msf and .fa formats (msa/human_gpcr.* and msa/drome_gpcr.*). They are based on the non-redundant sets of non-olfactory class A GPCRs, prepared and analyzed in Deville et al. (2009) and updated with the July 2009 release of Uniprot \url{http://www.uniprot.org}. 
  Each MSA file is related to a .csv file that assigns a group and a color to each sequence of the alignment (csv/human_gpcr_group.csv and csv/drome_gpcr_group.csv). 

  Pre-analyzed data from these two MSA files are in \code{gpcr}. Moreover, \code{gpcr} contain projection of GPCRs from \emph{N. vectensis} and from \emph{C. intestinalis} onto the active space of human GPCRs calculated by MDS analysis.
  }
}
  For an index of functions, use library(help = \code{bios2mds}).
}
\details{
\tabular{ll}{
Package \tab bios2mds\cr
Type \tab Package\cr
Version \tab 1.2\cr
Repository \tab CRAN\cr
Date \tab 2011-12-21\cr
License \tab GPL version 2 or newer\cr
Collate	\tab Other useful packages can be found in the CRAN task view.\cr
\tab See \url{http://cran.stat.sfu.ca/web/views/Multivariate.html}\cr
\tab and \url{http://cran.stat.sfu.ca/web/views/Cluster.html}}}
\author{
Julien Pele <julien.pele@yahoo.fr> with Jean-Michel Becu <jean-michel.becu@etu.univ-rouen.fr>, 
Herve Abdi <herve@utdallas.edu> and Marie Chabbert <marie.chabbert@univ-angers.fr>.

Maintainer: Marie Chabbert <marie.chabbert@univ-angers.fr>
}
\references{
citation('bios2mds')
}
\keyword{package}
\examples{
# The MSA files provided with the package correspond to the sequence 
# alignment of non-olfactory class A G-protein-coupled receptors from
# H. sapiens and D. melanogaster prepared by Deville et al. (2009).  

# loading GPCR data
data(gpcr)

# building distance matrices between the aligned GPCR sequences from
# H. sapiens and D. melanogaster
human <- import.fasta(system.file("msa/human_gpcr.fa", package = "bios2mds"))
drome <- import.fasta(system.file("msa/drome_gpcr.fa", package = "bios2mds"))

#active <- mat.dif(human, human)
# or
active <- gpcr$dif$sapiens.sapiens

#sup <- mat.dif(drome, human)
# or
sup <- gpcr$dif$melanogaster.sapiens

# performing MDS analysis of the GPCR sequences from H. sapiens 
mmds_active <- mmds(active, group.file=system.file(
"csv/human_gpcr_group.csv",package = "bios2mds"))

# performing MDS analysis of the GPCR sequences from H. sapiens 
# with projection of GPCRs from D. melanogaster 
# as supplementary elements onto the space of human GPCRs
mmds_sup <- mmds.project(mmds_active, sup,system.file(
"csv/drome_gpcr_group.csv",package = "bios2mds"))

# displaying MDS coordinates 
layout(matrix(1:6, 2, 3))

scree.plot(mmds_active$eigen.perc, lab = TRUE, title = "Scree plot of metric MDS")

mmds.2D.plot(mmds_active, title = "Sequence space of human GPCRs ")

mmds.2D.plot(mmds_active,mmds_sup, title = "Projection of GPCRs from D. melanogaster
 onto the space space of human GPCRs ", active.alpha = 0.3)

# writing PDB files for 3D visualization of MDS coordinates
write.mmds.pdb(mmds_active) 

}
