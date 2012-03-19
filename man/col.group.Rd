\name{col.group}
\Rdversion{1.1}
\alias{col.group}

\title{Links elements in a mmds object to specific groups and colors}

\description{
Links elements in a \code{mmds} object or \code{mmds.project} object to user-provided groups and colors.
}

\usage{
col.group(x,file,data = "active")
}
\arguments{
	\item{x}{
	  a 'mmds' or 'project' object obtained from \code{\link{mmds}} or \code{\link{mmds.project}} function
	}
	\item{file}{
	  a string of characters to indicate the file name assigning groups and colors to each active OR each supplementary element of the mmds object.
	}
	\item{data}{
	 a string of characters to specify whether the assigned elements in the mmds object are active ("active") or supplementary ("sup") data. Default is "active". 
	}
}
\details{
\code{col.group} assigns each element of the mmds object to user-provided groupings and colors for coloring and labeling mmds scatter plots. 

\code{col.group} requires a formatted file. See "csv/human_gpcr_group.csv" for an example. Each line corresponds to one element of the mmds object and must contain three parameters separated by ",". \cr
The first parameter is the element name, as given in the multiple sequence alignment file. \cr
The second parameter is the group name. Groupings must be provided by the user.\cr
The third parameter is the group color in full letters (example : "black","green"). Two or more groups can have the same color, but elements within the same group must have the same color. The group is colored by the first color encountered. 
}
\value{
Adds data to a mmds object in order to color and label mmds scatter plots with user-provided groupings and colors.
}

\seealso{
See \code{colors} function (default R package).\cr
See \code{getcol} in \code{made4} package for special color palette developed to maximize the contrast between colors.\cr
}
\author{
Jean-Michel Becu
}
\examples{
# performing metric MDS on human GPCRs with projection of 
# GPCRs from D. melanogaster as supplementary data:
data(gpcr)
active <- gpcr$dif$sapiens.sapiens
mmds_active <- mmds(active)
mmds_active<-col.group(mmds_active,system.file("csv/human_gpcr_group.csv"
,package = "bios2mds"))
}
\keyword{mmds}
