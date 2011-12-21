\name{mmds.2D.multi}
\Rdversion{1.1}
\alias{mmds.2D.multi}
\title{
Plots the mmds coordinates onto a 2D space
}
\description{
Displays a scatter plot of the active elements and the barycenter of supplementary elements, or of groups of supplementary elements, after a metric MDS analysis.
}
\usage{
mmds.2D.multi(x,project, title = NULL, axis = c(1, 2), xlim = NULL,
ylim = NULL, outfile.type = NULL,bary="p",
outfile.name = "mmds",new.plot = TRUE, active.col = x$col[,3],
active.alpha = 0.6, active.pch = 20, sup.pch = NULL , active.cex = 2,
sup.cex = 2, active.legend.cex = 2, sup.legend.cex = 2,
active.legend.lwd = 1, sup.legend.lwd = 2, active.lwd = 1, sup.lwd = 3,
legend = TRUE, active.legend.pos = "bottomleft",
sup.legend.pos = "bottomright", group.name = NULL,
ensemble.legend.name = "", group.col = NULL, outfile.width = NULL, outfile.height = NULL,
box.lwd = 1, cex.axis = 1, sup.legend.text = 1,
active.legend.text = 1, legend.axis = TRUE, grid = TRUE, axes = TRUE)
}
\arguments{
  \item{x}{
  an object of class 'mmds', obtained from \code{\link{mmds}} function.
}
  \item{project}{
  an object of class 'project' or a list of objects of class 'project', obtained from the \code{\link{mmds.project}} function. In the list the order of 'project' objects is very important because if the \code{bary} option equal to "l", the barycenters are linked in the same order as the list with the active barycenter linked as the last object.
}
  \item{title}{
  a string of characters representing the title of the plot. Default is "Metric MDS".  
}
  \item{bary}{
  a character to specify the representation of group barycenter with "p" for representation of group barycenter foreach 'project' and active space by dots and "l" for representation of barycenter linked foreach group in order of appearence in project list and with the barycenter of active group in last position. Default is "p".
}
  \item{axis}{
  a numeric vector of length two representing the principal components displayed on the plot. Default is c(1, 2). 
}
  \item{xlim}{
  a numeric vector representing the range for the x values. Default is full range.
}
  \item{ylim}{
  a numeric vector representing the range for the y values. Default is full range.
}
  \item{outfile.type}{
  a string indicating the extension type of the graph outfile. Default is NULL. If not NULL, this should be one of "pdf", "tiff", "png" or "postscript". In this case, the parameter outfile.name and the forthcoming parameters are activated.
}
  \item{outfile.name}{
  a string ("mmds", default) indicating the name and directory of pdf graph outfile. The extension file is added automaticaly. See \code{outfile.type}.
}
  \item{new.plot}{
  a boolean indicating whether a new graphical device should be created (TRUE) or not (FALSE). Default is TRUE.
}
  \item{active.pch}{
  an integer indicating the symbol of active elements. Default is 20, corresponding to dots.
}
  \item{sup.pch}{
  an integer vector representing the symbol for project barycenter. Default is NULL. 
  If sup.pch is NULL then symbol are choose randomly.
}
  \item{active.col}{
  a string of characters or character vector representing the color(s) of the active elements. Default is x$col[,3]. 
  It corresponds either to the user-provided colors if the \code{col.group} function has been used previously, 
  or to black (filled automaticaly with the \code{mmds} function).    
}
  \item{active.alpha}{
  a numeric value indicating the alpha value for opacity of active objects. This
  value must range from 0 (invisible) to 1 (full opacity). Default is 0.6.
}
  \item{active.cex}{
  a numeric value indicating the size of the active symbols. Default is 2. 
}
  \item{sup.cex}{
  a numeric value indicating the size of the supplementary symbols. Default is 2.  
}
  \item{active.legend.cex}{
  a numeric value indicating the size of active symbols in legend. Default is 2. 
}
  \item{sup.legend.cex}{
  a numeric value indicating the size of supplementary symbols in legend. Default is 2.
}
  \item{active.lwd}{
  a numeric value indicating the width of active symbols. Default is 1.  
}
  \item{sup.lwd}{
  a numeric value indicating the width of supplementary symbols. Default is 3. 
}
  \item{active.legend.lwd}{
  a numeric value indicating the width of active symbols in legend. Default is 1.  
}
  \item{sup.legend.lwd}{
  a numeric value indicating the width of supplementary objects in legend. Default is 4. 
}
  \item{legend}{
  a boolean indicating whether the legend should be displayed (TRUE) or not (FALSE). Default is TRUE.
}
  \item{active.legend.pos}{
  a string indicating the position of the legend for active elements. Default is "topleft". 
}
  \item{sup.legend.pos}{
  a string indicating the position of the legend for supplementary elements. Default is "topright".  
}
  \item{group.name}{
  a string vector indicating the names of the used groups. NULL vector allow to use all groups present in the 'mmds' object (x) and in all 'mmds.project' object. It corresponds either to the user-provided groups if the \code{col.group} function has been used previously, or to "NoGroup" (filled automaticaly with the \code{mmds} ans \code{mmds.project} functions). Each group in this vector must be presents in 'mmds' or 'mmds.project' objects and only group present in this vector will draw in the plot. Default is NULL.
}
  \item{ensemble.legend.name}{
  a string vector indicating the names of the \code{project} and the name of active space. Default is "".
  It corresponds either to the names of project object ans in the last position of vector the name of the active space.
}
  \item{group.col}{
  a string vector indicating the colors of appearance of the used groups. This vector must have the same length of the group.name vector. NULL vector associate automaticaly the color of groups in 'mmds' and 'mmds.project' object to the group in the group.name parameter. It corresponds either to the user-provided colors if the \code{col.group} function has been used previously, or to black (filled automaticaly with the \code{mmds} and \code{project.mmds} functions). Default is NULL.
}
  \item{outfile.width}{
  a numeric value in inches indicating the width of graph outfile. Default differs by outfile.type. See \code{pdf}, \code{png}, \code{postscript}.  
  The resolution for tiff and png figures is 150 dpi.  
}
  \item{outfile.height}{
  a numeric value in inches indicating the height of graph outfile. Default differs by outfile.type. The resolution for tiff and png figures is 150 dpi. See \code{pdf}, code{tiff}, \code{png}, \code{postscript}.
  
}
  \item{box.lwd}{
  a numeric value indicating the border width of graph box and legend box. Default is 1. 
}
  \item{cex.axis}{
  a numeric value indicating the character size for the x and y axes. Default is 1.  
}
  \item{sup.legend.text}{
  a numeric value indicating the character size of the supplementary tag in legend. Default is 1. 
}
  \item{active.legend.text}{
  a numeric value indicating the character size of active tag in legend. Default is 1.  
}
  \item{legend.axis}{
  a boolean indicating whether axis name should be displayed (TRUE) or not (FALSE). Default is TRUE.
}
  \item{grid}{
  a boolean indicating whether grid should be displayed (TRUE) or not (FALSE). Default is TRUE.
}
  \item{axes}{
  a boolean indicating whether x and y axes should be displayed (TRUE) or not (FALSE). Default is TRUE.
}
}
\details{
If \code{mmds.2D.plot} is used after the \code{col.group} function, 
the elements are colored by the color scheme provided in the .csv file (see \code{col.group} for details).
If the \code{col.group} function has not been used, the default colors are black and magenta for active and supplementary elements.

\code{mmds.2D.plot} helps identify patterns in data and compare active and supplementary elements.

\code{active.alpha} argument is helpful for visualization of supplementary elements because it allows the symbols of supplementary elements to be in the foreground as compared to active elements.
}
\value{
Produces a scatter plot on the active graphical device.
}
\author{
Jean-Michel Becu
}
\note{
\code{mmds.2D.plot} requires \code{alpha} function from \code{ggplot2} package.
}
\seealso{
\code{plot.PCA} function from \code{FactoMineR} package.\cr
\code{png}, \code{pdf}, \code{postscript} functions (default R package).
}
\examples{
# scatter plot of human GPCRs onto the first two axes obtained
# from MDS analysis with projection of GPCRs from N.vectensis 
# and C.intestinalis as supplementary elements:
data(gpcr)

mmds_human <- gpcr$mmds$sapiens.active
project_vectensis <-gpcr$mmds$vectensis.project
project_intestinalis <-gpcr$mmds$intestinalis.project

mmds.2D.multi(mmds_human,project=list(project_vectensis,project_intestinalis),
bary='l',cex.axis=0.01,active.cex = 1, sup.cex = 1,active.lwd=1.5,sup.lwd=3,
ensemble.legend.name=c('nemve','inte','human'),legend=FALSE,title='multi_human')

# with selected group

mmds.2D.multi(mmds_human,project=list(project_vectensis,project_intestinalis),
bary='l',cex.axis=0.01,active.cex = 1, sup.cex = 1,active.lwd=1.5,sup.lwd=3,
ensemble.legend.name=c('nemve','inte','human'),legend=FALSE,title='multi_human',
group.name=c('SO','PEP','OPN','ADENO'),group.col=c("red","forestgreen","orange","maroon"))

}
\keyword{plot}

