mmds.3D.plot <- function (x , title = NULL, axis = c(1:3), 
active.type = "s", sup.type = "p", active.size = 2, radius = 0.005,
sup.size = 10, active.col = x$active.col[,3], sup.col = x$sup.col[,3], 
box = TRUE, axes = TRUE, new.plot = TRUE, label = TRUE,
xlim = NULL, ylim = NULL, zlim = NULL, box.lwd = 2,
box.antialias = TRUE, ...) {

  #check arguments
  if (!inherits(x, "mmds"))
    stop("object of class 'mmds' expected")
  if (any(axis > length(x$eigen.perc)))
    stop("wrong axis")
  if (!length(which(active.type == c("p","s","h","l","n"))) == 1)
    stop("active.type doesn't match")
  if (!length(which(sup.type == c("p","s","h","l","n"))) == 1)
    stop("sup.type doesn't match")
  if(new.plot == TRUE)
    new.plot <- FALSE
  else
    new.plot <- TRUE

  if (is.null(xlim)) {
    if (is.null(x$sup.coord)) {
      x.min <- min(x$active.coord[, axis[1]])
      x.max <- max(x$active.coord[, axis[1]])
    }
    else {
      x.min <- min(x$active.coord[, axis[1]], x$sup.coord[, axis[1]])
      x.max <- max(x$active.coord[, axis[1]], x$sup.coord[, axis[1]])
    }
    xlim <- c(x.min, x.max) * 1.2
  }

  if (is.null(ylim)) {
    if (is.null(x$sup.coord)) {
      y.min <- min(x$active.coord[, axis[2]])
      y.max <- max(x$active.coord[, axis[2]])
    }
    else {
      y.min <- min(x$active.coord[, axis[2]], x$sup.coord[, axis[2]])
      y.max <- max(x$active.coord[, axis[2]], x$sup.coord[, axis[2]])
    }
    ylim <- c(y.min, y.max) * 1.2
  }
  if (is.null(zlim)) {
    if (is.null(x$sup.coord)) {
      z.min <- min(x$active.coord[, axis[3]])
      z.max <- max(x$active.coord[, axis[3]])
    }
    else {
      z.min <- min(x$active.coord[, axis[3]], x$sup.coord[, axis[3]])
      z.max <- max(x$active.coord[, axis[3]], x$sup.coord[, axis[3]])
    }
    zlim <- c(z.min, z.max) * 1.2
  }

  if(label == TRUE) {
    xlab <- colnames(x$active.coord)[1]
    ylab <- colnames(x$active.coord)[2]
    zlab <- colnames(x$active.coord)[3]
  }
  else {
    xlab <- ""
    ylab <- ""
    zlab <- ""
  }
  plot3d(x$active.coord[,axis[1]], x$active.coord[,axis[2]], x$active.coord[,axis[3]], col=active.col,type=active.type,size=active.size,xlab= xlab,ylab= ylab ,zlab= zlab,main=title, box = box, axes = axes, radius= radius, add = new.plot, xlim= xlim, ylim = ylim, zlim =zlim, ...)
  if (!is.null(x$sup.coord))
    plot3d(x$sup.coord[,axis[1]], x$sup.coord[,axis[2]], x$sup.coord[,axis[3]], col=sup.col,type=sup.type,size=sup.size,add = TRUE, radius = radius, xlim= xlim, ylim = ylim, zlim =zlim, ...)
  if (new.plot == FALSE && box == TRUE) {
    box3d(lwd= box.lwd,line_antialias= box.antialias)
    if (axes == TRUE)
      axes3d(lwd= box.lwd,line_antialias= box.antialias)
  }
}