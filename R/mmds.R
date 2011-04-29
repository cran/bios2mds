mmds <- function (active, sup = NULL, pc = 3) {

 #active validation
    if (!is.matrix(active))
	stop("active is not a matrix")
    if (nrow(active) != ncol(active))
	stop("active is not square")
    if (!isSymmetric.matrix(active))
	stop("active is not symmetric")
    if (sum(diag(active)) != 0)
	stop("active diagonal values are not zero")

    if (!is.numeric(active))                         
        stop("numeric values expected in active") 
    if (any(!is.finite(active)))
	stop("infinite or missing values not allowed in active")
    if (is.null(rownames(active)))
	rownames(active) <- paste("A", 1:nrow(active), sep = "")
    if (is.null(colnames(active)))
	colnames(active) <- rownames(active)

    #sup validation
    if (!is.null(sup)) {
	if (!is.matrix(sup))
	    stop("sup is not a matrix")
	if (any(is.infinite(sup)))
	    stop("infinite or missing values not allowed in sup")
	if (ncol(active) != ncol(sup))
	    stop("col numbers are unequal")
	if (is.null(rownames(sup)))
	    rownames(sup) <- paste("S", 1:nrow(sup), sep = "")
	if (is.null(colnames(sup)))
	    colnames(sup) <- rownames(active)
    }
    
	#results will be stored in a list
	res <- list ()

	#MDS of active data
	D <- active^2

	#identity matrix
	I <- diag(1, nrow(active))

	#active matrix of ones
	ONES <- matrix(1, nrow = nrow(active), ncol = nrow(active))

	#compute active cross-product matrix
	S <-  -0.5 * (I-((1/nrow(active)) * ONES)) %*% D %*% (I-((1/nrow(active)) * ONES))
	eigen <- eigen(S)
	eigen.perc <- (abs(eigen$values) * 100) / sum(eigen$values[eigen$values>0])
	res$eigen.perc <- round(eigen.perc[1:pc], 3)
	#only positive eigenvalues are kept
	eigen$vectors <- eigen$vectors[, eigen$values > 0]
	eigen$values <- eigen$values[eigen$values > 0]

	res$eigen <- round(eigen$values[1:pc], 3)

	#check principal components
	if (pc < 2)
		pc <- 3
	if (pc > length(eigen$values))
		pc <- length(eigen$values)

	#eigenvalues are transformed into percentage


	#compute active matrix of factor scores
	F <- eigen$vectors %*% diag(eigen$values^0.5)

	active.coord <- data.frame(F[, 1:pc])
	rownames(active.coord) <- rownames(active)
	colnames(active.coord) <- paste ("PC", (1:pc), sep = "")
	res$active.coord = round(active.coord, 3)
	res$active.group<-matrix(c("NoGroup","black"),1)
	colnames(res$active.group)<-c("group","color")
	#res$active.group<-as.data.frame.matrix(res$active.group)
	res$active.col<-matrix(c("","NoGroup","black"),1)
        colnames(res$active.col)<-c("element","group","color")
	#res$active.col<-as.data.frame.matrix(res$active.col)

	#MDS of supplementary data
	if (!is.null(sup)) {
		
		Dsup <- sup^2

		#supplementary matrix of ones
		ONESsup <- matrix(1, nrow = nrow(active), ncol = nrow(sup))

		#compute supplementary cross-product matrix
		Ssup <-  -0.5 * (I-((1/nrow(active)) * ONES)) %*% (t(Dsup) - (1/nrow(active)) * (D %*% ONESsup))

		#compute supplementary matrix of factor scores
		Fsup <- t(Ssup) %*% F %*% diag(eigen$values^-1)
		sup.coord <- data.frame(Fsup[, 1:pc])
		rownames(sup.coord) <- rownames(sup)
		colnames(sup.coord) <- paste ("PC", (1:pc), sep = "")
		res$sup.coord <- round(sup.coord, 3)
		res$sup.group<-matrix(c("NoGroup","magenta"),1)
		colnames(res$sup.group)<-c("group","color")
		#res$sup.group<-as.data.frame.matrix(res$sup.group)
		res$sup.col<-matrix(c("","NoGroup","magenta"),1)
	        colnames(res$sup.col)<-c("element","group","color")
		#res$sup.col<-as.data.frame.matrix(res$sup.col)
	}
	class (res) <- c("mmds")
	return (res)
}
