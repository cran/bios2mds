write.mmds.pdb <- function (x, project = NULL, axis = c(1, 2, 3), file.pdb = "R.pdb") {

  if (!inherits(x, "mmds"))
    stop("object of class 'mmds' expected")
  if (length(axis) != 3)
    stop("axis have to be of length 3")
  if (any(axis > length(x$eigen.perc)))
    stop("wrong axis")
  pdb.coord <- function(i,nbgroup, x, y, z) {
    format <- "%s%5s%3s%6s%3s%-7s%8.3f%8.3f%8.3f%6s%6s%12s"
    sprintf(format, "HETATM", i, "O", "HOH", groupId[nbgroup] , i, x, y, z, "1.00", "0.00", "O")
  } 

  pdb.axis <- function(i, x, y, z, atom) {
    format <- "%s%5s%3s%9s%-7s%8.3f%8.3f%8.3f%6s%6s%12s"
    sprintf(format, "HETATM", i, atom, "A ", i, x, y, z, "1.00", "0.00", atom)
  }

  pdb.conect <- function(i, j) {
    format <- "%s%5s%5s"
    sprintf(format, "CONECT", i, j)
  } 
  groupName<-"NoGroup"
  groupId<-c("Z ","A ","B ","C ","D ","E ","F ","G ","H ","I ","J ","K ","L ","M ","N ","O ","P ",
	"Q ","R ","S ","T ","U ","V ","W ","X ","Y ")  
  x$coord <- x$coord * 50
  active.nb <- nrow(x$coord)
  ncol <- nrow(x$col)
  if (!is.null(project) && !inherits(project,"project")) {
    project$coord <- project$coord * 50
    sup.nb <- nrow(project$coord)
  }

  pdb.lines <- NULL

  #pdb for active
  j <- 1
    for (i  in 1:nrow(x$group)) {
	if(!length(which(x$group[i]==groupName))==1){
		groupName<-c(groupName,x$group[i])
        }
    }
  for (i in 1:active.nb) {
    if(ncol!=1){
      nbgroup=which(x$col[i,2]==groupName)
    }
    else {
      nbgroup=which(x$col[1,2]==groupName)
    }
    if(nbgroup > 26){
	nbgroup<-1
    }
    pdb.lines <- rbind(pdb.lines, pdb.coord(j,nbgroup,x$coord[i, axis[1]], x$coord[i, axis[2]],
       x$coord[i, axis[3]]))
      j <- j + 1
  }
     
  #pdb for sup if given
  if (!is.null(project) && !inherits(project,"project")) {
    ncol <- nrow(project$col)
   for (i  in 1:nrow(project$group)) {
	if(!length(which(project$group[i]==groupName))==1){
		groupName<-c(groupName,project$group[i])
        }
    }
    j <-5001
    for (i in 1:sup.nb) {
      if(ncol!=1){
	nbgroup=which(project$col[i,2]==groupName)
      }
      else {
     
	nbgroup=which(project$col[i,2]==groupName)
      }
      if(nbgroup > 26){
  	nbgroup<-1
      }
      pdb.lines <- rbind(pdb.lines, pdb.coord(j,nbgroup, project$coord[i, axis[1]], project$coord[i, axis[2]],
         project$coord[i, axis[3]]))
        j <- j + 1
    }
  }

  m <- (max(x$coord[, axis[1]], x$coord[, axis[2]], x$coord[, axis[3]]) * 2)
  if (!is.null(project) && !inherits(project,"project")) {
    m <- (max(x$coord[, axis[1]], x$coord[, axis[2]], x$coord[, axis[3]], 
         project$coord[, axis[1]], project$coord[, axis[2]], project$coord[, axis[3]]) * 2)
  }

  all.nb <- active.nb
  if (!is.null(project) && !inherits(project,"project"))
    all.nb <- (active.nb + sup.nb)

  pdb.lines <- rbind(pdb.lines, pdb.axis(all.nb + 1, m, 0, 0, "O"))
  pdb.lines <- rbind(pdb.lines, pdb.axis(all.nb + 2, 0, m, 0, "C"))
  pdb.lines <- rbind(pdb.lines, pdb.axis(all.nb + 3, 0, 0, m, "N"))
  pdb.lines <- rbind(pdb.lines, pdb.axis(all.nb + 4, 0, 0, 0, "Po"))

  pdb.lines <- rbind(pdb.lines, pdb.conect(all.nb + 1, all.nb + 4))
  pdb.lines <- rbind(pdb.lines, pdb.conect(all.nb + 2, all.nb + 4))
  pdb.lines <- rbind(pdb.lines, pdb.conect(all.nb + 3, all.nb + 4))

  cat(pdb.lines, file = file.pdb, sep = "\n")
}