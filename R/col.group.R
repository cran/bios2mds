col.group <- function(x,file,data="active"){
	if (!inherits(x, "mmds")) 
	        stop("object of class 'mmds' expected")
	if(missing(file)) {
		stop("file is missing")
	}
	
	element<-read.table(file=file,comment.char="#",sep=",")
	id<-list()
	i<-1
	col<-matrix(c(NA,NA,NA),1)
	group<-matrix(c(NA,NA),1)
	if(data=="active"){
		names<-attributes(x$active.coord)$row.names
	}
	if(data=="sup"){
		names<-attributes(x$sup.coord)$row.names
	}
	if(length(which(c("active","sup")==data))==0){
		stop("data wrong attribute")
	}
	while(i <= length(names)){
		j<-which(element[,1]==names[i])
		if(length(j)>1){
			stop("group file in bad format : two or more element had the same name")
		}
		if(length(j) == 0){
			col<-rbind(col,matrix(c(names[i],"NoGroup","black"),1))
			if(length(which(group[,1] == "NoGroup"))==0){
				group<-rbind(group,matrix(c("NoGroup","black"),1))
			}
		}
		else{
			id<-append(id,element[j,1])
			if(length(element[j,])==3){
				col<-rbind(col,matrix(c(names[i],levels(element[,2])[element[j,2]],levels(element[,3])[element[j,3]]),1))
				if(length(which(group[,1] == element[j,2]))==0){
					group<-rbind(group,matrix(c(levels(element[,2])[element[j,2]],levels(element[,3])[element[j,3]]),1))
				}
				else{
					if(group[(which(group[,1] == element[j,2]))[1],][2]!=element[j,3]){
						stop("group file in bad format : one group had two color")
					}
				}
			}
			else{
				col<-rbind(col,matrix(c(names[i],"NoGroup","black"),1))
				if(length(which(group[,1] == "NoGroup"))==0){
					group<-rbind(group,matrix(c("NoGroup","black"),1))
				}
				print("Element in file hadn't all parameter")
			}
		}
		i<-i+1
	}
	colnames(col)<-c("element","group","color")
	colnames(group)<-c("group","color")
	if(data=="active"){
		x$active.col<-col[2:length(col[,1]),]
		x$active.group<-group[2:length(group[,1]),]
	}
	else{
		x$sup.col<-col[2:length(col[,1]),]
		x$sup.group<-group[2:length(group[,1]),]
	}
  	return(x)
}