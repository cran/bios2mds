mat.dis <- function (align1, align2, sub.mat.id = "PAM250", gap = NULL) {

  if (!exists("sub.mat"))
    data(sub.mat)

  if (!is.element(sub.mat.id, names(sub.mat)))
    stop("sub.mat does not contain sub.mat.id")
        
  if (!is.null(gap) && (length(gap) != 2))
    stop("gap is not of length 2")
  
  #get identifiers
  id1 <- names(align1)
  id2 <- names(align2)

  #get numbers
  nb.seq1 <- length(align1)
  nb.seq2 <- length(align2)

  #if align1 and align2 are identical
  #only compute the upper triangle of mat
  if (identical(align1, align2))
    pair <- combinations(nb.seq1, 2)
  else {
    pair <- expand.grid(list(seq_len(nb.seq1), seq_len(nb.seq2)))
    pair <- as.matrix(pair)
    dimnames(pair) <- NULL
  }
  
  dis <- mapply(function(i, j) dis(align1[[i]], align2[[j]], sub.mat.id = sub.mat.id, gap = gap), pair[, 1], pair[, 2])

  mat <- matrix(0, nrow = nb.seq1, ncol = nb.seq2)
  mat[pair] <- dis
  
  if (identical(align1, align2)) {
    if (nb.seq1 == 2)
      mat[pair[, 2], pair[, 1]] <- dis
    else
      mat[pair[, c(2, 1)]] <- dis
  }
  dimnames(mat) <- list(id1, id2)
  return (mat)
}
