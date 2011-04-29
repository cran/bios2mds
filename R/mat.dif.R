mat.dif <- function (align1, align2, gap = FALSE, aa.strict = FALSE) {

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

  #call dif function
  dif <- mapply(function(i, j) dif(align1[[i]], align2[[j]], gap = gap, aa.strict = aa.strict), pair[,1], pair[,2])
  mat <- matrix(0, nrow = nb.seq1, ncol = nb.seq2)
  mat[pair] <- dif

  #fill the lower triangle of mat
  if (identical(align1, align2)) {
    if (nb.seq1 == 2)
      mat[pair[, 2], pair[, 1]] <- dif
    else
      mat[pair[, c(2, 1)]] <- dif
  }
    dimnames(mat) <- list(id1, id2)
    return (mat)
}