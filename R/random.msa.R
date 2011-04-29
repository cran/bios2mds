random.msa <- function (nb.seq = 100, id = "SEQ", nb.pos = 100, gap = FALSE, aa.strict = FALSE) {

  #one letter codes for amino acids 
  aa <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K",
    "M", "F", "P", "S", "T", "W", "Y", "V", "B", "Z", "J", "X")

  #remove ambiguous amino acids
  if (aa.strict)
    aa <- aa[1:20]

  if (gap)
    aa <- c(aa, "-")

  msa <- lapply(seq_len(nb.seq), function (i) {sample(aa, nb.pos, replace = TRUE)})

  msa.names <- paste(id, seq_len(nb.seq), sep = "")

  names(msa) <- msa.names

  return(msa)
}