# Many to one mapping to sum matrix rows and columns

many_to_one <- function(mat, new_rows, new_cols) {
  # first sum up the rows
  row_idx <- match(new_rows, unique(new_rows))
  mat_fixed_rows <- t(sapply(1:max(row_idx), function(i) colSums(mat[row_idx == i, , drop = FALSE], na.rm = TRUE)))
  
  # then sum up the columns
  col_idx <- match(new_cols, unique(new_cols))
  mat_fixed_rowsandcols <- sapply(1:max(col_idx), function(i) rowSums(mat_fixed_rows[, col_idx == i, drop = FALSE], na.rm = TRUE))
  
  dimnames(mat_fixed_rowsandcols) <- list(unique(new_rows), unique(new_cols))
  return(mat_fixed_rowsandcols)
}

# One to many mapping to disaggregate one row into multiple, using ratios from a different set of data
# tougher than the many-to-one mapping



one_to_many <- function(mat, codes_to_split, code_split_keys, row_totals, col_totals) {
  # Loop through the codes to split and replace them in place with divided up versions.
  # Make sure that the order is preserved.
  for (i in 1:length(codes_to_split)) {
    # Split the rows
    old_row <- mat[codes_to_split[i], ]
    row_proportions <- row_totals[[i]]/sum(row_totals[[i]])
    new_rows <- t(sapply(row_proportions, function(p) old_row * p))
    idx_to_replace <- which(dimnames(mat)[[1]] == codes_to_split[i])
    mat <- rbind(mat[1:(idx_to_replace-1), ], new_rows, mat[(idx_to_replace+1):nrow(mat),])
    dimnames(mat)[[1]][idx_to_replace:(idx_to_replace+length(row_proportions)-1)] <- code_split_keys[[i]]
    
    # Split the columns
    old_col <- mat[, codes_to_split[i], drop = FALSE]
    col_proportions <- col_totals[[i]]/sum(col_totals[[i]])
    new_cols <- sapply(col_proportions, function(p) old_col * p)
    idx_to_replace <- which(dimnames(mat)[[2]] == codes_to_split[i])
    mat <- cbind(mat[, 1:(idx_to_replace-1)], new_cols, mat[, (idx_to_replace+1):ncol(mat)])
    dimnames(mat)[[2]][idx_to_replace:(idx_to_replace+length(col_proportions)-1)] <- code_split_keys[[i]]

  }
  mat
  
}
