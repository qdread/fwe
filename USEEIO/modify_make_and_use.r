# Function to modify and renormalize both the make and use table
# QDR / FWE / 18 April 2019

modify_and_renormalize_make_and_use <- function(M, U, R, r_mod) {
  # Arguments: 
  # M = original make table
  # U = original use table
  # R = vector of numeric multiplying factors to change both intermediate and final demand in the use table
  # r_mod = vector of row names to apply R to
  
  # 0. Create vectors of relevant row and column groups in U
  U_int_col <- dimnames(U)[[2]][1:(which(dimnames(U)[[2]] == 'T001') - 1)]
  U_fin_col <- dimnames(U)[[2]][(which(dimnames(U)[[2]] == 'T001') + 1):(which(dimnames(U)[[2]] == 'T004') - 1)]
  
  U_commod_row <- dimnames(U)[[1]][1:(which(dimnames(U)[[1]] == 'T005') - 1)]
  U_valueadded_row <- dimnames(U)[[1]][(which(dimnames(U)[[1]] == 'T005') + 1):(which(dimnames(U)[[1]] == 'T006') - 1)]
  
  # 1. U[i,j] * R, multiply all row i and column j, the industries and final demand receiving commodity i
  
  U[r_mod, U_int_col] <- sweep(U[r_mod, U_int_col], 1, R, '*')
  U[r_mod, U_fin_col] <- sweep(U[r_mod, U_fin_col], 1, R, '*')
  
  # 2. U[j, i] * R, multiply all rows in j and columns in i by the same factor, to show that industries i need to produce less
  
  U[U_commod_row, r_mod] <- sweep(U[U_commod_row, r_mod], 2, R, '*')
  
  # 3. Recalculate intermediate and final row sums in U, and intermediate column totals
  
  U[U_commod_row, 'T001'] <- apply(U[U_commod_row, U_int_col], 1, sum)
  U[U_commod_row, 'T004'] <- apply(U[U_commod_row, U_fin_col], 1, sum)
  U[U_commod_row, 'T007'] <- U[U_commod_row, 'T001'] + U[U_commod_row, 'T004']
  U['T005', ] <- apply(U[U_commod_row, ], 2, sum)
  
  # 4. Renormalize *columns* of M based on the new U row sums (set column sums of M to the row sums of U)
  
  # renormalization factor
  renorm_factor <- U[U_commod_row, 'T007'] / M['T007', U_commod_row]
  renorm_factor[renorm_factor == -Inf] <- 1
  
  M[, U_commod_row] <-  sweep(M[, U_commod_row], 2, as.numeric(renorm_factor), '*')
  
  # 5. Recalculate row sums of M
  
  M[, 'T008'] <- apply(M[, 1:(which(dimnames(M)[[2]] == 'T008') - 1)], 1, sum)
  
  # 6. Set final column totals of U to the row sums of M
  
  U['T008', c(U_int_col, 'T001')] <- M[, 'T008']
  
  # 7. Back-calculate new value-added rows in U based on the difference between intermediate column totals of U and final column totals of U
  # Here we do another normalization because there are 3 value-added rows in U, versus 1 in the simplified example.
  
  value_added <- U['T008', c(U_int_col, 'T001')] - U['T005', c(U_int_col, 'T001')]
  
  # Renormalization factor using old value added (T006)
  value_added_renorm_factor <- value_added / U['T006', c(U_int_col, 'T001')]
  U[c(U_valueadded_row, 'T006'), c(U_int_col, 'T001')] <- sweep(U[c(U_valueadded_row, 'T006'), c(U_int_col, 'T001')], 2, as.numeric(value_added_renorm_factor), '*')
  
  return(list(M = M, U = U))
}