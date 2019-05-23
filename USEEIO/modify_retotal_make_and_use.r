# New scenario analysis code. Implements qualitatively different kinds of changes to the make and use table.
# This reflects the fact that some FLW rate changes affect intermediate demand only, some affect a combination of intermediate and final, and others final demand only.
# It is a newer and extended version of the modify_and_renormalize_make_and_use() function with new options of where modification should occur:
# c_mod are sector columns to which the inputs are modified
# c_final_mod are final demand columns that are modified, only for the rows in r_mod
# r_mod are rows to which personal consumption expenditures are modified
# Also, I've split the functionality into 2: one function to modify the make and use tables and the other to renormalize them.
# QDR / FWE / 22 May 2019

modify_make_and_use <- function(M, U, R, c_int_mod = character(0), c_final_mod = character(0), r_mod = character(0)) {
  # Arguments: 
  # M = original make table
  # U = original use table
  # R = vector of numeric multiplying factors to change both intermediate and final demand in the use table
  # r_mod = vector of row names to apply R to
  
  # 0. Create vectors of relevant row and column groups in U
  U_commod_row <- dimnames(U)[[1]][1:(which(dimnames(U)[[1]] == 'T005') - 1)]

  # 1. Test if there are any modifications to be made to intermediate demand
  # If yes, multiply the intermediate inputs to the output rows in c_mod by the reduction factor.
  if (length(c_int_mod) > 0) {
    U[U_commod_row, c_int_mod] <- sweep(U[U_commod_row, c_int_mod], 2, R, '*')
  }
  
  # 2. Test if there are any modifications to be made to final demand/personal consumption expenditures
  # If yes, multiply the appropriate cells by the reduction factor as well.
  if (length(r_mod) > 0) {
    U[r_mod, c_final_mod] <- sweep(U[r_mod, c_final_mod], 1, R, '*')
  }
  
  return(list(M = M, U = U))
  
}  
    
retotal_make_and_use <- function(M, U) {  
  # Takes as input the make and use tables that have had cells modified in the modify_make_and_use function above, and retotals them.
  
  # 0. Create vectors of relevant row and column groups in U
  U_int_col <- dimnames(U)[[2]][1:(which(dimnames(U)[[2]] == 'T001') - 1)]
  U_final_col <- dimnames(U)[[2]][(which(dimnames(U)[[2]] == 'T001') + 1):(which(dimnames(U)[[2]] == 'T004') - 1)]
  
  U_commod_row <- dimnames(U)[[1]][1:(which(dimnames(U)[[1]] == 'T005') - 1)]
  U_valueadded_row <- dimnames(U)[[1]][(which(dimnames(U)[[1]] == 'T005') + 1):(which(dimnames(U)[[1]] == 'T006') - 1)]
  
  # 1. Recalculate intermediate and final row sums in U, and intermediate column totals
  
  U[U_commod_row, 'T001'] <- apply(U[U_commod_row, U_int_col], 1, sum)
  U[U_commod_row, 'T004'] <- apply(U[U_commod_row, U_final_col], 1, sum)
  U[U_commod_row, 'T007'] <- U[U_commod_row, 'T001'] + U[U_commod_row, 'T004']
  U['T005', ] <- apply(U[U_commod_row, ], 2, sum)
  
  # 2. Renormalize *columns* of M based on the new U row sums (set column sums of M to the row sums of U)
  
  # renormalization factor
  renorm_factor <- U[U_commod_row, 'T007'] / M['T007', U_commod_row]
  renorm_factor[renorm_factor == -Inf] <- 1
  
  M[, U_commod_row] <-  sweep(M[, U_commod_row], 2, as.numeric(renorm_factor), '*')
  
  # 3. Recalculate row sums of M
  
  M[, 'T008'] <- apply(M[, 1:(which(dimnames(M)[[2]] == 'T008') - 1)], 1, sum)
  
  # 4. Set final column totals of U to the row sums of M
  
  U['T008', c(U_int_col, 'T001')] <- M[, 'T008']
  
  # 5. Back-calculate new value-added rows in U based on the difference between intermediate column totals of U and final column totals of U
  # Here we do another normalization because there are 3 value-added rows in U, versus 1 in the simplified example.
  
  value_added <- U['T008', c(U_int_col, 'T001')] - U['T005', c(U_int_col, 'T001')]
  
  # Renormalization factor using old value added (T006)
  value_added_renorm_factor <- value_added / U['T006', c(U_int_col, 'T001')]
  U[c(U_valueadded_row, 'T006'), c(U_int_col, 'T001')] <- sweep(U[c(U_valueadded_row, 'T006'), c(U_int_col, 'T001')], 2, as.numeric(value_added_renorm_factor), '*')
  
  return(list(M = M, U = U))
}
