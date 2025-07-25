
# set-up the list of models

# experiment 1
model_list_exp1 <-
  list(m1 = log_B ~ N + M + P + N:M + N:P + M:P + N:M:P,
       m2 = log_B ~ bs(N, df = 2, degree = 1) + M + P + bs(N, df = 2, degree = 1):M + bs(N, df = 2, degree = 1):P + M:P + bs(N, df = 2, degree = 1):M:P,
       m3 = log_B ~ bs(N, df = 3, degree = 1) + M + P + bs(N, df = 3, degree = 1):M + bs(N, df = 3, degree = 1):P + M:P + bs(N, df = 3, degree = 1):M:P,
       m4 = log_B ~ bs(N, df = 3, degree = 2) + M + P + bs(N, df = 3, degree = 2):M + bs(N, df = 3, degree = 2):P + M:P + bs(N, df = 3, degree = 2):M:P,
       m5 = log_B ~ bs(N, df = 4, degree = 3) + M + P + bs(N, df = 4, degree = 3):M + bs(N, df = 4, degree = 3):P + M:P + bs(N, df = 4, degree = 3):M:P)

# experiment 2
model_list_exp2 <-
  list(m1 = log_B ~ N + M + I + N:M + N:I + M:I + N:M:I,
       m2 = log_B ~ bs(N, df = 2, degree = 1) + M + I + bs(N, df = 2, degree = 1):M + bs(N, df = 2, degree = 1):I + M:I + bs(N, df = 2, degree = 1):M:I,
       m3 = log_B ~ bs(N, df = 3, degree = 1) + M + I + bs(N, df = 3, degree = 1):M + bs(N, df = 3, degree = 1):I + M:I + bs(N, df = 3, degree = 1):M:I,
       m4 = log_B ~ bs(N, df = 3, degree = 2) + M + I + bs(N, df = 3, degree = 2):M + bs(N, df = 3, degree = 2):I + M:I + bs(N, df = 3, degree = 2):M:I,
       m5 = log_B ~ bs(N, df = 4, degree = 3) + M + I + bs(N, df = 4, degree = 3):M + bs(N, df = 4, degree = 3):I + M:I + bs(N, df = 4, degree = 3):M:I)

remove_three_way_interaction <- function(f) {
  
  # Convert formula to character and extract right-hand side
  rhs <- deparse(formula(f)[[3]])
  
  # Remove spline-based three-way interaction: bs(N, ...):M:P
  rhs <- gsub(" *\\+ *bs\\(N, df *= *[0-9]+, degree *= *[0-9]+\\):M:P", "", rhs)
  
  # Remove spline-based three-way interaction: bs(N, ...):M:I
  rhs <- gsub(" *\\+ *bs\\(N, df *= *[0-9]+, degree *= *[0-9]+\\):M:I", "", rhs)
  
  # Remove standard three-way interaction: N:M:P
  rhs <- gsub(" *\\+ *N:M:P", "", rhs)
  
  # Remove standard three-way interaction: N:M:I
  rhs <- gsub(" *\\+ *N:M:I", "", rhs)
  
  # Also handle the edge case where it's the only term (no preceding +)
  rhs <- gsub("^ *N:M:P *\\+? *", "", rhs)  # beginning of RHS
  rhs <- gsub(" *\\+? *N:M:P *$", "", rhs)  # end of RHS
  
  # handle the edge cases for the experiment 2
  rhs <- gsub("^ *N:M:I *\\+? *", "", rhs)  # beginning of RHS
  rhs <- gsub(" *\\+? *N:M:I *$", "", rhs)  # end of RHS
  
  # Build and return cleaned formula
  as.formula(paste(deparse(formula(f)[[2]]), "~", paste(rhs, collapse = " ")))
}
