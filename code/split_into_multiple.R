# https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns
# split variable into multiple columns
# (when total number of levels is unknown)
split_into_multiple <- function(column, pattern = ",", into_prefix) {
  
  cols <- str_split_fixed(column, pattern, n = Inf)
  # Sub out the ""'s returned by filling the matrix to the right, with NAs which are useful
  cols[which(cols == "")] <- NA
  cols <- as_tibble(cols)
  # name the 'cols' tibble as 'into_prefix_1', 'into_prefix_2', ..., 'into_prefix_m'
  # where m = # columns of 'cols'
  m <- dim(cols)[2]
  
  names(cols) <- paste(into_prefix, 1:m, sep = "_")
  return(cols)
  
}
