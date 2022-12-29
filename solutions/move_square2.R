move_square2 <- function(current) {
  # replace data.frame with a matrix (simpler data structure)
  rolls = matrix(sample(1:6, 6, replace = TRUE), ncol = 2)
  # create new vectors for new computations rather than extending objects
  # use rowSums (matricized function) instead of apply()
  Total = rowSums(rolls)			
  IsDouble = rolls[,1] == rolls[,2]
  # use && vs &
  if(IsDouble[1] && IsDouble[2] && IsDouble[3]) {
    current = 11#Go To Jail
  } else if(IsDouble[1] && IsDouble[2]) {
    current = current + sum(Total[1:3])
  } else if(IsDouble[1]) {
    current = current + sum(Total[1:2])
  } else {
    current = current + Total[1]
  }
  current
}

