toproper = function(str) {
  first = toupper(substr(dist, 1, 1))
  rest = tolower(substr(dist, 2, 100))
  return(paste0(first, rest))
}