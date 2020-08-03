toproper = function(str) {
  first = toupper(substr(str, 1, 1))
  rest = tolower(substr(str, 2, 100))
  return(paste0(first, rest))
}