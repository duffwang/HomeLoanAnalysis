#This file contains auxiliary functions used by the HMDA API

#Uses mixed case for easier readability
HmdaMixedCase <- Vectorize(function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
        sep = "", collapse = " ")
})

#Addresses in the form (New York, NY) have special case
HmdaMixedCaseAddress <- Vectorize(function(x) { 
  #First, the state abbreviation should remain capitalized
  s <- strsplit(x, ",")[[1]]
  
  #Capitalize the city name
  s2 <- strsplit(s[1], " ")[[1]]
  s2 <- paste(toupper(substring(s2, 1, 1)), tolower(substring(s2, 2)), sep = "", collapse = " ")
  
  #We must also take care to capitalize cities separated by a dash
  s2 <- strsplit(s2, "-")[[1]]
  s2 <- paste(toupper(substring(s2, 1, 1)), substring(s2, 2), sep = "", collapse = "-")
  
  paste(s2, ",", toupper(s[2]), sep = "", collapse = " ")
})

#This function correctly parses out NA values when casting to a numeric type.
#It is needed because of whitespace surrounding NA entries in the data file, which causes
#normal casting functions to throw warnings.
as.numeric.na = function(chr.input, bool.integer = FALSE) {
  if (!is.character(chr.input)) { return(chr.input) }
  bool.na = grepl("NA", chr.input) 
  chr.input[bool.na] = 0
  if (bool.integer) {
    chr.input = as.integer(chr.input)
    chr.input[bool.na] = NA_integer_
  } else {
    chr.input = as.numeric(chr.input)
    chr.input[bool.na] = NA_real_
  }
  chr.input
}