.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to Sana's Package, providing predictive modeling tools for your research. Hope you find it useful!")
}


#' Read dataframe file into R
#'
#' @param file A csv file with training dataframe that you want to read.
#' @return Returns training dataframe
#' @examples
#' data<-readfile("train.csv")

readfile<-function(file){
  data<- read.csv(file, header = TRUE, stringsAsFactors=TRUE);
}

