import_delim <- function(file, sep, dec){
  tab <- NULL
  enc <- readr::guess_encoding(file, threshold = .9)$encoding[1]
  if (is.na(enc) | enc == "ASCII") enc <- ""
  if (tolower(tools::file_ext(file)) == "txt" | sep != "\t" | dec != "."){
    tab <- read.delim(file, strip.white = TRUE, fileEncoding = enc,  na.strings=c("NA", "", " ", "."),
                      stringsAsFactors = FALSE, check.names = FALSE, sep = sep, dec = dec)
  } else if (tolower(tools::file_ext(file)) == "csv"){
    tab <- import_csv(file, enc = enc)
  }
  tab
}

import_csv <- function(file, enc = "") {
  tab <- read.csv2(file, na.strings=c("NA", "", " ", "."), fileEncoding = enc,
                   strip.white = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  if(length(tab) == 1){
    tab <- read.csv(file, na.strings=c("NA", ""," ", "."), strip.white = TRUE, fileEncoding = enc,
                    stringsAsFactors = FALSE, check.names = FALSE)
  }
  tab
}

import_file <- function(x, sep = "\t", dec = "."){
  ext <- tools::file_ext(x)
  if(ext %in% c("csv", "txt")){
    import_delim(x, sep = sep, dec = dec)
  }  else if (ext %in% c("xls", "xlsx", "xlsm")){
    return(read_excel(x))
  } else {
    toast_showNotif("Extension de fichier non prise en charge")
  }
}

