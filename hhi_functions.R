#This file contains the functions used by the main code to calculate the Labour Market Concentration Index.

##Function for cleaning the 'companyname' column

sep <- function(linha) {
  resp <- strsplit(linha," |/|-")
  resp <- unlist(resp)
  resp <- gsub(",|;|\\.","",resp)
  resp <- sort(resp[which(nchar(resp) > 2)])
  resp <- paste0(resp,collapse=" ")
  resp <- tolower(resp)
}


