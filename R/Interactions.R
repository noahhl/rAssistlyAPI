GetInteractions <- function(params, conditions=NULL, format=c("list", "object")) {
  url <- paste(strsplit(params$server$auth, ".com")[[1]][1], ".com/api/v1/interactions.json", sep="")
  for(i in 1:length(conditions)) {
    conditions[i] <- as.character(conditions[i])
  }
  interactions <- MakeRequest(params, url, "GET", conditions)
  if(format[1] == "list") {
    return(fromJSON(interactions))    
  }
}
