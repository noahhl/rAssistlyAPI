GetCases <- function(params, conditions=NULL, format=c("list", "object")) {
  url <- paste(strsplit(params$server$auth, ".com")[[1]][1], ".com/api/v1/cases.json", sep="")
  for(i in 1:length(conditions)) {
    conditions[i] <- as.character(conditions[i])
  }
  cases <- MakeRequest(params, url, "GET", conditions)
  if(format[1] == "list") {
    return(fromJSON(cases))    
  }
}

GetSpecificCase <- function(params, id, by="id", format=c("list", "object")) {
    url <- paste(strsplit(params$server$auth, ".com")[[1]][1], ".com/api/v1/cases/",as.character(id),".json", sep="")
    case <- MakeRequest(params, url, "GET", c(by=by))
    if(format[1] == "list") {
      return(fromJSON(case))    
    }
}


UpdateCase <- function(params, case_id, arguments) {
  url <- paste(strsplit(params$server$auth, ".com")[[1]][1], ".com/api/v1/cases/", case_id,".json", sep="")
  for(i in 1:length(arguments)) {
    arguments[i] <- as.character(arguments[i])
  }
  cases <- MakeRequest(params, url, "PUT", arguments)
  return(fromJSON(cases)) 
}