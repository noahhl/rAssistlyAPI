GetCustomers <- function(params, conditions=NULL, format=c("list", "object")) {
  url <- paste(strsplit(params$server$auth, ".com")[[1]][1], ".com/api/v1/customers.json", sep="")
  for(i in 1:length(conditions)) {
    conditions[i] <- as.character(conditions[i])
  }
  customers <- MakeRequest(params, url, "GET", conditions)
  if(format[1] == "list") {
    return(fromJSON(customers))    
  }
}

GetSpecificCustomer <- function(params, id, format=c("list", "object")) {
    url <- paste(strsplit(params$server$auth, ".com")[[1]][1], ".com/api/v1/customers/",as.character(id),".json", sep="")
    customer <- MakeRequest(params, url, "GET")
    if(format[1] == "list") {
      return(fromJSON(customer))    
    }
}


UpdateCustomer <- function(params, customer_id, arguments) {
  url <- paste(strsplit(params$server$auth, ".com")[[1]][1], ".com/api/v1/customers/", customer_id,".json", sep="")
  for(i in 1:length(arguments)) {
    arguments[i] <- as.character(arguments[i])
  }
  customer <- MakeRequest(params, url, "PUT", arguments)
  return(fromJSON(customer)) 
}
