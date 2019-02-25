login.jaqpot <- function(basepath){
  method <- readline("Please choose authentication method: login[1] / Provide Api Key[2]: ")
  if(method == 1){
    username <- readline("Username: ")
    password <- getPass(msg = "PASSWORD: ", noblank = FALSE, forcemask = FALSE)
    loginto <- paste(basepath, "jaqpot/services/aa/login/", sep = "")
    body <- list(username=username, password = password)
    httr::set_config(config(ssl_verifypeer = 0L))
    res <- POST(loginto, body = body, encode = "form")
    res <- content(res, "text")
    authResponse <- fromJSON(res)
    token = authResponse$authToken
  }
  if(method == 2){
    token <- getPass(msg = "Api Key: ", noblank = FALSE, forcemask = FALSE)
  }
  else{
    print("Wrong input");
  }
  return(token)
}
