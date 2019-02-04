login.jaqpot <- function(){
  basep <- readline("Base path of jaqpot *etc: https://api.jaqpot.org/ : ")
  username <- readline("Username: ")
  password <- getPass(msg = "PASSWORD: ", noblank = FALSE, forcemask = FALSE)
  loginto <- paste(basepath, "jaqpot/services/aa/login/", sep = "")
  body <- list(username=username, password = password)
  httr::set_config(config(ssl_verifypeer = 0L))
  res <- POST(loginto, body = body, encode = "form")
  res <- content(res, "text")
  authResponse <- fromJSON(res)
  return(authResponse)
}
