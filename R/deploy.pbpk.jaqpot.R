# This is JaqpotforR function of jaqpot named 'deploy.pbpk.jaqpot'
# which deploys a pbpk model on 'Jaqpot'.
#
#' deploy.pbpk.jaqpot takes as input the dataframe upon the predictions are made, in
#' order to create the dataset corresponding to the model
#'
#' a covariate model wich calculates physiological parameters based
#' on a patient's characteristics
#'
#'
#' @param dataframe
#' @param odes
#' @param covariate_model

deploy.pbpk.jaqpot <- function(dataframe, covariate_model, odes){
  # basep <- 'http://localhost:8080/'
  # basep <- 'https://api.jaqpot.org/'
  basep <- readline("Base path of jaqpot *etc: https://api.jaqpot.org/ : ")
  username <- readline("Username: ")
  password <- getPass(msg = "PASSWORD: ", noblank = FALSE, forcemask = FALSE)
  loginto <- paste(basep, "jaqpot/services/aa/login/", sep = "")
  print(loginto)
  body <- list(username=username, password = password)
  httr::set_config(config(ssl_verifypeer = 0L))
  res <- POST(loginto, body = body, encode = "form")
  # res <- postForm(loginto, username=username, password=password, style='POST')
  res <- content(res, "text")
  authResponse <- fromJSON(res)
  independentFeaturesfm <- colnames(dataframe)

  title <- readline("Title of the model: ")
  discription <- readline("Discription of the model:")
  algorithm <- 'ODESOLVER'
  libabry_in <- "deSolve"

  predicts <- list()
  i <- 1
  prompt <- paste()
  check <- 0
  while( check != 1){
    check <- readline("Provide the compartment with the same order as the ODE's or press 1 to exit: ")
    if(check ==1){
      break;
    }
    predicts[[i]] <- paste(i, check, sep="_")
    print(paste("Compartment number ", i, " has name: ", check, sep = ""))
    i <- i + 1
  }
  predicts[i+1] <- "0_time"
  predicts <- array(as.character(unlist(predicts)))
  model <- serialize(list(COVMODEL=covariate_model, ODEMODEL=odes),connection=NULL)
  tojson <- list(rawModel=model,runtime="pbpk-ode", implementedWith=libabry_in,pmmlModel=NULL,independentFeatures=independentFeaturesfm,
                 predictedFeatures=predicts, dependentFeatures=predicts, title=title, description=discription, algorithm=algorithm)
  json <- toJSON(tojson)
  bearer = paste("Bearer", authResponse$authToken, sep=" ")
  res = POST(basep, path="jaqpot/services/model",
             add_headers(Authorization=bearer),
             accept_json(),
             content_type("application/json"),
             body = json, encode = "json")
  code <- status_code(res)
  if(status_code(res) == 200 ){
    resp <- content(res, "text")
    respon <- fromJSON(resp)
    response <- paste("Model created. The id is: ", respon$modelId, ". Please visit: https://app.jaqpot.org/ to complete ", sep=" ")
    response
  }else{
    code
  }
}
