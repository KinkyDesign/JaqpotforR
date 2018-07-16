# This is the base function of jaqpot named 'deploy.on.jaqpot'
# which deploys a model on 'Jaqpot'.
#
#' deploy.on.jaqpot takes as input a trained model, it uploads it
#' after some prompt questions and returns a model id
#' @param model

deploy.on.jaqpot <- function(object){
  username <- readline("Username: ")
  password <- readline("Password: ")
  res <- postForm("http://localhost:8080/jaqpot/services/aa/login/swag", username=username, password=password, style='POST')
  authResponse <- fromJSON(res)
  checkfeatures <- array( names(coef(object)));
  if(checkfeatures[1]  %in% "(Intercept)"){
    independentFeaturesfm <- checkfeatures[!checkfeatures  %in% "(Intercept)"]
  }else{
    independentFeaturesfm <- checkfeatures
  }
  title <- readline("Title of the model: ")
  discription <- readline("Discription of the model:")
  algorithm <- readline("Type or algorithm trained upon: ")
  library_check <- readline("[1] base, [2] caret: ")
  if(library_check == 1){
    libabry_in <- "base"
  }
  predicts <- readline("Actual name of the predicted feature: ")
  model <- serialize(list(MODEL=object),connection=NULL)
  tojson <- list(rawModel=model,implementedIn="R", implementedWith=libabry_in,pmmlModel=NULL,independentFeatures=independentFeaturesfm,
                 predictedFeatures=predicts, dependentFeatures=predicts, title=title, discription=discription, algorithm=algorithm)
  json <- toJSON(tojson)
  url = "http://localhost:8080/"
  bearer = paste("Bearer", authResponse$authToken, sep=" ")
  res = POST(url, path="jaqpot/services/model",
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
