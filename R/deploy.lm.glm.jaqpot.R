# Deploy lm glm
#
# This is the base function of jaqpot named 'deploy.on.jaqpot'
# which deploys a model on 'Jaqpot'.
#
#' deploy.on.jaqpot takes as input a trained model, it uploads it
#' after some prompt questions and returns a model id
#'
#' Suports lm from base
#' @param model

# source('login.jaqpot.R', local = TRUE)

deploy.lm.glm.jaqpot <- function(object){

  basep <- readline("Base path of jaqpot *etc: https://api.jaqpot.org/ : ")
  token <- login.jaqpot(basep)

  checkfeatures <- array( names(coef(object)));
  if(checkfeatures[1]  %in% "(Intercept)"){
    independentFeaturesfm <- checkfeatures[!checkfeatures  %in% "(Intercept)"]
  }else{
    independentFeaturesfm <- checkfeatures
  }
  title <- readline("Title of the model: ")
  discription <- readline("Discription of the model:")
  if(library_check == 1){
    libabry_in <- "base"
  }
  predicts <- readline("Actual name of the predicted feature: ")
  model <- serialize(list(MODEL=object),connection=NULL)
  tojson <- list(rawModel=model,runtime="R-lm-glm", implementedWith="lm or a glm in r",pmmlModel=NULL,independentFeatures=independentFeaturesfm,
                 predictedFeatures=predicts, dependentFeatures=predicts, title=title, description=discription, algorithm="lm or a glm in r")
  json <- toJSON(tojson)
  bearer = paste("Bearer", token, sep=" ")
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
