#simplePLS
#Description: This library contains the functions utilized to run the PLS-PM
# algorithm.

#Function that estimates the PLS-PM Model
simplePLS <- function(obsData,smMatrix, mmMatrix, maxIt=300, stopCriterion=7){

  #Create list of Measurements Variables
  mmVariables <- mmMatrix[,"measurement"]

  #Create list of Latent Variables
  ltVariables <- unique(c(smMatrix[,1],smMatrix[,2]))

#  normData <- as.matrix(obsData[,mmVariables])
  #Extract and Normalize the measurements for the model
#  for(cols in mmVariables) {
#    if(!grepl("\\.", cols)) {
#      normData[,cols] <- scale(obsData[,cols],TRUE,TRUE)
#   } else {
#      normData[,cols] <- obsData[,cols]
#    }
#  }
  normData <- scale(obsData[,mmVariables],TRUE,TRUE)

  #Extract Mean and Standard Deviation of measurements for future prediction
  meanData <- attr(normData, "scaled:center")
  sdData <- attr(normData, "scaled:scale")

  #Create a matrix of outer_weights
  outer_weights <- matrix(data=0,
                          nrow=length(mmVariables),
                          ncol=length(ltVariables),
                          dimnames = list(mmVariables,ltVariables))

  #Initialize outer_weights matrix with value 1 for each relationship in the measurement model
  for (i in 1:length(ltVariables))  {
    outer_weights [mmMatrix[mmMatrix[,"latent"]==ltVariables[i],
                            "measurement"],
                   ltVariables[i]] =1
  }

  #Create a matrix of inner paths
  #? inner_paths => inner_weights?
  inner_paths <- matrix(data=0,
                        nrow=length(ltVariables),
                        ncol=length(ltVariables),
                        dimnames = list(ltVariables,ltVariables))

  #Iterative Process Starts here
  for (iterations in 0:maxIt)  {

    #Estimate Factor Scores from Outter Path
    #? fscores <- normData%*%outer_weights
    fscores <- normData[,mmVariables]%*%outer_weights

    #Standardize Factor Scores
#    for(cols in colnames(fscores)) {
#      if(!grepl("\\.", cols)) {
#        fscores[,cols] <- scale(fscores[,cols],TRUE,TRUE)
#      }
#    }

    fscores <- scale(fscores,TRUE,TRUE)

    #Estimate inner paths (symmetric matrix)
    for (i in 1:nrow(smMatrix))  {
      inner_paths[smMatrix[i,"source"],
                  smMatrix[i,"target"]] = cov(fscores[,smMatrix[i,"source"]],
                                              fscores[,smMatrix[i,"target"]])
      #? next step necessary?
      inner_paths[smMatrix[i,"target"],
                  smMatrix[i,"source"]] = cov(fscores[,smMatrix[i,"source"]],
                                              fscores[,smMatrix[i,"target"]])
    }

    #Estimate Factor Scores from Inner Path
    fscores<-fscores%*%inner_paths

    #Standarize Factor Scores
#    for(cols in colnames(fscores)) {
#      if(!grepl("\\.", cols)) {
#        fscores[,cols] <- scale(fscores[,cols],TRUE,TRUE)
#      }
#    }

    fscores <- scale(fscores,TRUE,TRUE)

    #Save last outer_weights
    last_outer_weights <- outer_weights

    #Update outer_weights
    for (i in 1:length(ltVariables))  {

      #If the measurement model is Formative
      if(mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"type"][1]=="F"){
        outer_weights[mmMatrix[mmMatrix[,"latent"]==ltVariables[i], "measurement"], ltVariables[i]] =
          solve(cor(normData[,mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"measurement"]])) %*%
                  cor(normData[,mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"measurement"]],
                fscores[,ltVariables[i]])
      }

      #If the measurement model is Reflective
      if(mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"type"][1]=="R"){
        outer_weights[mmMatrix[mmMatrix[,"latent"]==ltVariables[i], "measurement"], ltVariables[i]] =
          cov(normData[,mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"measurement"]],fscores[,ltVariables[i]])
      }
    }

    #Estimate Factor Scores from Outer Weights
    fscores <- normData[,mmVariables]%*%outer_weights

    #Standarize outer_weights
    for (i in 1:length(ltVariables))  {
      outer_weights [mmMatrix[mmMatrix[,"latent"]==ltVariables[i], "measurement"], ltVariables[i]] =
        outer_weights [mmMatrix[mmMatrix[,"latent"]==ltVariables[i], "measurement"], ltVariables[i]] / sd(fscores[,ltVariables[i]])
    }

    #Verify the stop criteria
    weightDiff <- sum(abs(outer_weights-last_outer_weights))
    if (weightDiff <(10^(-(stopCriterion))))
      break

  } #Finish Iterative Process

  #Estimate Factor Scores from Outter Path
  fscores <- normData[,mmVariables]%*%outer_weights


  #Initialize Matrix of Path Coefficients
  path_coef <- matrix(data=0,
                      nrow=length(ltVariables),
                      ncol=length(ltVariables),
                      dimnames = list(ltVariables,ltVariables))

  #Identify which variables have incoming paths
  dependant<-unique(smMatrix[,"target"])

  #We calculate a linear regresion for each dependant variable
  for (i in 1:length(dependant))  {

    #Indentify the independant variables
    independant<-smMatrix[smMatrix[,"target"]==dependant[i],"source"]

    #Solve the sistem of equations
    results<- solve(cor(fscores[,independant, drop=FALSE]),
                    cor(fscores[,independant], fscores[,dependant[i]]))

    #Transform to a generic vector
    coefficients <- as.vector(results)
    if(!is.null(rownames(results)))
      names(coefficients)<-rownames(results)
    else
      names(coefficients)<-names(results)

    #Assign the Beta Values to the Path Coefficient Matrix
    for (j in 1:length(independant))
      path_coef[independant[j],dependant[i]]=coefficients[independant[j]]

  }

  #Create a matrix of Outer Loadings
  outer_loadings <- matrix(data=0,
                           nrow=length(mmVariables),
                           ncol=length(ltVariables),
                           dimnames = list(mmVariables,ltVariables))


  #Calculate the Outer Loadings
  for (i in 1:length(ltVariables))  {
    outer_loadings [mmMatrix[mmMatrix[,"latent"]==ltVariables[i],
                             "measurement"],
                    ltVariables[i]] = cov(normData[,mmMatrix[mmMatrix[,"latent"]==ltVariables[i],"measurement"]],fscores[,ltVariables[i]])

  }

  #Calculate R Squared

  #Get smMatrix
  modelMatrix <- data.frame(smMatrix)

  #Get endogenous composites
  uniquetarget <- as.character(unique(modelMatrix$target))

  #Get composite scores
  valuesMatrix <- fscores

  #Calculate Linear Models
  lmmodels <- lapply(uniquetarget, function(x) {lm(as.formula(paste(x,"~ .", sep = "")),
                                                   data = data.frame(valuesMatrix[,colnames(valuesMatrix) %in%
                                                                                    c(x,as.character(modelMatrix$source[which(modelMatrix$target==x)]))]))})

  #Initialize matrix holder for Rsquared values
  rSquared <- matrix(,nrow=1,ncol=length(uniquetarget),byrow =TRUE,dimnames = list(1,uniquetarget))

  # Iterate and extract every R^2 value
  for (i in 1:length(lmmodels)) {
    rSquared[,i] <- summary(lmmodels[[i]])$r.squared
  }


  #Prepare return Object
  plsModel <- list(meanData = meanData,
                   sdData = sdData,
                   smMatrix = smMatrix,
                   mmMatrix = mmMatrix,
                   ltVariables = ltVariables,
                   mmVariables = mmVariables,
                   outer_loadings = outer_loadings,
                   outer_weights = outer_weights,
                   path_coef = path_coef,
                   iterations = iterations,
                   weightDiff = weightDiff,
                   fscores = fscores,
                   rSquared = rSquared)

  class(plsModel) <- "plsModel"
  return(plsModel)
}
