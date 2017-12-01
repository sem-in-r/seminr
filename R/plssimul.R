##
##  plssimul.r                                                                  R. Schlittgen 6.02.2015
##  

#' Simulation of pls-path models  
#' 
#' \code{plssimul}  simulates pure reflective and formative-reflective pls-path 
#' models using the covariance matrix of the manifest variables  
#'  
#' @param  B (q,q) lower triangular matrix of interrelations of the latent 
#'             variables. See \code{\link{plspath}} for details.
#' @param  lambda (p,1) vector of coeffients of the measurement models.  
#' @param  sxixi  (q1,q1) covariance matrix of the exogeneous latent variables   (reflective relations)
#' @param  sxx (p1,p1) covariance matrix of indicators of the exogeneous latent variables (formative relations)  
#' @param  syy (p2,p2) covariance matrix of indicators of the endogeneous latent variables (formative relations)  
#' @param  R2 coefficient of determination for the structural regression equations
#' @param  refl vector length p of integers  describing with which and in which 
#'         way the latent variables and the manifest variables are related. See 
#'         \code{\link{plspath}} for details.  
#' @param  distr  string, the distribution used to construct the manifest variables. 
#'         Implemented are: "normal" = N(0,1),  "lognormal" = LN(0,1), 
#'         "diflognormal" = difference of independent LN(0,1)
#' @param  n    scalar, the number of observations to be generated   
#'
#' @return dat  (n,p)-matrix, the values of the manifest variables   
#'




#outer function for the data generator, defining the loadings, correlations, etc.
datengenerator<-function(weights=weights,n=n,distr,B, seed){

  set.seed(seed)
  n_ind<-length(weights)
  Rx <- diag(n_ind)
  for(i in seq(length=n_ind/2,from=1,by=2)){
    Rx[i,i+1]<-k
    Rx[i+1,i]<-k  
  }

#  sxx <- rbind(cbind(Rx,matrix(0,n_ind,n_ind),matrix(0,n_ind,n_ind)),cbind(matrix(0,n_ind,n_ind),Rx,matrix(0,n_ind,n_ind)),cbind(matrix(0,n_ind,n_ind),matrix(0,n_ind,n_ind),Rx)) 
  sxx <- diag(12)
  
  refl <- (as.vector(rep(1,n_ind)%o%c(1:5)))*-1
  

  syy <- matrix(1,2*n_ind,2*n_ind)
  for(i in seq(length=2,from=1,by=n_ind)){
    syy[i:(i+ncol(Rx)-1),i:(i+ncol(Rx)-1)]<-Rx
  }
  
  
  manifestvariables<-plssimul(B,lambda=rep(weights,5),w=NULL,sxx=NULL,syy=NULL,sxixi=diag(1,3,3),R2=R2,refl=refl,n=n,distr=distr)
  
  colnames(manifestvariables) <- c("x1","x2","x3", "x4", "x5", "x6","x7","x8","x9","x10","x11","x12","y1","y2","y3","y4","y5","y6","y7","y8")

return(manifestvariables)
}



#main function as defined by Schlittgen 
plssimul <- function(B,lambda=NULL,w=NULL,sxx=NULL,syy=NULL,sxixi=NULL,R2,refl,n,distr="normal"){
  
  
   s <- plscov(B,lambda,w,sxx,syy,sxixi,R2,refl) 
   e <- eigen(s$S, symmetric=TRUE, only.values = TRUE)
   if(e$values[length(refl)] <= 0){ stop("parameter constellation does not allow computing covariance matrix") }
   C <- chol(s$S)   
   
   if(distr == "normal"){ 
     
     
     manifest <- matrix(rnorm(n*ncol(C)),n,ncol(C))%*%C 
    
        
   
   }
   else if(distr == "lognormal"){ manifest <- exp(matrix(rnorm(n*ncol(C)),n,ncol(C)))%*%C  }
   else if(distr == "diflognormal"){  
   manifest <- (exp(matrix(rnorm(n*ncol(C)),n,ncol(C))) - exp(matrix(rnorm(n*ncol(C)),n,ncol(C))))%*%C  
   }else{ stop("only normal, lognormal, diflognormal")   } 
   dat <- scale(manifest)  
  
return(dat) 
} 

 
#' Computation of the covariance matrix of the manifest variables
#' 
#' \code{plscov}  determines the covariance matrix of the manifest variables for 
#'            pure reflective and formative-reflective pls-path models  
#' @param  B  (q,q) lower triangular matrix with the path coefficients
#' @param  lambda vector of coeffients of the measurement models (reflective relations). 
#' @param  w vector of weights of the measurement models (formative relations).   
#' @param  sxx (p1,p1) covariance matrix of indicators of the exogeneous latent variables (formative relations)  
#' @param  syy (p2,p2) covariance matrix of indicators of the endogeneous latent variables (formative relations)  
#' @param  sxixi  (q1,q1) covariance matrix of the exogeneous latent variables 
#'         (reflective relations)  or  (p1,p1) covariance matrix of indicators of
#'         exogeneous latent variables (formative relations)  
#' @param  R2 coefficient of determination for the structural regression equations
#' @param  refl vector of length p of integers  absolute values from 1 to q1+q2  
#'         see \code{\link{plspath}} for details 
#' 
#' @return  s  (p,p)-matrix, the covariance matrix of the manifest variables   
#'

plscov <- function(B,lambda=NULL,w=NULL,sxx=NULL,syy=NULL,sxixi=NULL,R2=NULL,refl){   
 
q <- nrow(B)                        # number of latent variables
b <- rep(1,q)
q1 <- length(b[rowSums(B) == 0])    # number of exogeneous latent variables 
q2 <- q - q1                        # number of endogeneous latent variables
p <- length(refl)                   # number of manifest variables
p1 <- length(refl[abs(refl) <= q1]) # number of indicators of exogeneous latent v. 
p2 <- p - p1                        # number of indicators of endogeneous latent v.      
s <- 0   
B1 <- B[(q1+1):q,1:q1,drop=FALSE]   
B2 <- B[(q1+1):q,(q1+1):q,drop=FALSE] 
IB <- solve(diag(q2) - B2)   


if(all(refl < 0)){                                                               # all relations reflective
  
   x <- abs(refl[1:p1])
   Lx <- matrix(x,length(x),max(x))
   Lx <- 1*(Lx == matrix(unique(x),length(x),max(x),byrow=T))
   Lambdax <- Lx*lambda[1:p1]
   y <- abs(refl[(p1+1):p])-q1
   Ly <- matrix(y,length(y),max(y))
   Ly <- 1*(Ly == matrix(unique(y),length(y),max(y),byrow=T)) 
   Lambday <- Ly*lambda[(p1+1):p]     
   sxx <- Lambdax%*%sxixi%*%t(Lambdax)
   ThetaD <- 1 - diag(sxx)
   sxx <- sxx + diag(ThetaD) 
   # computation of szetazeta
   if(q2 == 1){szetazeta <- 1 - (IB%*%B1)%*%sxixi%*%(t(IB%*%B1))
   }else{
     fun <- function(szetazeta){ sum( (1 - diag( (IB%*%B1)%*%sxixi%*%(t(IB%*%B1)) + IB%*%diag(szetazeta)%*%t(IB)) )^2) }
     out <- optim(rep(1,q2),fun )   
     szetazeta <- diag(out$par) } 
   setaeta = IB%*%(B1%*%sxixi%*%t(B1) + szetazeta)%*%t(IB)    
   syy <- Lambday%*%setaeta%*%t(Lambday) 
   ThetaE <- 1 - diag(syy)
   syy <- syy + diag(ThetaE)
   sxy <- Lambdax%*%sxixi%*%t(B1)%*%t(IB)%*%t(Lambday) 
   s <- rbind(cbind(sxx,sxy),cbind(t(sxy),syy)) 
   
  }else if((all(refl[1:p1] > 0)) & (all(refl[(p1+1):(p1+p2)] < 0))){                    # formative-reflective 
   
   x <- abs(refl[1:p1])
   Wx <- matrix(x,length(x),max(x))
   Wx <- 1*(Wx == matrix(unique(x),length(x),max(x),byrow=T))
   Wx <- Wx*w 
   sxixi <- t(Wx)%*%sxx%*%Wx  
   # computation of szetazeta = diagonal matrix of errors of structural model relations
   if(q2 == 1){szetazeta <- 1 - (IB%*%B1)%*%sxixi%*%(t(IB%*%B1))
   }else{
     fun <- function(szetazeta){ sum( (1 - diag( IB%*%(B1%*%sxixi%*%t(B1) + diag(szetazeta))%*%t(IB)) )^2) }
     out <- optim(rep(1,q2),fun )   
     szetazeta <- diag(out$par) }
   setaeta <- IB%*%(B1%*%sxixi%*%t(B1) + szetazeta)%*%t(IB)   
   y <- abs(refl[(p1+1):p])-q1
   Ly <- matrix(y,length(y),max(y))
   Ly <- 1*(Ly == matrix(unique(y),length(y),max(y),byrow=T)) 
   Lambday <- Ly*lambda   
   syy <- Lambday%*%setaeta%*%t(Lambday) 
   ThetaE <- 1 - diag(syy)
   syy <- syy + diag(ThetaE)
   sxy <- sxx%*%Wx%*%t(B1)%*%t(IB)%*%t(Lambday)    
   s <- rbind(cbind(sxx,sxy),cbind(t(sxy),syy))  

}else if((all(refl > 0)) ){                                                          # formative-formative 
  
   x <- abs(refl[1:p1])
   Wx <- matrix(x,length(x),max(x))
   Wx <- 1*(Wx == matrix(unique(x),length(x),max(x),byrow=T))
   Wx <- Wx*w[1:p1]

# scaling of weights  
     
   Wx <- scale(Wx, center=F,scale=sqrt(diag((t(Wx)%*%sxx%*%Wx))))

   sxixi <- t(Wx)%*%sxx%*%Wx  

# scaling of path coefficients and computing covariance matrix of latent constructs 
 
   sll <- diag(q1+q2)
   sll[1:q1,1:q1] <- sxixi
   for (m in c((q1+1):(q1+q2))){  
    if (length(R2) > 0){ 
      tau <- sqrt(R2/(B[m,1:(m-1),drop=F]%*%sll[1:(m-1),1:(m-1)]%*%t(B[m,1:(m-1),drop=F])) )
      B[m,] <- as.vector(tau)*B[m,] 
      }
      for (j in c(1:m)){ 
             sll[j,m] <- sll[m,j] <- B[m,1:(m-1),drop=F]%*%sll[1:(m-1),j,drop=F] 
         diag(sll) <- 1  
       }  }   
   B1 <- B[(q1+1):q,1:q1,drop=FALSE]   
   B2 <- B[(q1+1):q,(q1+1):q,drop=FALSE] 
   IB <- solve(diag(q2) - B2)   
 
   setaeta <- sll[(q1+1):(q1+q2),(q1+1):(q1+q2)]
        
   y <- abs(refl[(p1+1):(p1+p2)]) - q1
   Wy <- matrix(y,length(y),max(y))
   Wy <- 1*(Wy == matrix(unique(y),length(y),max(y),byrow=T))
   Wy <- Wy*w[(p1+1):(p1+p2)] 
   Wy <- scale(Wy, center=F,scale=sqrt(diag((t(Wy)%*%syy%*%Wy)))) 

# modification of syy outside the blocks of indicators to satisfy Wy'*syy*Wy = setaeta  
 
   for (i in c(1:(q2-1))){  
       syy[i == y, i > y] <- 1  
       syy[i > y, i == y] <- 1  
       } 
   for (i in c(1:(q2-1))){     
       for ( j in c((i+1):q2) ){ 
          syy[i == y, j == y] <- setaeta[i,j]/sum(t(Wy[i == y,i,drop=F])%*%syy[i == y, j == y]%*%Wy[j == y,j,drop=F]) 
          syy[j == y, i == y] <- t(syy[i == y, j == y])  
      }}           
   sxy <- sxx%*%Wx%*%t(B1)%*%t(IB)%*%solve(setaeta)%*%t(Wy)%*%syy    
 
   s <- rbind(cbind(sxx,sxy),cbind(t(sxy),syy))   

   
}else if((all(refl[1:p1] < 0)) & (all(refl[(p1+1):(p1+p2)] > 0))){                    # reflective-formative

   x <- abs(refl[1:p1])
   Lx <- matrix(x,length(x),max(x))
   Lx <- 1*(Lx == matrix(unique(x),length(x),max(x),byrow=T))
   Lambdax <- Lx*lambda    
   sxx <- Lambdax%*%sxixi%*%t(Lambdax)
   ThetaD <- 1 - diag(sxx)
   sxx <- sxx + diag(ThetaD) 
   y <- abs(refl[(p1+1):(p1+p2)]) - q1
   Wy <- matrix(y,length(y),max(y))
   Wy <- 1*(Wy == matrix(unique(y),length(y),max(y),byrow=T))
   Wy <- Wy*w
   sxy <- Lambdax%*%sxixi%*%t(B1)%*%t(IB)%*%t(Wy)
   s <- rbind(cbind(sxx,sxy),cbind(t(sxy),syy))  
      
}else{  stop("error in measurement relations")   } 

#out <- list(S=s,Wx=Wx,Wy=Wy,sll=sll,B=B)
out <- list(S=s,B=B)

return(out)
}




