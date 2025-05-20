
## Sharma, P.N., Liengaard, B.D., Hair, J.F., Sarstedt, M., Ringle, C.M. (2023)
## "Predictive model assessment and selection in composite-based modeling using
## PLS-SEM: extensions and guidelines for using CVPAT", European Journal of
## Marketing, Vol. 57 No. 6, pp. 1662-1677.
## https://doi-org.elib.tcd.ie/10.1108/EJM-08-2020-0636
# Example
# Create measurement model ----
corp_rep_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
)

# Create structural model ----
sm_one <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"), to = c("CUSL")))

pls_model <- estimate_pls(
  data = corp_rep_data,
  measurement_model = corp_rep_mm,
  structural_model  = sm_one,
  missing = mean_replacement,
  missing_value = "-99")


assess_overall_cvpat(pls_model)
assess_overall_cvpat(corp_rep_pls_model_ext)

# Example usage
assess_overall_cvpat <- function(model) {

  # First we must calculate a IA model which is the "indicator average" model ----
  # we must identify endogenous latents and measures
  endo_lvs <- seminr:::all_endogenous(model$smMatrix)
  endo_mvs <- unlist(lapply(endo_lvs,
                     function(x) seminr:::items_of_construct(construct = x,
                                                             model = model)))
  # Indicator average (IA) from the training model
  IA <- model$meanData[endo_mvs]
  # Calculate IA predictive error
  IA_pred_error <- sweep(model$data[,endo_mvs],2 ,IA)

  # Calculate lm predictions
  pls_predict_model <- predict_pls(model, technique = predict_EA)

  PLS_predict_error <- pls_predict_model$PLS_out_of_sample_residuals[,endo_mvs,drop = F]
  LM_predict_error <- pls_predict_model$lm_out_of_sample_residuals[,endo_mvs,drop = F]

  # Calculate LV-specific losses
  ## for IA model
  LV_losses_IA <- do.call("cbind", lapply(endo_lvs,
         function(x) lv_loss(construct = x,
                             model = model,
                             error = IA_pred_error)))
  ## for LM model
  LV_losses_LM <- do.call("cbind", lapply(endo_lvs,
                                          function(x) lv_loss(construct = x,
                                                              model = model,
                                                              error = LM_predict_error)))
  ## for PLS model
  LV_losses_PLS <- do.call("cbind", lapply(endo_lvs,
                                          function(x) lv_loss(construct = x,
                                                              model = model,
                                                              error = PLS_predict_error)))
  # Name LVs
  colnames(LV_losses_IA) <-  colnames(LV_losses_LM) <- colnames(LV_losses_PLS) <- endo_lvs

  # Calculate overall loss
  ## for IA model
  IA_overall <- overall_loss(LV_losses_IA)
  ## for LM model
  LM_overall <- overall_loss(LV_losses_LM)
  # for PLS model
  PLS_overall <- overall_loss(LV_losses_PLS)

  # CVPAT: PLS vs IA overall
  PLS_v_IA_overall <- bootstrap_cvpat(PLS_overall,
                               IA_overall,
                              testtype = "two.sided",
                              BootSamp = 2000)

  # CVPAT: PLS vs LM overall
  PLS_v_LM_overall <- bootstrap_cvpat(LossM1 = PLS_overall,
                                      LossM2 = LM_overall,
                                      testtype = "two.sided",
                                      BootSamp = 2000)
  ia_cvpat <- cvpat_per_construct(loss_one = LV_losses_PLS,
                      loss_two = LV_losses_IA,
                      testtype = "two.sided",
                      BootSamp = 2000)
  lm_cvpat <- cvpat_per_construct(loss_one = LV_losses_PLS,
                      loss_two = LV_losses_LM,
                      testtype = "two.sided",
                      BootSamp = 2000)

  colMeans(LV_losses_PLS)
  colMeans(LV_losses_IA)
  colMeans(LV_losses_LM)
  mat_one <- cbind(colMeans(LV_losses_PLS),colMeans(LV_losses_LM),
        colMeans(LV_losses_PLS) - colMeans(LV_losses_LM),
        lm_cvpat[,-1])
  colnames(mat_one)[1:3] <- c("PLS Loss", "LM Loss", "Diff")
  mat_one
  mat_one <- rbind(mat_one, c(mean(PLS_overall),
                              mean(LM_overall),
                              mean(PLS_overall) -  mean(LM_overall),
                              PLS_v_LM_overall))
  rownames(mat_one)[nrow(mat_one)] <- "Overall"
  mat_one <- mat_one[,c(1,2,3,6,7)]

  mat_two <- cbind(colMeans(LV_losses_PLS),colMeans(LV_losses_IA),
                   colMeans(LV_losses_PLS) - colMeans(LV_losses_IA),
                   ia_cvpat[,-1])
  colnames(mat_two)[1:3] <- c("PLS Loss", "IA Loss", "Diff")
  PLS_v_IA_overall

  mat_two <- rbind(mat_two, c(mean(PLS_overall),
                              mean(IA_overall),
                              mean(PLS_overall) -  mean(IA_overall),
                              PLS_v_IA_overall))

  rownames(mat_two)[nrow(mat_two)] <- "Overall"
  mat_two <- mat_two[,c(1,2,3,6,7)]



  return(list(CVPAT_compare_LM = mat_one,
              CVPAT_compare_IA = mat_two))

}

#function to apply bootstrap to every LV
cvpat_per_construct <- function(loss_one = LV_losses_PLS,
                                loss_two = LV_losses_IA,
                                testtype = "two.sided",
                                BootSamp = 2000) {

    index <- colnames(loss_one)
    results <- matrix(nrow = 0, ncol = 6)
    colnames(results) <- c("Construct","Std. T value", "Std. P value", "Boot T value", "Boot P Value", "Perc. P Value")
    for (iter in index) {
       results <- rbind(results,c(iter,bootstrap_cvpat(loss_one[,iter],
                                       loss_two[,iter],
                                       testtype = testtype,
                                       BootSamp = BootSamp)))
    }
return(results)
}

lv_loss <- function(construct, model, error) {
  if(class(error) == "data.frame" | class(error) == "matrix") {
    loss <- rowMeans(error[,seminr:::items_of_construct(construct = construct,
                                                        model = model),drop = F]^2)
  }
  if (class(error) == "numeric") {
    loss <- error[,seminr:::items_of_construct(construct = construct,
                                               model = model),drop = F]^2
  }
  return(loss)
}

overall_loss <- function(error) {
  if(class(error)[1] == "data.frame" | class(error)[1] == "matrix") {
    loss <- rowMeans(error)
  }
  if (class(error)[1] == "numeric") {
    loss <- error
  }
  return(loss)
}

bootstrap_cvpat <- function(LossM1,
                            LossM2,
                            testtype = "two.sided",
                            BootSamp = 2000) {

  N <- length(LossM1)



  OrgTtest<-t.test(LossM2,LossM1,alternative = testtype, paired=TRUE)$statistic


  # Originial average difference in losses
  OrgDbar<-mean(LossM2-LossM1)
  # Differences in loss functions under the null
  D_0<-LossM2-LossM1-OrgDbar
  # Differences in loss functions
  D<-LossM2-LossM1
  #Allocating memory to bootrap
  BootSample <- matrix(0,ncol=2,nrow=length(D))
  BootDbar <- rep(0,BootSamp)
  m_losses<-cbind(LossM1,LossM2)
  tStat <- rep(0,BootSamp)
  for (b in 1:BootSamp) {
    BootSample <- m_losses[sample((1:length(D)), length(D), replace=TRUE),]
    tStat[b]<-t.test(BootSample[,2],BootSample[,1],mu=mean(D),alternative=testtype, paired=TRUE)$statistic
    BootDbar[b] <- mean(sample(D_0, length(D_0), replace=TRUE))
  }
  SorttStat<-sort(tStat, decreasing = FALSE)
  SortBootDbar<-sort(BootDbar, decreasing = FALSE)
  # Bootstrap variance on Dbar for t-test
  std<-sqrt(var(BootDbar))
  tstat_boot_Var<-OrgDbar/std
  # Calculating p-values
  if (testtype=="two.sided") {
    p.value_perc_Ttest<-(sum(SorttStat>abs(OrgTtest))+sum(SorttStat<=(-abs(OrgTtest))))/BootSamp
    p.value_perc_D<-(sum(SortBootDbar>abs(OrgDbar))+sum(SortBootDbar<=(-abs(OrgDbar))))/BootSamp
    p.value_var_ttest<-2*pt(-abs(tstat_boot_Var),(N-1), lower.tail = TRUE)
  }
  if (testtype=="greater") {
    p.value_perc_Ttest<-1-(head(which(SorttStat>OrgTtest),1)-1)/(BootSamp+1)
    p.value_perc_D<-1-(head(which(SortBootDbar>OrgDbar),1)-1)/(BootSamp+1)
    p.value_var_ttest<-pt(tstat_boot_Var,(N-1), lower.tail = FALSE)
    if (length(which(SorttStat>OrgTtest))==0){
      p.value_perc_Ttest=0
      p.value_perc_D=0
    }
  }
  # Load outputs 1
  Results1 <- c("Std. T value"=OrgTtest, "Std. P value"=p.value_perc_Ttest,
                "Boot T value" = tstat_boot_Var, "Boot P Value" = p.value_var_ttest,
                "Perc. P Value" = p.value_perc_D)
  return(Results1)
}

