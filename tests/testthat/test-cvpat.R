ontext("SEMinR correctly applies CV-PAT to a simple model\n")
set.seed(123)
corp_rep <- corp_rep_data

# Create measurement model ----
corp_rep_mm <- constructs(
  composite("COMP", multi_items("comp_", 1:3)),
  composite("LIKE", multi_items("like_", 1:3)),
  composite("CUSA", single_item("cusa")),
  composite("CUSL", multi_items("cusl_", 1:3))
  )

# Create structural model ----
sm_one <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA")),
  paths(from = c("CUSA"), to = c("CUSL")))

sm_two <- relationships(
  paths(from = c("COMP", "LIKE"), to = c("CUSA", "CUSL")),
  paths(from = c("CUSA"), to = c("CUSL")))


model_one <- estimate_pls(
  data = corp_rep,
  measurement_model = corp_rep_mm,
  structural_model  = sm_one,
  missing = mean_replacement,
  missing_value = "-99")

model_two <- estimate_pls(
  data = corp_rep,
  measurement_model = corp_rep_mm,
  structural_model  = sm_two,
  missing = mean_replacement,
  missing_value = "-99")

nick_mod_one <- predict_pls(model = model_one)
nick_mod_two <- predict_pls(model = model_two)


predict_error_lm_mod_one <- nick_mod_one$item_actuals[,colnames(nick_mod_one$lm_out_of_sample)] - nick_mod_one$lm_out_of_sample
predict_error_lm_mod_two <- nick_mod_two$item_actuals[,colnames(nick_mod_two$lm_out_of_sample)] - nick_mod_two$lm_out_of_sample
predict_error_pls_mod_one <- nick_mod_one$PLS_out_of_sample_residuals
predict_error_pls_mod_two <- nick_mod_two$PLS_out_of_sample_residuals

loss_two <- rowMeans(predict_error_lm_mod_one^2)
loss_one <- rowMeans(predict_error_lm_mod_two^2)
loss_three <- rowMeans(predict_error_pls_mod_one^2)

BootSamp <- 1000
N <- nrow(corp_rep)
LossM1 <- loss_one
LossM2 <- loss_two
testtype = "two.sided"
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
Results1 <- c("OrgTtest"=OrgTtest, "p.value.perc.t"=p.value_perc_Ttest,
             "t.stat.b.v" = tstat_boot_Var, "p.value.var.ttest" = p.value_var_ttest,
             "p.value.perc.D" = p.value_perc_D)

# Second Test PLS vs LM ----

BootSamp <- 1000
N <- nrow(corp_rep)
LossM1 <- loss_three
LossM2 <- loss_two
testtype = "two.sided"
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

# Load outputs 2
Results2 <- c("OrgTtest"=OrgTtest, "p.value.perc.t"=p.value_perc_Ttest,
              "t.stat.b.v" = tstat_boot_Var, "p.value.var.ttest" = p.value_var_ttest,
              "p.value.perc.D" = p.value_perc_D)

## Output originally created using following lines
# write.csv(rbind(Results1,Results2), file = "tests/fixtures/cvpat1.csv")

# Load controls
cvpat_control <- as.matrix(read.csv(file = paste(test_folder,"cvpat1.csv", sep = ""), row.names = 1))

# Testing
test_that("Seminr estimates rho_A correctly\n", {
  expect_equal(rbind(Results1,Results2), cvpat_control, tolerance = 0.00001)
})
