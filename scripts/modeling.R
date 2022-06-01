

library(modelr)
library(tidyverse)
library(tidymodels)
library(knitr)
library(doParallel)

## Read in Data

ad<-read_rds("../data/cleaned/admit_data.rds")

## Wrangle and subset
ad_sub <- ad %>%
  mutate(yield_f = as_factor(ifelse(yield == 1, "Enrolled", "Not Enrolled"))) %>%
  mutate(yield_f = relevel(yield_f, ref = "Not Enrolled")) %>%
  mutate(sat=sat/100,
         income=income/1000,
         distance=distance/1000)%>%
  select(ID,
         yield_f,
         legacy,
         visit,
         registered,
         sent_scores,
         sat,
         income,
         gpa,
         distance,
         top_hs,
         fit,
         net_price)

options(yardstick.event_first = TRUE)

## Structure for Cross Validation
cv_reps<-1000

ad_rs<-mc_cv(ad_sub,times=cv_reps)

## Set up enet model
yield_enet_mod<-
  logistic_reg(penalty=tune(),
               mixture=tune()) %>%
  set_engine("glmnet")%>%
  set_mode("classification")

## Specify tuning grid
enet_grid<-grid_regular(parameters(yield_enet_mod) ,levels=5)

## Set formula
yield_formula <- as.formula(
  "yield_f~.")

## Create recipe
yield_recipe<-recipe(yield_formula,ad_sub)%>%
  update_role(ID, new_role = "id variable")%>%
  step_log(distance)%>%
  step_poly(income,sat)%>%
  step_normalize(all_predictors())%>%
  step_naomit(all_predictors())

## Set workflow
yield_wf<-workflow()%>%
  add_model(yield_enet_mod)%>%
  add_recipe(yield_recipe)


tune_fit_all=FALSE


if(tune_fit_all){
## Fit model to resampled Data

cl<-makeCluster(4)
registerDoParallel(cl)

yield_tune_fit_all <-
  yield_wf %>%
  tune_grid(ad_rs,
            grid=enet_grid,
            metrics = metric_set(roc_auc, sens, spec,accuracy))

save(yield_tune_fit_all,file="yield_tune_fit_all.Rdata")


## Extract best fit model
yield_all_final <-
  finalize_workflow(yield_wf,
                    select_best(yield_tune_fit_all,
                                metric = "roc_auc")) %>%
  fit(ad_sub)

save(yield_all_final,file="yield_all_final.Rdata")

} else{
load("yield_tune_fit_all.Rdata")
  load("yield_all_final.Rdata")
}

## Check on model fit
yield_tune_fit_all%>%
  collect_metrics()%>%
  filter(.metric=="accuracy")%>%
  arrange(-mean)

yield_tune_fit_all%>%
  collect_metrics()%>%
  filter(.metric=="sens")%>%
  arrange(-mean)

yield_tune_fit_all%>%
  collect_metrics()%>%
  filter(.metric=="spec")%>%
  arrange(-mean)

yield_tune_fit_all%>%
  collect_metrics()%>%
  filter(.metric=="roc_auc")%>%
  arrange(-mean)


## Summarize results
yield_all_final%>%
  extract_fit_parsnip()%>%
  tidy()%>%
  select(term,estimate)%>%
  kable()


################################
# Only Academics and Admissions
#################################


## Set formula
yield_formula <- as.formula(
  "yield_f~fit+top_hs+gpa+net_price")

## Create recipe
yield_recipe<-recipe(yield_formula,ad_sub)%>%
  step_normalize(all_predictors())%>%
  step_naomit(all_predictors())

## Set workflow
yield_wf<-yield_wf%>%
  update_recipe(yield_recipe)


## Fit model to resampled Data

tune_fit_academics<-FALSE

if(tune_fit_academics){

cl<-makeCluster(4)
registerDoParallel(cl)

yield_tune_fit_academics <-
  yield_wf %>%
  tune_grid(ad_rs,
            grid=enet_grid,
            metrics = metric_set(roc_auc, sens, spec,accuracy))

save(yield_tune_fit_academics,file="yield_tune_fit_academics.Rdata")

yield_fit_academics_final <-
  finalize_workflow(yield_wf,
                    select_best(yield_tune_fit_academics,
                                metric = "roc_auc")) %>%
  fit(ad_sub)

save(yield_fit_academics_final,file="yield_fit_acaademics_final.Rdata")

}else{
  load("yield_tune_fit_academics.Rdata")
  load("yield_fit_acaademics_final.Rdata")
}


################################
# No Test Scores
#################################

ad_sub<-ad_sub%>%select(-sat)

## Set formula
yield_formula <- as.formula(
  "yield_f~.")

## Create recipe
yield_recipe<-recipe(yield_formula,ad_sub)%>%
  update_role(ID, new_role = "id variable")%>%
  step_normalize(all_predictors())%>%
  step_naomit(all_predictors())

## Set workflow
yield_wf<-yield_wf%>%
  update_recipe(yield_recipe)

tune_fit_notest<-FALSE

if(tune_fit_notest){

cl<-makeCluster(4)
registerDoParallel(cl)

## Fit model to resampled Data
yield_tune_fit_notest <-
  yield_wf %>%
  tune_grid(ad_rs,
            grid=enet_grid,
            metrics = metric_set(roc_auc, sens, spec,accuracy))

save(yield_tune_fit_notest,file="yield_tune_fit_notest.Rdata")

yield_fit_notest_final <-
  finalize_workflow(yield_wf,
                    select_best(yield_tune_fit_notest,
                                metric = "roc_auc")) %>%
  fit(ad_sub)

save(yield_fit_notest_final,file="yield_fit_notest_final.Rdata")

}else{
  load("yield_tune_fit_notest.Rdata")
  load("yield_fit_notest_final.Rdata")
}


## Create data structures


## CV results

yield_tune_fit_all$`Model Type`="All Variables"
yield_tune_fit_academics$`Model Type`="Academic Variables Only"
yield_tune_fit_notest$`Model Type`="No Test Scores"


model_fit_data<-
show_best(yield_tune_fit_all,metric="roc_auc")%>%
slice(1)%>%
  bind_rows(
    show_best(yield_tune_fit_all,metric="sens")%>%
      slice(1))%>%
bind_rows(
  show_best(yield_tune_fit_all,metric="spec")%>%
    slice(1))%>%
bind_rows(
  show_best(yield_tune_fit_all,metric="accuracy")%>%
    slice(1))%>%
  bind_rows(
    show_best(yield_tune_fit_academics,metric="roc_auc")%>%
      slice(1))%>%
      bind_rows(
        show_best(yield_tune_fit_academics,metric="sens")%>%
          slice(1))%>%
      bind_rows(
        show_best(yield_tune_fit_academics,metric="spec")%>%
          slice(1))%>%
      bind_rows(
        show_best(yield_tune_fit_academics,metric="accuracy")%>%
          slice(1))%>%
  bind_rows(
    show_best(yield_tune_fit_notest,metric="roc_auc")%>%
      slice(1))%>%
  bind_rows(
    show_best(yield_tune_fit_notest,metric="sens")%>%
      slice(1))%>%
  bind_rows(
    show_best(yield_tune_fit_notest,metric="spec")%>%
      slice(1))%>%
  bind_rows(
    show_best(yield_tune_fit_notest,metric="accuracy")%>%
      slice(1))%>%
  select(`Model Type`,
         .metric,
         mean,
         std_err)%>%
  rename(Metric=.metric,
         Estimate=mean,
         SE=std_err)%>%
  mutate(Metric=recode(Metric,
                       "roc_auc"="AUC",
                       "sens"="Sensitivity",
                       "spec"="Specificity",
                       "accuracy"="Accuracy"))%>%
add_case(`Model Type`="Baseline",Metric="Accuracy",Estimate=.681,SE=NA)%>%
  mutate(`Model Type`=fct_relevel(`Model Type`,
         c("All Variables",
         "Academic Variables Only",
         "No Test Scores",
         "Baseline")))

save(model_fit_data,file="model_fit_data.rdata")



fit_list<-list(yield_all_final,yield_fit_academics_final,yield_fit_notest_final)
names(fit_list)<-c("All Variables","Academics Only","No Test Scores")

save(fit_list,file="fit_list.rdata")

## pull model

## adjust dataset

## make predictions

## generate: revenues, admits, perc with GPA above, percent with income below


