

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
         net_price) ## Do like three different versions

## Structure for Cross Validation
ad_rs<-mc_cv(ad_sub,times=1000)

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


tune_fit_all=TRUE


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
                    select_best(yield_tune_fit,
                                metric = "roc_auc")) %>%
  fit(ad_sub)

save(yield_all_final,file="yield_all_final.Rdata")

} else{
load("yield_tune_fit_all.Rdata")
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
  "yield_f~fit+top_hs+gpa")

## Create recipe
yield_recipe<-recipe(yield_formula,ad_sub)%>%
  update_role(ID, new_role = "id variable")%>%
  step_normalize(all_predictors())%>%
  step_naomit(all_predictors())

## Set workflow
yield_wf<-
  update_recipe(yield_recipe)


## Fit model to resampled Data

tune_fit_academics<-TRUE

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
                    select_best(yield_tune_fit,
                                metric = "roc_auc")) %>%
  fit(ad_sub)

save(yield_fit_academics_final,file="yield_fit_acaademics_final.Rdata")

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
yield_wf<-
  update_recipe(yield_recipe)

tune_fit_notest<-TRUE

if(tune_fit_notest){

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

}
