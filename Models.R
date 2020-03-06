#Import data
binary_3_week <- read.csv("C:/Users/Kedar Deshpande/Desktop/UMD/Fall/Capstone/binary_3_week.csv")

#Train and test split
train = binary_3_week[1:637,]
test = binary_3_week[638:802,]

#Converting response variable to binary factor
train$mkt_ret_regimes = as.factor(train$mkt_ret_regimes)
test$mkt_ret_regimes = as.factor(test$mkt_ret_regimes)

#Logistic Regression
lr1 = glm(mkt_ret_regimes~. ,data = train,family = 'binomial')

pred = predict(lr1, newdata = test,type = 'response')

predictions = ifelse(pred>0.9,1,0)

table(test$mkt_ret_regimes,predictions,dnn = c('Actual','Predicted'))

#Accuracy 56.36%

summary(lr1)

#A lot of variables showing perfect multi-collinearity

train$finance_neg_accounting_changes = NULL
test$finance_neg_accounting_changes = NULL

train$finance_neg_accounting_red_flags = NULL
test$finance_neg_accounting_red_flags = NULL

train$improve_credit_profile =  NULL
test$improve_credit_profile = NULL

train$industry_neg_competition_increase =  NULL
test$industry_neg_competition_increase = NULL

train$industry_neg_interest_rate_increase =  NULL
test$industry_neg_interest_rate_increase = NULL

train$industry_neg_unemployment_rate_bad =  NULL
test$industry_neg_unemployment_rate_bad = NULL

train$industry_pos_competition_decrease =  NULL
test$industry_pos_competition_decrease = NULL

train$industry_pos_unemployment_rate_good =  NULL
test$industry_pos_unemployment_rate_good = NULL

train$management_growth_buzz =  NULL
test$management_growth_buzz = NULL

train$management_kpi_buzz =  NULL
test$management_kpi_buzz = NULL

train$management_neg_cyber_attack =  NULL
test$management_neg_cyber_attack = NULL

train$management_opportunity_buzz =  NULL
test$management_opportunity_buzz = NULL

train$management_price_shilling = NULL
test$management_price_shilling = NULL

train$management_stability_buzz = NULL
test$management_stability_buzz = NULL

train$management_strategy_buzz = NULL
test$management_strategy_buzz = NULL

train$negative_government_reaction = NULL
test$negative_government_reaction = NULL

train$operations_neg_capacity_decrease = NULL
test$operations_neg_capacity_decrease = NULL

train$operations_neg_capacity_shortage = NULL
test$operations_neg_capacity_shortage = NULL

train$operations_neg_component_shortage = NULL
test$operations_neg_component_shortage = NULL

train$operations_neg_delivery_quality = NULL
test$operations_neg_delivery_quality = NULL

train$operations_neg_equipment_failure = NULL
test$operations_neg_equipment_failure = NULL

train$operations_neg_union_strike = NULL
test$operations_neg_union_strike = NULL

train$operations_pos_alt_tech_dev = NULL
test$operations_pos_alt_tech_dev = NULL

train$operations_pos_capacity_increase = NULL
test$operations_pos_capacity_increase = NULL

train$operations_pos_contingency_planning = NULL
test$operations_pos_contingency_planning = NULL

train$operations_pos_crisis_management = NULL
test$operations_pos_crisis_management = NULL

train$operations_pos_delivery_quality = NULL
test$operations_pos_delivery_quality = NULL

train$operations_pos_disruption_advantages = NULL
test$operations_pos_disruption_advantages = NULL

train$operations_pos_lead_time_reduction = NULL
test$operations_pos_lead_time_reduction = NULL

train$operations_pos_market_responsive_goal = NULL
test$operations_pos_market_responsive_goal = NULL

train$operations_pos_monitor_leading_indicators = NULL
test$operations_pos_monitor_leading_indicators = NULL

train$opportunity_recognition = NULL
test$opportunity_recognition = NULL

train$patent_disputes = NULL
test$patent_disputes = NULL

train$plant_opening = NULL
test$plant_opening = NULL

train$positive_government_reaction = NULL
test$positive_government_reaction = NULL

train$power_outage = NULL
test$power_outage = NULL

train$process_efficient_inventory_strategy = NULL
test$process_efficient_inventory_strategy = NULL

train$process_efficient_supply_chain_goal = NULL
test$process_efficient_supply_chain_goal = NULL

train$product_obsolescence = NULL
test$product_obsolescence = NULL

train$product_recall = NULL
test$product_recall = NULL

train$production_freeze = NULL
test$production_freeze = NULL

train$research_restrictions = NULL
test$research_restrictions = NULL

train$resource_mobilization = NULL
test$resource_mobilization = NULL

train$short_term_strong_strategy = NULL
test$short_term_strong_strategy = NULL

train$supply_decrease = NULL
test$supply_decrease = NULL

train$supply_increase = NULL
test$supply_increase = NULL

train$transportation_disruption = NULL
test$transportation_disruption = NULL

train$unstable_demand = NULL
test$unstable_demand = NULL

train$work_stoppage = NULL
test$work_stoppage = NULL

#Logistic Regression

lr = glm(mkt_ret_regimes~., data = train, family = 'binomial')

pred = predict(lr, newdata = test,type = 'response')

predictions = ifelse(pred>0.5,1,0)

table(test$mkt_ret_regimes,predictions,dnn = c('Actual','Predicted'))





## Lasso
library(glmnet)

## LASSO works the same way as Ridge, just with alpha=1:

train_x = train[,c(1:260)]
test_x = test[,c(1:260)]

glmnet_lasso = glmnet(as.matrix(train_x),train$mkt_ret_regimes,family="binomial",alpha=1
                      , standardize = TRUE)
glmnet_lasso.cv=cv.glmnet(as.matrix(train_x),train$mkt_ret_regimes,family="binomial",alpha=1)
plot(glmnet_lasso.cv)
best.lambda=glmnet_lasso.cv$lambda.min
best.lambda


lasso.probs = predict(glmnet_lasso,s=best.lambda,newx=as.matrix(test_x),type="response")
lasso.class = ifelse(lasso.probs>0.5,1,0)
sum(ifelse(lasso.class==test_x$mkt_ret_regimes,1,0))/nrow(test_x)
table(lasso.class,test$mkt_ret_regimes,dnn=c('Predicted','Actual'))

## Let's say we want to see what these models actually look like
## We can actually use the predict function to print out the coefficients here:
predict(glmnet_lasso,s=best.lambda,type="coefficients")


#Logistic using above variables

lr_lasso = glm(mkt_ret_regimes~bad_delivery_quality+co_market_interest_pos+
                 co_market_interest_neg+contingency_planning+copper_price_decrease+copper_price_increase+
                 credit_profile+debt_delinquency+earthquake+economy_neg_currency_weak+
                 economy_neg_economic_comparison+economy_neg_economy_turbulent+
                 economy_neg_exchange_rate_bad+economy_neg_inflation+
                 economy_neg_interest_rate_increase+economy_neg_macroeconomic+
                 economy_neg_trade_relations_poor+economy_neg_yield_curve_inverting+
                 economy_pos_imports_increase+economy_pos_unemployment_rate+
                 economy_pos_yield_curve_steep+esg_ltip_drivers+
                 finance_neg_accounting_error_restatements+finance_neg_debt_delinquency+
                 finance_neg_underperform+finance_pos_earnings_forecast+finance_pos_outperform+
                 general_ltip+good_delivery_quality+industry_downturn+industry_neg_accident+
                 industry_neg_demand_unstable+industry_neg_downgrade+industry_neg_governance_risks+
                 industry_neg_headwinds+industry_neg_margin_larger+industry_neg_market_based_risks+
                 industry_neg_political_change+industry_neg_trade_war+
                 industry_pos_crime_low+industry_pos_government_reaction+
                 industry_pos_interest_rate_increase+industry_pos_market_share_stable+
                 industry_pos_products_better+industry_pos_tailwinds+lead_time_reduction+
                 management_buzz_growth+management_neg_mgmt_price_shilling+
                 management_neg_new_share_issuance+management_pos_marketing_strategy+
                 management_pos_mgmt_change+monitoring_leading_indicators+
                 operations_neg_low_consumer_demand+
                 operations_neg_stock_warehouse_concerns+
                 operations_pos_market_responsive_inv_strategy+
                 tsr_ltip_drivers, data = train, family = 'binomial')

pred = predict(lr_lasso, newdata = test,type = 'response')

predictions = ifelse(pred>0.5,1,0)

table(test$mkt_ret_regimes,predictions,dnn = c('Actual','Predicted'))

summary(lr_lasso)

#BAGGING 

library(randomForest)

trees = c(10,25,50,100,300,500,700,1000,1500,2000,3000,5000)
acc = rep(0,length(trees))

for (i in 1:length(trees)){
  bag.trees = randomForest(mkt_ret_regimes~.,data = train, ntree=trees[i], mtry=260,importance=TRUE)
  bagging_preds=predict(bag.trees,newdata=test,type="prob")
  bagging_probs=bagging_preds[,2]
  bagging_class=ifelse(bagging_probs>0.5,1,0)
  acc[i] = sum(ifelse(bagging_class==test$mkt_ret_regimes,1,0))/nrow(test)

}

plot(trees,acc)

trees
acc

bag.trees = randomForest(mkt_ret_regimes~.,data = train, ntree=100, mtry=260,importance=TRUE)
bagging_preds=predict(bag.trees,newdata=test,type="prob")
bagging_probs=bagging_preds[,2]
bagging_class=ifelse(bagging_probs>0.5,1,0)
sum(ifelse(bagging_class==test$mkt_ret_regimes,1,0))/nrow(test)

bag.trees
importance(bag.trees)
varImpPlot(bag.trees)



#------------------------------------------------------------------------------------------------

#Random Forest

trees = c(10,25,50,100,300,500,700,1000,1500,2000,3000,5000)
acc = rep(0,length(trees))

for (i in 1:length(trees)){
  rf.trees = randomForest(mkt_ret_regimes~.,data = train, ntree=trees[i], mtry=100,importance=TRUE)
  bagging_preds=predict(rf.trees,newdata=test,type="prob")
  bagging_probs=bagging_preds[,2]
  bagging_class=ifelse(bagging_probs>0.5,1,0)
  acc[i] = sum(ifelse(bagging_class==test$mkt_ret_regimes,1,0))/nrow(test)
  
}

plot(trees,acc)

trees
acc


m = c(1:260)
accu = rep(0,length(m))

for (i in 1:length(m)){
  rf.trees = randomForest(mkt_ret_regimes~.,data = train, ntree=300, mtry=m[i],importance=TRUE)
  bagging_preds=predict(rf.trees,newdata=test,type="prob")
  bagging_probs=bagging_preds[,2]
  bagging_class=ifelse(bagging_probs>0.5,1,0)
  accu[i] = sum(ifelse(bagging_class==test$mkt_ret_regimes,1,0))/nrow(test)
  
}

plot(m,accu)

m
accu

rf.trees=randomForest(mkt_ret_regimes~.,data=train, ntree = 300,mtry = 25,importance=TRUE)

rf_preds=predict(rf.trees,newdata=test,type="prob")
rf_probs=rf_preds[,2]
rf_class=ifelse(rf_probs>0.5,1,0)

table(test$mkt_ret_regimes,rf_class)
sum(ifelse(rf_class==test$mkt_ret_regimes,1,0))/nrow(test)

varImpPlot(rf.trees)


