library("ggplot2")
library("dplyr")
library("tidyverse")
#install.packages("tidymodels")
library("tidymodels")
#install.packages("repr")
library("repr")
#install.packages("kknn")
library("kknn")
install.packages("GGally")
library("GGally")

qog <- read.csv("https://www.qogdata.pol.gu.se/data/qog_std_cs_jan22.csv")%>%
  mutate(br_dem = as_factor(br_dem))

qog[qog==""]<-NA
qog2 <- select(qog,cname,br_dem, bti_foe , ti_cpi, fhp_mcpp5,al_ethnic2000
 )%>%
      drop_na()

set.seed(1)

qog_split <- initial_split(qog2, prop = 0.8, strata = br_dem)
qog_train <- training(qog_split)
qog_test <- testing(qog_split) 

qog_train_proportions <- qog_train |>
  group_by(br_dem) |>
  summarize(n = n()) |>
  mutate(percent = 100*n/nrow(qog_train))

qog_train_proportions

summary(qog_train)
qog_train %>% summarise_if(is.numeric,sd)


qog_recipe <- recipe(br_dem ~ bti_foe + ti_cpi+ fhp_mcpp5, data = qog_train) |>
  step_scale(all_predictors()) |>
  step_center(all_predictors())

knn_spec <- nearest_neighbor(weight_func = "rectangular", neighbors = 3) |>
  set_engine("kknn") |>
  set_mode("classification")

knn_fit <- workflow() |>
  add_recipe(qog_recipe) |>
  add_model(knn_spec) |>
  fit(data = qog_train)

knn_fit

qog_test_predictions <- predict(knn_fit, qog_test) |>
  bind_cols(qog_test)

qog_test_predictions



qog3 <- select(qog_train,br_dem,bti_foe,ti_cpi, fhp_mcpp5,al_ethnic2000
)

options(repr.plot.width = 16, repr.plot.height = 16) 
ggpairs(qog3, 
      columns= 2:5,
      aes(colour=br_dem),
      columnLabels = c("Freedom of Expression", "Preception of Corruption", "Control of Media", "Ethnic Fraction")
      )
      

conf_mat <- qog_test_predictions %>% 
  metrics(truth = br_dem, estimate = .pred_class)

conf_mat
