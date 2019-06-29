install.packages(setdiff("pacman", installed.packages()))
pacman::p_load(
  tidyverse,
  stringr,
  skimr,
  ggplot2,
  ggthemes,
  FeatureHashing,
  mlr,
  mlrCPO,
  mlrMBO
  )
pacman::p_load_gh(
  "clauswilke/colorblindr",
  "franapoli/pbarETA"
  )
# install するが load 不要
install.packages(setdiff(c(
  "stringi",
  "caret",
  "flexmix",
  "ranger",
  "quantregForest",
  "xgboost"
  ), installed.packages()))


list_df <- list()
for(f in list.files("data", pattern = "housing_[0-9]+\\.RData", full.names=T)){
  load(f)
  print(f)
  list_df[[str_extract(f, "[0-9]+")]] <- df
}
df <- bind_rows(list_df)
df <- df %>% mutate(
  sect_id=as.character(sect_id),
  ID=row_number() %>% as.character()
  )
df <- unnest(df, .preserve="accessibility")
rm(list_df)

df <- df %>% rename(manage_fee=manege_fee)
# save(df, file="housing_all.RData")

# モデルに入力するデータの変換
preproc_housing <- function(data){
  data %>%  mutate(rent_price=rent_price + manage_fee) %>%
    dplyr::select(-deposit, -key_money, -manage_fee) %>%
    mutate(
      rooms_extra=str_extract(layout, "^[0-9]*") %>% as.integer %>% replace_na(0) %>%
        + if_else(str_detect(layout, "ワンルーム"), 1, 0),
      rooms_S=str_detect(layout, "S") %>% as.integer,
      rooms_L=str_detect(layout, "L") %>% as.integer,
      rooms_D=str_detect(layout, "D") %>% as.integer,
      rooms_K=str_detect(layout, "K") %>% as.integer,
      floor_max=floor_general %>% str_replace("平屋", "1階建")  %>% str_extract("[0-9]*階建") %>% str_remove("階建") %>% as.integer %>% replace_na(-1),
      floor_building_cat=cut(floor_max, breaks=c(-Inf, 1, 2, 4, 6, 7, 8, 9, 10, Inf), ordered_result=F),
      floor_b_max=floor_general %>% str_extract("地下[0-9]*") %>% str_remove("地下") %>% as.integer %>% replace_na(0),
      floor_b_cat=cut(floor_b_max, breaks=c(-Inf, 0, 1, 2, 3, Inf), ordered_result=F),
      floor_int=floor %>% str_replace("-", "1階") %>% str_extract("[0-9]*階") %>% str_remove("階") %>% as.integer,
      floor_cat=cut(floor_int, breaks=c(-Inf, 1, 2, 4, 6, 7, 8, 9, 10, Inf), ordered_result=F)
    ) %>% dplyr::select(-layout, -floor_general, -floor, -floor_max, -floor_int, -floor_b_max) %>%
    mutate(
      address_district=str_remove(address, "^東京都") %>% str_replace("^(.+区).*", "\\1") %>% as.factor,
      address_town=str_remove(address, "^東京都") %>% str_replace("^.+区(.*)", "\\1") %>% hashed.value() %% 2^6 %>% as.integer
    ) %>% dplyr::select(-address) %>%
    mutate(
      address_district=as.factor(address_district)
    ) %>%
    mutate(
      size_log=log(size),
    ) %>% dplyr::select(-size) %>%
    mutate(rent_category=as.factor(rent_category)) %>%
    dplyr::select(-sect_id, -page, -url, -title, -accessibility)
}

df %>% preproc_housing %>% skim

# task を定義
tsk_housing <- makeRegrTask(
  id="hoge",
  data=df %>% preproc_housing %>% dplyr::select(-ID) %>% as.data.frame %>% drop_na, target="rent_price"
  ) %>% createDummyFeatures(method="reference")
# xgboost に合わせて factor はすべて数値にする method="reference" にしないと特異行列になるのでアルゴリズムによってはエラー原因になる

# 登録した learner 読み込み
source("resgist_mlr_learners.R")

# learner を定義
# 全て目的変数を対数変換
learner_log_lmnet <- cpoScale(center=T, scale=T) %>>% cpoLogTrafoRegr() %>>%  makeLearner("regr.glmnet")
learner_log_rf <- cpoScale(center=T, scale=T) %>>% cpoLogTrafoRegr() %>>% makeLearner("regr.ranger", num.trees=100, num.threads=4)
learner_log_qrf <- cpoScale(center=T, scale=T) %>>% cpoLogTrafoRegr() %>>% makeLearner("regr.quantregForest", num.trees=100, num.threads=4)
learner_log_xgboost <- cpoScale(center=T, scale=T) %>>% cpoLogTrafoRegr() %>>% makeLearner("regr.xgboost")
learner_log_mixute <- cpoScale(center=T, scale=T) %>>% cpoLogTrafoRegr() %>>% makeLearner("regr.flexmix", k=3)

# 学習
m_lin <- train(learner_log_lmnet, tsk_housing)
m_rf <- train(learner_log_rf, tsk_housing)
m_xgb <- train(learner_log_xgboost, tsk_housing)
m_qrf <- train(learner_log_qrf, tsk_housing)
m_mix <- train(learner_log_mixute, tsk_housing)

save(m_lin, m_rf, m_xgb, m_qrf, m_mix, file="models.RData")

performance(predict(m_lin, tsk_housing), list(mse, rmse, mae))
performance(predict(m_rf, tsk_housing), list(mse, rmse, mae))
performance(predict(m_xgb, tsk_housing), list(mse, rmse, mae))
performance(predict(m_qrf, tsk_housing), list(mse, rmse, mae))
performance(predict(m_mix, tsk_housing), list(mse, rmse, mae))