require(tidyverse)
require(stringr)
require(skimr)
require(ggplot2)
require(ggthemes)
require(colorblindr) # https://github.com/clauswilke/colorblindr
require(pbarETA)  # https://github.com/franapoli/pbarETA

load()

#### データの品質管理 ####

df$rooms <- with(df, unlist(rooms))  # ?
df <- unnest(df)
df %>% skim
df %>% distinct_all %>% skim

df %>% filter(rent_category=="賃貸その他") %>% dplyr::select(floor_general) %>% table
df %>% filter(rent_category=="賃貸一戸建て") %>% dplyr::select(floor_general) %>% table
df %>% filter(rent_category=="賃貸テラス・タウンハウス") %>% dplyr::select(floor_general) %>% table

filter(df, floor=="-") %>% dplyr::select(floor_general, rent_category) %>% table
df %>% mutate(
  floor_max=floor_general %>% str_extract("[0-9]*階建") %>% str_remove("階建") %>% as.integer,
  floor_b_max=floor_general %>% str_extract("地下[0-9]*") %>% str_remove("地下") %>% as.integer %>% replace_na(0),
  floor_int=floor %>% str_extract("[0-9]*階") %>% str_remove("階") %>% as.integer
  ) %>% dplyr::select(starts_with("floor"), rent_category) %>% mutate(is_sound=floor_max + floor_b_max >= floor_int) %>% filter(!is_sound)

map(df, class)

df$rent_category %>% table
df$floor %>% table

# 階数

df %>% mutate(
  address_district=str_remove(address, "^東京都") %>% str_replace("^(.+区).*", "\\1"),
  address_town=str_remove(address, "^東京都") %>% str_replace("^.+区(.*)", "\\1")
  ) %>% dplyr::select(-address)