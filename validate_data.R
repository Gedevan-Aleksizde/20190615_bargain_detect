require(tidyverse)
require(stringr)
require(skimr)
require(ggplot2)
require(ggthemes)
require(colorblindr) # https://github.com/clauswilke/colorblindr
require(pbarETA)  # https://github.com/franapoli/pbarETA
require(DataExplorer)

url_head <- "https://suumo.jp"
list_df <- list()
for(f in list.files("data", pattern = "housing_[0-9]+\\.RData", full.names=T)){
  load(f)
  list_df[[str_extract(f, "[0-9]+")]] <- df
}

df <- bind_rows(list_df)
df$sect_id <- with(df, as.character(sect_id))
rm(list_df)

#### データの品質管理 ####

# 簡単なバグチェック: 建物ごとの部屋数とオブジェクト数が一致しているか
df %>% mutate(len=map_dbl(items, NROW)) %>% dplyr::select(len, rooms) %>% mutate(check=len==rooms) %>% skim

# ID を付与する
df <- df %>% mutate(
  ID=1:NROW(df) %>% as.character
)


# 最寄り駅情報
df$accessibility %>% unlist %>% unique
(df$accessibility %>% unlist %>% unique %>% str_split_fixed(" ", 2))[, 2] %>% unique


# 数値変数はすべて欠落なし
df %>% skim

df %>% introduce
df %>% plot_intro
df %>% profile_missing
df %>% plot_missing
df %>% plot_bar
df %>% dplyr::select(sect_id, rent_price) %>% plot_bar(with="rent_price")
df %>% plot_histogram
df %>% plot_correlation

# URL を落とすといくらかレコードが重複している: 同じ建物の物件で重複?
test <- tibble(a=1:3, b=rep(list(c("a", "b", "c")), 3)) %>% mutate(c=map_chr(b, ~ reduce(., paste0)))

NROW(df)
df %>% dplyr::select(-url, -ID) %>% mutate(accessibility=map_chr(accessibility, ~ reduce(., paste0) )) %>%  distinct_all %>% NROW
df %>% dplyr::select(title) %>% distinct_all %>% NROW

# 物件の形態別
table(df$rent_category)
# アパート・マンション以外
df %>% filter(!rent_category %in% c("賃貸アパート", "賃貸マンション")) %>%
  dplyr::select(rent_category, floor_general) %>% table
# 物件の階数が "-" となっているもの
filter(df, floor=="-") %>% dplyr::select(floor_general, rent_category) %>% table

df <- df %>% unnest(.preserve=accessibility) %>% mutate(url=paste(url_head, url, sep="/"))

# 階数がビルの最高階より大きいものがあるが…
df %>% mutate(
  floor_max=floor_general %>% str_extract("[0-9]*階建") %>% str_remove("階建") %>% as.integer,
  floor_b_max=floor_general %>% str_extract("地下[0-9]*") %>% str_remove("地下") %>% as.integer %>% replace_na(0),
  floor_int=floor %>% str_extract("[0-9]*階") %>% str_remove("階") %>% as.integer,
  floor_b_int=floor %>% str_replace("^B(.+)階", "\\1") %>% as.integer
  ) %>% filter(floor_max < floor_int) %>% dplyr::select(title, rent_category, floor_max, floor, url)

# 間取り
table(df$layout)
paste(df$layout %>% unique, collapse="") %>% str_remove("ワンルーム") %>% str_remove_all("[0-9]") %>% str_split("") %>% unlist %>% unique

# 住所は区名とそれ以下に分ける
df %>% mutate(address_district=str_remove(address, "^東京都") %>% str_replace("^(.+区).*", "\\1")) %>%
  dplyr::select(sect_id, address_district) %>% table

df %>% mutate(address_district=str_remove(address, "^東京都") %>% str_replace("^(.+区).*", "\\1")) %>%
  dplyr::select(sect_id, address_district) %>% table

df %>% mutate(address_town=str_remove(address, "^東京都") %>% str_replace("^.+区(.*)", "\\1") %>%
                stringi::stri_trans_nfkc() %>% str_replace("^(.+)[0-9].*$", "\\1")) %>% dplyr::select(address_town) %>% table()

# 住所どうする
df %>% mutate(
  town=str_remove(address, "^東京都") %>% str_replace("^.+区(.*)", "\\1") %>% stringi::stri_trans_nfkc() %>% str_remove_all("[0-9]") %>% as.factor,
  address_town=str_remove(address, "^東京都") %>% str_replace("^.+区(.*)", "\\1") %>% hashed.value() %% 2^5 %>% as.factor) %>%
  dplyr::select(address, address_town, town) %>% skim

ggplot(df_p, aes(x=rent_price)) + geom_histogram(bins=40) + scale_x_log10()

