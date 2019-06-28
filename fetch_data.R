install.packages(setdiff("pacman", installed.packages()))
pacman::p_load(
  tidyverse,
  robotstxt,
  rvest,
  stringr
  )
pacman::p_load_gh("franapoli/pbarETA")

url_target <- "http://suumo.jp"
se <- html_session( "https://httpbin.org/user-agent")
robots_text <- robotstxt(domain=url_target)
print(robots_text$bots)
print(robots_text$permissions %>% filter(useragent=="*") %>% filter(grepl("/jj/chintai/ichiran", value)))
# 一覧を見るだけなら禁じられてない

robotstxt::paths_allowed(paste(url_target, sep="/"), user_agent = se$response$request$options$useragent)

build_wise_page_to_tb <- function(query_result){
  # 「建物ごと表示」の結果を tibble にする. 部屋情報は入れ子になっているので tidyr::unnest
  data_html <- query_result %>% html_nodes("div.cassetteitem")
  casset_info <- data_html %>% html_nodes("div.cassetteitem-detail")
  casset_items <- data_html %>% html_nodes("div.cassetteitem-item table.cassetteitem_other")
  parse_items_to_tb <- function(casset_items){
    
  }
  tb <- tibble(
    title=casset_info %>% html_node("div.cassetteitem_content-title") %>% html_text,
    rent_category=casset_info %>% html_node("span.ui-pct.ui-pct--util1") %>% html_text,
    address=casset_info %>% html_nodes("li.cassetteitem_detail-col1") %>% html_text,
    accessibility=casset_info %>% html_nodes("li.cassetteitem_detail-col2") %>% map(. %>% html_nodes(css="div") %>% html_text),
    age=casset_info %>% html_nodes("li.cassetteitem_detail-col3 div:nth-of-type(1)") %>% html_text %>%
      str_replace("新築", "築0年") %>% str_replace("^築([0-9]*)年$", "\\1") %>% as.integer,
    floor_general=casset_info %>% html_nodes("li.cassetteitem_detail-col3 div:nth-of-type(2)") %>% html_text,
    rooms=map_dbl(casset_items, . %>% html_nodes("tr.js-cassette_link td:nth-of-type(3)") %>% length),
    items=map(casset_items, function(x) tibble(
      floor=x %>% html_nodes("tr.js-cassette_link td:nth-of-type(3)") %>% html_text %>% str_trim,
      rent_price=x %>% html_nodes("tr.js-cassette_link td:nth-of-type(4) span.cassetteitem_price.cassetteitem_price--rent") %>% html_text %>%
        str_replace("万", "0000") %>% str_remove("円")  %>% str_remove("\\.") %>% as.numeric,
      manage_fee=x %>% html_nodes("tr.js-cassette_link td:nth-of-type(4) span.cassetteitem_price.cassetteitem_price--administration") %>% html_text %>%
        str_replace("万", "0000") %>% str_remove("円") %>% str_remove("\\.") %>% str_replace("-", "0") %>% as.numeric,
      deposit=x %>% html_nodes("tr.js-cassette_link td:nth-of-type(5) span.cassetteitem_price.cassetteitem_price--deposit") %>% html_text %>%
        str_replace("万", "0000") %>% str_remove("円")  %>% str_remove("\\.") %>% str_replace("-", "0") %>% as.numeric,
      key_money=x %>% html_nodes("tr.js-cassette_link td:nth-of-type(5) span.cassetteitem_price.cassetteitem_price--gratuity") %>% html_text %>%
        str_replace("万", "0000") %>% str_remove("円")  %>% str_remove("\\.") %>% str_replace("-", "0") %>% as.numeric,
      layout=x %>% html_nodes("tr.js-cassette_link td:nth-of-type(6) span.cassetteitem_madori") %>% html_text,
      size=x %>% html_nodes("tr.js-cassette_link td:nth-of-type(6) span.cassetteitem_menseki") %>% html_node(xpath="text()") %>%
        html_text %>% str_replace("m$", "") %>% as.numeric,
      url=x %>% html_nodes("tr.js-cassette_link td.ui-text--midium.ui-text--bold a") %>% html_attr("href")
      )
    )
  )
  return(tb)
}

# クエリ一覧を読み込み
tb_q <- read_csv("query_list.csv", col_types="ci")

li_df <- list()
for(i in 1:NROW(tb_q)){
  sect_id <- tb_q$sect_id[i]
  query <- paste(url_target, tb_q$query[i], sep="/")
  print(paste(query))
  # 検索結果1ページ目を取得
  page1 <- read_html(query)
  # 結果の総ページ数取得
  int_pages <- page1 %>% html_nodes("ol.pagination-parts li:last-child") %>% html_text %>% as.integer
  # 1ページ目を tibble に
  df <- build_wise_page_to_tb(page1) %>% mutate(page=1)
  Sys.sleep(10)
  pb <- txtProgressBar(min=2, max=int_pages, style=3, char="█", width=getOption("width") - 30)
  for(page in 2:int_pages){
    setTxtProgressBar(pb, page)
    # 一応確認
    if(paths_allowed(paste(url_target, query, sep="/"), user_agent = se$response$request$options$useragent)) {
      new_page <- read_html(paste0(query, "&pn=", page))
      df <- bind_rows(df, build_wise_page_to_tb(new_page) %>% mutate(page=page))
    }
    Sys.sleep(10)
  }
  df <- df %>% mutate(sect_id=sect_id)
  save(df, file=paste0("data_20190628/housing_", sect_id, ".RData"))
  li_df[[as.character(sect_id)]] <- df
}
# TODO なんかエラー出る
save(li_df, file="data_20190628/housing_all.RData")
