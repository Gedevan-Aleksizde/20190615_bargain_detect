require(tidyverse)
require(robotstxt)
require(rvest)
require(stringr)
require(pbarETA)  # https://github.com/franapoli/pbarETA

url <- "http://suumo.jp"
se <- html_session( "https://httpbin.org/user-agent")
robots_text <- robotstxt(domain=url)
print(robots_text$bots)
print(robots_text$permissions %>% filter(useragent=="*") %>% filter(grepl("/jj/chintai/ichiran", value)))
# 一覧を見るだけなら禁じられてない

# 建物ごと
query <- "jj/chintai/ichiran/FR301FC001/?url=%2Fchintai%2Fichiran%2FFR301FC001%2F&ar=030&bs=040&pc=50&smk=&po1=12&po2=99&shkr1=03&shkr2=03&shkr3=03&shkr4=03&cb=0.0&ct=9999999&et=9999999&mb=0&mt=9999999&cn=9999999&ta=13"

robotstxt::paths_allowed(paste(url, query, sep="/"), user_agent = se$response$request$options$useragent)

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
    accessibility=casset_info %>% html_nodes("li.cassetteitem_detail-col2") %>% html_text %>% str_squish,
    age=casset_info %>% html_nodes("li.cassetteitem_detail-col3 div:nth-of-type(1)") %>% html_text %>%
      str_replace("新築", "築0年") %>% str_replace("^築([0-9]*)年$", "\\1") %>% as.integer,
    floor_general=casset_info %>% html_nodes("li.cassetteitem_detail-col3 div:nth-of-type(2)") %>% html_text,
    rooms=map_dbl(casset_items, . %>% html_nodes("tr.js-cassette_link td:nth-of-type(3)") %>% length),
    items=map(casset_items, function(x) tibble(
      floor=x %>% html_nodes("tr.js-cassette_link td:nth-of-type(3)") %>% html_text %>% str_trim,
      rent_price=x %>% html_nodes("tr.js-cassette_link td:nth-of-type(4) span.cassetteitem_price.cassetteitem_price--rent") %>% html_text %>%
        str_replace("万", "0000") %>% str_remove("円")  %>% str_remove("\\.") %>% as.numeric,
      manege_fee=x %>% html_nodes("tr.js-cassette_link td:nth-of-type(4) span.cassetteitem_price.cassetteitem_price--administration") %>% html_text %>%
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

# 部屋ごと
# query <- "jj/chintai/ichiran/FR301FC005/?ar=030&bs=040&ta=13&sc=13121&cb=0.0&ct=9999999&mb=0&mt=9999999&et=9999999&cn=9999999&shkr1=03&shkr2=03&shkr3=03&shkr4=03&sngz=&po1=01&po2=99&pc=100"

room_wise_page_to_tb <- function(query_result){
  # xml_doc から物件情報取り出してtibble
  # 「部屋ごと」表示に対応.
  # 列
  html_data <- query_result %>% html_nodes("div.property_group")
  area <- html_data %>%  html_nodes("div.detailbox-property table tr td:nth-of-type(3)")
  cat_vintage <- html_data %>%  html_nodes("div.detailbox-property table tr td:nth-of-type(4)")
  extra_charges <- html_data %>% html_nodes("td.detailbox-property-col.detailbox-property--col2")
  return(
    tibble(
      title=html_data %>% html_nodes("h2.property_inner-title a") %>% html_nodes(xpath="text()") %>% as.character, #物件名
      rent_price=html_data %>% html_nodes("div.detailbox-property-point")  %>% html_nodes(xpath="text()") %>%
        as.character %>% str_replace("万円", "0000") %>% str_remove("\\.") %>% as.numeric,  # 家賃
      management_fee=html_data %>% html_nodes('td.detailbox-property-col.detailbox-property--col1') %>%
        html_node(":not(.detailbox-property-point)") %>% html_text %>%
        str_remove("管理費") %>% str_replace("万", "0000") %>% str_remove("円") %>% str_replace("-", "0") %>% str_trim %>% as.numeric,  # 管理費等
      deposit=extra_charges %>% html_nodes("div:nth-of-type(1)") %>% html_node(xpath="text()") %>%
        html_text %>% str_replace("\\.", "") %>% str_replace("万円", "0000") %>% str_replace("-", "0") %>% as.numeric, # 敷金
      key_money=extra_charges %>% html_nodes("div:nth-of-type(2)") %>% html_node(xpath="text()") %>%
        html_text %>% str_replace("\\.", "") %>% str_replace("万円", "0000") %>% str_replace("-", "0") %>% as.numeric,  # 礼金
      extra_bond=extra_charges %>% html_nodes("div.detailbox-property-inactive:nth-of-type(3)") %>% html_text %>%
        str_replace("保証金", "") %>% str_trim %>% str_replace("-", "0"),  # 保証金
      redeumption=extra_charges %>% html_nodes("div.detailbox-property-inactive:nth-of-type(4)") %>% html_text,
      layout=area %>% html_nodes("div:nth-child(1)") %>% html_text,  # 間取り
      size=area %>% html_nodes("div:nth-child(2)") %>% html_nodes(xpath="text()") %>% html_text %>% str_remove("m") %>% as.numeric,  # 面積
      direction=area %>% html_nodes("div:nth-child(3)") %>% html_text,  # 向き
      type=cat_vintage %>% html_nodes("div:nth-child(1)") %>% html_text,  # 戸建て/アパート/etc
      age=cat_vintage %>% html_nodes("div:nth-child(2)") %>% html_text %>%
        str_replace("新築", "築0年") %>% str_replace("^築([0-9]*)年$", "\\1") %>% as.integer,  # 築年数
      address=html_data %>%  html_nodes("div.detailbox-property table tr td.detailbox-property-col:nth-of-type(5)") %>%
        html_text %>% str_trim,  # 住所
      accessibility_text=html_data %>% html_nodes("div.detailnote div.detailnote-box:nth-of-type(1)") %>%  html_text %>% str_squish
    )
  )
}



"&sc=13101&sc=13102&sc=13103&sc=13104&sc=13105&sc=13113&sc=13106&sc=13107&sc=13108&sc=13118&sc=13121&sc=13122&sc=13123&sc=13109&sc=13110&sc=13111&sc=13112&sc=13114&sc=13115&sc=13120&sc=13116&sc=13117&sc=13119" %>% str_split("&") %>% unlist %>% str_remove("sc=") %>% as.integer %>% sort

li_df <- list()
for(sect_id in 13100 + 2:23){
  print(paste("&sc=", sect_id))
  # 検索結果1ページ目を取得
  query <- paste0(query, "&sc=", sect_id)
  page1 <- read_html(paste(url, query, sep="/"))
  # 結果の総ページ数取得
  int_pages <- page1 %>% html_nodes("ol.pagination-parts li:last-child") %>% html_text %>% as.integer
  # 1ページ目を tibble に
  df <- build_wise_page_to_tb(page1) %>% mutate(page=1, sect_id=sect_id)
  Sys.sleep(10)
  pb <- txtProgressBar(min=2, max=int_pages, style=3, char="█", width=getOption("width") - 30)
  for(page in 2:int_pages){
    setTxtProgressBar(pb, page)
    # 一応確認
    if(paths_allowed(paste(url, query, sep="/"), user_agent = se$response$request$options$useragent)) {
      new_page <- read_html(paste(url, query, sep="/") %>% paste0("&pn=", page))
      df <- bind_rows(df, build_wise_page_to_tb(new_page) %>% mutate(page=page))
    }
    Sys.sleep(10)
  }
  df <- df %>% mutate(sect_id=sect_id)
  save(df, file=paste0("housing_", sect_id, ".RData"))
  li_df[[as.character(sect_id)]] <- df
}
save(li_df, file="housing_all.RData")

# 簡単なバグチェック: 建物ごとの部屋数とオブジェクト数が一致しているか
# df %>% mutate(len=map_dbl(items, NROW)) %>% dplyr::select(len, rooms) %>% mutate(check=len==rooms) %>% skim