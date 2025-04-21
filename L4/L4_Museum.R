library(rvest)

url <- "https://kudamoscow.ru/place/avtomuzej-motory-oktjabrja/"

page <- read_html(url)

museum_data <- data.frame(
  name = page %>% 
    html_node("h1") %>% 
    html_text() %>% 
    trimws(),
  
  address = page %>% 
    html_node("p[itemprop='address']") %>% 
    html_text() %>% 
    trimws(),
  
  link = page %>% 
    html_node("a[title='Сайт организаторов']") %>% 
    html_attr("href"),
  
  stringsAsFactors = FALSE
)

museum_data

