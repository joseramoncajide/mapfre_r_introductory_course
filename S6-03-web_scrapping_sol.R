estrellas <- html_libro %>% 
  html_nodes("#cm_cr-review_list") %>% 
  html_nodes(".a-icon-star") %>% 
  html_text() %>% 
  str_sub(0, 3) %>% 
  str_replace(',', '.') %>% 
  as.numeric()


