install.packages("rvest")
library(rvest)


library(httr)

url <- "https://ecshweb.pchome.com.tw/search/v3.3/all/results?q=iphone&page=1&sort=rnk/dc"
res = GET(url)
res_json = content(res)
do.call(rbind,res_json$prods)
View(data.frame(do.call(rbind,res_json$prods)))

