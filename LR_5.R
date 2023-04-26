# Вариант 19. Netherlands, Switzerland, Croatia, United Arab Emirates, Egypt
library(rvest)

countries <- c('Canada', 'United States', 'Turkey', 'Greece', 'Denmark')

for (i in 2014:2021)
{
  str_tmp = paste0('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=', i)
  url = read_html(str_tmp)
  nodes = html_nodes(url, 'table')
  
  df_tmp = html_table(nodes[[2]]) %>% as.data.frame()
  df_tmp <- df_tmp[df_tmp$Country %in% countries, -1]
  assign(paste0("df_", i), df_tmp)
}

#Канада, США, Турция, Греция, Дания

for (i in countries)
{
  df_tmp <- merge(c(2014:2021), df_2014[df_2014$Country == i, ], all=T)
  df_tmp[2, c(2:11)] <- df_2015[df_2015$Country == i, ]
  df_tmp[3, c(2:11)] <- df_2016[df_2016$Country == i, ]
  df_tmp[4, c(2:11)] <- df_2017[df_2017$Country == i, ]
  df_tmp[5, c(2:11)] <- df_2018[df_2018$Country == i, ]
  df_tmp[6, c(2:11)] <- df_2019[df_2019$Country == i, ]
  df_tmp[7, c(2:11)] <- df_2020[df_2020$Country == i, ]
  df_tmp[8, c(2:11)] <- df_2021[df_2021$Country == i, ]
  
  
  names(df_tmp)[1] <- 'Year'
  df_tmp <- df_tmp[, -2]
  assign(paste0("df_", i), df_tmp)
}
rm(i, str_tmp, nodes, url, df_tmp)


pl_col <- c('red', 'green', 'gold', '#BA43B4', 'blue')
pl_countries <- c('Канада', 'США', 'Турция', 'Греция', 'Дания')

plot(df_Canada$Year, df_Canada$`Quality of Life Index`, type="o", 
     pch=19, col=pl_col[1], xaxt="n", ylim = c(-10, 300),
     xlab="Год", ylab="Значение", main="Индекс уровня жизни")
lines(`df_United States`$Year, `df_United States`$`Quality of Life Index`, type="o", pch=19, col=pl_col[2])
lines(df_Turkey$Year, df_Turkey$`Quality of Life Index`, type="o", pch=19, col=pl_col[3])
lines(df_Greece$Year, df_Greece$`Quality of Life Index`,
      type="o", pch=19, col=pl_col[4])
lines(df_Denmark$Year, df_Denmark$`Quality of Life Index`, type="o", pch=19, col=pl_col[5])
axis(side=1, at=df_Canada$Year)
legend('topright', cex = 0.5, pl_countries, fill=pl_col)



plot(df_Canada$Year, df_Canada$`Purchasing Power Index`, type="o", 
     pch=19, col=pl_col[1], xaxt="n", ylim = c(0, 200),
     xlab="Год", ylab="Значение", main="Индекс покупательной способности")
lines(`df_United States`$Year, `df_United States`$`Purchasing Power Index`, type="o", pch=19, col=pl_col[2])
lines(df_Turkey$Year, df_Turkey$`Purchasing Power Index`, type="o", pch=19, col=pl_col[3])
lines(df_Greece$Year, df_Greece$`Purchasing Power Index`,
      type="o", pch=19, col=pl_col[4])
lines(df_Denmark$Year, df_Denmark$`Purchasing Power Index`, type="o", pch=19, col=pl_col[5])
axis(side=1, at=df_Canada$Year)
legend('topright', cex = 0.5, pl_countries, fill=pl_col)


plot(df_Canada$Year, df_Canada$`Safety Index`, type="o", 
     pch=19, col=pl_col[1], xaxt="n", ylim = c(40, 110),
     xlab="Год", ylab="Значение", main="Индекс безопасности")
lines(`df_United States`$Year, `df_United States`$`Safety Index`, type="o", pch=19, col=pl_col[2])
lines(df_Turkey$Year, df_Turkey$`Safety Index`, type="o", pch=19, col=pl_col[3])
lines(df_Greece$Year, df_Greece$`Safety Index`,
      type="o", pch=19, col=pl_col[4])
lines(df_Denmark$Year, df_Denmark$`Safety Index`, type="o", pch=19, col=pl_col[5])
axis(side=1, at=df_Canada$Year)
legend('topright', cex = 0.5, pl_countries, fill=pl_col)


plot(df_Canada$Year, df_Canada$`Health Care Index`, type="o", 
     pch=19, col=pl_col[1], xaxt="n", ylim = c(40, 110),
     xlab="Год", ylab="Значение", main="Индекс здравоохранения")
lines(`df_United States`$Year, `df_United States`$`Health Care Index`, type="o", pch=19, col=pl_col[2])
lines(df_Turkey$Year, df_Turkey$`Health Care Index`, type="o", pch=19, col=pl_col[3])
lines(df_Greece$Year, df_Greece$`Health Care Index`,
      type="o", pch=19, col=pl_col[4])
lines(df_Denmark$Year, df_Denmark$`Health Care Index`, type="o", pch=19, col=pl_col[5])
axis(side=1, at=df_Canada$Year)
legend('topright', cex = 0.6, pl_countries, fill=pl_col)


plot(df_Canada$Year, df_Canada$`Cost of Living Index`, type="o", 
     pch=19, col=pl_col[1], xaxt="n", ylim = c(20, 210),
     xlab="Год", ylab="Значение", main="Индекс стоимости проживания")
lines(`df_United States`$Year, `df_United States`$`Cost of Living Index`, type="o", pch=19, col=pl_col[2])
lines(df_Turkey$Year, df_Turkey$`Cost of Living Index`, type="o", pch=19, col=pl_col[3])
lines(df_Greece$Year, df_Greece$`Cost of Living Index`,
      type="o", pch=19, col=pl_col[4])
lines(df_Denmark$Year, df_Denmark$`Cost of Living Index`, type="o", pch=19, col=pl_col[5])
axis(side=1, at=df_Canada$Year)
legend('topright', cex = 0.6, pl_countries, fill=pl_col)


plot(df_Canada$Year, df_Canada$`Property Price to Income Ratio`, type="o", 
     pch=19, col=pl_col[1], xaxt="n", ylim = c(4, 21),
     xlab="Год", ylab="Значение", main="Отношение стоимости недвижимости к заработной плате")
lines(`df_United States`$Year, `df_United States`$`Property Price to Income Ratio`, type="o", pch=19, col=pl_col[2])
lines(df_Turkey$Year, df_Turkey$`Property Price to Income Ratio`, type="o", pch=19, col=pl_col[3])
lines(df_Greece$Year, df_Greece$`Property Price to Income Ratio`,
      type="o", pch=19, col=pl_col[4])
lines(df_Denmark$Year, df_Denmark$`Property Price to Income Ratio`, type="o", pch=19, col=pl_col[5])
axis(side=1, at=df_Canada$Year)
legend('topright', cex = 0.6, pl_countries, fill=pl_col)


plot(df_Canada$Year, df_Canada$`Traffic Commute Time Index`, type="o", 
     pch=19, col=pl_col[1], xaxt="n", ylim = c(25, 70),
     xlab="Год", ylab="Значение", main="Индекс времени в дороге")
lines(`df_United States`$Year, `df_United States`$`Traffic Commute Time Index`, type="o", pch=19, col=pl_col[2])
lines(df_Turkey$Year, df_Turkey$`Traffic Commute Time Index`, type="o", pch=19, col=pl_col[3])
lines(df_Greece$Year, df_Greece$`Traffic Commute Time Index`,
      type="o", pch=19, col=pl_col[4])
lines(df_Denmark$Year, df_Denmark$`Traffic Commute Time Index`, type="o", pch=19, col=pl_col[5])
axis(side=1, at=df_Canada$Year)
legend('topright', cex = 0.6, pl_countries, fill=pl_col)


plot(df_Canada$Year, df_Canada$`Pollution Index`, type="o", 
     pch=19, col=pl_col[1], xaxt="n", ylim = c(20, 115),
     xlab="Год", ylab="Значение", main="Индекс уровня загрязнения")
lines(`df_United States`$Year, `df_United States`$`Pollution Index`, type="o", pch=19, col=pl_col[2])
lines(df_Turkey$Year, df_Turkey$`Pollution Index`, type="o", pch=19, col=pl_col[3])
lines(df_Greece$Year, df_Greece$`Pollution Index`,
      type="o", pch=19, col=pl_col[4])
lines(df_Denmark$Year, df_Denmark$`Pollution Index`, type="o", pch=19, col=pl_col[5])
axis(side=1, at=df_Canada$Year)
legend('topright', cex = 0.5, pl_countries, fill=pl_col)


plot(df_Canada$Year, df_Canada$`Climate Index`, type="o", 
     pch=19, col=pl_col[1], xaxt="n", ylim = c(5, 135),
     xlab="Год", ylab="Значение", main="Индекс качества климата")
lines(`df_United States`$Year, `df_United States`$`Climate Index`, type="o", pch=19, col=pl_col[2])
lines(df_Turkey$Year, df_Turkey$`Climate Index`, type="o", pch=19, col=pl_col[3])
lines(df_Greece$Year, df_Greece$`Climate Index`,
      type="o", pch=19, col=pl_col[4])
lines(df_Denmark$Year, df_Denmark$`Climate Index`, type="o", pch=19, col=pl_col[5])
axis(side=1, at=df_Canada$Year)
legend('topright', cex = 0.5, pl_countries, fill=pl_col)


url = read_html('https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/')
selector_name <- "a.post-list-item-title-link"
titles <- html_nodes(url, selector_name) %>% html_text() %>% as.array()
titles <- trimws(titles)
titles <- titles[1:38]

links <- html_elements(url, selector_name) %>% html_attr('href')
links <- links[1:38]

selector_name <- "span.addressItem.addressItem--single"
addresses <- html_nodes(url, selector_name) %>% html_text() %>% as.array(); addresses
addresses <- trimws(addresses); addresses
addresses <- substring(addresses, 3); addresses

museums <- data.frame('Название' = titles, 'Адрес' = addresses, 'Ссылка' = links); View(museums)