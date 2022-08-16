library(rvest)

# URL for the Visit Ithaca website, wineries page
url <- read_html("http://www.visitithaca.com/attractions/wineries.html")

# Pull out the names of the wineries and breweries
selector_name <- "div.indSearchListingContainer__grid__prop-cta__title"

fnames <- html_nodes(url, selector_name) %>% html_text()%>%as.array()
print(fnames)

selector_name <- ".indSearchListingContainer__grid__prop-cta"

fnames_addr <- html_nodes(url, selector_name) %>% html_attr("data-org-href")
print(fnames_addr)

fnames_addr1 <- paste0("http://www.visitithaca.com", fnames_addr)
print(fnames_addr1)

url_sub <- read_html("http://www.visitithaca.com/attractions/thirsty-owl-wine-company")

sub_selector_name <- ".indMetaInfoWrapper"

f1_address <- html_text(html_node(url_sub, sub_selector_name), trim=TRUE)
print(f1_address)

f1_address <- html_text(html_nodes(url_sub, sub_selector_name), trim=TRUE)
print(f1_address)

# url= read_html('http://ladiesvenue.ru/olimpiada-2018-tablica-medalej-rezultaty-rossijskix-sportsmenov-15-fevralya/')
# nodes = html_nodes(url, 'table')
# print(nodes)
# 
# df1 = html_table(nodes[[1]])%>%as.data.frame()
# df2 = html_table(nodes[[2]])%>%as.data.frame()

# cоставить data.frame (возможно для каждой страны) так,
# чтобы иметь возможность проанализировать с помощью графиков
# изменение рейтингов для всех 10 показателей для всех своих 5-ти стран
# необходимо нарисовать на одном и том же графике рейтинг всех 5 стран,
# подобрать наилучший (с вашей точки зрения) способ визуалазации

# Германия, Британия, Греция, Китай, Россия

url_21 <- read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021')
url_20 <- read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020')
url_19 <- read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2019')
url_18 <- read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2018')
url_17 <- read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2017')
url_16 <- read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2016')
url_15 <- read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2015')
url_14 <- read_html('https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2014')

nodes_21 <- html_nodes(url_21, 'table')
nodes_20 <- html_nodes(url_20, 'table')
nodes_19 <- html_nodes(url_19, 'table')
nodes_18 <- html_nodes(url_18, 'table')
nodes_17 <- html_nodes(url_17, 'table')
nodes_16 <- html_nodes(url_16, 'table')
nodes_15 <- html_nodes(url_15, 'table')
nodes_14 <- html_nodes(url_14, 'table')

df_21 <- html_table(nodes_21[[2]])%>%as.data.frame()
df_20 <- html_table(nodes_20[[2]])%>%as.data.frame()
df_19 <- html_table(nodes_19[[2]])%>%as.data.frame()
df_18 <- html_table(nodes_18[[2]])%>%as.data.frame()
df_17 <- html_table(nodes_17[[2]])%>%as.data.frame()
df_16 <- html_table(nodes_16[[2]])%>%as.data.frame()
df_15 <- html_table(nodes_15[[2]])%>%as.data.frame()
df_14 <- html_table(nodes_14[[2]])%>%as.data.frame()

rownames(df_21) <- df_21[, 2]
rownames(df_20) <- df_20[, 2]
rownames(df_19) <- df_19[, 2]
rownames(df_18) <- df_18[, 2]
rownames(df_17) <- df_17[, 2]
rownames(df_16) <- df_16[, 2]
rownames(df_15) <- df_15[, 2]
rownames(df_14) <- df_14[, 2]

df_21 <- df_21[, 3:11]
df_20 <- df_20[, 3:11]
df_19 <- df_19[, 3:11]
df_18 <- df_18[, 3:11]
df_17 <- df_17[, 3:11]
df_16 <- df_16[, 3:11]
df_15 <- df_15[, 3:11]
df_14 <- df_14[, 3:11]

countries <- c("Germany", "United Kingdom", "Greece", "China", "Russia")
countries_rus <- c("Германия", "Великобритания", "Греция", "Китай", "Россия")
years <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")

country = "Germany"
df <- data.frame(df_14[country,])
colnames(df) <- colnames(df_14)

df <- rbind(df, df_15[country,])
df <- rbind(df, df_16[country,])
df <- rbind(df, df_17[country,])
df <- rbind(df, df_18[country,])
df <- rbind(df, df_19[country,])
df <- rbind(df, df_20[country,])
df <- rbind(df, df_21[country,])

rownames(df) <- years
df_germany <- df

country = "United Kingdom"
df <- data.frame(df_14[country,])
colnames(df) <- colnames(df_14)

df <- rbind(df, df_15[country,])
df <- rbind(df, df_16[country,])
df <- rbind(df, df_17[country,])
df <- rbind(df, df_18[country,])
df <- rbind(df, df_19[country,])
df <- rbind(df, df_20[country,])
df <- rbind(df, df_21[country,])

rownames(df) <- years
df_UK <- df

country = "Greece"
df <- data.frame(df_14[country,])
colnames(df) <- colnames(df_14)

df <- rbind(df, df_15[country,])
df <- rbind(df, df_16[country,])
df <- rbind(df, df_17[country,])
df <- rbind(df, df_18[country,])
df <- rbind(df, df_19[country,])
df <- rbind(df, df_20[country,])
df <- rbind(df, df_21[country,])

rownames(df) <- years
df_greece <- df

country = "China"
df <- data.frame(df_14[country,])
colnames(df) <- colnames(df_14)

df <- rbind(df, df_15[country,])
df <- rbind(df, df_16[country,])
df <- rbind(df, df_17[country,])
df <- rbind(df, df_18[country,])
df <- rbind(df, df_19[country,])
df <- rbind(df, df_20[country,])
df <- rbind(df, df_21[country,])

rownames(df) <- years
df_china <- df

country = "Russia"
df <- data.frame(df_14[country,])
colnames(df) <- colnames(df_14)

df <- rbind(df, df_15[country,])
df <- rbind(df, df_16[country,])
df <- rbind(df, df_17[country,])
df <- rbind(df, df_18[country,])
df <- rbind(df, df_19[country,])
df <- rbind(df, df_20[country,])
df <- rbind(df, df_21[country,])

rownames(df) <- years
df_russia <- df

# индекс покупательной способности (чем выше, тем лучше), 
# индекс загрязнения (чем ниже, тем лучше), 
# отношение цены на жилье к доходу (ниже). лучше), 
# индекс прожиточного минимума (чем ниже, тем лучше), 
# индекс безопасности (чем выше, тем лучше), 
# индекс медицинского обслуживания (чем выше, тем лучше), 
# индекс времени движения на дороге (чем ниже, тем лучше) 
# климатический индекс (чем выше, тем лучше).

names_indexes <- colnames(df_russia)
print(names_indexes)

country_colors = rainbow(length(countries))

index <- "Quality of Life Index"

germany <- c()
UK <- c()
greece <- c()
china <- c()
russia <- c()

for(year in years)
{
  germany <- c(germany, df_germany[year, index])
  UK <- c(UK, df_UK[year, index])
  greece <- c(greece, df_greece[year, index])
  china <- c(china, df_china[year, index])
  russia <- c(russia, df_russia[year, index])
}

plot(years, germany, pch = 20, col = country_colors[1],
     main = "Тенденция изменения индекса качества жизни",
     xlab = "Год", ylab = "Индекс качества жизни",
     xlim = c(2014, 2021), ylim=c(0.0, 200.0),
     cex.axis=1, cex.lab=1, cex.main=1)
lines(years, germany,  col = country_colors[1], lwd = 1)

points(years, UK, pch = 20, col = country_colors[2])
lines(years, UK,  col = country_colors[2], lwd = 1)

points(years, greece, pch = 20, col = country_colors[3])
lines(years, greece,  col = country_colors[3], lwd = 1)

points(years, china, pch = 20, col = country_colors[4])
lines(years, china,  col = country_colors[4], lwd = 1)

points(years, russia, pch = 20, col = country_colors[5])
lines(years, russia,  col = country_colors[5], lwd = 1)

legend("bottomright", countries_rus, cex = 0.6, ncol = 2,
       x.intersp = 1, y.intersp = 1, text.width = 1,
       title = " Страна", fill =  country_colors)

index <- "Purchasing Power Index"

germany <- c()
UK <- c()
greece <- c()
china <- c()
russia <- c()

for(year in years)
{
  germany <- c(germany, df_germany[year, index])
  UK <- c(UK, df_UK[year, index])
  greece <- c(greece, df_greece[year, index])
  china <- c(china, df_china[year, index])
  russia <- c(russia, df_russia[year, index])
}

plot(years, germany, pch = 20, col = country_colors[1],
     main = "Тенденция изменения индекса покупательной способности",
     xlab = "Год", ylab = "Индекс покупательной способности",
     xlim = c(2014, 2021), ylim=c(0, 200),
     cex.axis=1, cex.lab=1, cex.main=1)
lines(years, germany,  col = country_colors[1], lwd = 1)

points(years, UK, pch = 20, col = country_colors[2])
lines(years, UK,  col = country_colors[2], lwd = 1)

points(years, greece, pch = 20, col = country_colors[3])
lines(years, greece,  col = country_colors[3], lwd = 1)

points(years, china, pch = 20, col = country_colors[4])
lines(years, china,  col = country_colors[4], lwd = 1)

points(years, russia, pch = 20, col = country_colors[5])
lines(years, russia,  col = country_colors[5], lwd = 1)

legend("topright", countries_rus, cex = 0.6, ncol = 2,
       x.intersp = 1, y.intersp = 1, text.width = 1,
       title = " Страна", fill =  country_colors)

index <- "Safety Index"

germany <- c()
UK <- c()
greece <- c()
china <- c()
russia <- c()

for(year in years)
{
  germany <- c(germany, df_germany[year, index])
  UK <- c(UK, df_UK[year, index])
  greece <- c(greece, df_greece[year, index])
  china <- c(china, df_china[year, index])
  russia <- c(russia, df_russia[year, index])
}

plot(years, germany, pch = 20, col = country_colors[1],
     main = "Тенденция изменения индекса безопасности",
     xlab = "Год", ylab = "Индекс безопасности",
     xlim = c(2014, 2021), ylim=c(0, 100),
     cex.axis=1, cex.lab=1, cex.main=1)
lines(years, germany,  col = country_colors[1], lwd = 1)

points(years, UK, pch = 20, col = country_colors[2])
lines(years, UK,  col = country_colors[2], lwd = 1)

points(years, greece, pch = 20, col = country_colors[3])
lines(years, greece,  col = country_colors[3], lwd = 1)

points(years, china, pch = 20, col = country_colors[4])
lines(years, china,  col = country_colors[4], lwd = 1)

points(years, russia, pch = 20, col = country_colors[5])
lines(years, russia,  col = country_colors[5], lwd = 1)

legend("bottomright", countries_rus, cex = 0.6, ncol = 2,
       x.intersp = 1, y.intersp = 1, text.width = 1,
       title = " Страна", fill =  country_colors)

index <- "Health Care Index"

germany <- c()
UK <- c()
greece <- c()
china <- c()
russia <- c()

for(year in years)
{
  germany <- c(germany, df_germany[year, index])
  UK <- c(UK, df_UK[year, index])
  greece <- c(greece, df_greece[year, index])
  china <- c(china, df_china[year, index])
  russia <- c(russia, df_russia[year, index])
}

plot(years, germany, pch = 20, col = country_colors[1],
     main = "Тенденция изменения индекса медицинского обслуживания",
     xlab = "Год", ylab = "Индекс медицинского обслуживания",
     xlim = c(2014, 2021), ylim=c(0, 100),
     cex.axis=1, cex.lab=1, cex.main=1)
lines(years, germany,  col = country_colors[1], lwd = 1)

points(years, UK, pch = 20, col = country_colors[2])
lines(years, UK,  col = country_colors[2], lwd = 1)

points(years, greece, pch = 20, col = country_colors[3])
lines(years, greece,  col = country_colors[3], lwd = 1)

points(years, china, pch = 20, col = country_colors[4])
lines(years, china,  col = country_colors[4], lwd = 1)

points(years, russia, pch = 20, col = country_colors[5])
lines(years, russia,  col = country_colors[5], lwd = 1)

legend("bottomright", countries_rus, cex = 0.6, ncol = 2,
       x.intersp = 1, y.intersp = 1, text.width = 1,
       title = " Страна", fill =  country_colors)

index <- "Cost of Living Index"

germany <- c()
UK <- c()
greece <- c()
china <- c()
russia <- c()

for(year in years)
{
  germany <- c(germany, df_germany[year, index])
  UK <- c(UK, df_UK[year, index])
  greece <- c(greece, df_greece[year, index])
  china <- c(china, df_china[year, index])
  russia <- c(russia, df_russia[year, index])
}

plot(years, germany, pch = 20, col = country_colors[1],
     main = "Тенденция изменения индекса прожиточного минимума",
     xlab = "Год", ylab = "Индекс прожиточного минимума",
     xlim = c(2014, 2021), ylim=c(0, 100),
     cex.axis=1, cex.lab=1, cex.main=1)
lines(years, germany,  col = country_colors[1], lwd = 1)

points(years, UK, pch = 20, col = country_colors[2])
lines(years, UK,  col = country_colors[2], lwd = 1)

points(years, greece, pch = 20, col = country_colors[3])
lines(years, greece,  col = country_colors[3], lwd = 1)

points(years, china, pch = 20, col = country_colors[4])
lines(years, china,  col = country_colors[4], lwd = 1)

points(years, russia, pch = 20, col = country_colors[5])
lines(years, russia,  col = country_colors[5], lwd = 1)

legend("bottomleft", countries_rus, cex = 0.6, ncol = 2,
       x.intersp = 1, y.intersp = 1, text.width = 1,
       title = " Страна", fill =  country_colors)

index <- "Property Price to Income Ratio"

germany <- c()
UK <- c()
greece <- c()
china <- c()
russia <- c()

for(year in years)
{
  germany <- c(germany, df_germany[year, index])
  UK <- c(UK, df_UK[year, index])
  greece <- c(greece, df_greece[year, index])
  china <- c(china, df_china[year, index])
  russia <- c(russia, df_russia[year, index])
}

plot(years, germany, pch = 20, col = country_colors[1],
     main = "Тенденция изменения отношение цены на жилье к доходу",
     xlab = "Год", ylab = "Отношение цены на жилье к доходу",
     xlim = c(2014, 2021), ylim=c(0, 50),
     cex.axis=1, cex.lab=1, cex.main=1)
lines(years, germany,  col = country_colors[1], lwd = 1)

points(years, UK, pch = 20, col = country_colors[2])
lines(years, UK,  col = country_colors[2], lwd = 1)

points(years, greece, pch = 20, col = country_colors[3])
lines(years, greece,  col = country_colors[3], lwd = 1)

points(years, china, pch = 20, col = country_colors[4])
lines(years, china,  col = country_colors[4], lwd = 1)

points(years, russia, pch = 20, col = country_colors[5])
lines(years, russia,  col = country_colors[5], lwd = 1)

legend("topright", countries_rus, cex = 0.6, ncol = 2,
       x.intersp = 1, y.intersp = 1, text.width = 1,
       title = " Страна", fill =  country_colors)

index <- "Traffic Commute Time Index"

germany <- c()
UK <- c()
greece <- c()
china <- c()
russia <- c()

for(year in years)
{
  germany <- c(germany, df_germany[year, index])
  UK <- c(UK, df_UK[year, index])
  greece <- c(greece, df_greece[year, index])
  china <- c(china, df_china[year, index])
  russia <- c(russia, df_russia[year, index])
}

plot(years, germany, pch = 20, col = country_colors[1],
     main = "Тенденция изменения индекса времени движения на дороге",
     xlab = "Год", ylab = "Индекс времени движения на дороге",
     xlim = c(2014, 2021), ylim=c(0, 60),
     cex.axis=1, cex.lab=1, cex.main=1)
lines(years, germany,  col = country_colors[1], lwd = 1)

points(years, UK, pch = 20, col = country_colors[2])
lines(years, UK,  col = country_colors[2], lwd = 1)

points(years, greece, pch = 20, col = country_colors[3])
lines(years, greece,  col = country_colors[3], lwd = 1)

points(years, china, pch = 20, col = country_colors[4])
lines(years, china,  col = country_colors[4], lwd = 1)

points(years, russia, pch = 20, col = country_colors[5])
lines(years, russia,  col = country_colors[5], lwd = 1)

legend("bottomright", countries_rus, cex = 0.6, ncol = 2,
       x.intersp = 1, y.intersp = 1, text.width = 1,
       title = " Страна", fill =  country_colors)

index <- "Pollution Index"

germany <- c()
UK <- c()
greece <- c()
china <- c()
russia <- c()

for(year in years)
{
  germany <- c(germany, df_germany[year, index])
  UK <- c(UK, df_UK[year, index])
  greece <- c(greece, df_greece[year, index])
  china <- c(china, df_china[year, index])
  russia <- c(russia, df_russia[year, index])
}

plot(years, germany, pch = 20, col = country_colors[1],
     main = "Тенденция изменения индекса загрязнения",
     xlab = "Год", ylab = "Индекс загрязнения",
     xlim = c(2014, 2021), ylim=c(0, 150),
     cex.axis=1, cex.lab=1, cex.main=1)
lines(years, germany,  col = country_colors[1], lwd = 1)

points(years, UK, pch = 20, col = country_colors[2])
lines(years, UK,  col = country_colors[2], lwd = 1)

points(years, greece, pch = 20, col = country_colors[3])
lines(years, greece,  col = country_colors[3], lwd = 1)

points(years, china, pch = 20, col = country_colors[4])
lines(years, china,  col = country_colors[4], lwd = 1)

points(years, russia, pch = 20, col = country_colors[5])
lines(years, russia,  col = country_colors[5], lwd = 1)

legend("topright", countries_rus, cex = 0.6, ncol = 2,
       x.intersp = 1, y.intersp = 1, text.width = 1,
       title = " Страна", fill =  country_colors)

index <- "Climate Index"

germany <- c()
UK <- c()
greece <- c()
china <- c()
russia <- c()

for(year in years)
{
  germany <- c(germany, df_germany[year, index])
  UK <- c(UK, df_UK[year, index])
  greece <- c(greece, df_greece[year, index])
  china <- c(china, df_china[year, index])
  russia <- c(russia, df_russia[year, index])
}

plot(years, germany, pch = 20, col = country_colors[1],
     main = "Тенденция изменения климатического индекса",
     xlab = "Год", ylab = "Климатический индекс",
     xlim = c(2016, 2021), ylim=c(0, 160),
     cex.axis=1, cex.lab=1, cex.main=1)
lines(years, germany,  col = country_colors[1], lwd = 1)

points(years, UK, pch = 20, col = country_colors[2])
lines(years, UK,  col = country_colors[2], lwd = 1)

points(years, greece, pch = 20, col = country_colors[3])
lines(years, greece,  col = country_colors[3], lwd = 1)

points(years, china, pch = 20, col = country_colors[4])
lines(years, china,  col = country_colors[4], lwd = 1)

points(years, russia, pch = 20, col = country_colors[5])
lines(years, russia,  col = country_colors[5], lwd = 1)

legend("topright", countries_rus, cex = 0.6, ncol = 2,
       x.intersp = 1, y.intersp = 1, text.width = 1,
       title = " Страна", fill =  country_colors)

# c одной из страниц
# https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/
# или https://tonkosti.ru/Музеи_Санкт-Петербурга
# собрать информацию в data.frame, которя содержала бы:
# Название музея, его адрес и ссылку для перехода при клике на фото  музея

url<-read_html('https://kudago.com/spb/list/33-luchshih-muzeya-peterburga/')

selector_name <- 'a.post-list-item-title-link'
fnames <- html_nodes(url, selector_name)%>%html_text()%>%as.vector()

selector_name <- 'address.post-list-item-info'
fnames2 <- html_nodes(url, selector_name)%>%html_text()%>%as.vector()

selector_name <- '.post-list-item-title-link'
fnames_addr <- html_nodes(url, selector_name)%>%html_attr('href')
fnames_addr2 <- fnames_addr

d<-data.frame(fnames[1:40], fnames2, fnames_addr2[1:40])
colnames(d)<-c('Музей', 'Адрес', 'Ссылка')