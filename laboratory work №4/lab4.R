# в ходе лабораторной работы,
# поэкспериментируйте c оформлением графиков и диаграмм

num <- c(1:12)
square <- num * num
plot(num, square, type="b")

# создает копию текущих параметров
opar <- par(no.readonly = TRUE)

# назначает тип линии – пунктирная (lty = 2) вместо сплошной
# по умолчанию и тип символа – заполненный треугольник (pch = 17)
par(lty=2, pch=17)  

plot(num,square,type="b")

#восстановление исходных значений параметров
par(opar)

plot(num, square, type="b", lty=2, pch=17)

n <- 10
mycolors <- rainbow(n)

# круговая диаграмма
pie(rep(1, n), labels = mycolors, col = mycolors) 

mygrays <- gray(0:n/n)
pie(rep(1, n), labels = mygrays, col = mygrays)

windowsFonts(
  A=windowsFont("Arial Black"),
  B=windowsFont("Bookman Old Style"),
  C=windowsFont("Comic Sans MS")
)

par(family="С") 

opar <- par(no.readonly = TRUE)

par(pin = c(1, 1))                    
par(lwd = 2, cex = 1.5)            
par(cex.axis = .75, font.axis = 3)      
plot(num, square, type = "b", pch = 19, lty = 2, col = "red")    
plot(num, square, type = "b", pch = 23, lty = 6, col = "blue", bg = "green")

par(opar)

month <- c("янв","февр","март" ,"апр" ,"май" ,"июнь" ,"июль","авг","сент", "окт","ноя","дек")
month.f <- month
month.o <- ordered(month.f, levels = month)

plot(month.o, square, type = "b", pch = 23, lty = 6, col = "blue", bg = "green")
plot(num, square, type = "b", pch = 23, lty = 6, col = "blue", bg = "green", las = 3)

plot(num, square, type="b", 
     col="green", lty=2, pch=2, lwd=2,
     main="Квадратичная зависимость", 
     sub="Просто квадрат числа", 
     xlab="Month", ylab="Квадрат числа",
     xlim=c(0, 12), ylim=c(0, 300))

plot(num, square, type="b",  ann=FALSE,
     col="green", lty=2, pch=2, lwd=2,
     xlim=c(0, 12), ylim=c(0, 300))

title(main="Квадратичная зависимость", col.main="red", 
      sub="Просто квадрат числа", col.sub="blue", 
      xlab="Month", ylab="Квадрат числа",
      col.lab="green", cex.lab=1)

attach(mtcars)
opar <- par(no.readonly=TRUE)

par(mfrow=c(2,2)) # будет четыре графика

plot(wt ,mpg, main = "Диаграмма рассеяния для \n расхода топлива и веса машины")
plot(wt, disp, main = "Диаграмма рассеяния для \n объема двигателя и веса машины")
hist(wt, main = "Распределение  значений \n веса машины")
boxplot(wt, main = "Ящик-с-усами \n для веса машины")

par(opar)
detach(mtcars)

attach(mtcars)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
hist(wt)
hist(mpg)
hist(disp)
detach(mtcars)

# вывести графики изменения олимпийских достижений cтраны:
# столбчатую диаграмма по количеству мест 1-8 (спортсменов Страны)
# по каждой Олимпиаде по назначенному виду спорта,
# круговую диаграмму по количеству первых мест в каждой из олимпиад,
# функциональные графики - тенденции изменения количества призовых мест
# отдельно по мужчинам и женщинам за последние 20 лет

# россия, биатлон

Sys.setlocale("LC_CTYPE", "russian")
setwd('C:/Users/diana/Desktop/big-data/laboratory work №4/olympic')

# не имеет смысла, олимп. игры по биатлону проводятся зимой
summer <- read.csv('summer.csv', sep = ',' , header = TRUE)
summer_rus <- summer[summer$Country == 'RUS',]

winter <- read.csv('winter.csv', sep = ',' , header = TRUE)
winter_rus_b <- winter[winter$Country == 'RUS' & winter$Sport == 'Biathlon',]
df <- winter_rus_b

medal_names <- c('Золото', 'Серебро', 'Бронза')
medal_colors <- c('gold', 'gray', 'brown')

medals_1994 <- df[df$Year == 1994,]$Medal
gold_1994 <- length(medals_1994[medals_1994 == 'Gold'])
silver_1994 <- length(medals_1994[medals_1994 == 'Silver'])
bronze_1994 <- length(medals_1994[medals_1994 == 'Bronze'])
olympic_1994 <- c(gold_1994, silver_1994, bronze_1994)

barplot(olympic_1994, xlab = 'Медаль', ylab = 'Количество',
        main = 'Медали, полученные российскими спортсменами
        на олимпийских играх по биатлону 1994 г.', names.arg = medal_names,
        col.main = "red", col = medal_colors)

medals_1998 <- df[df$Year == 1998,]$Medal
gold_1998 <- length(medals_1998[medals_1998 == 'Gold'])
silver_1998 <- length(medals_1998[medals_1998 == 'Silver'])
bronze_1998 <- length(medals_1998[medals_1998 == 'Bronze'])
olympic_1998 <- c(gold_1998, silver_1998, bronze_1998)

barplot(olympic_1998, xlab = 'Медаль', ylab = 'Количество',
        main = 'Медали, полученные российскими спортсменами
        на олимпийских играх по биатлону 1998 г.', names.arg = medal_names,
        col.main = "red", col = medal_colors)

medals_2002 <- df[df$Year == 2002,]$Medal
gold_2002 <- length(medals_2002[medals_2002 == 'Gold'])
silver_2002 <- length(medals_2002[medals_2002 == 'Silver'])
bronze_2002 <- length(medals_2002[medals_2002 == 'Bronze'])
olympic_2002 <- c(gold_2002, silver_2002, bronze_2002)

barplot(olympic_2002, xlab = 'Медаль', ylab = 'Количество',
        main = 'Медали, полученные российскими спортсменами
        на олимпийских играх по биатлону 2002 г.', names.arg = medal_names,
        col.main = "red", col = medal_colors)

medals_2006 <- df[df$Year == 2006,]$Medal
gold_2006 <- length(medals_2006[medals_2006 == 'Gold'])
silver_2006 <- length(medals_2006[medals_2006 == 'Silver'])
bronze_2006 <- length(medals_2006[medals_2006 == 'Bronze'])
olympic_2006 <- c(gold_2006, silver_2006, bronze_2006)

barplot(olympic_2006, xlab = 'Медаль', ylab = 'Количество',
        main = 'Медали, полученные российскими спортсменами
        на олимпийских играх по биатлону 2006 г.', names.arg = medal_names,
        col.main = "red", col = medal_colors)

medals_2010 <- df[df$Year == 2010,]$Medal
gold_2010 <- length(medals_2010[medals_2010 == 'Gold'])
silver_2010 <- length(medals_2010[medals_2010 == 'Silver'])
bronze_2010 <- length(medals_2010[medals_2010 == 'Bronze'])
olympic_2010 <- c(gold_2010, silver_2010, bronze_2010)

barplot(olympic_2010, xlab = 'Медаль', ylab = 'Количество',
        main = 'Медали, полученные российскими спортсменами
        на олимпийских играх по биатлону 2010 г.', names.arg = medal_names,
        col.main = "red", col = medal_colors)

medals_2014 <- df[df$Year == 2014,]$Medal
gold_2014 <- length(medals_2014[medals_2014 == 'Gold'])
silver_2014 <- length(medals_2014[medals_2014 == 'Silver'])
bronze_2014 <- length(medals_2014[medals_2014 == 'Bronze'])
olympic_2014 <- c(gold_2014, silver_2014, bronze_2014)

barplot(olympic_2014, xlab = 'Медаль', ylab = 'Количество',
        main = 'Медали, полученные российскими спортсменами
        на олимпийских играх по биатлону 2014 г.', names.arg = medal_names,
        col.main = "red", col = medal_colors)

# результаты за 1994 - 2014 г.
olympic_all <- olympic_1994 + olympic_1998 + olympic_2002 +
  olympic_2006 + olympic_2010 + olympic_2014

# столбчатая диаграмма
barplot(olympic_all, xlab = 'Медаль', ylab = 'Количество',
        main = 'Медали, полученные российскими спортсменами
        на олимпийских играх по биатлону за 1994 - 2014 гг.',
        names.arg = medal_names,
        col.main = "red", col = medal_colors)

gold_all <- c(gold_1994, gold_1998, gold_2002,
              gold_2006, gold_2010, gold_2014)
print(gold_all)

pie_percent<- round(100*gold_all/sum(gold_all), 1)

medal_labels <- c('1994', '1998', '2002', '2006', '2010', '2014')

# setwd('C:/Users/diana/Desktop/big-data/laboratory work №4')
# png(file = "olympic_games_gold_biathlon.jpg")

# pie(gold_all, labels = medal_labels,
#    main = 'Золотые медали, полученные российскими спортсменами
#        на олимпийских играх по биатлону за 1994 - 2014 гг.', clockwise = TRUE)

# круговая диаграмма
pie(gold_all, labels = pie_percent,
    main = 'Золотые медали, полученные российскими спортсменами
        на олимпийских играх по биатлону за 1994 - 2014 гг.',
    radius = 1,
    col = rainbow(length(gold_all)))

legend("topright", medal_labels, cex = 1,
       fill = rainbow(length(gold_all)))

# dev.off()

# тенденция изменения
years_w <- df[df$Gender == 'Women', ]$Year
years_m <- df[df$Gender == 'Men', ]$Year

w_1994 <- length(years_w[years_w == 1994])
w_1998 <- length(years_w[years_w == 1998])
w_2002 <- length(years_w[years_w == 2002])
w_2006 <- length(years_w[years_w == 2006])
w_2010 <- length(years_w[years_w == 2010])
w_2014 <- length(years_w[years_w == 2014])
w_all <- c(w_1994, w_1998, w_2002, w_2006, w_2010, w_2014)

m_1994 <- length(years_m[years_m == 1994])
m_1998 <- length(years_m[years_m == 1998])
m_2002 <- length(years_m[years_m == 2002])
m_2006 <- length(years_m[years_m == 2006])
m_2010 <- length(years_m[years_m == 2010])
m_2014 <- length(years_m[years_m == 2014])
m_all <- c(m_1994, m_1998, m_2002, m_2006, m_2010, m_2014)

years <- c(1994, 1998, 2002, 2006, 2010, 2014)

plot(years, w_all, pch = 20, col = "red",
     main = "Тенденция изменения количества призовых мест
     российских спортсменов по биатлону 1994 - 2014 гг.",
     xlab="Год", ylab="Количество", xlim = c(1994, 2015), ylim=c(0, 8))

lines(years, w_all, col = "red")

points(years, m_all, pch = 20, col = "forestgreen")
lines(years, m_all, pch = 20, col = "forestgreen")

legend("topright", c("Женщины", "Мужчины"), cex = 0.8,
       fill =  c("red", "forestgreen"))

# вывести графики изменения спортивных достижений
# 1) по золотым медалям
# 2) по призовым 3-местам
# по 7-и странам-призерам (разными цветами и точками)
# за последние 4 олимпиады
# 2010 2014 2018 2022

countries <- c("Канада", "Германия", "США", "Норвегия",
               "Южная Корея", "Швейцария", "Китай")
years_gold <- c(2010, 2014, 2018, 2022)
colors_gold <- rainbow(7)

canada_2010 <- c(14, 7, 5)
germany_2010 <- c(10, 13, 7)
usa_2010 <- c(9, 15, 13)
norway_2010 <- c(9, 8, 6)
south_korea_2010 <- c(6, 6, 2)
switzerland_2010 <- c(6, 0, 3)
china_2010 <- c(5, 2, 4)

canada_2014 <- c(10, 10, 5)
germany_2014 <- c(8, 6, 5)
usa_2014 <- c(9, 7, 12)
norway_2014 <- c(11, 5, 10)
south_korea_2014 <- c(3, 3, 2)
switzerland_2014 <- c(6, 3, 2)
china_2014 <- c(3, 4, 2)

canada_2018 <- c(11, 8, 10)
germany_2018 <- c(14, 10, 7)
usa_2018 <- c(9, 8, 6)
norway_2018 <- c(14, 14, 11)
south_korea_2018 <- c(5, 8, 4)
switzerland_2018 <- c(5, 6, 4)
china_2018 <- c(1, 6, 2)

canada_2022 <- c(4, 8, 15)
germany_2022 <- c(12, 10, 4)
usa_2022 <- c(8, 10, 7)
norway_2022 <- c(16, 18, 13)
south_korea_2022 <- c(2, 5, 2)
switzerland_2022 <- c(7, 2, 6)
china_2022 <- c(9, 4, 1)

canada_gold <- c(canada_2010[1], canada_2014[1],
                 canada_2018[1], canada_2022[1])

germany_gold <- c(germany_2010[1], germany_2014[1],
                  germany_2018[1], germany_2022[1])

usa_gold <- c(usa_2010[1], usa_2014[1],
              usa_2018[1], usa_2022[1])

norway_gold <- c(norway_2010[1], norway_2014[1],
                 norway_2018[1], norway_2022[1])

south_korea_gold <- c(south_korea_2010[1], south_korea_2014[1],
                      south_korea_2018[1], south_korea_2022[1])

switzerland_gold <- c(switzerland_2010[1], switzerland_2014[1],
                      switzerland_2018[1], switzerland_2022[1])

china_gold <- c(china_2010[1], china_2014[1],
                china_2018[1], china_2022[1])

plot(years_gold, canada_gold, pch = 20, col = colors_gold[1],
     main = "Тенденция изменения количества золотых медалей 2010 - 2022 гг.",
     xlab="Год", ylab="Количество", xlim = c(2010, 2022), ylim=c(0, 30),
     cex.axis=1, cex.lab=1, cex.main=1)
lines(years_gold, canada_gold,  col = colors_gold[1], lwd = 2)

points(years_gold, germany_gold, pch = 20, col = colors_gold[2])
lines(years_gold, germany_gold,  col = colors_gold[2], lwd = 2)

points(years_gold, usa_gold, pch = 20, col = colors_gold[3])
lines(years_gold, usa_gold,  col = colors_gold[3], lwd = 2)

points(years_gold, norway_gold, pch = 20, col = colors_gold[4])
lines(years_gold, norway_gold,  col = colors_gold[4], lwd = 2)

points(years_gold, south_korea_gold, pch = 20, col = colors_gold[5])
lines(years_gold, south_korea_gold,  col = colors_gold[5], lwd = 2)

points(years_gold, switzerland_gold, pch = 20, col = colors_gold[6])
lines(years_gold, switzerland_gold,  col = colors_gold[6], lwd = 2)

points(years_gold, china_gold, pch = 20, col = colors_gold[7])
lines(years_gold, china_gold, col = colors_gold[7], lwd = 2)

legend("topright", countries, cex = 0.6, ncol = 2, text.width = 1,
       title = " Страна", fill =  colors_gold)

canada_all <- c(sum(canada_2010), sum(canada_2014),
                 sum(canada_2018), sum(canada_2022))

germany_all <- c(sum(germany_2010), sum(germany_2014),
                 sum(germany_2018), sum(germany_2022))

usa_all <- c(sum(usa_2010), sum(usa_2014),
             sum(usa_2018), sum(usa_2022))

norway_all <- c(sum(norway_2010), sum(norway_2014),
                sum(norway_2018), sum(norway_2022))

south_korea_all <- c(sum(south_korea_2010), sum(south_korea_2014),
                     sum(south_korea_2018), sum(south_korea_2022))

switzerland_all <- c(sum(switzerland_2010), sum(switzerland_2014),
                     sum(switzerland_2018), sum(switzerland_2022))

china_all <- c(sum(china_2010), sum(china_2014),
               sum(china_2018), sum(china_2022))

plot(years_gold, canada_all, pch = 20, col = colors_gold[1],
     main = "Тенденция изменения количества призовых мест 2010 - 2022 гг.",
     xlab="Год", ylab="Количество", xlim = c(2010, 2022), ylim=c(0, 80),
     cex.axis=1, cex.lab=1, cex.main=1)
lines(years_gold, canada_all,  col = colors_gold[1], lwd = 2)

points(years_gold, germany_all, pch = 20, col = colors_gold[2])
lines(years_gold, germany_all,  col = colors_gold[2], lwd = 2)

points(years_gold, usa_all, pch = 20, col = colors_gold[3])
lines(years_gold, usa_all,  col = colors_gold[3], lwd = 2)

points(years_gold, norway_all, pch = 20, col = colors_gold[4])
lines(years_gold, norway_all,  col = colors_gold[4], lwd = 2)

points(years_gold, south_korea_gold, pch = 20, col = colors_gold[5])
lines(years_gold, south_korea_gold,  col = colors_gold[5], lwd = 2)

points(years_gold, switzerland_all, pch = 20, col = colors_gold[6])
lines(years_gold, switzerland_all,  col = colors_gold[6], lwd = 2)

points(years_gold, china_all, pch = 20, col = colors_gold[7])
lines(years_gold, china_all, col = colors_gold[7], lwd = 2)

# https://runebook.dev/ru/docs/r/library/graphics/html/legend
legend("topright", countries, cex = 0.6, ncol = 2,
       x.intersp = 1, y.intersp = 1, text.width = 1,
       title = " Страна", fill =  colors_gold)

