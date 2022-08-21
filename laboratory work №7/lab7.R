Sys.setlocale("LC_CTYPE", "russian")
setwd('C:/Users/diana/Desktop/big-data/laboratory work №7')

df <- read.csv('athlete_events.csv', sep = ',' , header = TRUE)

# Проверьте гипотезу о среднем весе спортсменов 
# выбранного вида спорта (вид спорта остается по ЛР4)

df1 <- df[which(df[, "Sport"] == "Basketball"),]
df1 <- df1[-which(is.na(df1[, "Weight"])),]

x <- df1[1:nrow(df1), "Weight"]

# Проверка на нормальность распределения.
# Тест Шапиро-Уилкса (Shapiro-Wilk test)
shapiro.test(x)

# Графический способ
# Гистограмма с линией плотности
x2 <- seq(min(x), max(x), length = length(x))
fun <- dnorm(x2, mean = mean(x), sd = sd(x))
hist(x, freq = FALSE, col = "gray")
lines(x2, fun, col = 2, lwd = 2)

# Квантильно-квантильный график
qqnorm(x)
qqline(x, col = 4, lwd = 2)

# Одномерные статистические тесты

# Тест Стьюдента
t.test(x, mu=mean(x), conf.int = TRUE)

# Тест Уилкоксона
wilcox.test(x, mu = mean(x), conf.int = TRUE)

# Проверьте гипотезу о равенстве среднего веса женщин (мужчин)
# в двух разных выбранных видах спорта
# (сравнение двух независимых выборок – двухвыборочный критерий)  

df2 <- df[which(df[, "Sport"] %in% c("Athletics", "Freestyle Skiing")),]
df2 <- df2[-which(is.na(df2[, "Weight"])),]
df2 <- df2[which(df2[, "Sex"] == "F"),]

x <- df2[1:nrow(df2), "Weight"]

# Квантильно-квантильный график
qqnorm(x)
qqline(x, col = 4, lwd = 2)

# Сравнение двух независимых выборок

# tapply(х, INDEX, FUN = ...)
# применяет функцию FUN к каждой совокупности значений х,
# созданной в соответствии с уровнями определенного фактора;
# перечень факторов указывается при помощи аргумента INDEX

tapply(df2$Weight, df2$Sport, mean)

# Тест на равенство дисперсий 
bartlett.test(df2$Weight ~ df2$Sport, data = df2)

# различаются ли эти средние значения статистически,
# проверим на основе гипотезы об отсутствии разницы при помощи t-теста
t.test(df2$Weight~df2$Sport)
t.test(df2$Weight~df2$Sport, paired=FALSE, var.equal=TRUE)
