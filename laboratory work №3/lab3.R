# выполнить учебный импорт любых таблиц данных из csv-файла и xls-таблицы.
Sys.setlocale("LC_CTYPE", "russian")
setwd('C:/Users/diana/Desktop/big-data/laboratory work №3')

df <- read.csv('psyho.csv', sep = ';' , header = TRUE)
names(df) <- c('Студент', 'Дих.\nмышление', 'Катастр.', 'Обесц.\nпозитивного',
                'Навешивание\nярлыков', 'Минимизация', 'Чтение\nмыслей',
                'Персонал.', 'Долженств.', 'Я\n(не) должен', 'Нагнетение')

# выполнить дескриптивный анализ данных из ЛР №2
# гистограмма, боксплот, серединные меры

# NA - пропускаем
df_na_pass = sapply(df[-1], FUN = function(x)
  {
    x <- na.omit(x)
    return(mean(x))
  }
)

hist(df_na_pass, xlab = 'Когнитивные искажения', ylab = 'Оценки',
     main = "Средняя оценка", col = rainbow(10))

barplot(df_na_pass, xlab = 'Когнитивные искажения', ylab = 'Оценки',
        main = 'Средняя оценка', col = rainbow(10))

boxplot(df[-1], xlab = 'Когнитивные искажения', ylab = 'Оценки',
        main = 'Количество предпочтений', col = rainbow(10))

# NA - среднее
df_na_average <- sapply(df[-1], FUN = function(x)
  {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    return(x)
  }
)

boxplot(df_na_average, xlab = 'Когнитивные искажения', ylab = 'Оценки',
        main = 'Количество предпочтений', col = rainbow(10))

# NA - ноль
df_na_zero = sapply(df[-1], FUN = function(x)
  {
    x[is.na(x)] <- 0
    return(x)
  }
)

boxplot(df_na_zero, xlab = 'Когнитивные искажения', ylab = 'Оценки',
        main = 'Количество предпочтений', col = rainbow(10))

summary(df)

# выполнить сортировку наборов данных по выбранному признаку
barplot(sort(df_na_pass, decreasing = FALSE), xlab = 'Когнитивные искажения',
        ylab = 'Оценки', main = 'Средняя оценка', col = rainbow(10))

# cформировать отдельные наборы данных по одинаковому признаку
# вывести результат,  выполнить подсчет размерностей новых таблиц,
# снова выполнить их анализ
df_data <- df
df_data[df_data > 7] <- 0

boxplot(df_data[-1], xlab = 'Когнитивные искажения', ylab = 'Оценки',
        main = 'Количество предпочтений', col = rainbow(10))