Sys.setlocale("LC_CTYPE", "russian")
setwd('C:/Users/diana/Desktop/big-data/laboratory work №2')

df <- read.csv('psyho.csv', sep = ';' , header = TRUE)

# некорректно отображаются пробелы в названии столбцов
names(df) <- c('Студент', 'Дихотомическое мышление', 'Катастрофизация', 'Обесценивание позитивного', 'Навешивание ярлыков', 'Минимизация', 'Чтение мыслей', 'Персонализация', 'Долженствование', 'Я (не) должен', 'Нагнетение')
print(df)

# вычислить max, min, mean по каждому столбцу
main_funcs <- function(x)
{
  x <- na.omit(x)
  return(c(max(x), min(x), mean(x)))
}

sapply(df[-1], main_funcs)

# подсчитать количество людей, отдавших предпочтение >7 и <3 (составить вектор)
get_pref <- function(x)
{
  x <- na.omit(x)
  
  if (length(x[x > 7]) > length(x)/2)
    return(TRUE)
  
  if (length(x[x < 3]) > length(x)/2)
    return(TRUE)
  
  return(FALSE)
}

prefs <- factor(apply(df[-1], MARGIN = 1, FUN = get_pref))

suitable_inds <- which(prefs == TRUE)
suitable_people <- df$Студент[suitable_inds]

print(suitable_people)

# вывести рейтинг фильмов (книг...)  в списке по убыванию
score <- apply(df[-1], MARGIN = 2, FUN = function(x)
  {
    x <- na.omit(x)
    return (sum(x))
  }
)
print(sort(score, decreasing = TRUE))

# sort_df <- df[-1][order(score, decreasing = TRUE)]
# print(names(sort_df))
         
# построить столбчатую диаграмму оценок (можно сделать разными способами)
barplot(score, col = "red",
        names.arg = c('Дих.\nмышление', 'Катастр.', 'Обесц.\nпозитивного',
                      'Навешивание\nярлыков', 'Минимизация', 'Чтение\nмыслей',
                      'Персонал.', 'Долженств.', 'Я\n(не) должен', 'Нагнетение'),
        xlab = "Психологическая защита",
        ylab = "Оценка",
        las = 1)