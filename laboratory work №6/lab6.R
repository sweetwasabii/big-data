# Выполнить иерархическую кластеризацию Вашего набора данных,
# построив дендрограмму. Подробно обосновать Ваш выбор числа групп

Sys.setlocale("LC_CTYPE", "russian")
setwd('C:/Users/diana/Desktop/big-data/laboratory work №6')

cacao <- read.csv('flavors_of_cacao.csv', sep = ',' , header = TRUE)
names(cacao) <- c("Company (Maker-if known)",
                  "Specific Bean Origin or Bar Name", "REF",
                  "ReviewDate","CocoaPercent","CompanyLocation",
                  "Rating","BeanType","Broad BeanOrigin")

# Шаг 1
# Сначала проведите дескриптивный анализ и
# анализ распределения независимых параметров

# Шаг 2.1 - Чтение данных, отбросить столбец, классифицирующий какао

labels_cacao <- cacao$CompanyLocation
cacao_buf <- cacao[, c(5, 7, 6)]
cacao_c <- cacao[, c(5, 7)]
cacao_c[, 1] <- as.numeric(gsub("%", "", cacao_c[, 1]))

# Шаг 2.2 - Удаление пропущенных значений
# В данной задаче пропущенных значений нет
# Удалять нечего

# Шаг 2.3 - Стандартизация переменных
# В данной задаче переменные существенно различны
# Стандартизировать надо

maxs <- apply(cacao_c, 2, max)
mins <- apply(cacao_c, 2, min)
cacao_c <- scale(cacao_c, center = mins, scale = maxs - mins)

# Шаг 2.3
# Создаем матрицу попарных расстояний
# (по умолчанию - Евклидово расстояние)
dist_cacao <- dist(cacao_c)

# Шаг 2.4
# Проводим кластерный анализ,
# результаты записываем в список clust.iris
# hclust ожидает матрицу расстояния, а не исходные данные
clust_cacao <- hclust(dist_cacao, "ward.D")

# Построение дендрограммы
plot(clust_cacao)

plot(clust_cacao, labels_cacao, cex = 0.5)
rect.hclust(clust_cacao, k = 3, border="red")

# Шаг 2.6 - Разделим выборку на 3 кластера
# Вектор groups содержит номер кластера,
# в который попал классифицируемый объект 
groups <- cutree(clust_cacao, k = 3) 

# Для каждой группы определяем средние значения характеристик и строим датафрейм

#  в 1-ом кластере
g1 <- colMeans(cacao_c[groups == 1, 1:2])
#  во 2-ом кластере
g2 <- colMeans(cacao_c[groups == 2, 1:2])
#  в 3-ем кластере
g3 <- colMeans(cacao_c[groups == 3, 1:2])
#  в 4-ом кластере
# g4 <- colMeans(cacao_c[groups == 4, 1:2])
#  в 5-ом кластере
# g5 <- colMeans(cacao_c[groups == 5, 1:2])
#  в 6-ом кластере
# g6 <- colMeans(cacao_c[groups == 6, 1:2])

df <- data.frame(g1, g2, g3) #, g4, g5, g6)

df1 <- t(df)
df <- t(df1)

# построим график 
barplot(df, col=c("red","green"))

barplot(df, ylim = c(0, 1.2),  
        main = "Groups of cacao", axes = FALSE, col=c("red","green"), 
        beside = TRUE)

axis(2, at = 0:5, labels = 0:5)
legend("top", legend = rownames(df), col=c("red","green"), lwd=10, bty = "n")

# Построить диаграмму "Каменная осыпь"
plot(1:1794, clust_cacao$height, type = 'b')

# диаграмма рессеяния

library(lattice)

Group <- groups
my_data <- cbind(cacao_c, Group)
my_data <- as.data.frame(my_data)
my_data$Group <- as.factor(my_data$Group)
head(my_data)

xyplot(Rating ~ CocoaPercent, data = my_data)

xyplot(Rating ~ CocoaPercent, group = my_data$Group,
       data = my_data, auto.key = TRUE)

boxplot(Rating ~ Group, data = my_data, frame = FALSE, col="green")

# *** 6.2 ***

library(klaR)

# my_data$Rating <- as.factor(my_data$Rating)

naive_cacao <- NaiveBayes(my_data$Group ~ ., data = my_data)
naive_cacao$tables
naive_cacao

# ядерные функции плотности условной вероятности для таблицы

opar <- par()
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
plot(naive_cacao, lwd = 2, legendplot = TRUE)
par <- opar

# удалим столбец таблицы, содержащий классификацию,
# и выполним классификацию по "новым данным

pred <- predict(naive_cacao, my_data[, -3])$class
table(Факт = my_data$Group, Прогноз = pred)

# определение точности
acc <- mean(pred == my_data$Group)
acc 
paste("Точность = ", round(100 * acc, 2), "%", sep="")

# классификация Decision Tree

# подготовка данных
set.seed(1234)
ind<-sample(2, nrow(my_data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- my_data[ind==1,]
testData <- my_data[ind==2,]
nrow(trainData)
nrow(testData)
nrow(my_data)

# построение модели
library(party)
myFormula <- Group ~ CocoaPercent + Rating
data_ctree <- ctree(myFormula, data = trainData)

# обучение модели
table(predict(data_ctree), trainData$Group)

# визуализация
plot(data_ctree)

# применение модели
test_predicted<-predict(data_ctree, newdata=testData)
table(test_predicted, testData$Group)

# алгоритм Random Forest

# построение модели
library(randomForest)

rf <- randomForest(Group~.,data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$Group)

# информация о модели
print(rf)

# альтернативная реализация
library(party)
cf <- cforest(Group~., data=trainData, control=cforest_unbiased(mtry=2,ntree=100))
table(predict(cf), trainData$Group)
