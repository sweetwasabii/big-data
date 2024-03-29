# ��������� ������������� ������������� ������ ������ ������,
# �������� ������������. �������� ���������� ��� ����� ����� �����

Sys.setlocale("LC_CTYPE", "russian")
setwd('C:/Users/diana/Desktop/big-data/laboratory work �6')

cacao <- read.csv('flavors_of_cacao.csv', sep = ',' , header = TRUE)
names(cacao) <- c("Company (Maker-if known)",
                  "Specific Bean Origin or Bar Name", "REF",
                  "ReviewDate","CocoaPercent","CompanyLocation",
                  "Rating","BeanType","Broad BeanOrigin")

# ��� 1
# ������� ��������� ������������� ������ �
# ������ ������������� ����������� ����������

# ��� 2.1 - ������ ������, ��������� �������, ���������������� �����

labels_cacao <- cacao$CompanyLocation
cacao_buf <- cacao[, c(5, 7, 6)]
cacao_c <- cacao[, c(5, 7)]
cacao_c[, 1] <- as.numeric(gsub("%", "", cacao_c[, 1]))

# ��� 2.2 - �������� ����������� ��������
# � ������ ������ ����������� �������� ���
# ������� ������

# ��� 2.3 - �������������� ����������
# � ������ ������ ���������� ����������� ��������
# ����������������� ����

maxs <- apply(cacao_c, 2, max)
mins <- apply(cacao_c, 2, min)
cacao_c <- scale(cacao_c, center = mins, scale = maxs - mins)

# ��� 2.3
# ������� ������� �������� ����������
# (�� ��������� - ��������� ����������)
dist_cacao <- dist(cacao_c)

# ��� 2.4
# �������� ���������� ������,
# ���������� ���������� � ������ clust.iris
# hclust ������� ������� ����������, � �� �������� ������
clust_cacao <- hclust(dist_cacao, "ward.D")

# ���������� ������������
plot(clust_cacao)

plot(clust_cacao, labels_cacao, cex = 0.5)
rect.hclust(clust_cacao, k = 3, border="red")

# ��� 2.6 - �������� ������� �� 3 ��������
# ������ groups �������� ����� ��������,
# � ������� ����� ���������������� ������ 
groups <- cutree(clust_cacao, k = 3) 

# ��� ������ ������ ���������� ������� �������� ������������� � ������ ���������

#  � 1-�� ��������
g1 <- colMeans(cacao_c[groups == 1, 1:2])
#  �� 2-�� ��������
g2 <- colMeans(cacao_c[groups == 2, 1:2])
#  � 3-�� ��������
g3 <- colMeans(cacao_c[groups == 3, 1:2])
#  � 4-�� ��������
# g4 <- colMeans(cacao_c[groups == 4, 1:2])
#  � 5-�� ��������
# g5 <- colMeans(cacao_c[groups == 5, 1:2])
#  � 6-�� ��������
# g6 <- colMeans(cacao_c[groups == 6, 1:2])

df <- data.frame(g1, g2, g3) #, g4, g5, g6)

df1 <- t(df)
df <- t(df1)

# �������� ������ 
barplot(df, col=c("red","green"))

barplot(df, ylim = c(0, 1.2),  
        main = "Groups of cacao", axes = FALSE, col=c("red","green"), 
        beside = TRUE)

axis(2, at = 0:5, labels = 0:5)
legend("top", legend = rownames(df), col=c("red","green"), lwd=10, bty = "n")

# ��������� ��������� "�������� �����"
plot(1:1794, clust_cacao$height, type = 'b')

# ��������� ���������

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

# ������� ������� ��������� �������� ����������� ��� �������

opar <- par()
layout(matrix(c(1,2), 1, 2, byrow = TRUE))
plot(naive_cacao, lwd = 2, legendplot = TRUE)
par <- opar

# ������ ������� �������, ���������� �������������,
# � �������� ������������� �� "����� ������

pred <- predict(naive_cacao, my_data[, -3])$class
table(���� = my_data$Group, ������� = pred)

# ����������� ��������
acc <- mean(pred == my_data$Group)
acc 
paste("�������� = ", round(100 * acc, 2), "%", sep="")

# ������������� Decision Tree

# ���������� ������
set.seed(1234)
ind<-sample(2, nrow(my_data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- my_data[ind==1,]
testData <- my_data[ind==2,]
nrow(trainData)
nrow(testData)
nrow(my_data)

# ���������� ������
library(party)
myFormula <- Group ~ CocoaPercent + Rating
data_ctree <- ctree(myFormula, data = trainData)

# �������� ������
table(predict(data_ctree), trainData$Group)

# ������������
plot(data_ctree)

# ���������� ������
test_predicted<-predict(data_ctree, newdata=testData)
table(test_predicted, testData$Group)

# �������� Random Forest

# ���������� ������
library(randomForest)

rf <- randomForest(Group~.,data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$Group)

# ���������� � ������
print(rf)

# �������������� ����������
library(party)
cf <- cforest(Group~., data=trainData, control=cforest_unbiased(mtry=2,ntree=100))
table(predict(cf), trainData$Group)
