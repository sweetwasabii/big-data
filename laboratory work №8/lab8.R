setwd("C:/Users/diana/Desktop/big-data/laboratory work �8")
df<-read.csv("Lab8_Data.csv", sep = ",", header = TRUE)

china <- df[which(df[,"Country.Name"] == "China"),]

# ����� ��� � �������� ���������
GDP <- china[which(china[, "Series.Name"] %in% 
                     c("GDP growth (annual %)", 
                       "Population growth (annual %)")), 5:34]
x <- 1989 : 2017
y <- as.numeric(GDP[1, 1:29])

plot(x, y, ylim = c(min(y), max(y)), type = "b", 
     lty = 1, pch = 1, 
     xlab = "����", ylab = "��������", main = "���")

x1 <- as.numeric(GDP[1, 1 : 29])
x2 <- as.numeric(GDP[2, 1 : 29])

cor(x1, x2, method="spearman")

# �������� ��������� �� �������� �����������
# ���-�� �� ���
population <- china[
  which(china[, "Series.Name"] %in% 
          c("Population growth (annual %)", 
            "Unemployment with advanced education (% of total labor force with advanced education)")),
  5:34]

x1<-as.numeric(population[1, 1:29])
x2<-as.numeric(population[2, 1:29])

cor(x1, x2, method="spearman")

# ��������� �������� �� �������� � ����������
# ����������������� ����� � ����������

health <- china[
  which(china[,"Series.Name"] %in% 
          c("Domestic general government health expenditure per capita (current US$)", 
            "Life expectancy at birth, total (years)", 
            "Death rate, crude (per 1,000 people)")), 5:34]

x1 <- as.numeric(health[1, 12:28])
x2 <- as.numeric(health[2, 12:28])
x3 <- as.numeric(health[3, 12:28])

cor(x1, x2, method = "spearman")
cor(x1, x3, method = "spearman")

# ������� ����� � ������ ������������ �� ���� �������� �������
# � �� ������� �������������������� ������������.
# ���-�� �� ���
educational <- china[
  which(china[,"Series.Name"] %in% 
          c("Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)", 
            "Exports of goods and services (annual % growth)", 
            "Medium and high-tech Industry (including construction) (% manufacturing value added)")), 
  5:34]

x1 <- as.numeric(educational[1, 12:28])
x2 <- as.numeric(educational[2, 12:28])
x3 <- as.numeric(educational[3, 12:28])

cor(x1, x2, method = "spearman")
cor(x1, x3, method = "spearman")

# �������� �� ����������� �� �
# ������������ ������� ��������������� ������
# ���-�� �� ���
educational_female <- china[
  which(china[, "Series.Name"] %in% 
          c("Government expenditure on education, total (% of GDP)", 
            "Educational attainment, at least Bachelor's or equivalent, population 25+, female (%) (cumulative)")), 
  5:34]

x1 <- as.numeric(educational[1, 12:28])
x2 <- as.numeric(educational[2, 12:28])

cor(x1, x2, method = "spearman")

# ������� ����� � ������ ������������ �� ��������
# ������� ���������� (������� ������ � ������� ��������)
# ���-�� �� ���
scientific <- china[
  which(china[, "Series.Name"] %in% 
          c("Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative)", 
            "Scientific and technical journal articles")), 5:34]

x1 < -as.numeric(educational[1, 12:28])
x2 <- as.numeric(educational[2, 12:28])
cor(x1, x2, method = "spearman")

# � ������� �������������� ������� ������� ���������
# ���������� � �������� ������� �� ��� ����������� ����������

res <- as.data.frame(t(china[, 5:33]))
colnames(res) <- china$Series.Name

res <- res[which(
  res$`Domestic general government health expenditure per capita (current US$)` > 1),]

res$`Domestic general government health expenditure per capita (current US$)` <- 
  as.numeric(res$`Domestic general government health expenditure per capita (current US$)`)

res$`Life expectancy at birth, total (years)` <- 
  as.numeric(res$`Life expectancy at birth, total (years)`)

fit <- lm(`Life expectancy at birth, total (years)` ~ 
            `Domestic general government health expenditure per capita (current US$)`, 
          data = res)
fit

# � ������� ������� predict() (��. ������ � help())
# ��������� ������� �� ������ �������������� ��� ��������

pred <- predict(fit, res)
pred

par(mfrow = c(1, 2))
plot(res$`Domestic general government health expenditure per capita (current US$)`, 
     res$`Life expectancy at birth, total (years)`, 
     xaxt="n", xlab="������� �� ��������", 
     ylab="����������������� �����", col="blue", 
     main="������ �������� �������� ����������������� �����")
abline(fit, col = "red")

plot(pred, xaxt = "n", 
     xlab = "������� �� ��������", 
     ylab="����������������� �����", col="blue", 
     main="������ �������������� �������� ����������������� �����")
