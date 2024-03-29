Sys.setlocale("LC_CTYPE", "russian")
setwd('C:/Users/diana/Desktop/big-data/laboratory work �7')

df <- read.csv('athlete_events.csv', sep = ',' , header = TRUE)

# ��������� �������� � ������� ���� ����������� 
# ���������� ���� ������ (��� ������ �������� �� ��4)

df1 <- df[which(df[, "Sport"] == "Basketball"),]
df1 <- df1[-which(is.na(df1[, "Weight"])),]

x <- df1[1:nrow(df1), "Weight"]

# �������� �� ������������ �������������.
# ���� ������-������ (Shapiro-Wilk test)
shapiro.test(x)

# ����������� ������
# ����������� � ������ ���������
x2 <- seq(min(x), max(x), length = length(x))
fun <- dnorm(x2, mean = mean(x), sd = sd(x))
hist(x, freq = FALSE, col = "gray")
lines(x2, fun, col = 2, lwd = 2)

# ����������-����������� ������
qqnorm(x)
qqline(x, col = 4, lwd = 2)

# ���������� �������������� �����

# ���� ���������
t.test(x, mu=mean(x), conf.int = TRUE)

# ���� ����������
wilcox.test(x, mu = mean(x), conf.int = TRUE)

# ��������� �������� � ��������� �������� ���� ������ (������)
# � ���� ������ ��������� ����� ������
# (��������� ���� ����������� ������� � �������������� ��������)  

df2 <- df[which(df[, "Sport"] %in% c("Athletics", "Freestyle Skiing")),]
df2 <- df2[-which(is.na(df2[, "Weight"])),]
df2 <- df2[which(df2[, "Sex"] == "F"),]

x <- df2[1:nrow(df2), "Weight"]

# ����������-����������� ������
qqnorm(x)
qqline(x, col = 4, lwd = 2)

# ��������� ���� ����������� �������

# tapply(�, INDEX, FUN = ...)
# ��������� ������� FUN � ������ ������������ �������� �,
# ��������� � ������������ � �������� ������������� �������;
# �������� �������� ����������� ��� ������ ��������� INDEX

tapply(df2$Weight, df2$Sport, mean)

# ���� �� ��������� ��������� 
bartlett.test(df2$Weight ~ df2$Sport, data = df2)

# ����������� �� ��� ������� �������� �������������,
# �������� �� ������ �������� �� ���������� ������� ��� ������ t-�����
t.test(df2$Weight~df2$Sport)
t.test(df2$Weight~df2$Sport, paired=FALSE, var.equal=TRUE)
