Sys.setlocale("LC_CTYPE", "russian")
setwd('C:/Users/diana/Desktop/big-data/laboratory work �2')

df <- read.csv('psyho.csv', sep = ';' , header = TRUE)

# ����������� ������������ ������� � �������� ��������
names(df) <- c('�������', '�������������� ��������', '���������������', '������������� �����������', '����������� �������', '�����������', '������ ������', '��������������', '���������������', '� (��) ������', '����������')
print(df)

# ��������� max, min, mean �� ������� �������
main_funcs <- function(x)
{
  x <- na.omit(x)
  return(c(max(x), min(x), mean(x)))
}

sapply(df[-1], main_funcs)

# ���������� ���������� �����, �������� ������������ >7 � <3 (��������� ������)
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
suitable_people <- df$�������[suitable_inds]

print(suitable_people)

# ������� ������� ������� (����...)  � ������ �� ��������
score <- apply(df[-1], MARGIN = 2, FUN = function(x)
  {
    x <- na.omit(x)
    return (sum(x))
  }
)
print(sort(score, decreasing = TRUE))

# sort_df <- df[-1][order(score, decreasing = TRUE)]
# print(names(sort_df))
         
# ��������� ���������� ��������� ������ (����� ������� ������� ���������)
barplot(score, col = "red",
        names.arg = c('���.\n��������', '�������.', '�����.\n�����������',
                      '�����������\n�������', '�����������', '������\n������',
                      '��������.', '���������.', '�\n(��) ������', '����������'),
        xlab = "��������������� ������",
        ylab = "������",
        las = 1)