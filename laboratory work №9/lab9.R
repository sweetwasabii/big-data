# install.packages("igraph")
# install.packages("network")
# install.packages("sna")
# install.packages("ndtv")

library(igraph)
N <- 11

# �������� ��������� ���� g �� ��������� ������ ������
# G_size (�� N+10 �� (N/10+5)^2+5N)
# �������� ����� ����� � ������ ����� �����
# ��������� ����, �������� ��� ������� ���������

G_size <- sample((N+10):((N %/% 10 + 5) ** 2 + 5 * N), 1)
g <- graph.ring(n = G_size)

ecount(g)
vcount(g)

plot(g, edge.arrow.size = .2, vertex.size = 13)

g[]

# �������� ���� g1 �� ������� ����� � ������ ������ G_size ������� �����
# �������� ��� 8N ��������� �����, �������������� �� ������� ������, 
# �������� ����� ������� ������, ��������� ����
# � �������� ���  ������� ���������
# �������� ����� g1 ��� 10N ��������� �����, 
# �������������� �� ������� ������, 
# �������� ����� ����� ������, ��������� ����
# � �������� ��� ������� ���������.
g1 <- graph.empty() + vertices(1:G_size, color = 'yellow')
g1 <- g1 + edges(sample(V(g1), 2 * 8 * N, replace = TRUE), color = 'red')
plot(g1, edge.arrow.size = .2, vertex.size = 13)
g1[]

g1 <- g1 + edges(sample(V(g1), 2 * 10 * N, replace = TRUE), color = 'blue')
plot(g1, edge.arrow.size = .2, vertex.size = 13)
g1[]

# �������� ����� ����� �������� 2N+23 � 2N+20, 
# 2N+12 � N+15, 2N-1 � N+8, 2N � 2N+1, N+7 � N+13,
# �������� �� � ������ ���� (�������������� ��������� 
# ���������� �� ����� ������� � �������� %in% ���� match,
# ��� �������������� ������ ����� �� ����������)
# ��������� ����
# �������� ������� N-� �������, �����, ����������� ���� �������
# ��������� �� ������� N+10 � N+12? 
# �������� ������� ���������
v <- c(2 * N + 23, 2 * N + 20, 2 * N + 12, N + 15, 2 * N - 1, 
       N + 8, 2 * N, 2 * N + 1, N + 7, N + 13)

for (i in seq(1, length(v), 2)) {
  if (v[i] %in% V(g1) && v[i+1] %in% V(g1)) {
    g1 <- add.edges(g1, c(v[i], v[i + 1]), color = 'black')
  }
}

plot(g1, edge.arrow.size = .2, vertex.size = 13)

neighbors(g1, V(g1)[N], mode = 'out')
incident(g1, V(g1)[N], mode = 'all')

if ((N + 10) %in% V(g1) && (N + 12) %in% V(g1)) {
  are.connected(g1, V(g1)[N + 10], V(g1)[N + 12])
}

g1[]

# ���������� ��������� ���������� ������ ����� 
# (in_circle, as_tree, lattice)
coords <- layout_(g1, in_circle())
plot(g1, layout = coords, edge.arrow.size = .2)

coords <- layout_(g1, as_tree())
plot(g1, layout = coords, edge.arrow.size = .2)

g1 <- graph.lattice(length = 47,dim = 1,nei = 5, circular = FALSE)
plot(g1, vertex.size = 2, vertex.label = NA, 
     layout = layout.kamada.kawai, edge.arrow.size = .1)

# ��������� ��������� �������� ����� g1, 
# �������� ������ ����� �������� ����� ��� ������ �������
# � ������������ �������� ������ �������� �� ��������
diameter(g1)
all_shortest_paths(g1, 1, to = V(g1), mode = 'all', weights = NULL)

deg <- degree(g1, mode = 'all')
plot(g1, edge.arrow.size = .2, vertex.size = deg)

# ������ �2
setwd("C:/Users/diana/Desktop/big-data/laboratory work �9")
df <- read.csv("graph.csv", sep=";", header = FALSE)

n <- df[1, 1]
m <- df[1, 2]

edges <- as.numeric(rbind(df[-1, 1], df[-1, 2]))
g <- graph(edges, n = n, directed = TRUE)

V(g)$color <- rainbow(n)
plot(g)

v < -as.numeric(topo_sort(g))
v

V(g)[v]$label <- 1:n

plot(g)
