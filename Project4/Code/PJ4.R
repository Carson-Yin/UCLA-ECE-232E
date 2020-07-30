setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library('igraph')
library('ggplot2')
source('utils.R')

data_dir = '../finance_data'

Name_file <- file.path(data_dir, 'Name_sector.csv')
data_df <- read.csv(Name_file)
num_stock <- nrow(data_df)
data_df['Sector']<-sapply(data_df['Sector'], unclass)
sector_arr <-data_df[,'Sector']
r_list <-list()
for (i in 1:num_stock)
{
  symbol <- data_df[i, 'Symbol']
  file_name <- paste(symbol, '.csv', sep ='')
  file <- file.path(data_dir,'data', file_name)
  stock_df <-read.csv(file)
  close_arr <-stock_df[,'Close']
  len <- length(close_arr)
  r_arr <- log(close_arr[2:len]/close_arr[1:len-1])
  r_list[[i]] <-r_arr
}

A <- adjacency_matrix(r_list)
g <- graph_from_adjacency_matrix(A, mode="upper", weighted = TRUE)
p<-qplot(E(g)$weight, geom="histogram", xlab='Weight', fill=I('blue'), col=I("red"), alpha = I(.2))
plot(p)

g_mst <- mst(g)
colbar <-rainbow(max(sector_arr))
V(g_mst)$color <- colbar[sector_arr]
p1<-plot(g_mst, layout = layout_as_tree, vertex.color=V(g_mst)$color, vertex.size=6, vertex.label=NA)
V(g_mst)$sector <- sector_arr

alpha <-0
for (node in V(g_mst))
{
  s <- V(g_mst)$sector[node]
  nn <- neighbors(g_mst, node)
  neighbor_s <- V(g_mst)$sector[nn]
  len <- length(neighbor_s)
  cardinality <- length(which(s == neighbor_s))
  alpha <- alpha + cardinality/len
}
alpha <- alpha/num_stock
print (alpha)

cardinality_S <- rep(0, max(sector_arr))
for (i in 1:length(sector_arr))
{
  s <- sector_arr[i]
  cardinality_S[s]<- cardinality_S[s]+1
}
alpha<-0
for (i in 1: length(cardinality_S))
{
  alpha <-cardinality_S[i]**2/(num_stock**2)
}
print (alpha)

#Question 5
symbol <- data_df[1, 'Symbol']
file_name <- paste(symbol, '.csv', sep ='')
file <- file.path(data_dir,'data', file_name)
stock_df <-read.csv(file)
close_arr <-stock_df[,'Close']
date_arr <-stock_df[,'Date']
date_arr <- as.Date(date_arr)
weekday_arr <- weekdays(date_arr)
isMonday <-rep(0, length(weekday_arr))
for (i in 1:length(weekday_arr))
{
  w <-weekday_arr[i]
  if (identical(w, 'Monday'))
  {isMonday[i]=1}
}
r_list <-list()
for (i in 1:num_stock)
{
  symbol <- data_df[i, 'Symbol']
  file_name <- paste(symbol, '.csv', sep ='')
  file <- file.path(data_dir,'data', file_name)
  stock_df <-read.csv(file)
  close_arr <-stock_df[,'Close']
  Monday_arr <- tail(isMonday, length(close_arr))
  close_arr <- close_arr[which(1==Monday_arr)]
  len <- length(close_arr)
  r_arr <- log(close_arr[2:len]/close_arr[1:len-1])
  r_list[[i]] <-r_arr
}
A <- adjacency_matrix(r_list)
g <- graph_from_adjacency_matrix(A, mode="upper", weighted = TRUE)
g_mst <- mst(g)
colbar <-rainbow(max(sector_arr))
V(g_mst)$color <- colbar[sector_arr]
p1<-plot(g_mst, layout = layout_as_tree, vertex.color=V(g_mst)$color, vertex.size=6, vertex.label=NA)
V(g_mst)$sector <- sector_arr

alpha <-0
for (node in V(g_mst))
{
  s <- V(g_mst)$sector[node]
  nn <- neighbors(g_mst, node)
  neighbor_s <- V(g_mst)$sector[nn]
  len <- length(neighbor_s)
  cardinality <- length(which(s == neighbor_s))
  alpha <- alpha + cardinality/len
}
alpha <- alpha/num_stock
print (alpha)
