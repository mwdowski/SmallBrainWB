#wykres
data <- read.csv("results.csv", sep=";")

library(tidyverse)
library(ggplot2)


rownames(data)<-data[,1]

data 
data2 <- data[1:16,2:6]


data2 <- data2 %>%
    rownames_to_column() %>%
    gather(colname, value, -rowname)



data2$rowname <- factor(data2$rowname, levels=sort(unique(data2$rowname), decreasing = TRUE))
data2$colname <- factor(data2$colname, levels = c( "FULLY", "MOSTLY", "CHANGE", "CH.SD", "NOREP"))

ggplot(data2, aes(x = rowname, y = colname, fill = value)) +
    geom_tile() + coord_flip() + scale_fill_gradient(low='white', high='black') + 
    xlab("Package") + ylab("Category")
    

data3 <- data[17,2:6]
data3 <- t(data3)
data3 <- data.frame(data3, rownames(data3))

data3[,2] <- rownames(data3)
data3$rownames.data3. <- factor(data3$rownames.data3., levels = c( "FULLY", "MOSTLY", "CHANGE", "CH.SD", "NOREP"))

ggplot(data3, aes(x=rownames.data3., y=SUM, fill=SUM)) +
    geom_bar(stat="identity") + scale_fill_gradient(low='gray', high='black') + theme_light() +
    xlab("Category") + ylab("Summarized values") + theme(legend.position = "none")
