library(tidyverse)
# All possible strategies
all_strats <- combn(rep(c("D","R"),4),4) %>% t
all_strats <- paste0(all_strats[,1],all_strats[,2],all_strats[,3],all_strats[,4])
all_strats <- unique(all_strats)
all_strats <- expand.grid(all_strats,all_strats)
colnames(all_strats) <- c("p1","p2")
write_csv(all_strats,"all_strategies.csv")
