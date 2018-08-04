library(TSP)
library(tspmeta)
library(readr)
library(tidyverse)

input <- read_delim("D:/R/input.txt", ";", 
                    escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)

colnames(input) <- c("ID", "Lat", "Long", "Name")
selected_id = c("105005", "115551", "107013", "119334", "121293", "117198", "117630", "124498",
                "124617", "125599", "129338")

user_select <- input %>%
  filter(ID %in% selected_id)%>%
  select(Lat:Name)

coords.mx <- as.matrix(user_select[,1:2])

# Compute distance matrix
dist.mx <- dist(coords.mx)

# Construct a TSP object
tsp.ins <- tsp_instance(coords.mx, dist.mx)
tour <- run_solver(tsp.ins, method="2-opt", control = list(clo = "-V"))

#Plot
autoplot(tsp.ins, tour)

distdf <- as.matrix(dist.mx)
colnames(distdf) <- user_select$Name
rownames(distdf) <- user_select$Name














methods <- c("nearest_insertion", "farthest_insertion",
             "cheapest_insertion", "arbitrary_insertion", "nn", "repetitive_nn",
             "2-opt")
tours <- sapply(methods, FUN = function(m) run_solver(tsp.ins, method = m), simplify = FALSE)
dotchart(c(sapply(tours, FUN = attr, "tour_length"), "Concorde, NEOS)" = 3916),
         xlab = "Tour length (n=225)", main="Heuristic Solutions vs. Exact (concorde)")