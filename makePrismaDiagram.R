# Load libraries
library(tidyverse)
library(PRISMA2020)

# Set up
data <- read.csv("PRISMA.csv") %>% PRISMA_data
PRISMA_flowdiagram(data,
                   previous = FALSE,
                   interactive = FALSE,
                   detail_databases = TRUE,
                   detail_registers = FALSE,
                   fontsize = 11)