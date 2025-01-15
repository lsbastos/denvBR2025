# Preparing data
pacman::p_load(tidyverse)

dir.path <- "~/Data/Sinan/Dengue/"
files <- dir(path = dir.path)
denv.list <- list()

for(k in 1:10){
  aux <- read_csv(file = paste0(dir.path, files[k]))
  denv.list[[k]] <- aux |> select(ID_MN_RESI, SG_UF, DT_NOTIFIC, 
                                DT_SIN_PRI, SOROTIPO)
}

denvbr <- denv.list |> list_rbind() 

denvbr |> write_csv("data/denvbr20152024.csv.gz")
