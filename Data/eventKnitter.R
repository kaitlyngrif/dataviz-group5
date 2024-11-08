library(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y))

year = 1415
dataFile = "C:/Users/admin/Desktop/VIZA 676/dataviz-group5/Data/20142015"
setwd(dataFile)

my.files <- list.files(pattern = "*csv", recursive = TRUE)
all_files <- read.csv(my.files[1])

for(q in 2:length(my.files)) {
  x <- read.csv(my.files[q])
  all_files <- bind_rows(all_files, x)
  print(paste(q, ' ', my.files[q]))
}

events = all_files %>%
  select(-X) %>%
  distinct(gameID, sortOrder, .keep_all = TRUE) %>%
  group_by(gameID) %>%
  mutate(sparse_rate = (sum(typeDescKey %in% c('goal', 'penalty'))/n()),
         bugCheck = ifelse(sparse_rate > .4, 1, 0)) %>%
  ungroup() %>%
  select(-sparse_rate)


filename = paste0("roughEvents_", year, ".rds")
write_rds(events, file = filename)
