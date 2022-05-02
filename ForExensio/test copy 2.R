library(dplyr)
library(tidyr)
#library(party)

dat = read.table("https://www.dropbox.com/s/w85p6egja4y3jsa/Bin%20Map%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)
datfil <- dplyr::filter(dat, grepl("Id",BinName))


depr_df <- datfil %>% 
  mutate(Quadrant = case_when(
    diex >= (max(dat$diex) - min(dat$diex))/2 & diey <= (max(dat$diey) - min(dat$diey))/2     ~ "PP",
    diex < (max(dat$diex) - min(dat$diex))/2 & diey <= (max(dat$diey) - min(dat$diey))/2     ~ "PN",
    diex < (max(dat$diex) - min(dat$diex))/2 & diey > (max(dat$diey) - min(dat$diey))/2     ~ "NN",
    diex >= (max(dat$diex) - min(dat$diex))/2 & diey > (max(dat$diey) - min(dat$diey))/2     ~ "NP"
    ))


View(depr_df)

grp_df <- depr_df %>% count(wafer, Quadrant, sort = TRUE)

View(grp_df)




grp_df_2 <- grp_df %>%
  pivot_wider(names_from = Quadrant, 
              values_from = n, 
              values_fill = 0)

View(grp_df_2)
#This is for decision tree - maybe I would like to have categorization based on different algorithm
## Give the chart file a name.
#png(file = "decision_tree.png")
#
## Create the tree.
#  output.tree <- ctree(
#  nativeSpeaker ~ age + shoeSize + score, 
#  data = input.dat)
#
## Plot the tree.
#plot(output.tree)
#
## Save the file.
#dev.off()