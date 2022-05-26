library(dplyr)
library(tidyr)
library(ClusterR)

dat = read.table("https://www.dropbox.com/s/w85p6egja4y3jsa/Bin%20Map%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)
resp = read.table("https://www.dropbox.com/s/hhws0dedltkgy9t/Bin%20Map%20wf%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)


grp_df <- dplyr::filter(dat, grepl("Id",BinName)) %>% mutate(Quadrant = case_when(
    diex >= (max(dat$diex) - min(dat$diex))/2 & diey <= (max(dat$diey) - min(dat$diey))/2     ~ "PP",
    diex < (max(dat$diex) - min(dat$diex))/2 & diey <= (max(dat$diey) - min(dat$diey))/2     ~ "PN",
    diex < (max(dat$diex) - min(dat$diex))/2 & diey > (max(dat$diey) - min(dat$diey))/2     ~ "NN",
    diex >= (max(dat$diex) - min(dat$diex))/2 & diey > (max(dat$diey) - min(dat$diey))/2     ~ "NP")) %>% count(wafer, Quadrant, sort = TRUE)


grp_df_2 <- grp_df %>%
  pivot_wider(names_from = Quadrant, 
              values_from = n, 
              values_fill = 0)


df = left_join(grp_df_2,resp[, c("Wafer","yield....")],by = c("wafer" = "Wafer"))





X = df[, -which(names(df) == "yield...." & names(df) == "wafer")]   # data (excluding the response variable)
y = df[, c("yield....")]    # the response variable

dat = center_scale(X, mean_center = T, sd_scale = T)  # centering and scaling the data

dime(dat)

gmm = GMM(dat, 2, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10, em_iter = 10, verbose = F)          
print(gmm)
## predict centroids, covariance matrix and weights
#pr = predict(gmm, newdata = dat)