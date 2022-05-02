library(dplyr)
library(tidyr)
library(ClusterR)

dat = read.table("https://www.dropbox.com/s/w85p6egja4y3jsa/Bin%20Map%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)
resp = read.table("https://www.dropbox.com/s/hhws0dedltkgy9t/Bin%20Map%20wf%20-%20leader.txt?dl=1", header=TRUE,  sep = '\t', skipNul=TRUE)

depr_df <- dplyr::filter(dat, grepl("Id",BinName)) %>% 
  mutate(Quadrant = case_when(
    diex >= (max(dat$diex) - min(dat$diex))/2 & diey <= (max(dat$diey) - min(dat$diey))/2     ~ "PP",
    diex < (max(dat$diex) - min(dat$diex))/2 & diey <= (max(dat$diey) - min(dat$diey))/2     ~ "PN",
    diex < (max(dat$diex) - min(dat$diex))/2 & diey > (max(dat$diey) - min(dat$diey))/2     ~ "NN",
    diex >= (max(dat$diex) - min(dat$diex))/2 & diey > (max(dat$diey) - min(dat$diey))/2     ~ "NP"
    ))


grp_df <- depr_df %>% count(wafer, Quadrant, sort = TRUE)


grp_df_2 <- grp_df %>%
  pivot_wider(names_from = Quadrant, 
              values_from = n, 
              values_fill = 0)


View(grp_df_2)
View(resp)
#df = merge(x=grp_df_2,y=resp,by="Wafer")


#
#X = dietary_survey_IBS[, -ncol(dietary_survey_IBS)]   # data (excluding the response variable)
#
#y = dietary_survey_IBS[, ncol(dietary_survey_IBS)]    # the response variable
#
#dat = center_scale(X, mean_center = T, sd_scale = T)  # centering and scaling the data
#gmm = GMM(dat, 2, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10,
#          em_iter = 10, verbose = F)          
#
## predict centroids, covariance matrix and weights
#pr = predict(gmm, newdata = dat)