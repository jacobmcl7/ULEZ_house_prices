# this R script does the synthetic control near-far analysis

# see the stata do-file of the same name to get a full description of the structure and methods of each

########################################
# DO PRELIMINARY PREPARATION
########################################
# set directory 
setwd("C:/Users/jpmcl/OneDrive/Documents/Economics/Papers/ULEZ on house prices/Data")

# load in packages:
library(tidyverse)
library(ggplot2)
library(readxl)
library(openxlsx)
########################################



########################################
# MODIFIED K-MEANS
########################################

set.seed(12345)

# load in the cross-sectional data
dataxsec <- as.data.frame(read_excel("Temp/pp_ready_collapsed_xsec.xlsx"))

# first generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
dataxsec <- dataxsec %>%
    mutate(pcsect = paste(pcsect_original, in_ULEZ))

# remove all observations with missings
dataxsec <- na.omit(dataxsec)


# we now do the matching separately for 2021 and 2023

#### first for 2021

# filter the data to keep only pcsects in the expansion zones at least 5km from the inner border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
dataxsec21 <- dataxsec %>%
    filter((in_ULEZ == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50)

# filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
dataxsec21 <- dataxsec21 %>%
    filter(!grepl("^BS\\d{1,2}", pcsect) & !grepl("^B\\d{1,2}", pcsect) & !grepl("^OX\\d{1,2}", pcsect))

# assign a cluster variable at random, then pre-define clusters for those in the 2021 expansion zone
k <- 15
dataxsec21 <- dataxsec21 %>%
    mutate(cluster = sample(1:k, nrow(dataxsec21), replace = TRUE)) %>%
    mutate(cluster = ifelse(in_ULEZ == "In 2021 ULEZ", 1, cluster))

# now do the k-means clustering algorithm manually, using the covariates
covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold", "pcsect_imd", "pcsect_pop_density", "pcdist_share_flat", "pcdist_share_detached", "pcdist_share_semi", "pcdist_share_new", "pcdist_share_leasehold", "pcdist_imd", "pcdist_pop_density")

# extract a new dataset with the covariates and cluster information
cluster_data <- dataxsec21 %>%
    select(pcsect, cluster, in_ULEZ, all_of(covariates))

# scale the covariates
cluster_data[, covariates] <- scale(cluster_data[, covariates])

# count how many postcode sectors outside London
sum(cluster_data$in_ULEZ == "Not in ULEZ")

# count how many postcode sectors in the 2021 analysis zone
sum(cluster_data$in_ULEZ != "Not in ULEZ")

# now do the k-means clustering

# print pre-clustering summary of cluster sizes
table(cluster_data$cluster)

# now run the modified k-means clustering algorithm
# we repeat the algorithm until the percentage of cluster assignments that change is less than 1%, or we reach a maximum number of iterations (100)

# save existing cluster assignments (so we can calculate percentage changed)
prev_clusters <- cluster_data$cluster

# set the max number of iterations, and initialize the iteration counter
max_iter <- 100
i <- 1

# begin the loop
repeat {
    # Compute cluster means for each covariate and cluster
    cluster_means <- cluster_data %>%
        group_by(cluster) %>%
        summarise(across(all_of(covariates), \(x) mean(x, na.rm = FALSE))) %>%
        right_join(data.frame(cluster = 1:k), by = "cluster") %>%
        arrange(cluster) %>%
        select(-cluster) %>%
        as.matrix()

    # Only update clusters for "Not in ULEZ"
    idx <- which(cluster_data$in_ULEZ == "Not in ULEZ")
    obs_mat <- as.matrix(cluster_data[idx, covariates, drop = FALSE])
    dists <- as.matrix(dist(rbind(cluster_means, obs_mat)))[-(1:k), 1:k, drop = FALSE]
    dists[is.na(dists)] <- Inf
    new_clusters <- cluster_data$cluster
    new_clusters[idx] <- max.col(-dists)

    # Calculate percent changed
    pct_changed <- mean(prev_clusters != new_clusters) * 100

    print(paste("Iteration", i, "done. Percent changed:", round(pct_changed, 2)))
    print(table(new_clusters))

    # Stop if less than 1% changed or max_iter reached
    if (pct_changed < 1 | i >= max_iter) {
        cluster_data$cluster <- new_clusters
        break
    }

    # Update for next iteration
    cluster_data$cluster <- new_clusters
    prev_clusters <- new_clusters
    i <- i + 1
}

# now get the cluster variable into the main data
dataxsec21$cluster <- cluster_data$cluster


# get the postcodes in cluster 1 (i.e. the ones with the 2021 zone)
# first export one file for visualisation
export21 <- dataxsec21 %>%
    filter(cluster == 1) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub(" In 2021 ULEZ", "", pcsect)) %>%
    mutate(tag = 1) %>%
    write.xlsx("Temp/matched_pcsects_2021_kmeans_vis.xlsx", rowNames = FALSE)

# now export one for further analysis in stata
export21 <- dataxsec21 %>%
    filter(cluster == 1) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub("In 2021 ULEZ", "2021", pcsect)) %>%
    write.xlsx("Temp/matched_pcsects_2021_kmeans.xlsx", rowNames = FALSE)




#### now for 2023 (identical process)

# filter the data to keep only pcsects in the expansion zones at least 5km from the inner border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
dataxsec23 <- dataxsec %>%
    filter((in_ULEZ == "In 2023 ULEZ" & pcsect_dist_from_2021_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50)

# filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
dataxsec23 <- dataxsec23 %>%
    filter(!grepl("^BS\\d{1,2}", pcsect) & !grepl("^B\\d{1,2}", pcsect) & !grepl("^OX\\d{1,2}", pcsect))

# assign a cluster variable at random, then pre-define clusters for those in the expansion zones
k <- 15
dataxsec23 <- dataxsec23 %>%
    mutate(cluster = sample(1:k, nrow(dataxsec23), replace = TRUE)) %>%
    mutate(cluster = ifelse(in_ULEZ == "In 2023 ULEZ", 1, cluster))

# now do the k-means clustering algorithm manually, using the covariates
covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold", "pcsect_imd", "pcsect_pop_density", "pcdist_share_flat", "pcdist_share_detached", "pcdist_share_semi", "pcdist_share_new", "pcdist_share_leasehold", "pcdist_imd", "pcdist_pop_density")

# extract a new dataset with the covariates and cluster information
cluster_data <- dataxsec23 %>%
    select(pcsect, cluster, in_ULEZ, all_of(covariates))

# scale the covariates
cluster_data[, covariates] <- scale(cluster_data[, covariates])

# count how many postcode sectors in the 2023 analysis zone
sum(cluster_data$in_ULEZ != "Not in ULEZ")

# now do the k-means clustering

# print pre-clustering summary of cluster sizes
table(cluster_data$cluster)

# now run the modified k-means clustering algorithm
# we repeat the algorithm until the percentage of cluster assignments that change is less than 1%, or we reach a maximum number of iterations (100)

# save existing cluster assignments (so we can calculate percentage changed)
prev_clusters <- cluster_data$cluster

# set the max number of iterations, and initialize the iteration counter
max_iter <- 100
i <- 1

# begin the loop
repeat {
    # Compute cluster means for each covariate and cluster
    cluster_means <- cluster_data %>%
        group_by(cluster) %>%
        summarise(across(all_of(covariates), \(x) mean(x, na.rm = FALSE))) %>%
        right_join(data.frame(cluster = 1:k), by = "cluster") %>%
        arrange(cluster) %>%
        select(-cluster) %>%
        as.matrix()

    # Only update clusters for "Not in ULEZ"
    idx <- which(cluster_data$in_ULEZ == "Not in ULEZ")
    obs_mat <- as.matrix(cluster_data[idx, covariates, drop = FALSE])
    dists <- as.matrix(dist(rbind(cluster_means, obs_mat)))[-(1:k), 1:k, drop = FALSE]
    dists[is.na(dists)] <- Inf
    new_clusters <- cluster_data$cluster
    new_clusters[idx] <- max.col(-dists)

    # Calculate percent changed
    pct_changed <- mean(prev_clusters != new_clusters) * 100

    print(paste("Iteration", i, "done. Percent changed:", round(pct_changed, 2)))
    print(table(new_clusters))

    # Stop if less than 1% changed or max_iter reached
    if (pct_changed < 1 | i >= max_iter) {
        cluster_data$cluster <- new_clusters
        break
    }

    # Update for next iteration
    cluster_data$cluster <- new_clusters
    prev_clusters <- new_clusters
    i <- i + 1
}


# now get the cluster variable into the main data
dataxsec23$cluster <- cluster_data$cluster


# get the postcodes in 1 (i.e. the ones with the 2023 zone)
# first export one file for visualisation
export23 <- dataxsec23 %>%
    filter(cluster == 1) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub(" In 2023 ULEZ", "", pcsect)) %>%
    mutate(tag = 1) %>%
    write.xlsx("Temp/matched_pcsects_2023_kmeans_vis.xlsx", rowNames = FALSE)

# now export one for further analysis in stata
export23 <- dataxsec23 %>%
    filter(cluster == 1) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub("In 2023 ULEZ", "2023", pcsect)) %>%
    write.xlsx("Temp/matched_pcsects_2023_kmeans.xlsx", rowNames = FALSE)


# the analysis is now completed in Stata - see the do-file























































###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
# pre Aug 11
###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################


########################################
# DO PRELIMINARY PREPARATION
########################################
# set directory 
setwd("C:/Users/jpmcl/OneDrive/Documents/Economics/Papers/ULEZ on house prices/Data")

# load in packages:
library(tidyverse)
library(ggplot2)
library(readxl)
library(microsynth) # for synthetic control
library(openxlsx) # for k-means
library(microsynth) # for microsynth
########################################




########################################
# 1) CEM 
########################################

# this is all done in stata - see the do-file




########################################
# 2) SYNTHETIC CONTROL
########################################

# load in data
data <- as.data.frame(read_excel("Temp/pp_ready_collapsed_panel.xlsx"))

# now generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
data <- data %>%
    mutate(pcsect = paste(pcsect_original, in_ULEZ))

# remove all observations with missings
data <- na.omit(data)

# balance the data
data <- data %>%
    group_by(pcsect) %>%
    filter(n_distinct(saleyear) == length(unique(data$saleyear))) %>%
    ungroup()


# we now proceed separately for 2021 and 2023, as with the other methods

######## 2a) 2021

# first copy the dataset, making a variable for whether the pcsect is ever treated, and whether it is currently being treated
data21 <- data %>%
    mutate(ever_treated = ifelse(in_ULEZ == "In 2021 ULEZ", 1, 0),
           treatment = ifelse(in_ULEZ == "In 2021 ULEZ" & saleyear >= 2021, 1, 0))

# filter the data to keep only pcsects in the 2021 expansion zone at least 5km from the 2019 border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
data21 <- data21 %>%
    filter(in_ULEZ == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5 | pcsect_dist_from_2023_ULEZ > 50)

# convert the postcode sector to a unique numeric identifier, with the treated ones first
data21 <- data21 %>%
    arrange(desc(treatment), pcsect, saleyear) %>%
    mutate(pcsect_numeric = as.integer(factor(pcsect, levels = unique(pcsect))))

# count how many pcsects are treated, so we can then loop over them
n_treated21 <- max(data21$pcsect_numeric[data21$treatment == 1]); n_treated21


# loop over these units, applying the synthetic control method to each one via microsynth

all_results <- as.data.frame(matrix(0, nrow = n_treated21, ncol = 12))
colnames(all_results) <- c("pcsect",  "diff_2015", "diff_2016", "diff_2017", "diff_2018", "diff_2019", "diff_2020", "diff_2021", "diff_2022", "diff_2023", "diff_2024", "diff_2025")

for (i in 1:n_treated21) {

    # keep only untreated units and the current treated unit
    data_subset <- as.data.frame(data21) %>%
        filter(pcsect_numeric == i | ever_treated == 0)

    # choose covariates to match on
    covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold")

    # now do the microsynth analysis
    result <- microsynth(data_subset, idvar = "pcsect_numeric", timevar = "saleyear", intvar = "treatment", start.pre = 2015, end.pre = 2020, end.post = 2021:2025, match.out.min = "avg_log_price", match.covar.min = covariates, result.var = "avg_log_price", use.backup = T, check.feas = T)

    # display the results
    plot_microsynth(result, plot.var = "avg_log_price", start.pre = 2015, end.pre = 2020, end.post = 2025)

    # append to the results matrix
    all_results[i, ] <- c(data_subset$pcsect[1], result$Plot.Stats$Difference[,,1:11])

    print(paste("Finished for pcsect", i, "of", n_treated21))

}

# now get whether the matching was done successfully, based on pre-treatment differences
for (j in 2:12) {
    all_results[, j] <- as.numeric(all_results[, j])
}

# work out which were matched successfully, and what proportion of the treated units
# for now, define a successful match as one where the squares of the pre-treatment differences are less than 0.00001 on average
all_results$matched <- ifelse(rowSums(((all_results[, 2:7])^2)/6) < 0.00001, 1, 0)
n_matched <- sum(all_results$matched)
print(paste(n_matched, "out of", n_treated21, "treated units matched successfully"))


# now plot the average time differences, for the matched units
plot_data <- all_results %>%
    filter(matched == 1) %>%
    select(-pcsect, -matched) %>%
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
    t() %>%
    as.data.frame() %>%
    mutate(saleyear = 2015:2025) %>%
    rename(average_diff = V1)

ggplot(data = plot_data) +
    geom_line(aes(x = saleyear, y = average_diff), lwd = 2, color = "red") +
    labs(title = "Difference in Average Prices",
         x = "Sale Year",
         y = "Average Price Difference") +
    theme_minimal() + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", lwd = 1) +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "red", lwd = 0.5) +
    scale_x_continuous(breaks = seq(2015, 2025, by = 1))





######## 2b) 2023

# first copy the dataset, making a variable for whether the pcsect is ever treated, and whether it is currently being treated
data23 <- data %>%
    mutate(ever_treated = ifelse(in_ULEZ == "In 2023 ULEZ", 1, 0),
           treatment = ifelse(in_ULEZ == "In 2023 ULEZ" & saleyear >= 2023, 1, 0))

# filter the data to keep only pcsects in the 2023 expansion zone at least 5km from the 2021 border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
data23 <- data23 %>%
    filter(in_ULEZ == "In 2023 ULEZ" & pcsect_dist_from_2021_ULEZ > 5 | pcsect_dist_from_2023_ULEZ > 50)

# convert the postcode sector to a unique numeric identifier, with the treated ones first
data23 <- data23 %>%
    arrange(desc(treatment), pcsect, saleyear) %>%
    mutate(pcsect_numeric = as.integer(factor(pcsect, levels = unique(pcsect))))

# count how many pcsects are treated, so we can then loop over them
n_treated23 <- max(data23$pcsect_numeric[data23$treatment == 1]); n_treated23


# loop over these units, applying the synthetic control method to each one via microsynth

all_results <- as.data.frame(matrix(0, nrow = n_treated23, ncol = 12))
colnames(all_results) <- c("pcsect",  "diff_2015", "diff_2016", "diff_2017", "diff_2018", "diff_2019", "diff_2020", "diff_2021", "diff_2022", "diff_2023", "diff_2024", "diff_2025")

for (i in 1:n_treated23) {

    # keep only untreated units and the current treated unit
    data_subset <- as.data.frame(data23) %>%
        filter(pcsect_numeric == i | ever_treated == 0)

    # choose covariates to match on
    covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold")

    # now do the microsynth analysis
    result <- microsynth(data_subset, idvar = "pcsect_numeric", timevar = "saleyear", intvar = "treatment", start.pre = 2015, end.pre = 2022, end.post = 2023:2025, match.out.min = "avg_log_price", match.covar.min = covariates, result.var = "avg_log_price", check.feas = TRUE, use.backup = TRUE)

    # display the results
    plot_microsynth(result, plot.var = "avg_log_price", start.pre = 2015, end.pre = 2022, end.post = 2025)

    # append to the results matrix

    all_results[i, ] <- c(data_subset$pcsect[1], result$Plot.Stats$Difference[,,1:11])

    print(paste("Finished for pcsect", i, "of", n_treated23))

}

# now get whether the matching was done successfully, based on pre-treatment differences
for (j in 2:12) {
    all_results[, j] <- as.numeric(all_results[, j])
}

# work out which were matched successfully, and what proportion of the treated units 
# successful match = squares of the pre-treatment differences are less than 0.00001 on average
all_results$matched <- ifelse(rowSums(((all_results[, 2:9])^2)/8) < 0.00001, 1, 0)
n_matched <- sum(all_results$matched)
print(paste(n_matched, "out of", n_treated23, "treated units matched successfully"))


# now plot the average time differences, for the matched units
plot_data <- all_results %>%
    filter(matched == 1) %>%
    select(-pcsect, -matched) %>%
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
    t() %>%
    as.data.frame() %>%
    mutate(saleyear = 2015:2025) %>%
    rename(average_diff = V1)

# # get sd too
# to_merge <- all_results %>%
#     filter(matched == 1) %>%
#     select(-pcsect, -matched) %>%
#     summarise(across(everything(), \(x) sd(x, na.rm = TRUE))) %>%
#     t() %>%
#     as.data.frame() %>%
#     mutate(saleyear = 2015:2025) %>%
#     rename(sd_diff = V1)

# # merge on saleyear
# plot_data <- merge(plot_data, to_merge, by = "saleyear")

# plot
ggplot(data = plot_data) +
    geom_line(aes(x = saleyear, y = average_diff), lwd = 2, color = "red") +
    labs(title = "Difference in Average Prices",
         x = "Sale Year",
         y = "Average Price Difference") +
    theme_minimal() + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", lwd = 1) +
    geom_vline(xintercept = 2022, linetype = "dashed", color = "red", lwd = 0.5) +
    scale_x_continuous(breaks = seq(2015, 2025, by = 1))





# NEXT STEPS:
# - get average weight placed on each control unit
# - get a map of what matched well
# - work out how to get SEs (can we use the jackknife stuff from the package for each unit?)
# - address the fact that the matching is often poor, especially with more controls to match on (though maybe try without min)
# - use different sets of controls
# - maybe try without resorting to second/third model, and allow for small misses






########################################
# 3) MODIFIED K-MEANS
########################################

set.seed(12345)

# load in the cross-sectional data
dataxsec <- as.data.frame(read_excel("Temp/pp_ready_collapsed_xsec.xlsx"))

# first generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
dataxsec <- dataxsec %>%
    mutate(pcsect = paste(pcsect_original, in_ULEZ))

# remove all observations with missings
dataxsec <- na.omit(dataxsec)

# filter the data to keep only pcsects in the expansion zones at least 5km from the inner border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
dataxsec <- dataxsec %>%
    filter((in_ULEZ == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5) | (in_ULEZ == "In 2023 ULEZ" & pcsect_dist_from_2021_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50)


# filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
dataxsec <- dataxsec %>%
    filter(!grepl("^BS\\d{1,2}", pcsect) & !grepl("^B\\d{1,2}", pcsect) & !grepl("^OX\\d{1,2}", pcsect))

# assign a cluster variable at random, then pre-define clusters for those in the expansion zones
k <- 15
dataxsec <- dataxsec %>%
    mutate(cluster = sample(1:k, nrow(dataxsec), replace = TRUE)) %>%
    mutate(cluster = ifelse(in_ULEZ == "In 2021 ULEZ", 1, cluster),
           cluster = ifelse(in_ULEZ == "In 2023 ULEZ", 2, cluster))

# now do the k-means clustering algorithm manually, using the covariates (use postcode-district-level ones too?)
covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold", "pcsect_imd", "pcsect_pop_density", "pcdist_share_flat", "pcdist_share_detached", "pcdist_share_semi", "pcdist_share_new", "pcdist_share_leasehold", "pcdist_imd", "pcdist_pop_density")

# extract a new dataset with the covariates and cluster information
cluster_data <- dataxsec %>%
    select(pcsect, cluster, in_ULEZ, all_of(covariates))

# scale the covariates
cluster_data[, covariates] <- scale(cluster_data[, covariates])

# now do the k-means clustering

# print pre-clustering summary of cluster sizes
table(cluster_data$cluster)

# now run the modified k-means clustering algorithm
# we repeat the algorithm until the percentage of cluster assignments that change is less than 1%, or we reach a maximum number of iterations (100)

# save existing cluster assignments (so we can calculate percentage changed)
prev_clusters <- cluster_data$cluster

# set the max number of iterations, and initialize the iteration counter
max_iter <- 100
i <- 1

# begin the loop
repeat {
    # Compute cluster means for each covariate and cluster
    cluster_means <- cluster_data %>%
        group_by(cluster) %>%
        summarise(across(all_of(covariates), \(x) mean(x, na.rm = FALSE))) %>%
        right_join(data.frame(cluster = 1:k), by = "cluster") %>%
        arrange(cluster) %>%
        select(-cluster) %>%
        as.matrix()

    # Only update clusters for "Not in ULEZ"
    idx <- which(cluster_data$in_ULEZ == "Not in ULEZ")
    obs_mat <- as.matrix(cluster_data[idx, covariates, drop = FALSE])
    dists <- as.matrix(dist(rbind(cluster_means, obs_mat)))[-(1:k), 1:k, drop = FALSE]
    dists[is.na(dists)] <- Inf
    new_clusters <- cluster_data$cluster
    new_clusters[idx] <- max.col(-dists)

    # Calculate percent changed
    pct_changed <- mean(prev_clusters != new_clusters) * 100

    print(paste("Iteration", i, "done. Percent changed:", round(pct_changed, 2)))
    print(table(new_clusters))

    # Stop if less than 1% changed or max_iter reached
    if (pct_changed < 1 | i >= max_iter) {
        cluster_data$cluster <- new_clusters
        break
    }

    # Update for next iteration
    cluster_data$cluster <- new_clusters
    prev_clusters <- new_clusters
    i <- i + 1
}


# now get the cluster variable into the main data
dataxsec$cluster <- cluster_data$cluster


# get the postcodes in 1 (i.e. the ones with the 2021 zone)
# first export one file for visualisation
export21 <- dataxsec %>%
    filter(cluster == 1) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub(" In 2021 ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub(" In 2023 ULEZ", "", pcsect)) %>%
    mutate(tag = 1) %>%
    write.xlsx("Temp/matched_pcsects_2021_kmeans_vis.xlsx", rowNames = FALSE)

# now export one for further analysis in stata
export21 <- dataxsec %>%
    filter(cluster == 1) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub("In 2021 ULEZ", "2021", pcsect)) %>%
    mutate(pcsect = gsub("In 2023 ULEZ", "2023", pcsect)) %>%
    write.xlsx("Temp/matched_pcsects_2021_kmeans.xlsx", rowNames = FALSE)

# get the postcodes in 2
# again, first for visualisation
export23 <- dataxsec %>%
    filter(cluster == 2) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub(" In 2021 ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub(" In 2023 ULEZ", "", pcsect)) %>%
    mutate(tag = 1) %>%
    write.xlsx("Temp/matched_pcsects_2023_kmeans_vis.xlsx", rowNames = FALSE)

# now for analysis in stata
export23 <- dataxsec %>%
    filter(cluster == 2) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub("In 2021 ULEZ", "2021", pcsect)) %>%
    mutate(pcsect = gsub("In 2023 ULEZ", "2023", pcsect)) %>%
    write.xlsx("Temp/matched_pcsects_2023_kmeans.xlsx", rowNames = FALSE)


# the analysis is now completed in Stata - see the do-file

































#########################################
#########################################
#########################################
#########################################
# ARCHIVE
#########################################
#########################################
#########################################
#########################################


# approaches:
# 1) do a simple synthetic control analysis for each postcode sector, and average them out (keeping only those that have a good match)
# 2) do a micro-level combined synthetic control analysis (e.g. microsynth)
# 3) do some reweighting to balance the treated and control groups, and then do DiD on the reweighted sample
# 4) do some matching to keep only units of similar characteristics (e.g. via CEM, mahalanobis distance, etc), and then do DiD on the matched sample

# for each, do 2021 and 2023 separately

# NOTE: WITH WEIGHTINGS WE ARE STILL FINE - DO TWO INDIVIDUAL DYNAMIC DIDS JUST IN TERMS OF SAMPLE MEANS (and test vs what a regression w weights would give)


#############################################################

# we first do 4): do coarsened exact matching to keep units of similar characteristics, then export the matched pcsectors to do DiD on those

# load the necessary libraries
library(readxl)
library(cem)
library(tidyverse)
library(foreign)

# load in the cross-sectional data
dataxsec <- as.data.frame(read_excel("Temp/pp_ready_collapsed_xsec.xlsx"))

# first collect up all treated units and generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
dataxsec <- dataxsec %>%
    mutate(treatment = ifelse(in_ULEZ == "In 2021 ULEZ", 1, 0)) %>%
    mutate(pcsect = paste(pcsect_original, in_ULEZ))

# remove all observations with missings
dataxsec <- na.omit(dataxsec)

# filter the data to keep only pcsects in the 2021 expansion zone at least 5km from the 2019 border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
dataxsec <- dataxsec %>%
    filter(in_ULEZ == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5 | pcsect_dist_from_2023_ULEZ > 50)

# filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
dataxsec <- dataxsec %>%
    filter(!grepl("^BS\\d{1,2}", pcsect) & !grepl("^B\\d{1,2}", pcsect) & !grepl("^OX\\d{1,2}", pcsect))


# now do the CEM matching

# filter the data to give only the variables for matching
covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold")
small_data <- dataxsec %>%
    select("pcsect", "treatment", "pcsect_avg_log_price", covariates)

imbalance(group = dataxsec$treatment, data = small_data, drop = c("pcsect_avg_log_price", "pcsect"))

# do automated coarsening
output <- cem(treatment = "treatment", data = small_data, drop = c("pcsect_avg_log_price", "pcsect"), eval.imbalance = TRUE, verbose = 1, keep.all = TRUE)

# recover the data
matched_data <- small_data[which(output$matched == TRUE), ]

# keep only the postcode sectors that were matched
matched_data <- matched_data %>%
    select("pcsect")

# export the data ready for analysis, as a dta file
write.dta(matched_data, "Temp/matched_pcsects_2021.dta")




# now do it for 2023, in the same way

# NOTE: BAD MATCHING!! SEE THE MAP

# load in the cross-sectional data
dataxsec <- as.data.frame(read_excel("Temp/pp_ready_collapsed_xsec.xlsx"))

dataxsec <- dataxsec %>%
    mutate(treatment = ifelse(in_ULEZ == "In 2023 ULEZ", 1, 0)) %>%
    mutate(pcsect = paste(pcsect_original, in_ULEZ))

dataxsec <- na.omit(dataxsec)

# filter the data to keep only pcsects in the 2023 expansion zone at least 5km from the 2021 border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
dataxsec <- dataxsec %>%
    filter(in_ULEZ == "In 2023 ULEZ" & pcsect_dist_from_2021_ULEZ > 5 | pcsect_dist_from_2023_ULEZ > 50)

# filter the data to remove all pcsects in which class D LEZs have been implemented (Birmingham (B) and Bristol (BS))
dataxsec <- dataxsec %>%
    filter(!grepl("^BS\\d{1,2}", pcsect) & !grepl("^B\\d{1,2}", pcsect))


# now do the CEM matching

# filter the data to give only the variables for matching
covariates <- c("pcsect_imd", "pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold")
small_data <- dataxsec %>%
    select("pcsect", "treatment", "pcsect_avg_log_price", covariates)

imbalance(group = dataxsec$treatment, data = small_data, drop = c("pcsect_avg_log_price", "pcsect"))

# do automated coarsening
output <- cem(treatment = "treatment", data = small_data, drop = c("pcsect_avg_log_price", "pcsect"), eval.imbalance = TRUE, verbose = 1, keep.all = TRUE)

# recover the data
matched_data <- small_data[which(output$matched == TRUE), ]

# keep only the postcode sectors that were matched
matched_data <- matched_data %>%
    select("pcsect")

# export the data ready for analysis, as a dta file
write.dta(matched_data, "Temp/matched_pcsects_2023.dta")




#############################################################
# ARCHIVE, PART 2
#############################################################


################

# first 1)

# load the necessary libraries
library(readxl)
library(tidyverse)
library(Synth)
library(microsynth)


# set directory 
setwd("C:/Users/jpmcl/OneDrive/Documents/Economics/Papers/ULEZ on house prices/Data")

# load in data
data <- as.data.frame(read_excel("Temp/pp_ready_collapsed_panel.xlsx"))

# first make a variable for whether the pcsect is ever treated, and whether it is currently being treated
data <- data %>%
    mutate(ever_treated = ifelse(in_ULEZ == "In 2021 ULEZ", 1, 0),
           treatment = ifelse(in_ULEZ == "In 2021 ULEZ" & saleyear >= 2021, 1, 0))

# now generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
data <- data %>%
    mutate(pcsect = paste(pcsect_original, in_ULEZ))

# remove all observations with missings
data <- na.omit(data)

# balance the data
data <- data %>%
    group_by(pcsect) %>%
    filter(n_distinct(saleyear) == length(unique(data$saleyear))) %>%
    ungroup()


# first do the 2021 zone

# filter the data to keep only pcsects in the 2021 expansion zone at least 5km from the 2019 border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
data21 <- data %>%
    filter(in_ULEZ == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5 | pcsect_dist_from_2023_ULEZ > 50)

# convert the postcode sector to a unique numeric identifier, with the treated ones first
data21 <- data21 %>%
    arrange(desc(treatment), pcsect, saleyear) %>%
    mutate(pcsect_numeric = as.integer(factor(pcsect, levels = unique(pcsect))))

# count how many pcsects are treated, so we can then loop over them
n_treated21 <- max(data21$pcsect_numeric[data21$treatment == 1]); n_treated21



# loop over these units, applying the synthetic control method to each one via microsynth

all_results <- as.data.frame(matrix(0, nrow = n_treated21, ncol = 12))
colnames(all_results) <- c("pcsect",  "diff_2015", "diff_2016", "diff_2017", "diff_2018", "diff_2019", "diff_2020", "diff_2021", "diff_2022", "diff_2023", "diff_2024", "diff_2025")

for (i in 1:n_treated21) {

    # keep only untreated units and the current treated unit
    data_subset <- as.data.frame(data21) %>%
        filter(pcsect_numeric == i | ever_treated == 0)

    # choose covariates to match on
    covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold")

    # now do the microsynth analysis
    result <- microsynth(data_subset, idvar = "pcsect_numeric", timevar = "saleyear", intvar = "treatment", start.pre = 2015, end.pre = 2020, end.post = 2021:2025, match.out.min = "avg_log_price", match.covar.min = covariates, result.var = "avg_log_price", check.feas = TRUE, use.backup = TRUE)

    # display the results
    plot_microsynth(result, plot.var = "avg_log_price", start.pre = 2015, end.pre = 2020, end.post = 2025)

    # append to the results matrix

    all_results[i, ] <- c(data_subset$pcsect[1], result$Plot.Stats$Difference[,,1:11])

    print(paste("Finished for pcsect", i, "of", n_treated21))

}

# now get whether the matching was done successfully, based on pre-treatment differences
for (j in 2:12) {
    all_results[, j] <- as.numeric(all_results[, j])
}

# work out which were matched successfully, and what proportion of the treated units
all_results$matched <- ifelse(rowSums((all_results[, 2:7])^2) < 0.00001, 1, 0)
n_matched <- sum(all_results$matched)
print(paste(n_matched, "out of", n_treated, "treated units matched successfully"))


# now plot the average time differences, for the matched units
plot_data <- all_results %>%
    filter(matched == 1) %>%
    select(-pcsect, -matched) %>%
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
    t() %>%
    as.data.frame() %>%
    mutate(saleyear = 2015:2025) %>%
    rename(average_diff = V1)

ggplot(data = plot_data) +
    geom_line(aes(x = saleyear, y = average_diff), lwd = 2, color = "red") +
    labs(title = "Difference in Average Prices",
         x = "Sale Year",
         y = "Average Price Difference") +
    theme_minimal() + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", lwd = 1) +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "red", lwd = 0.5) +
    scale_x_continuous(breaks = seq(2015, 2025, by = 1))


# this seems like a good idea, but a few issues
# - not clear how to get std errors (can we use the jackknife stuff from the package for each unit?)
# - the matching is often poor, especially with more controls to match on (though maybe try without min)

# next steps
# - clear this up (out of archive, replace 4)
# - get average weight placed on each control unit
# - get a map of what matched well
# - work out how to get SEs



# note that microsynth gives cumulative percentage change from summing treatment and control, not percentage change at that point in time. really we want the series of differences



# alternative: the synth package (this seems too slow though - are there other standard SC packages that are faster?)

# now loop over these treated units, running synth each time
for (i in 1:1) {

    # keep only untreated units and the current treated unit
    data_subset <- as.data.frame(data) %>%
        filter(pcsect_numeric == i | treatment == 0)

    # prepare the data for the synthetic control
    dataprep_out <- dataprep(data_subset, predictors = "avg_log_price", dependent = "avg_log_price", unit.variable = "pcsect_numeric", time.variable = "saleyear", treatment.identifier = i, controls.identifier = c((n_treated+1):(max(data_subset$pcsect_numeric))), time.predictors.prior = c(2015:2019), time.optimize.ssr = c(2015:2020), time.plot = c(2015:2025))

    # run the synthetic control
    synth_out <- synth(dataprep_out)

    # plot the paths
    path.plot(synth.res = synth_out, dataprep.res = dataprep_out)

}





##################

# now 2)

# load the necessary libraries
library(readxl)
library(tidyverse)
library(microsynth)
library(ggplot2)

# write plotting functions for time-invariant and time-variant variables
compare_invar <- function(dataset, plot.var, treatment = "in_ULEZ") {
    dataset <- dataset %>%
        mutate(!!sym(treatment) := factor(!!sym(treatment))) %>%
        filter(saleyear == min(saleyear)) # filter to the first year of the data, to avoid duplicates

    ggplot(dataset) +
        geom_density(aes(x = !!sym(plot.var), color = !!sym(treatment)), alpha = 0.5, lwd = 2) +
        theme_minimal() +
        labs(x = plot.var, y = "Density", color = treatment)
}

compare_var <- function(dataset, plot.var, id.var = "pcsect", treatment = "in_ULEZ") {
    dataset <- dataset %>%
        mutate(!!sym(treatment) := factor(!!sym(treatment))) %>%
        group_by(!!sym(id.var)) %>%
        mutate(ever_treated = as.factor(max(as.numeric(!!sym(treatment))))) %>%
        ungroup()

    ggplot(dataset) +
        geom_density(aes(x = !!sym(plot.var), color = ever_treated), alpha = 0.5, lwd = 2) +
        theme_minimal() +
        labs(x = plot.var, y = "Density", color = "Ever Treated")
}

# set directory 
setwd("C:/Users/jpmcl/OneDrive/Documents/Economics/Papers/ULEZ on house prices/Data")

# load in data
data <- as.data.frame(read_excel("Temp/pp_ready_collapsed_panel.xlsx"))

# first make a variable for whether the pcsect is CURRENTLY treated
data <- data %>%
    mutate(ULEZ_status = ifelse(in_ULEZ == "In 2021 ULEZ" & saleyear >= 2021, 1, 0))

# now generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
data <- data %>%
    mutate(pcsect = paste(pcsect_original, in_ULEZ))

# remove all observations with missings
data <- na.omit(data)

# balance the data
data <- data %>%
    group_by(pcsect) %>%
    filter(n_distinct(saleyear) == length(unique(data$saleyear))) %>%
    ungroup()

# filter the data to keep only pcsects in the 2021 expansion zone at least 5km from the 2019 border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
data <- data %>%
    filter(in_ULEZ == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5 | pcsect_dist_from_2023_ULEZ > 50)



# now do microsynth: first create a vector giving the target variable
match.var <- "avg_log_price"

# first just match on previous price history
result <- microsynth(data, idvar = "pcsect", timevar = "saleyear", intvar = "ULEZ_status", start.pre = 2015, end.pre = 2020, end.post = 2021:2025, match.out.min = match.var, result.var = match.var, check.feas = TRUE, use.backup = TRUE)
summary(result)
plot_microsynth(result, plot.var = "avg_log_price", start.pre = 2015, end.pre = 2020, end.post = 2025)

# get the weights, to check things seem right with where they are coming from (cities?)
head(result$w$Weights)
sum(result$w$Weights)
# seems to work with match.out.min, but not match.out??


# now the same, with placebo-based inference (jackknife and permutation-based) and all the other things
# result <- microsynth(data, idvar = "pcsect", timevar = "saleyear", intvar = "ULEZ_status", start.pre = 2015, end.pre = 2020, end.post = 2021:2025, match.out = match.var, result.var = match.var, jack = TRUE, perm = 50, check.feas = TRUE, use.backup = TRUE)
# summary(result)
# plot_microsynth(result, plot.var = "avg_log_price", start.pre = 2015, end.pre = 2020, end.post = 2025)




# now match on other characteristics too

# check covariate balance
compare_var(data, "avg_log_price")
compare_invar(data, "pcsect_imd")
compare_invar(data, "pcsect_pop_density")
compare_invar(data, "pcsect_no22019")
compare_invar(data, "pcsect_pm102019g")
compare_invar(data, "pcsect_share_flat")
compare_invar(data, "pcsect_share_detached")
compare_invar(data, "pcsect_share_semi")
compare_invar(data, "pcsect_share_new")
compare_invar(data, "pcsect_share_leasehold")


match.covar <- c("pcsect_imd") 
result <- microsynth(data, idvar = "pcsect", timevar = "saleyear", intvar = "ULEZ_status", start.pre = 2015, end.pre = 2020, end.post = 2021:2025, match.out.min = match.var, match.covar.min = match.covar, result.var = match.var, check.feas = TRUE, use.backup = TRUE)
summary(result)
plot_microsynth(result, plot.var = "avg_log_price", start.pre = 2015, end.pre = 2020, end.post = 2025)


match.covar <- c("pcsect_imd", "pcsect_no22019")
result <- microsynth(data, idvar = "pcsect", timevar = "saleyear", intvar = "ULEZ_status", start.pre = 2015, end.pre = 2020, end.post = 2021:2025, match.out.min = match.var, match.covar.min = match.covar, result.var = match.var, check.feas = TRUE, use.backup = TRUE)
summary(result)
plot_microsynth(result, plot.var = "avg_log_price", start.pre = 2015, end.pre = 2020, end.post = 2025)


match.covar <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold") 
result <- microsynth(data, idvar = "pcsect", timevar = "saleyear", intvar = "ULEZ_status", start.pre = 2015, end.pre = 2020, end.post = 2021:2025, match.out.min = match.var, match.covar.min = match.covar, result.var = match.var, check.feas = TRUE, use.backup = TRUE)
summary(result)
plot_microsynth(result, plot.var = "avg_log_price", start.pre = 2015, end.pre = 2020, end.post = 2025)


# convergence issues, especially when more controls are introduced
# more iterations don't fix things
# suggests that the matching is not feasible, so we need to trim the data


# NOTE: i don't think trimming the data works well, because the units from the trimmed subsection are disproportionately the ones being used anyway
# need another approach!!



# synthetic control is a bit jumpy when we have too many covariates, I think because it is overly relying on a few pcsects



# trim the treatment data based on mahalanobis distance: make the mahalanobis distance, get it as a mean for each pcsect, and then filter out the high ones in the treated group

# first make a function that takes in the data and the control variables and returns a trimmed dataset based on mahalanobis distance ready for analysis
trim_mahalanobis <- function(dataset, covariates) {
    subset <- c(match.var, covariates)
    dataset_temp <- dataset[c(subset, "pcsect", "in_ULEZ", "saleyear", "ULEZ_status")]

    dataset_temp$mahalanobis_dist <- mahalanobis(
    dataset_temp[subset],
    colMeans(dataset_temp[dataset_temp$in_ULEZ == "Not in ULEZ", subset]),
    cov(dataset_temp[dataset_temp$in_ULEZ == "Not in ULEZ", subset])
    )

    a <- sum(dataset_temp$in_ULEZ == "In 2021 ULEZ") / 11

    # we do the trimming based on position of postcode sector relative to median
    dataset_temp <- dataset_temp %>%
        group_by(pcsect) %>%
        mutate(mahalanobis_dist = mean(mahalanobis_dist)) %>%
        ungroup() %>%
        filter(mahalanobis_dist <= median(mahalanobis_dist[in_ULEZ == "In 2021 ULEZ"]) | in_ULEZ != "In 2021 ULEZ")

    b <- sum(dataset_temp$in_ULEZ == "In 2021 ULEZ") / 11

    print(paste(a - b, "out of", a, "treated units dropped"))

    return(dataset_temp)

}


# now redo the matching on trimmed versions of the data

match.covar <- c("pcsect_imd") 
data_temp <- trim_mahalanobis(data, match.covar)
result <- microsynth(data_temp, idvar = "pcsect", timevar = "saleyear", intvar = "ULEZ_status", start.pre = 2015, end.pre = 2020, end.post = 2021:2025, match.out.min = match.var, match.covar.min = match.covar, result.var = match.var, check.feas = TRUE, use.backup = TRUE)
summary(result)
plot_microsynth(result, plot.var = "avg_log_price", start.pre = 2015, end.pre = 2020, end.post = 2025)


match.covar <- c("pcsect_imd", "pcsect_no22019")
data_temp <- trim_mahalanobis(data, match.covar)
result <- microsynth(data_temp, idvar = "pcsect", timevar = "saleyear", intvar = "ULEZ_status", start.pre = 2015, end.pre = 2020, end.post = 2021:2025, match.out.min = match.var, match.covar.min = match.covar, result.var = match.var, check.feas = TRUE, use.backup = TRUE)
summary(result)
plot_microsynth(result, plot.var = "avg_log_price", start.pre = 2015, end.pre = 2020, end.post = 2025)


match.covar <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold") 
data_temp <- trim_mahalanobis(data, match.covar)
result <- microsynth(data_temp, idvar = "pcsect", timevar = "saleyear", intvar = "ULEZ_status", start.pre = 2015, end.pre = 2020, end.post = 2021:2025, match.out.min = match.var, match.covar.min = match.covar, result.var = match.var, check.feas = TRUE, use.backup = TRUE)
summary(result)
plot_microsynth(result, plot.var = "avg_log_price", start.pre = 2015, end.pre = 2020, end.post = 2025)


# Q: why does this do so poorly, even after trimming? Is it because of very different characteristics of the treated and control units? 
# Q: is there a better way to trim? Maybe CEM?
# Q: how to interpret? Maybe we want actual level average prices, and then use percent. This may work better


# the issues seems to be as follows:
# - poor balance in controls and treated group, even after trimming
# - more?




# go through the example!!!!

# look through controls in the panel data - do they look right?







###############

# now 3)

# import necessary libraries
library(ebal)

# load in the cross-sectional data
dataxsec <- as.data.frame(read_excel("Temp/pp_ready_collapsed_xsec.xlsx"))

# first collect up all treated units and generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
dataxsec <- dataxsec %>%
    mutate(treatment = ifelse(in_ULEZ == "In 2021 ULEZ", 1, 0)) %>%
    mutate(pcsect = paste(pcsect_original, in_ULEZ))

# remove all observations with missings
dataxsec <- na.omit(dataxsec)

# filter the data to keep only pcsects in the 2021 expansion zone at least 5km from the 2019 border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
dataxsec <- dataxsec %>%
    filter(in_ULEZ == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5 | pcsect_dist_from_2023_ULEZ > 50)

# choose covariates to balance on
covariates <- c("pcsect_imd", "pcsect_no22019", "pcsect_pop_density", "pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold")

# now do entropy balancing
eb.out <- ebalance(Treatment = dataxsec$treatment, X = dataxsec[, covariates], max.iterations = 2000)

eb.out$converged

# means in treatment group data
apply(as.data.frame(dataxsec[dataxsec$treatment==1, covariates]),2,mean)
# means in reweighted control group data
apply(as.data.frame(dataxsec[dataxsec$treatment==0, covariates]),2,weighted.mean,w=eb.out$w)
# means in raw data control group data
apply(as.data.frame(dataxsec[dataxsec$treatment==0, covariates]),2,mean)

# now collect the weights for each control unit
weights <- dataxsec %>%
    filter(treatment == 0) %>%
    select(pcsect) %>%
    mutate(wt = eb.out$w / sum(eb.out$w))


# now get the weighted average of prices in the control and treatment groups (may have to load in and preprocess data again)

# first for the treated region
avg_prices_treated <- data %>%
    filter(in_ULEZ == "In 2021 ULEZ") %>%
    select(pcsect, avg_log_price, saleyear) %>%
    group_by(saleyear) %>%
    summarise(avg_log_price = mean(avg_log_price))

# now for the untreated region
avg_prices_control <- data %>%
    filter(in_ULEZ == "Not in ULEZ") %>%
    select(pcsect, avg_log_price, saleyear) %>%
    left_join(weights, by = "pcsect") %>%
    group_by(saleyear) %>%
    summarise(avg_log_price = weighted.mean(avg_log_price, w = wt, na.rm = TRUE))

# now plot the difference between average prices in the treated and control groups
plot_data <- left_join(avg_prices_treated, avg_prices_control, by = "saleyear", suffix = c("_treated", "_control")) %>%
    mutate(price_diff = avg_log_price_treated - avg_log_price_control)

ggplot(plot_data, aes(x = saleyear)) +
    geom_line(aes(y = price_diff - price_diff[saleyear == 2020]), lwd = 2, color = "red") +
    labs(title = "Difference in Average Prices",
         x = "Sale Year",
         y = "Price Difference") +
    theme_minimal() + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", lwd = 1) +
    geom_vline(xintercept = 2020, linetype = "dashed", color = "red", lwd = 0.5) +
    scale_x_continuous(breaks = seq(2015, 2025, by = 1))

# doesn't really work - the pre-trends are bad

# can we do proper DiD with this weighted data? By WLS?



# could also get weights using microsynth
# can then take a cutoff of units with weights above a certain threshold, and then do DiD on this sample without weights
    # logic - if parallel trends holds with this control group, then the DiD works



# alternative: examine weights, keep the good ones, and then do DiD on this sample without weights?








#######################

# now 4)

# do this first with CEM, matching the cross-sectional data

# load the necessary libraries
library(cem)

# load in the cross-sectional data
dataxsec <- as.data.frame(read_excel("Temp/pp_ready_collapsed_xsec.xlsx"))

# first collect up all treated units and generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
dataxsec <- dataxsec %>%
    mutate(treatment = ifelse(in_ULEZ == "In 2021 ULEZ", 1, 0)) %>%
    mutate(pcsect = paste(pcsect_original, in_ULEZ))

# remove all observations with missings
dataxsec <- na.omit(dataxsec)

# filter the data to keep only pcsects in the 2021 expansion zone at least 5km from the 2019 border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
dataxsec <- dataxsec %>%
    filter(in_ULEZ == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5 | pcsect_dist_from_2023_ULEZ > 50)

# now do the CEM matching

# filter the data to give only the variables for matching
covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold")
small_data <- dataxsec %>%
    select("pcsect", "treatment", "pcsect_avg_log_price", covariates)

imbalance(group = dataxsec$treatment, data = small_data, drop = c("pcsect_avg_log_price", "pcsect"))

# do automated coarsening
output <- cem(treatment = "treatment", data = small_data, drop = c("pcsect_avg_log_price", "pcsect"), eval.imbalance = TRUE, verbose = 1, keep.all = TRUE)

# adjust for number of cutpoints
output <- cem(treatment = "treatment", data = small_data, cutpoints = list(pcsect_imd = 2), drop = c("pcsect_avg_log_price", "pcsect"), eval.imbalance = TRUE, verbose = 1, keep.all = TRUE)

# recover the data
matched_data <- small_data[which(output$matched == TRUE), ]

# recover the cutpoints
output$breaks

# can now do original analysis on this


# work out how CEM works, understand L1.profile (where is the randomness coming from?), then understand how to prune
# in the meantime, match on the useful variables, then start to do the analysis on the matched data




# analyse the match

# first get L1 for lots of random different coarsenings
L1.profile(group = dataxsec$treatment, data = small_data, drop = c("pcsect_avg_log_price", "pcsect"))

output2 <- imbspace(output, data = small_data, depth = 1)
# how do we pick the one we want? Maybe of those below and left, such that no more are more below and more left, the one w most matches?



# now relax the coarsening to get more matches
output2 <- relax.cem(output, data = small_data, depth = 1, use.weights = TRUE)

# take the first match that matches 50% of the treated units
arg <- output2$G1$Relaxed[min(which(output2$G1$PercG1 >= 50))]
output_filtered <- cem(treatment = "treatment", data = small_data, drop = c("pcsect_avg_log_price", "pcsect"), eval.imbalance = TRUE, verbose = 1, keep.all = TRUE)



# how do we actually get these matches???? And by what rule do we choose? Most cutpoints giving x% of treated units matched?





#############################################################

# 5): do k-means clustering, adjusted for this purpose

set.seed(12345)

# load in the cross-sectional data
dataxsec <- as.data.frame(read_excel("Temp/pp_ready_collapsed_xsec.xlsx"))

# first generate a unique cross-sectional identifier (i.e. the pseudo-postcode sector)
dataxsec <- dataxsec %>%
    mutate(pcsect = paste(pcsect_original, in_ULEZ))

# remove all observations with missings
dataxsec <- na.omit(dataxsec)

# filter the data to keep only pcsects in the expansion zones at least 5km from the inner border, or those outside ULEZ and at least 50km from the outer 2023 zone (to address spillover issues)
dataxsec <- dataxsec %>%
    filter((in_ULEZ == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5) | (in_ULEZ == "In 2023 ULEZ" & pcsect_dist_from_2021_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50)


# filter the data to remove all pcsects in which class D LEZs or ZEZs have been implemented (Birmingham (B), Bristol (BS) and Oxford (OX))
dataxsec <- dataxsec %>%
    filter(!grepl("^BS\\d{1,2}", pcsect) & !grepl("^B\\d{1,2}", pcsect) & !grepl("^OX\\d{1,2}", pcsect))

# assign a cluster variable at random, then pre-define clusters for those in the expansion zones
k <- 10
dataxsec <- dataxsec %>%
    mutate(cluster = sample(1:k, nrow(dataxsec), replace = TRUE)) %>%
    mutate(cluster = ifelse(in_ULEZ == "In 2021 ULEZ", 1, cluster),
           cluster = ifelse(in_ULEZ == "In 2023 ULEZ", 2, cluster))

# now do the k-means clustering algorithm manually, using the covariates
covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold", "pcsect_imd", "pcsect_pop_density", "pcdist_share_flat", "pcdist_share_detached", "pcdist_share_semi", "pcdist_share_new", "pcdist_share_leasehold", "pcdist_imd", "pcdist_pop_density")

# extract a new dataset with the covariates and cluster information
cluster_data <- dataxsec %>%
    select(pcsect, cluster, in_ULEZ, all_of(covariates))

# scale the covariates
cluster_data[, covariates] <- scale(cluster_data[, covariates])

# now do the k-means clustering

n <- 10

for (i in 1:n) {

    # create a dataframe to hold the means of the covariates for each cluster
    cluster_means <- data.frame(matrix(0, nrow = k, ncol = length(covariates)))
    
    for (j in 1:k) {

        for (word in covariates) {
            
            # get the mean of the covariate for the current cluster
            assign(paste0("mean_", word, "_", j), mean(cluster_data[cluster_data$cluster == j, word], na.rm = TRUE))
        }
    
        # fill the dataframe with the mean vector for cluster j
        cluster_means[j, ] <- sapply(covariates, function(word) get(paste0("mean_", word, "_", j)))
    }

    # for each observation in the non-ULEZ data, calculate the distance to the cluster means and assign the cluster with shortest distance
    for (j in 1:nrow(cluster_data)) {

        if (cluster_data$in_ULEZ[j] == "Not in ULEZ") {

            # calculate the distance to each cluster mean (note: can replace this with a distance function if needed)
            distances <- sapply(1:k, function(x) sum((cluster_data[j, covariates] - cluster_means[x, ])^2))

            # assign the cluster with the shortest distance
            cluster_data$cluster[j] <- which.min(distances)
        }

        if (j %% 100 == 0) {
        print(paste("Obs number", j, "done"))
        }
    }

    print(paste("Iteration", i, "done"))

}

# get the cluster variable into the main data
dataxsec$cluster <- cluster_data$cluster


# get the postcodes in 1
library(writexl)

export21 <- dataxsec %>%
    filter(cluster == 1) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub(" In 2021 ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub(" In 2023 ULEZ", "", pcsect)) %>%
    mutate(tag = 1) %>%
    write.xlsx("Temp/matched_pcsects_2021_kmeans_vis.xlsx", rowNames = FALSE)

export21 <- dataxsec %>%
    filter(cluster == 1) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub("In 2021 ULEZ", "2021", pcsect)) %>%
    mutate(pcsect = gsub("In 2023 ULEZ", "2023", pcsect)) %>%
    write.xlsx("Temp/matched_pcsects_2021_kmeans.xlsx", rowNames = FALSE)

# get the postcodes in 2
export23 <- dataxsec %>%
    filter(cluster == 2) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub(" In 2021 ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub(" In 2023 ULEZ", "", pcsect)) %>%
    mutate(tag = 1) %>%
    write.xlsx("Temp/matched_pcsects_2023_kmeans_vis.xlsx", rowNames = FALSE)

export23 <- dataxsec %>%
    filter(cluster == 2) %>%
    select(pcsect) %>%
    mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
    mutate(pcsect = gsub("In 2021 ULEZ", "2021", pcsect)) %>%
    mutate(pcsect = gsub("In 2023 ULEZ", "2023", pcsect)) %>%
    write.xlsx("Temp/matched_pcsects_2023_kmeans.xlsx", rowNames = FALSE)







########################



# extra: do the microsynth eg

data <- seattledmi %>%
    arrange(ID, time)


cov.var <- c('TotalPop', 'BLACK', 'HISPANIC', 'Males_1521',
             'HOUSEHOLDS', 'FAMILYHOUS', 'FEMALE_HOU', 'RENTER_HOU', 'VACANT_HOU')
match.out <- c('any_crime')

sea1 <- microsynth(data,
                   idvar='ID', timevar='time', intvar='Intervention',
                   start.pre=1, end.pre=12, end.post=16,
                   match.out=match.out, match.covar=cov.var,
                   result.var=match.out)
                   
summary(sea1)
plot_microsynth(sea1)

# this works well, with min too



