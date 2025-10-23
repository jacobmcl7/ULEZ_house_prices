# this R script implements the k-means matching algorithm for the long-distance results, to create a control group for each region
# these controls are then used in chunks 11 and 12 of the analysis_house_prices.do file


############################################################################
# DO PRELIMINARY PREPARATION
############################################################################
# set directory 
setwd("C:/Users/jpmcl/OneDrive/Documents/Economics/Papers/ULEZ on house prices/Data")

# load in packages:
library(tidyverse)
library(ggplot2)
library(readxl)
library(openxlsx)
############################################################################



############################################################################
# RUN THE BASELINE MODIFIED K-MEANS
############################################################################

# set seed for reproducibility
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
# first export one file for visualisation via ArcGIS, done in the postcode_sector_visualisation.py file
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



############################################################################



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
# first export one file for visualisation via ArcGIS, done in the postcode_sector_visualisation.py file
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

############################################################################





############################################################################
# ROBUSTNESS - VARY k
############################################################################


# now create the samples required for robustness checks, by varying k

# first generate the control groups for the 2021 zone


dataxsec21 <- dataxsec %>%
    # get the correct sample
    filter((in_ULEZ == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50) %>%
    # remove LEZ zones
    filter(!grepl("^BS\\d{1,2}", pcsect) & !grepl("^B\\d{1,2}", pcsect) & !grepl("^OX\\d{1,2}", pcsect))

# now set up the clustering, as before

covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold", "pcsect_imd", "pcsect_pop_density", "pcdist_share_flat", "pcdist_share_detached", "pcdist_share_semi", "pcdist_share_new", "pcdist_share_leasehold", "pcdist_imd", "pcdist_pop_density")
cluster_data <- dataxsec21 %>%
    select(pcsect, in_ULEZ, all_of(covariates))
cluster_data[, covariates] <- scale(cluster_data[, covariates])


# now do the k-means clustering, looping over different k from 10 to 20 in multiples of 2

for (k in seq(10, 20, by = 2)) {

    # print the k we are now using
    cat("\n")
    print("--------------------------------------")
    print(paste0("k = ", k))
    print("--------------------------------------")
    cat("\n")    


    # initialise the cluster
    cluster_data <- cluster_data %>%
        mutate(cluster = sample(1:k, nrow(cluster_data), replace = TRUE)) %>%
        mutate(cluster = ifelse(in_ULEZ == "In 2021 ULEZ", 1, cluster))

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

    # now export a file for further analysis in stata
    export21 <- dataxsec21 %>%
        filter(cluster == 1) %>%
        select(pcsect) %>%
        mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
        mutate(pcsect = gsub("In 2021 ULEZ", "2021", pcsect)) %>%
        write.xlsx(paste0("Temp/matched_pcsects_2021_kmeans_", k, ".xlsx"), rowNames = FALSE)

}



############################################################################



# now do it for the 2023 region

dataxsec23 <- dataxsec %>%
    # get the correct sample
    filter((in_ULEZ == "In 2023 ULEZ" & pcsect_dist_from_2021_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50) %>%
    # remove LEZ zones
    filter(!grepl("^BS\\d{1,2}", pcsect) & !grepl("^B\\d{1,2}", pcsect) & !grepl("^OX\\d{1,2}", pcsect))

# now set up the clustering, as before

covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold", "pcsect_imd", "pcsect_pop_density", "pcdist_share_flat", "pcdist_share_detached", "pcdist_share_semi", "pcdist_share_new", "pcdist_share_leasehold", "pcdist_imd", "pcdist_pop_density")
cluster_data <- dataxsec23 %>%
    select(pcsect, in_ULEZ, all_of(covariates))
cluster_data[, covariates] <- scale(cluster_data[, covariates])


# now do the k-means clustering, looping over different k from 10 to 20 in multiples of 2

for (k in seq(10, 20, by = 2)) {

    # print the k we are now using
    cat("\n")
    print("--------------------------------------")
    print(paste0("k = ", k))
    print("--------------------------------------")
    cat("\n")    


    # initialise the cluster
    cluster_data <- cluster_data %>%
        mutate(cluster = sample(1:k, nrow(cluster_data), replace = TRUE)) %>%
        mutate(cluster = ifelse(in_ULEZ == "In 2023 ULEZ", 1, cluster))

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


    # get the postcodes in cluster 1 (i.e. the ones with the 2021 zone)

    # now export a file for further analysis in stata
    export23 <- dataxsec23 %>%
        filter(cluster == 1) %>%
        select(pcsect) %>%
        mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
        mutate(pcsect = gsub("In 2023 ULEZ", "2023", pcsect)) %>%
        write.xlsx(paste0("Temp/matched_pcsects_2023_kmeans_", k, ".xlsx"), rowNames = FALSE)

}

############################################################################




############################################################################
# ROBUSTNESS - VARY THE INITIAL ASSIGNMENT (I.E. THE SEED)
############################################################################



# now do robustness for different initial allocations, with k = 15 - start with 2021


dataxsec21 <- dataxsec %>%
    # get the correct sample
    filter((in_ULEZ == "In 2021 ULEZ" & pcsect_dist_from_2019_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50) %>%
    # remove LEZ zones
    filter(!grepl("^BS\\d{1,2}", pcsect) & !grepl("^B\\d{1,2}", pcsect) & !grepl("^OX\\d{1,2}", pcsect))

# now set up the clustering, as before

covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold", "pcsect_imd", "pcsect_pop_density", "pcdist_share_flat", "pcdist_share_detached", "pcdist_share_semi", "pcdist_share_new", "pcdist_share_leasehold", "pcdist_imd", "pcdist_pop_density")
cluster_data <- dataxsec21 %>%
    select(pcsect, in_ULEZ, all_of(covariates))
cluster_data[, covariates] <- scale(cluster_data[, covariates])



# set the number of groups
k <- 15

# now loop over seeds
for (seed in seq(1000, 5000, by = 1000)) {

    # print the k we are now using
    cat("\n")
    print("--------------------------------------")
    print(paste0("seed = ", seed))
    print("--------------------------------------")
    cat("\n")    

    # set the seed
    set.seed(seed)

    # initialise the cluster
    cluster_data <- cluster_data %>%
        mutate(cluster = sample(1:k, nrow(cluster_data), replace = TRUE)) %>%
        mutate(cluster = ifelse(in_ULEZ == "In 2021 ULEZ", 1, cluster))

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

    # now export a file for further analysis in stata
    export21 <- dataxsec21 %>%
        filter(cluster == 1) %>%
        select(pcsect) %>%
        mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
        mutate(pcsect = gsub("In 2021 ULEZ", "2021", pcsect)) %>%
        write.xlsx(paste0("Temp/matched_pcsects_2021_kmeans_seed_", seed, ".xlsx"), rowNames = FALSE)

}



############################################################################



# now 2023


dataxsec23 <- dataxsec %>%
    # get the correct sample
    filter((in_ULEZ == "In 2023 ULEZ" & pcsect_dist_from_2021_ULEZ > 5) | pcsect_dist_from_2023_ULEZ > 50) %>%
    # remove LEZ zones
    filter(!grepl("^BS\\d{1,2}", pcsect) & !grepl("^B\\d{1,2}", pcsect) & !grepl("^OX\\d{1,2}", pcsect))

# now set up the clustering, as before

covariates <- c("pcsect_share_flat", "pcsect_share_detached", "pcsect_share_semi", "pcsect_share_new", "pcsect_share_leasehold", "pcsect_imd", "pcsect_pop_density", "pcdist_share_flat", "pcdist_share_detached", "pcdist_share_semi", "pcdist_share_new", "pcdist_share_leasehold", "pcdist_imd", "pcdist_pop_density")
cluster_data <- dataxsec23 %>%
    select(pcsect, in_ULEZ, all_of(covariates))
cluster_data[, covariates] <- scale(cluster_data[, covariates])


# set the number of groups
k <- 15

# now loop over seeds
for (seed in seq(1000, 5000, by = 1000)) {

    # print the k we are now using
    cat("\n")
    print("--------------------------------------")
    print(paste0("seed = ", seed))
    print("--------------------------------------")
    cat("\n")    

    # set the seed
    set.seed(seed)

    # initialise the cluster
    cluster_data <- cluster_data %>%
        mutate(cluster = sample(1:k, nrow(cluster_data), replace = TRUE)) %>%
        mutate(cluster = ifelse(in_ULEZ == "In 2023 ULEZ", 1, cluster))

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


    # get the postcodes in cluster 1 (i.e. the ones with the 2021 zone)

    # now export a file for further analysis in stata
    export23 <- dataxsec23 %>%
        filter(cluster == 1) %>%
        select(pcsect) %>%
        mutate(pcsect = gsub(" Not in ULEZ", "", pcsect)) %>%
        mutate(pcsect = gsub("In 2023 ULEZ", "2023", pcsect)) %>%
        write.xlsx(paste0("Temp/matched_pcsects_2023_kmeans_seed_", seed, ".xlsx"), rowNames = FALSE)

}