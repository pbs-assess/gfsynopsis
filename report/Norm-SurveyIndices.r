# R code (revised) to calculate survey indices and confidence intervals using
# bootstrapping.

####################################################################################
# get.sql.data
#    Executes a SQL command on an external SQL Server database and returns a data
#    frame of the results, if applicable.
####################################################################################
get.sql.data <- function(sql, db="PacHarvest", svr="DFBCV9TWVASP001", ...) {
#svr="(local)"
   # Requires the RODBC package
   require("RODBC")
   # Construct a connection string
   constr <- paste("Driver={SQL Server};Server=",svr,";Database=",db,";", sep="")
   # Connect to the database
   cnn <- odbcDriverConnect(constr)
   # Execute the SQL command and store the results in a data frame
   df <- sqlQuery(cnn, sql, ...)
   # Close the connection
   odbcClose(cnn)
   
   # Return the results
   return(df)
}

####################################################################################
# calc.biomass
#    Resamples catch densities with replication from catch matrix output and
#    calculates a total biomass based on the average catch density per stratum
#    multiplied by the stratum area, summed over all strata for the total biomass
####################################################################################
calc.biomass <- function(dat, i, sa, is_ll=FALSE, is_sable=FALSE, is_jig=FALSE,
   is_dog=FALSE) {

   # If i is only a single number, use a vector of the row indices instead
   # (used for testing)
   if (length(i)==1)
      i <- 1:nrow(dat)

   # Resample from the data frame using the vector i
   dat <- dat[i,]

   # Calculate the mean biomass (kg/km2) for each stratum.  The "if-else" structure
   # is for handling the special cases of a longline (is_ll), sablefish
   # (is_sable), and jig (is_jig) surveys that use different measures of catch.
   mu <- numeric(nrow(sa))
   if (is_sable) {
      mb <- tapply(dat$CPUE_PPT, list(dat$GROUPING_CODE), mean)
      mu[sa$GROUPING_CODE %in% names(mb)] <- mb
      res <- mean(mu)
   } else if (is_ll) {
      mb <- tapply(dat$DENSITY_PPKM2, list(dat$GROUPING_CODE), mean)
      mu[sa$GROUPING_CODE %in% names(mb)] <- mb
      res <- sum(mu * sa$AREA_KM2)
   } else if (is_jig) {
      mb <- tapply(dat$CPUE_PPH, list(dat$GROUPING_CODE), mean)
	  mu[sa$GROUPING_CODE %in% names(mb)] <- mb
	  res <- mean(mu)
   } else if (is_dog) {
      mb <- tapply(dat$DENSITY_PPKM2, list(dat$GROUPING_CODE), mean)
      mu[sa$GROUPING_CODE %in% names(mb)] <- mb
      res <- mean(mu)
   } else {
      mb <- tapply(dat$DENSITY_KGPM2 * 1000000, list(dat$GROUPING_CODE), mean)
      mu[sa$GROUPING_CODE %in% names(mb)] <- mb
      res <- sum(mu * sa$AREA_KM2)
   }
   #mu[sa$GROUPING_CODE %in% names(mb)] <- mb

   # Expand mean biomass per stratum to the entire survey area to get total biomass
   #return(sum(mu * sa$AREA_KM2))
   return(res)

}

####################################################################################
# boot.species
#    Performs a bootstrap procedure for a given species from a given survey and
#    returns a list containing various elements from the bootstrap analysis.
####################################################################################
boot.species <- function(survey_id, species_code, stat=calc.biomass, r=1000,
   is_ll=FALSE, is_sable=FALSE, is_jig=FALSE, is_dog=FALSE, resample=FALSE) {

   # Requires the boot package
   require(boot)

   # Get a dataframe of the number of sets and area per survey stratum
   sa <- get.sql.data(paste("EXEC proc_stratum_info", survey_id), "GFBioSQL")

   # Get a data frame of the catch density per fishing event for the species/survey
   # Sablefish surveys have to be handled by a seperate procedure.
   if (is_sable) {
      dm <- get.sql.data(paste("EXEC proc_catmat_sable ", survey_id,
         ", '", species_code, "'", sep=""), "GFBioSQL", 
         as.is=c(F,F,T,T,F,F,F,F,F,F,F,F,F,F,F,F,F))
   } else if (is_ll) {
      dm <- get.sql.data(paste("EXEC proc_catmat_ll_2013 ", survey_id,
         ", '", species_code, "'", sep=""), "GFBioSQL", 
         as.is=c(T,F,F,F,F,F,F,F,F,F,F,F))
   } else if (is_jig) {
      dm <- get.sql.data(paste("EXEC proc_catmat_jig ", survey_id,
         ", '", species_code, "'", sep=""), "GFBioSQL", 
         as.is=c(F,F,T,T,F,F,F,F,F,F,F,F,F,F,F))
   } else if (is_dog) {
      dm <- get.sql.data(paste("EXEC proc_catmat_dog ", survey_id,
         ", '", species_code, "'", sep=""), "GFBioSQL", 
         as.is=c(T,F,F,F,F,F,F,F,F,F,F))
   } else {
      dm <- get.sql.data(paste("EXEC proc_catmat_2011 ", survey_id,
         ", '", species_code, "'", sep=""), "GFBioSQL", 
         as.is=c(F,F,T,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F))
   }

   # This is a kludge for the Hecate Strait multispecies survey, where only the
   # first 7 strata are used for biomass indices calculation.
   if (survey_id >= 4 & survey_id <= 14) {
      dm <- dm[dm$GROUPING_CODE >= 77 & dm$GROUPING_CODE <= 83,]
      sa <- sa[sa$GROUPING_CODE >= 77 & sa$GROUPING_CODE <= 83,]
   }

   # Are we resampling?
   if (resample) {
      dm <- resample(survey_id, dm)
   }

   # Construct a data frame to store results
   smy <- data.frame("species_code"=character(1), "biomass"=numeric(1),
      "boot_mean"=numeric(1), "boot_median"=numeric(1), "boot_lower_ci"=numeric(1),
      "boot_upper_ci"=numeric(1), "boot_re"=numeric(1), "catch_weight"=numeric(1),
      "num_sets"=numeric(1), "num_pos_sets"=numeric(1), "year"=numeric(1),
      "survey_id"=numeric(1), "survey_series_id"=numeric(1),
      "survey"=character(1), stringsAsFactors=FALSE)

   # Perform the bootstrap analysis.  If the data frame has no rows or if the 
   # catch densities or catch counts are all zero (i.e. the species wasn't caught)
   # then return "NA"
   if (nrow(dm) > 0 & !(!is_ll & all(dm$DENSITY_KGPM2==0)) |
                      !(!is_ll & all(dm$DENSITY_PPKM2==0))) {

      # Perform the bootstrap
      boot_obj <- boot(dm, stat, r, strata=dm$GROUPING_CODE, sa=sa, is_ll=is_ll,
         is_sable=is_sable, is_jig=is_jig, is_dog=is_dog, sim="ordinary")

      # Calculate the bias corrected and adjusted confidence limits
      bootci_obj <- boot.ci(boot_obj, type="perc")

      # Save various elements to the results data frame
      smy$species_code <- species_code
      smy[1,2:13] <- c(boot_obj$t0, mean(boot_obj$t), median(boot_obj$t),
         bootci_obj$bca[1,4], bootci_obj$perc[c(4,5)], 
         sd(boot_obj$t) / mean(boot_obj$t),
         ifelse(all(dm$CATCH_WEIGHT==0), sum(dm$CATCH_COUNT, na.rm=TRUE),
            sum(dm$CATCH_WEIGHT, na.rm=TRUE)),
         nrow(dm), 
         max(length(dm$CATCH_WEIGHT[dm$CATCH_WEIGHT > 0]),
            length(dm$CATCH_COUNT[dm$CATCH_COUNT > 0])),
         dm$YEAR[1],
         dm$SURVEY_ID[1], dm$SURVEY_SERIES_ID[1])
      smy[1,14] <- dm$SURVEY_DESC[1]

      # Build a list of final results
      results <- list("stratum_info"=sa, "catch_matrix"=dm, "boot_obj"=boot_obj,
         "bootci_obj"=bootci_obj, "summary"=smy, "species_code"=smy$species_code,
         "survey_id"=survey_id)
      class(results) <- "bootres"
   } else {
      warning(paste("Survey Id", survey_id, "Species", species_code, 
      "produces an empty catch matrix"))
      results <- NA
   }

   # Return the results list
   return(results)

}

####################################################################################
# boot.survey
# A wrapper for boot.species; performs the bootstrap analysis for multiple species
# for a given survey.  The list of species is obtained from a query that returns
# all fish species that were caught in a given survey.
####################################################################################
boot.survey <- function(survey_id, is_ll=FALSE, is_sable=FALSE, is_jig=FALSE, 
   is_dog=FALSE, resample=FALSE) {

   # Get the list of species.  The query used depends on the type of survey.
   if (is_ll) {
      species <- get.sql.data(paste("EXEC proc_longline_survey_fish_species",
         survey_id), "GFBioSQL", as.is=c(T,T))
   }
   else if (is_sable) {
      species <- get.sql.data(paste("EXEC proc_sablefish_survey_fish_species",
         survey_id), "GFBioSQL", as.is=c(T,T))
   }
   else if (is_jig) {
      species <- get.sql.data(paste("EXEC proc_jig_survey_fish_species",
	     survey_id), "GFBioSQL", as.is=c(T,T))
   }
   else if (is_dog) {
      species <- get.sql.data(paste("EXEC proc_longline_survey_fish_species",
         survey_id), "GFBioSQL", as.is=c(T,T))
   }
   else {
      species <- get.sql.data(paste("EXEC proc_survey_fish_species", survey_id),
         "GFBioSQL", as.is=c(T,T))
   }

   # Run the boot.species function for each species in the list, collecting only
   # the summary information from each run.
   res <- boot.species(survey_id, species$SPECIES_CODE[1], is_ll=is_ll,
      is_sable=is_sable, is_jig=is_jig, is_dog=is_dog, resample=resample)$summary
   for (i in 2:nrow(species)) {
   cat(species$SPECIES_CODE[i], "\n")
      br <- boot.species(survey_id, species$SPECIES_CODE[i], is_ll=is_ll,
         is_sable=is_sable, is_jig=is_jig, is_dog=is_dog, resample=resample)
      if (class(br)=="bootres") {
         br <- br$summary
         res <- rbind(res, br)
      }         
   }

   # Return the data frame of summary information.  Each row of this data frame
   # gives the information for a species.
   return(res)

}

####################################################################################
# boot.survey.series.species
# A wrapper for boot.species; performs the bootstrap analysis for a given species
# for every survey in a survey series.
####################################################################################
boot.survey.series.species <- function(species_code, survey_series_id,
   is_ll=FALSE, is_sable=FALSE, is_jig=FALSE, is_dog=FALSE, resample=FALSE) {

   # Get the survey ids for all surveys in the survey series
   sql <- paste("SELECT SURVEY_ID, SURVEY_DESC FROM SURVEY WHERE SURVEY_SERIES_ID =", 
      survey_series_id, "ORDER BY SURVEY_ID")
   sids <- get.sql.data(sql, "GFBioSQL")

   # Build a data frame for the results
   res <- data.frame(matrix(rep(NA,14), nrow=1))
   names(res) <- c("species_code", "biomass", "boot_mean", "boot_median",
      "boot_lower_ci", "boot_upper_ci", "boot_re", "catch_weight", "num_sets",
      "num_pos_sets", "year", "survey_id", "survey_series_id", "survey")

   # For each survey, run the boot.species function for the given species and
   # store each result
   for (i in 1:nrow(sids)) {
      br <- boot.species(sids$SURVEY_ID[i], species_code,
         is_ll=is_ll, is_sable=is_sable, is_jig=is_jig, is_dog=is_dog, resample=resample)
      if (!is.na(br)) {
         br <- br$summary
      } else {
         br <- res[1,]
      }
         res <- rbind(res, br)
   }

   # Remove the top dummy row from the results, add some addition info, and return
   res <- res[2:nrow(res),]
   res$SURVEY_ID = sids$SURVEY_ID
   res$SURVEY_DESC = sids$SURVEY_DESC
   attr(res, "survey_series_id") <- survey_series_id

   return(res)

}

# Saves the results from boot.survey
upload.results <- function(br, desc="") {
   
   boot_id <- get.sql.data(paste("EXEC proc_new_boot_2011 ", br$survey_id[1], 
      ", '", desc, "'", sep=""),
      "GFBioSQL")$BOOT_ID
   
   for (i in 1:nrow(br)) {
      sql <- paste("INSERT INTO BOOT_DETAIL VALUES (", boot_id, ",'",
         br$species_code[i], "',", br$biomass[i], ",", br$boot_mean[i], ",",
         br$boot_median[i], ",", br$boot_lower_ci[i], ",", br$boot_upper_ci[i], ",",
         br$boot_re[i], ",", br$catch_weight[i], ",", br$num_sets[i], ",",
         br$num_pos_sets[i], ",", br$survey_series_id, ",", br$survey_id, ")", sep="")
      get.sql.data(sql, "GFBioSQL")
   }

   cat("Results uploaded with BOOT_ID", boot_id, "\n")
   invisible()

}

boot.survey.series <- function(survey_series_id, is_ll=FALSE, is_sable=FALSE,
   is_jig=FALSE, is_dog=FALSE, resample=FALSE) {

   surveys <- get.sql.data(paste("SELECT SURVEY_ID, SURVEY_DESC FROM SURVEY",
      "WHERE SURVEY_SERIES_ID =", survey_series_id), "GFBioSQL")

   for (i in 1:nrow(surveys)) {
      res <- boot.survey(surveys$SURVEY_ID[i], is_ll=is_ll, is_sable=is_sable,
         is_jig=is_jig, is_dog=is_dog, resample=resample)
      if (class(res)=="data.frame") {
         cat("Survey Id", surveys$SURVEY_ID[i], "Description:",
            surveys$SURVEY_DESC[i], "\n")
         upload.results(res, surveys$SURVEY_DESC[i])
      }
   }

   return(invisible())

}

resample <- function(survey_id, dm) {
   
   strat <- get.sql.data(paste("EXEC proc_new_stratum_info", survey_id),
      "GFBioSQL")
   res <- dm[0,]
   for (i in 1:nrow(strat)) {
      x <- dm[dm$GROUPING_CODE == strat$GROUPING_CODE[i],]
      x <- x[sample(1:nrow(x), size=strat$TARGET_ALLOCATION[i], replace=TRUE),]
      res <- rbind(res, x)
   }
   rownames(res) <- 1:nrow(res)
   return(res)
}
