#=================================================================================================================
################################      Set Variables    ###########################################################
#=================================================================================================================
min_length <- 60 #can set a minimum size for the fish if worried the minimum size fish in data are errors
max_length <- 600 #max is 1557
size_cat <- 'adult' #size category: adult (>179), juvenile_LY (>130mm), juvenile_YOY (i.e. <130)

#specify quarters, leave empty "c()" if you don't want quarters from that year
quarters_currentYear <- c('Q3')
quarters_lagYear <- c()

#set work directory
setwd("C:\\Users\\pwm5174\\OneDrive - The Pennsylvania State University\\individual_bass_model")



#=================================================================================================================
#=================================================================================================================

#==============================================
#==============================================
# Load libraries and load and adjust data
#==============================================
#==============================================

#load libraries 
library(rstanarm)
library(dplyr)

#----------------------------------------
# Smallmouth data
#----------------------------------------

#Raw smallmouth bass data
smb_dat <- read.csv('smb_raw_data.csv', stringsAsFactors = TRUE, 
                    na.strings= c("NA", ""))
#only consider fall data
smb_dat <- smb_dat[which(smb_dat$season=='Fall'),]
#only consider fish of a certain length
smb_dat <- smb_dat[which(smb_dat$length_mm>=min_length),]
smb_dat <- smb_dat[which(smb_dat$length_mm<=max_length),]

# remove samples with no effort data and start_comid
smb_dat <- smb_dat %>% 
  #mutate(date = mdy_hm(date)) %>% 
  filter(!is.na(effort_hours)) %>% 
  filter(!is.na(fish_COMID_start)) %>% 
  filter(!is.na(length_mm))

# Remove some columns
smb_dat <- smb_dat %>% 
  select(-source_file, -fish_COMID_stop)

# Correct effort_hours - it's in minutes right now
smb_dat <- smb_dat %>% 
  mutate(effort_hours = effort_sec/3600)

# Create size classes of catch
# adult: >179 mm
# previous year juvenile (juvenile_LY): 130-179 mm
# current year juvenile: < 130 mm
smb_dat <- smb_dat %>% 
  mutate(size_cat = as.factor(ifelse(length_mm > 179, 'adult', 
                                     ifelse(length_mm > 130, 'juvenile_LY', 'juvenile_YOY'))))
#summary(smb_dat)


#----------------------------------------
# Quarterly AccWW
#----------------------------------------
# Quaterty accww data
Qaccww_dat <- read.csv('quarterly_accww.csv', stringsAsFactors = TRUE, 
                       na.strings= c("NA", ""))
# add station_name column
names(Qaccww_dat)[1] <- 'station_name'




#======================================================
#======================================================
# Construct data for model fitting
#======================================================
#======================================================


#------------------------------------------
# Construct catch per effort (CPE) data
#------------------------------------------

#setup data.frame with total catch per site per year per fish size
total_cpe <- data.frame()

#loop over stations
for(i in 1:length(unique(smb_dat$station_name))){
 
  #smb data for current site for all years
  smb.i <- smb_dat[which(smb_dat$station_name==unique(smb_dat$station_name)[i]),]
  
  #loop over years
  for(j in 1:length(unique(smb.i$year))){
    
    #smb data for current site and year
    smb.ij <- smb.i[which(smb.i$year==unique(smb.i$year)[j]),]
    
    #total catch for fish size category at current site/year
    total_catch.ij <- nrow(smb.ij[which(smb.ij$size_cat==size_cat),])
    #total_catch.ij <- nrow(smb.ij)
    
    #add to data.frame
    total_cpe <- data.frame(rbind(total_cpe,
                                  data.frame(station_name=smb.i$station_name[1],
                                             year=smb.ij$year[1],
                                             total_catch=total_catch.ij,
                                             effort=mean(smb.ij$effort_hours), #always use total effort
                                             cpe=total_catch.ij/mean(smb.ij$effort_hours))))
  }
}
total_cpe <- total_cpe[order(total_cpe$station_name,total_cpe$year),]
rownames(total_cpe) <- 1:nrow(total_cpe)


#------------------------------------
# Add AccWW data to CPE data
#------------------------------------
add_cols <- data.frame()

#quarterly data
qdat <- Qaccww_dat

for(i in 1:nrow(total_cpe)){
  
  #current station_name and year
  sn.i <- total_cpe$station_name[i]
  year.i <- total_cpe$year[i]
  
  #pick indexes for current site and year
  index.i <- c()
  if(length(quarters_currentYear)>0){
    #current year indexes
    index.i <- c(index.i, which(qdat$station_name==as.vector(sn.i) &
                                  qdat$year==as.vector(year.i) &
                                  qdat$quarter %in% quarters_currentYear))
  }
  if(length(quarters_lagYear)>0){
    #lagged year indexes
    index.i <- c(index.i,
                 which(qdat$station_name==as.vector(sn.i) &
                         qdat$year==(as.vector(year.i)-1) &
                         qdat$quarter %in% quarters_lagYear))
  }
  
  if(!identical(index.i,integer(0))){
    #add accww_dat data
    add_cols <- data.frame(rbind(add_cols,
                                 data.frame(qdat[index.i,c(2,3,5:10)][1,],
                                            median_accww_per=mean(qdat[index.i,11]),
                                            mean_accww_per=mean(qdat[index.i,12]))))
  }else{
    #add NA's if there wasn't for station and year
    add_cols <- data.frame(rbind(add_cols,
                                 rep(NA,12)))
  }
}
#add new columns to the cpe data
cpe_dat <- cbind(total_cpe,add_cols)


#add random factors colums
cpe_dat$site_ran <- match(cpe_dat$station_name,sort(unique(cpe_dat$station_name)))
cpe_dat$year_ran <- match(cpe_dat$year,sort(unique(cpe_dat$year)))


#Remove observations with no accww data
cpe_dat <- cpe_dat[-which(is.na(cpe_dat$median_accww_per)),]

# Prepare predictors
# Transform, as needed, and scale
cpe_dat$median_accww_z <- as.numeric(scale(cpe_dat$median_accww_per))
cpe_dat$mean_accww_z <- as.numeric(scale(cpe_dat$mean_accww_per))

# drop sites from the factor levels that don't have any data anymore
cpe_dat$station_name <- droplevels(cpe_dat$station_name)

##plot catch
#plot(cpe_dat$median_accww_per,log(cpe_dat$total_catch))






#==============================================
#==============================================
# Model fitting
#==============================================
#==============================================

options(mc.cores = parallel::detectCores())

# Fit negative binomial model
m2 <- stan_glmer(formula = total_catch ~ 1 + median_accww_z +
                   (1 | year_ran) + (1|site_ran), family = neg_binomial_2(link='log'),
                 offset = log(effort),
                 data = cpe_dat,
                 seed = 349, iter = 5000, chains = 3)
print(m2, digits=3)

# Overall intercept
mu_a_sims <- as.matrix(m2,
                       pars = "(Intercept)")
#AccWW beta
b_accww <- as.matrix(m2,
                     pars = "median_accww_z")
quantile(b_accww, c(0.025, 0.5, 0.975)) # 95% CRI
quantile(b_accww, c(0.05, 0.5, 0.95)) # 90% CRI
mean(b_accww <0) #posterior probability AccWW beta is negative



