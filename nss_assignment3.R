#########################################################################################
###NSS 77th ROUND: DEBT AND INVESTMENT SURVEY SCHEDULE 18.2: HOUSEHOLD LEVEL ANALYSIS####
#########################################################################################

#Loading Required Packages

library(readr)
library(dplyr)
library(tidyr)
library(rmarkdown)
library(ggplot2)
library(AER)
library(lmtest)
library(gridExtra)

#Level 1 Data Extraction

level_one <- read_fwf(file = "NSS/r77182v1L01.txt", 
                      fwf_cols(centrecd = c(1,3), fsuslno = c(4,8),
                               round = c(9,10), schedule = c(11,13),
                               sample = c(14,14), sector = c(15,15),
                               region = c(16, 18), district = c(19,20),
                               stratum = c(21,22), sub_stratum = c(23,24),
                               sub_round = c(25,25), fod = c(26,29),
                               second_stage_stratum = c(30,30), hhno = c(31,32),
                               visit = c(33,33), level = c(34, 35),
                               filler = c(36, 40), informant = c(41, 42),
                               response_code = c(43, 43), survey_code = c(44,44),
                               casualty = c(45,45), empcode1 = c(46,49),
                               empcode2  = c(50,53), empcode3 = c(54,57),
                               survey_date = c(58,63), despatch = c(64,69),
                               canvass = c(70,72), no_of_inv = c(73,73),
                               remarks1 = c(74,74), remarks2 = c(75,75),
                               remarks3 = c(76,76), remarks4 = c(77,77),
                               total_insurance_policies = c(78,80), blank = c(81,126),
                               nsc = c(127,129), mlt = c(130,139)),
                      col_types = cols(centrecd = col_character(), fsuslno = col_character(),
                                       round = col_character(), schedule = col_character(),
                                       sample = col_character(), sector = col_integer(),
                                       region = col_character(), district = col_character(),
                                       stratum = col_character(), sub_stratum = col_character(),
                                       sub_round = col_character(), fod = col_character(),
                                       second_stage_stratum = col_character(), hhno = col_character(),
                                       visit = col_integer(), level = col_character(),
                                       filler = col_character(), informant = col_character(),
                                       response_code = col_character(), survey_code = col_character(),
                                       casualty = col_character(), empcode1 = col_character(),
                                       empcode2 = col_character(), empcode3 = col_character(),
                                       survey_date = col_date(), despatch = col_date(),
                                       canvass = col_character(), no_of_inv = col_character(),
                                       remarks1 = col_character(), remarks2 = col_character(),
                                       remarks3 = col_character(),  remarks4 = col_character(),
                                       total_insurance_policies = col_integer(), blank = col_character(),
                                       nsc = col_number(),  mlt = col_number()))


#Level 3 Data Extraction


level_three = read_fwf(file = "NSS/r77182v1L03.txt",
                       fwf_cols(common_id = c(1,33), level = c(34,35),
                                filler = c(36,40), hhsize = c(41,43),
                                religion = c(44,44), social_group = c(45,45),
                                hhtype = c(46,46), homestead_land_area = c(47,52),
                                owned_and_possessed = c(53,58), leased_in = c(59,64),
                                otherwise_possessed = c(65,70), leased_out = c(71,76),
                                agri_last_365 = c(77,77), area_operated_by_hh = c(78,83),
                                kitchen_garden = c(84,89), pmjjby = c(90,91),
                                pmsby = c(92,93), apy = c(94,95), blank = c(96,126),
                                nsc = c(127,129), mlt = c(130,139)),
                       col_types = cols(common_id = col_character(), level = col_character(),
                                        filler = col_character(), hhsize = col_character(),
                                        religion = col_character(), social_group = col_character(),
                                        hhtype = col_character(), homestead_land_area = col_character(),
                                        owned_and_possessed = col_character(), leased_in = col_character(),
                                        otherwise_possessed = col_character(), leased_out = col_character(),
                                        agri_last_365 = col_character(), area_operated_by_hh = col_character(),
                                        kitchen_garden = col_character(), pmjjby = col_character(),
                                        pmsby = col_character(), apy = col_character(), blank = col_character(),
                                        nsc = col_number(), mlt = col_number()))



#Level 4 Data Extraction

level_four = read_fwf(file = "NSS/r77182v1L04.txt",
                      fwf_cols(common_id = c(1,33), level = c(34,35),
                               filler = c(36,40), out_of_purchase = c(41,50),
                               imputed_home_stock = c(51,60), imputed_wages_in_kind = c(61,70),
                               durables_365 = c(71,80), total_monthly_expenditure = c(81,90),
                               blank = c(91,126), nsc = c(127,129), mlt = c(130,139)),
                      col_types = cols(common_id = col_character(), level = col_character(),
                                       filler = col_character(), out_of_purchase = col_character(),
                                       imputed_home_stock = col_character(), imputed_wages_in_kind = col_character(),
                                       durables_365 = col_character(), total_monthly_expenditure = col_character(),
                                       blank = col_character(), nsc = col_number(), mlt = col_number()))
#Level 5 Data Extraction

level_five = read_fwf(file = "NSS/r77182v1L05.txt",
                      fwf_cols(common_id = c(1,33), level = c(34,35),
                               filler = c(36,38), plotsn = c(39,40),
                               land_type = c(41,42), land_owned_area = c(43,48),
                               land_owned_value = c(49,60), female_ownership = c(61,61),
                               female_share_area = c(62,67), blank = c(68,126),
                               nsc = c(127,129), mlt = c(130,139)),
                      col_types = cols(common_id = col_character(), level = col_character(),
                                       filler = col_character(), plotsn = col_character(),
                                       land_type = col_character(), land_owned_area = col_character(),
                                       land_owned_value = col_character(), female_ownership = col_character(),
                                       female_share_area = col_character(), blank = col_character(),
                                       nsc = col_number(), mlt = col_number()))

#Creating Common ID for Level 1 (The Debt and Investment Survey uses the Common ID for ALL OTHER LEVELS)                     

level_one$common_id <- paste0(level_one$centrecd, level_one$fsuslno, level_one$round, level_one$schedule, level_one$sample, level_one$sector, level_one$region, level_one$district, level_one$stratum, level_one$sub_stratum, level_one$sub_round, level_one$fod, level_one$second_stage_stratum, level_one$hhno, level_one$visit)
                       
#Merging Raw Data

merged_raw_data_13 <- merge(level_one, level_three, by = "common_id", all = T )
merged_raw_data_134 <- merge(merged_raw_data_13, level_four, by = "common_id", all = T)                       
full_merged_raw_data <- merge(merged_raw_data_134, level_five, by = "common_id", all = T)                       

                       
#Creating Multiplier Weights for Each Level

level_one$weight <- level_one$mlt/100
level_three$weight <- level_three$mlt/100
level_four$weight <- level_four$mlt/100
level_five$weight <- level_five$mlt/100

####################################################################################################################
###How does female financial empowerment measured in terms of land ownership impact mpce components and land use?###
####################################################################################################################


selected_data <- full_merged_raw_data%>%dplyr::select(common_id, centrecd, fsuslno, region, district, hhno, hhsize, religion, area_operated_by_hh, out_of_purchase, imputed_home_stock, imputed_wages_in_kind, durables_365, total_monthly_expenditure, land_owned_area, land_owned_value, female_ownership, social_group)
selected_data$hhsize <- as.numeric(selected_data$hhsize)
selected_data$area_operated_by_hh <-as.numeric(selected_data$area_operated_by_hh)
selected_data$out_of_purchase <-as.numeric(selected_data$out_of_purchase)
selected_data$imputed_home_stock <- as.numeric(selected_data$imputed_home_stock)
selected_data$imputed_wages_in_kind <- as.numeric(selected_data$imputed_wages_in_kind)
selected_data$durables_365 <- as.numeric(selected_data$durables_365)
selected_data$total_monthly_expenditure <- as.numeric(selected_data$total_monthly_expenditure)
selected_data$land_owned_area <- as.numeric(selected_data$land_owned_area)
selected_data$land_owned_value <- as.numeric(selected_data$land_owned_value)
selected_data$female_ownership <- as.numeric(selected_data$female_ownership)
selected_data$female_ownership <- replace(selected_data$female_ownership, selected_data$female_ownership == 2, 0)
selected_data$social_group <- as.numeric(selected_data$social_group)

#Reference Category for Female Ownership Variable is "No Land Owned By Female HH Members"

########################
###Data Visualisation###
########################

#Creating the MPCE Variable

selected_data <- selected_data%>%dplyr::mutate(mpce = total_monthly_expenditure/hhsize)
selected_data <- selected_data%>%dplyr::filter(!is.na(female_ownership))

#Plotting National MPCE Distribution

mpce_aggregate <- ggplot(selected_data) + geom_density(aes(mpce), size = 1, fill = "aquamarine4", alpha = 0.4) + scale_x_continuous(limits = c(0,20000)) + theme_gray() + xlab("Monthly Per Capita Expenditure") + ylab("Density") + labs(caption = "Data: NSS 77") + ggtitle("National MPCE Distribution")
mpce_aggregate

#Plotting National MPCE by Caste

caste_labels = c("1" = "ST", "2"="SC", "3"="OBC", "9"="Others")

mpce_caste <- ggplot(selected_data) + geom_density(aes(mpce), size = 1, fill = "aquamarine4", alpha = 0.4) + facet_grid(as.factor(social_group)~., labeller = as_labeller(caste_labels)) + scale_x_continuous(limits = c(0,5000)) + theme_gray() + xlab("Monthly Per Capita Expenditure") + ylab("Density") + labs(caption = "Data: NSS 77") + ggtitle("National MPCE Distribution")
mpce_caste

#Plotting Aggregate MPCE with Land Ownership

mpce_land <- ggplot(selected_data) + geom_jitter(aes(mpce, land_owned_area)) +  scale_x_continuous(limits = c(0, 6000)) + scale_y_continuous(limits = c(0, 100)) + xlab('Aggregate MPCE') + ylab("Land Area Owned (In Acres)") + ggtitle("MPCE According to Land Area Owned") + labs(caption = "Data: NSS 77") + theme_gray()
mpce_land

#Plotting Aggregate MPCE by Female Land Owner Dummy

mpce_aggregate_female <- ggplot(selected_data, aes(as.factor(female_ownership), mpce, fill = as.factor(female_ownership))) + geom_violin(alpha = 0.5) +  scale_y_continuous(limits = c(0,7500)) + xlab("Female Ownership of HH Land") + ylab("MPCE") + ggtitle("Aggregate MPCE by Female Land Ownership") + labs(caption = "Data: NSS 77") + theme_gray() + scale_fill_discrete(name = "Female Ownership", labels = c("No Ownership", "Some Female Ownership")) + stat_summary(fun = "mean", geom = "point") 
mpce_aggregate_female

#Plotting Household Purpose Expenditure Per Capita wrt Female Land Ownership

selected_data <- selected_data%>%dplyr::mutate(mpce_out_of_purchase = out_of_purchase/hhsize)
mpce_out_of_purchase_female <- ggplot(selected_data, aes(as.factor(female_ownership), mpce_out_of_purchase, fill = as.factor(female_ownership))) + geom_violin(alpha = 0.5) +  scale_y_continuous(limits = c(0,7500)) + xlab("Female Ownership of HH Land") + ylab("MPCE: Household Expenditure") + ggtitle("HH Purpose MPCE by Female Land Ownership") + labs(caption = "Data: NSS 77") + theme_gray() + scale_fill_discrete(name = "Female Ownership", labels = c("No Ownership", "Some Female Ownership")) + stat_summary(fun = "mean", geom = "point") 
mpce_out_of_purchase_female

#Plotting Imputed Homegrown Stock Expenditure wrt Female Land Ownership

selected_data <- selected_data%>%dplyr::mutate(mpce_homegrown_stock = imputed_home_stock/hhsize)
mpce_home_stock_female <- ggplot(selected_data, aes(as.factor(female_ownership), mpce_homegrown_stock, fill = as.factor(female_ownership))) + geom_violin(alpha = 0.5) +  scale_y_continuous(limits = c(0,1000)) + xlab("Female Ownership of HH Land") + ylab("MPCE: Homegrown Stock") + ggtitle("Imputed Home Stock MPCE by Female Land Ownership") + labs(caption = "Data: NSS 77") + theme_gray() + scale_fill_discrete(name = "Female Ownership", labels = c("No Ownership", "Some Female Ownership")) + stat_summary(fun = "mean", geom = "point") 
mpce_home_stock_female
                                               
mpce_categories_female_graph <- grid.arrange(mpce_aggregate_female, mpce_out_of_purchase_female, mpce_home_stock_female)
mpce_categories_female_graph

#What time of land holdings are characterised by female land ownership?

land_area_female <- ggplot(selected_data, aes(as.factor(female_ownership), land_owned_area, fill = as.factor(female_ownership))) + geom_violin(alpha = 0.5) +  scale_y_continuous(limits = c(0,2)) + xlab("Female Ownership of HH Land") + ylab("Area of HH Land Owned") + ggtitle("Land Ownership by Female Land Ownership") + labs(caption = "Data: NSS 77") + theme_gray() + scale_fill_discrete(name = "Female Ownership", labels = c("No Ownership", "Some Female Ownership")) + stat_summary(fun = "mean", geom = "point") 
land_area_female


land_value_female <- ggplot(selected_data, aes(as.factor(female_ownership), land_owned_value, fill = as.factor(female_ownership))) + geom_violin(alpha = 0.5) +  scale_y_continuous(limits = c(0,1000000)) + xlab("Female Ownership of HH Land") + ylab("Value of HH Land Owned") + ggtitle("Land Value by Female Land Ownership") + labs(caption = "Data: NSS 77") + theme_gray() + scale_fill_discrete(name = "Female Ownership", labels = c("No Ownership", "Some Female Ownership")) + stat_summary(fun = "mean", geom = "point") 
land_value_female

land_characteristics_by_female <- grid.arrange(land_area_female, land_value_female)


#Is female land ownership more likely when households work on the land themselves?

hh_work_land_female <- ggplot(selected_data, aes(as.factor(female_ownership), area_operated_by_hh, fill = as.factor(female_ownership))) + geom_violin(alpha = 0.5) +  scale_y_continuous(limits = c(0,4)) + xlab("Female Ownership of HH Land") + ylab("Area Operated by HH") + ggtitle("HH Land Use by Female Land Ownership") + labs(caption = "Data: NSS 77") + theme_gray() + scale_fill_discrete(name = "Female Ownership", labels = c("No Ownership", "Some Female Ownership")) + stat_summary(fun = "mean", geom = "point") 
hh_work_land_female


#Does land ownership translate into homegrown income?

land_owners <- selected_data%>%dplyr::filter(land_owned_value != 0)

homegrown_land <- ggplot(land_owners, aes(land_owned_value, mpce_homegrown_stock)) + geom_point() + scale_x_continuous(limits = c(0, 250000000)) + scale_y_continuous(limits = c(0, 6000)) + theme_gray() + xlab("Value of Land Owned") + ylab("MPCE: Homegrown Stock") + ggtitle("Land Ownership and Homegrown Consumption") + labs(caption = "Data: NSS 77")
homegrown_land


##########################
###Econometric Analysis###
##########################

#Creating Social Group Dummy Variables. Others is a Reference Category

selected_data <- selected_data%>%dplyr::mutate(sc_dummy = social_group)
selected_data$sc_dummy <- replace(selected_data$sc_dummy, selected_data$sc_dummy != 2, 0)
selected_data <- selected_data%>%dplyr::mutate(st_dummy = social_group)
selected_data$st_dummy <- replace(selected_data$st_dummy, selected_data$st_dummy != 1, 0)
selected_data <- selected_data%>%dplyr::mutate(obc_dummy = social_group)
selected_data$obc_dummy <- replace(selected_data$obc_dummy, selected_data$obc_dummy != 3, 0)

#FOCUS ON MPCE COMPONENTS

#Regression on Aggregate MPCE: Invoke CLT owing to large sample for normality 

reg1 <- lm(data = selected_data, mpce ~ female_ownership + area_operated_by_hh + hhsize + female_ownership*area_operated_by_hh + sc_dummy + st_dummy + obc_dummy + female_ownership*st_dummy + female_ownership*sc_dummy + female_ownership*obc_dummy)
summary(reg1)

reg2 <- lm(data = selected_data, mpce_out_of_purchase ~ female_ownership + area_operated_by_hh + hhsize + female_ownership*area_operated_by_hh + sc_dummy + st_dummy + obc_dummy + female_ownership*st_dummy + female_ownership*sc_dummy + female_ownership*obc_dummy)
summary(reg2)

reg3 <- lm(data = selected_data, mpce_homegrown_stock ~ female_ownership + area_operated_by_hh + hhsize + female_ownership*area_operated_by_hh + sc_dummy + st_dummy + obc_dummy + female_ownership*st_dummy + female_ownership*sc_dummy + female_ownership*obc_dummy)
summary(reg3)

#FOCUS ON DURABLES and LAND VALUE

reg4 <- lm(data = selected_data, durables_365 ~ female_ownership + area_operated_by_hh + hhsize + female_ownership*area_operated_by_hh + sc_dummy + st_dummy + obc_dummy + female_ownership*st_dummy + female_ownership*sc_dummy + female_ownership*obc_dummy)
summary(reg4)

reg5 <- lm(data = selected_data, land_owned_value ~ female_ownership + area_operated_by_hh + hhsize + female_ownership*area_operated_by_hh + sc_dummy + st_dummy + obc_dummy + female_ownership*st_dummy + female_ownership*sc_dummy + female_ownership*obc_dummy)
summary(reg5)

