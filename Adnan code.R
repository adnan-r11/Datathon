setwd("C:/Users/adnan/Datathon")

install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("htmlwidgets")
install.packages("tidyr")
install.packages("lubridate")
install.packages("readr")
install.packages("plyr")

library(dplyr)
library("tidyverse")
library("ggplot2")
library("htmlwidgets")
library("tidyr")
library("lubridate")
library(readr)
library(plyr)

# "Import Global Mobility Data Set"

Global.Mobility.Report.clean <- read.csv("C:/Users/adnan/Datathon/Data/Global Mobility Report clean.csv")

# "Removed all data from 17-8 due to missing data sets from majority of countries"

GM.ultra.clean <- na.omit(Global.Mobility.Report.clean)

# Remove columns 5 to 7 as we are not using those datasets

GM.ultra.clean <- GM.ultra.clean[,-c(5:7)]


# "Find the number of countries in whole data frame, helping with next lot of sorting"
country_count <-subset(GM.ultra.clean, date == "2020-02-15")


no_countries = 121

# "Organise each countrty for its own data frame"

ANG_country <- subset(GM.ultra.clean, country_region == "Angola")


####Rest of Countries####

ARG_country <- subset(GM.ultra.clean, country_region == "Argentina")
AUS_country <- subset(GM.ultra.clean, country_region == "Australia")
AUT_country <- subset(GM.ultra.clean, country_region == "Austria")
BHR_country <- subset(GM.ultra.clean, country_region == "Bahrain")
BAN_country <- subset(GM.ultra.clean, country_region == "Bangladesh")
BRB_country <- subset(GM.ultra.clean, country_region == "Barbados")
BRL_country <- subset(GM.ultra.clean, country_region == "Belarus")
BEL_country <- subset(GM.ultra.clean, country_region == "Belgium")
BEN_country <- subset(GM.ultra.clean, country_region == "Benin")
BOL_country <- subset(GM.ultra.clean, country_region == "Bolivia")
BIH_country <- subset(GM.ultra.clean, country_region == "Bosnia and Herzegovina")
BOT_country <- subset(GM.ultra.clean, country_region == "Botswana")
BRA_country <- subset(GM.ultra.clean, country_region == "Brazil")
BUL_country <- subset(GM.ultra.clean, country_region == "Bulgaria")
BFA_country <- subset(GM.ultra.clean, country_region == "Burkina Faso")
CAM_country <- subset(GM.ultra.clean, country_region == "Cambodia")
CMR_country <- subset(GM.ultra.clean, country_region == "Cameroon")
CAN_country <- subset(GM.ultra.clean, country_region == "Canada")
CHI_country <- subset(GM.ultra.clean, country_region == "Chile")
COL_country <- subset(GM.ultra.clean, country_region == "Colombia")
CRC_country <- subset(GM.ultra.clean, country_region == "Costa Rica")
CRO_country <- subset(GM.ultra.clean, country_region == "Croatia")
CZE_country <- subset(GM.ultra.clean, country_region == "Czechia")
CIV_country <- subset(GM.ultra.clean, country_region == "Côte d'Ivoire")
DEN_country <- subset(GM.ultra.clean, country_region == "Denmark")
DOM_country <- subset(GM.ultra.clean, country_region == "Dominican Republic")
ECU_country <- subset(GM.ultra.clean, country_region == "Ecuador")
EGY_country <- subset(GM.ultra.clean, country_region == "Egypt") 
SLV_country <- subset(GM.ultra.clean, country_region == "El Salvador")
EST_country <- subset(GM.ultra.clean, country_region == "Estonia")
FIG_country <- subset(GM.ultra.clean, country_region == "Fiji")
FIN_country <- subset(GM.ultra.clean, country_region == "Finland")
FRA_country <- subset(GM.ultra.clean, country_region == "France")
GAB_country <- subset(GM.ultra.clean, country_region == "Gabon")
GER_country <- subset(GM.ultra.clean, country_region == "Germany")
GHA_country <- subset(GM.ultra.clean, country_region == "Ghana")
GRE_country <- subset(GM.ultra.clean, country_region == "Greece")
GUA_country <- subset(GM.ultra.clean, country_region == "Guatemala")
HAI_country <- subset(GM.ultra.clean, country_region == "Haiti")
HON_country <- subset(GM.ultra.clean, country_region == "Honduras")
HKG_country <- subset(GM.ultra.clean, country_region == "Hong Kong")
HUN_country <- subset(GM.ultra.clean, country_region == "Hungary")
IND_country <- subset(GM.ultra.clean, country_region == "India")
IDN_country <- subset(GM.ultra.clean, country_region == "Indonesia")
IRG_country <- subset(GM.ultra.clean, country_region == "Iraq")
IRL_country <- subset(GM.ultra.clean, country_region == "Ireland")
ISR_country <- subset(GM.ultra.clean, country_region == "Israel")
ITA_country <- subset(GM.ultra.clean, country_region == "Italy")
JAM_country <- subset(GM.ultra.clean, country_region == "Jamaica")
JAP_country <- subset(GM.ultra.clean, country_region == "Japan")
JOR_country <- subset(GM.ultra.clean, country_region == "Jordan")
KAZ_country <- subset(GM.ultra.clean, country_region == "Kazakhstan")
KEN_country <- subset(GM.ultra.clean, country_region == "Kenya")
KUW_country <- subset(GM.ultra.clean, country_region == "Kuwait")
KGZ_country <- subset(GM.ultra.clean, country_region == "Kyrgyzstan")
LAO_country <- subset(GM.ultra.clean, country_region == "Laos")
LVA_country <- subset(GM.ultra.clean, country_region == "Latvia")
LBN_country <- subset(GM.ultra.clean, country_region == "Lebanon")
LIY_country <- subset(GM.ultra.clean, country_region == "Libya")
LTU_country <- subset(GM.ultra.clean, country_region == "Lithuania")
LUX_country <- subset(GM.ultra.clean, country_region == "Luxembourg")
MAS_country <- subset(GM.ultra.clean, country_region == "Malaysia")
MLT_country <- subset(GM.ultra.clean, country_region == "Malta")
MTN_country <- subset(GM.ultra.clean, country_region == "Mauritius")
MEX_country <- subset(GM.ultra.clean, country_region == "Mexico")
MDA_country <- subset(GM.ultra.clean, country_region == "Moldova")
MNG_country <- subset(GM.ultra.clean, country_region == "Mongolia")
MAR_country <- subset(GM.ultra.clean, country_region == "Morocco")
MOZ_country <- subset(GM.ultra.clean, country_region == "Mozambique")
MYA_country <- subset(GM.ultra.clean, country_region == "Myanmar (Burma)")
NAM_country <- subset(GM.ultra.clean, country_region == "Namibia")
NEP_country <- subset(GM.ultra.clean, country_region == "Nepal")
NED_country <- subset(GM.ultra.clean, country_region == "Netherlands")
NZL_country <- subset(GM.ultra.clean, country_region == "New Zealand")
NCA_country <- subset(GM.ultra.clean, country_region == "Nicaragua")
NIG_country <- subset(GM.ultra.clean, country_region == "Niger")
NGA_country <- subset(GM.ultra.clean, country_region == "Nigeria")
MKD_country <- subset(GM.ultra.clean, country_region == "North Macedonia")
NOR_country <- subset(GM.ultra.clean, country_region == "Norway")
OMA_country <- subset(GM.ultra.clean, country_region == "Oman")
PAK_country <- subset(GM.ultra.clean, country_region == "Pakistan")
PAN_country <- subset(GM.ultra.clean, country_region == "Panama")
PAR_country <- subset(GM.ultra.clean, country_region == "Paraguay")
PER_country <- subset(GM.ultra.clean, country_region == "Peru")
PHI_country <- subset(GM.ultra.clean, country_region == "Philippines")
POL_country <- subset(GM.ultra.clean, country_region == "Poland")
POR_country <- subset(GM.ultra.clean, country_region == "Portugal")
PUR_country <- subset(GM.ultra.clean, country_region == "Puerto Rico")
QAT_country <- subset(GM.ultra.clean, country_region == "Qatar")
ROU_country <- subset(GM.ultra.clean, country_region == "Romania")
RUS_country <- subset(GM.ultra.clean, country_region == "Russia")
RWA_country <- subset(GM.ultra.clean, country_region == "Rwanda")
KSA_country <- subset(GM.ultra.clean, country_region == "Saudi Arabia")
SEN_country <- subset(GM.ultra.clean, country_region == "Senegal")
SIN_country <- subset(GM.ultra.clean, country_region == "Singapore")
SVK_country <- subset(GM.ultra.clean, country_region == "Slovakia")
SVN_country <- subset(GM.ultra.clean, country_region == "Slovenia")
RSA_country <- subset(GM.ultra.clean, country_region == "South Africa")
KOR_country <- subset(GM.ultra.clean, country_region == "South Korea")
ESP_country <- subset(GM.ultra.clean, country_region == "Spain")
SRI_country <- subset(GM.ultra.clean, country_region == "Sri Lanka")
SWE_country <- subset(GM.ultra.clean, country_region == "Sweden")
SUI_country <- subset(GM.ultra.clean, country_region == "Switzerland")
TPE_country <- subset(GM.ultra.clean, country_region == "Taiwan")
TJI_country <- subset(GM.ultra.clean, country_region == "Tajikistan")
TAN_country <- subset(GM.ultra.clean, country_region == "Tanzania")
THA_country <- subset(GM.ultra.clean, country_region == "Thailand")
BAH_country <- subset(GM.ultra.clean, country_region == "The Bahamas")
TOG_country <- subset(GM.ultra.clean, country_region == "Togo")
TRI_country <- subset(GM.ultra.clean, country_region == "Trinidad and Tobago")
TUR_country <- subset(GM.ultra.clean, country_region == "Turkey")
UGA_country <- subset(GM.ultra.clean, country_region == "Uganda")
UKR_country <- subset(GM.ultra.clean, country_region == "Ukraine")
UAE_country <- subset(GM.ultra.clean, country_region == "United Arab Emirates")
UK__country <- subset(GM.ultra.clean, country_region == "United Kingdom")
USA_country <- subset(GM.ultra.clean, country_region == "United States")
VEN_country <- subset(GM.ultra.clean, country_region == "Venezuela")
VIE_country <- subset(GM.ultra.clean, country_region == "Vietnam")
YEM_country <- subset(GM.ultra.clean, country_region == "Yemen")
ZAM_country <- subset(GM.ultra.clean, country_region == "Zambia")
ZIM_country <- subset(GM.ultra.clean, country_region == "Zimbabwe")

#### end ####

# Import all countries' datasets into a single list
country_list<-list(ANG_country, AUS_country,AUT_country #start####
                    ,BHR_country 
                    ,BAN_country 
                    ,BRB_country 
                    ,BRL_country 
                    ,BEL_country 
                    ,BEN_country 
                    ,BOL_country 
                    ,BIH_country 
                    ,BOT_country 
                    ,BRA_country 
                    ,BUL_country 
                    ,BFA_country 
                    ,CAM_country 
                    ,CMR_country 
                    ,CAN_country 
                    ,CHI_country 
                    ,COL_country 
                    ,CRC_country 
                    ,CRO_country 
                    ,CZE_country 
                    ,CIV_country 
                    ,DEN_country 
                    ,DOM_country 
                    ,ECU_country 
                    ,EGY_country 
                    ,SLV_country 
                    ,EST_country 
                    ,FIG_country 
                    ,FIN_country 
                    ,FRA_country 
                    ,GAB_country 
                    ,GER_country 
                    ,GHA_country 
                    ,GRE_country 
                    ,GUA_country 
                    ,HAI_country 
                    ,HON_country 
                    ,HKG_country 
                    ,HUN_country 
                    ,IND_country 
                    ,IDN_country 
                    ,IRG_country 
                    ,IRL_country 
                    ,ISR_country 
                    ,ITA_country 
                    ,JAM_country 
                    ,JAP_country 
                    ,JOR_country 
                    ,KAZ_country 
                    ,KEN_country 
                    ,KUW_country 
                    ,KGZ_country 
                    ,LAO_country 
                    ,LVA_country 
                    ,LBN_country 
                    ,LIY_country 
                    ,LTU_country 
                    ,LUX_country 
                    ,MAS_country 
                    ,MLT_country 
                    ,MTN_country 
                    ,MEX_country 
                    ,MDA_country 
                    ,MNG_country 
                    ,MAR_country 
                    ,MOZ_country 
                    ,MYA_country 
                    ,NAM_country 
                    ,NEP_country 
                    ,NED_country 
                    ,NZL_country 
                    ,NCA_country 
                    ,NIG_country 
                    ,NGA_country 
                    ,MKD_country 
                    ,NOR_country 
                    ,OMA_country 
                    ,PAK_country 
                    ,PAN_country 
                    ,PAR_country 
                    ,PER_country 
                    ,PHI_country 
                    ,POL_country 
                    ,POR_country 
                    ,PUR_country 
                    ,QAT_country 
                    ,ROU_country 
                    ,RUS_country 
                    ,RWA_country 
                    ,KSA_country 
                    ,SEN_country 
                    ,SIN_country 
                    ,SVK_country 
                    ,SVN_country 
                    ,RSA_country 
                    ,KOR_country 
                    ,ESP_country 
                    ,SRI_country 
                    ,SWE_country 
                    ,SUI_country 
                    ,TPE_country 
                    ,TJI_country 
                    ,TAN_country 
                    ,THA_country 
                    ,BAH_country 
                    ,TOG_country 
                    ,TRI_country 
                    ,TUR_country 
                    ,UGA_country 
                    ,UKR_country 
                    ,UAE_country 
                    ,UK__country 
                    ,USA_country 
                    ,VEN_country 
                    ,VIE_country 
                    ,YEM_country 
                    ,ZAM_country 
                    ,ZIM_country #end####
                  )

# Divsor for weekly average
n = 7


# Function that creates mean dataset for each country
meandf_func <- function(df) {
  
  mean.df<-aggregate(df,list(rep(1:(nrow(df)%/%n),each = n, len = nrow(df))),mean)[-(2:4)];
  
  return (mean.df)
}


# "Mean for each country" now in another single list

Mega_mean<-lapply(country_list,meandf_func)


# "Merge Data into one group" from the list

Q<- Reduce(function(x,y)merge(x,y,all = TRUE),Mega_mean)


#Final Plot for three variables for all countries' mean

plot(Q$Group.1,Q$retail_and_recreation_percent_change_from_baseline, 
          xlab = "Weeks since outbreak", ylab= "Percentage change in retail and recreational activities")


plot(Q$Group.1,Q$workplaces_percent_change_from_baseline)

plot(Q$Group.1,Q$residential_percent_change_from_baseline)



# Specific countries analysis of plots, slide decks has it combined and better looking where need, applyed to 6 countries

plot(LBN_country$date,LBN_country$retail_and_recreation_percent_change_from_baseline)
plot(LBN_country$date,LBN_country$workplaces_percent_change_from_baseline)


#Plots for specific countries for all variables

par(mfrow=c(1,3))

plot(LBN_country$date, LBN_country$retail_and_recreation_percent_change_from_baseline,las = 2, ylab = 
       "Percentage change", main = "COVID-19 Effect on Retail and Recreation in Lebanon")
      title(xlab="Dates", line=-1, cex.lab=1.2)
      abline(lm(LBN_country$retail_and_recreation_percent_change_from_baseline ~ as.numeric(LBN_country$date)), col = "Blue", lwd =2)

plot(LBN_country$date, LBN_country$workplaces_percent_change_from_baseline,las = 2, ylab = 
       "Percentage change in Workplaces", main = "COVID-19 Effect on Workplaces in Lebanon") 
title(xlab="Dates", line=-1, cex.lab=1.2)
abline(lm(LBN_country$workplaces_percent_change_from_baseline ~ as.numeric(LBN_country$date)), col = "Blue", lwd =2)

plot(LBN_country$date, LBN_country$residential_percent_change_from_baseline,las = 2, ylab = 
       "Percentage change in Household Time", main = "COVID-19 Effect on households in Lebanon") 
title(xlab="Dates", line=-1, cex.lab=1.2)
abline(lm(LBN_country$residential_percent_change_from_baseline ~ as.numeric(LBN_country$date)), col = "Blue", lwd =2)





##Repeat same for these countries as beginning for comparison, within the slide decks


Australia

USA

Lebanon

















