#Importing data and th data dictionary

library(readr)
library(DT)
library(dplyr)
library(sf)
library(corrplot)
library(car)

dd <- read_csv(file.path("./data/data-dictionary-2018.csv"))
dd |>
  select(var_name, description) |>
  datatable()

#read data function
rawdata <- function(path,state){
  ## read data in specific state:CALIFORNIA,FLORIDA,MICHIGAN,NEW YORK
  d <- read_sf(path)
  head(d)
  d<-d[d$STATE==state,]
}


#data clean function
data_clean <- function(d,SVI){
  ##clean data
  ##choose the SVI:RPL_THEME1,RPL_THEME2,RPL_THEME3,RPL_THEME4
  ##standardization
  num_cols <- unlist(lapply(d, is.numeric))         # Identify numeric columns
  d <- d[ , num_cols]                        # Subset numeric columns of data
  colname <- c(SVI,'EP_UNINSUR','E_TOTPOP','EP_NOHSDP','EP_POV','EP_AGE65',
               'E_DAYPOP','E_CROWD','E_HU','EP_MOBILE','E_MUNIT','EP_NOVEH',
               'EP_UNEMP','EP_DISABL','E_HH','E_SNGPNT','E_GROUPQ','EP_PCI',
               'E_POV','E_LIMENG','E_AGE17','E_NOHSDP','E_MINRTY')
  d <- d[,colname]

  #delete no use sample
  if (SVI=="RPL_THEME1"){
    d <- filter(d, d$RPL_THEME1 !=-999)
  }

  if (SVI=="RPL_THEME2"){
    d <- filter(d, d$RPL_THEME2 !=-999)
  }

  if (SVI=="RPL_THEME3"){
    d <- filter(d, d$RPL_THEME3 !=-999)
  }

  if (SVI=="RPL_THEME4"){
    d <- filter(d, d$RPL_THEME4 !=-999)
  }

  d <-st_set_geometry(d, NULL)

  #standardization
  d[,2:23] <-scale(d[,2:23])
  d
}

#EDA function
EDA<-function(d){
  ##explore the correlation between variables using pearson method
  ##visualization of the correlation
  cor_re <- cor(d,method='pearson')
  cor_re
  #visualization
  corrplot(corr=cor_re,method = "circle",
           tl.cex=0.5,type= 'upper',diag = FALSE,tl.pos="lt")
  corrplot(corr=cor_re, add = TRUE, type = "lower", method = "number",
           number.cex=0.5,tl.cex=0.5,tl.pos="n",cl.pos = "n")
}

#regression analyse function
regression<-function(d,expression){
  ##using linear regression model to fit the data
  fit<-lm(expression,data=d)
}

#hypothetical test function
hyppthsis_test <- function(fit,d){
  ##linear test
  crPlots(fit)
  #independent test
  durbinWatsonTest(fit)
}


data_path = file.path('./data/SVI2018_US',"SVI2018_US_tract.shp")


###Analyse for CALIFORNIA###
data1<-rawdata(data_path,state = "CALIFORNIA")
data1

######
##using SVI RPL_THEME1
data1_1<-data_clean(data1,SVI = "RPL_THEME1")
data1_1

EDA(data1_1)

#selected variables according to the correlations
selected_var <- c('RPL_THEME1','EP_UNINSUR','EP_NOHSDP','EP_POV',
                 'EP_UNEMP','EP_PCI','E_POV','E_NOHSDP')
processed_data <- data1_1[,selected_var]
processed_data

expression1_1 = RPL_THEME1~EP_UNINSUR+EP_NOHSDP+EP_POV+
  EP_UNEMP+EP_PCI+E_POV+E_NOHSDP

fit <- regression(processed_data,expression1_1)
summary(fit)

hyppthsis_test(fit,processed_data)
######
##Analysis according to results above:
###1.As the correlation matrix between the SVI1 'RPL_THEME1'(Percentile ranking for Socioeconomic theme summary)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.7) including:
#####‘EP_UNINSUR','EP_NOHSDP','EP_POV','EP_UNEMP','EP_PCI','E_POV','E_NOHSDP'
###2.In the selected variables,the RPL_THEME1 is negatively correlated to EP_PCI, but is positively correlated to the others.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI1 RPL_THEME1 are most influenced by EP_NOHSDP(poor education estimate),EP_POV(poverty estimate),EP_UNEMP(Unemployment Rate estimate),EP_PCI(income estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.29 , which means positive autocorrelation in the residuals and it is acceptable.


######
###using SVI RPL_THEME2
data1_2<-data_clean(data1,SVI = "RPL_THEME2")
data1_2

EDA(data1_2)

#selected variables according to the correlations
selected_var <- c('RPL_THEME2','EP_DISABL',
                  'E_SNGPNT','EP_PCI','E_POV')
processed_data <- data1_2[,selected_var]
processed_data

expression1_2 = RPL_THEME2~EP_DISABL+E_SNGPNT+EP_PCI+E_POV

fit <- regression(processed_data,expression1_2)
summary(fit)

hyppthsis_test(fit,processed_data)
######
##Analysis according to results above:
###1.As the correlation matrix between the SVI2 'RPL_THEME2'(Percentile ranking for Household Composition theme summary)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.45) including:
#####'EP_DISABL','E_SNGPNT','EP_PCI','E_POV'.
###2.In the selected variables,the RPL_THEME2 is negatively correlated to EP_PCI, but is positively correlated to the others.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI2 RPL_THEME2 are most influenced by EP_DISABL(Disability estimate),E_SNGPNT(Single parent household with children under 18 estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.42 , which means positive autocorrelation in the residuals and it is acceptable.


######
###using SVI RPL_THEME3
data1_3<-data_clean(data1,SVI = "RPL_THEME3")
data1_3

EDA(data1_3)

#selected variables according to the correlations
selected_var <- c('RPL_THEME3','E_CROWD',
                  'EP_PCI','E_LIMENG','E_NOHSDP','E_MINRTY')
processed_data <- data1_3[,selected_var]
processed_data

expression1_3 = RPL_THEME3~E_CROWD+EP_PCI+E_LIMENG+E_NOHSDP+E_MINRTY

fit <- regression(processed_data,expression1_3)
summary(fit)

hyppthsis_test(fit,processed_data)
######
##Analysis according to results above:
###1.As the correlation matrix between the SVI3 'RPL_THEME3'(Percentile ranking for Minority Status/Language theme)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.50) including:
#####'E_CROWD','EP_PCI','E_LIMENG','E_NOHSDP','E_MINRTY'.
###2.In the selected variables,the RPL_THEME3 is negatively correlated to EP_PCI, but is positively correlated to the others.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI3 RPL_THEME3 are most influenced by E_LIMENG(Persons who speak English "less than well" estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.1, which means positive autocorrelation in the residuals and it is acceptable.


######
###using SVI RPL_THEME4
data1_4<-data_clean(data1,SVI = "RPL_THEME4")
data1_4

EDA(data1_4)

#selected variables according to the correlations
selected_var <- c('RPL_THEME4','EP_POV','E_CROWD','EP_NOVEH'
                 )
processed_data <- data1_4[,selected_var]
processed_data

expression1_4 = RPL_THEME4~EP_POV+E_CROWD+EP_NOVEH

fit <- regression(processed_data,expression1_4)
summary(fit)

hyppthsis_test(fit,processed_data)
######
##Analysis according to results above:
###1.As the correlation matrix between the SVI4 'RPL_THEME4'(Percentile ranking for Housing Type/Transportation theme)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.45) including:
#####'EP_POV',E_CROWD','EP_NOVEH'.
###2.In the selected variables,the RPL_THEME4 is positively correlated to all selected variables.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI4 RPL_THEME4 are equally influenced by EP_POV(Poverty estimate),E_CROWD(More people than rooms estimate),EP_NOVEH(No vehicle available estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.50, which means positive autocorrelation in the residuals and it is acceptable.


###Analyse for FLORIDA###
data2<-rawdata(data_path,state = "FLORIDA")
data2

######
###using SVI RPL_THEME1
data2_1<-data_clean(data2,SVI = "RPL_THEME1")
data2_1

EDA(data2_1)

#selected variables according to the correlations
selected_var <- c('RPL_THEME1','EP_UNINSUR','EP_NOHSDP','EP_POV',
                  'EP_UNEMP','EP_PCI','E_POV','E_NOHSDP')
processed_data <- data2_1[,selected_var]
processed_data

expression2_1 = RPL_THEME1~EP_UNINSUR+EP_NOHSDP+EP_POV+
  EP_UNEMP+EP_PCI+E_POV+E_NOHSDP

fit <- regression(processed_data,expression2_1)
summary(fit)

hyppthsis_test(fit,processed_data)
######
##Analysis according to results above:
###1.As the correlation matrix between the SVI1 'RPL_THEME1'(Percentile ranking for Socioeconomic theme summary)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.6) including:
#####‘EP_UNINSUR','EP_NOHSDP','EP_POV','EP_UNEMP','EP_PCI','E_POV','E_NOHSDP'.
###2.In the selected variables,the RPL_THEME1 is negatively correlated to EP_PCI, but is positively correlated to the others.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI1 RPL_THEME1 are most influenced by EP_NOHSDP(poor education estimate),EP_POV(poverty estimate),EP_UNEMP(Unemployment Rate estimate),EP_PCI(income estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.48, which means positive autocorrelation in the residuals and it is acceptable.


######
###using SVI RPL_THEME2
data2_2<-data_clean(data1,SVI = "RPL_THEME2")
data2_2

EDA(data2_2)

#selected variables according to the correlations
selected_var <- c('RPL_THEME2','EP_DISABL','E_SNGPNT','EP_PCI')
processed_data <- data2_2[,selected_var]
processed_data

expression2_2 = RPL_THEME2~EP_DISABL+E_SNGPNT+EP_PCI

fit <- regression(processed_data,expression2_2)
summary(fit)

hyppthsis_test(fit,processed_data)
######
##Analysis according to results above:
###1.As the correlation matrix between the SVI2 'RPL_THEME2'(Percentile ranking for Household Composition theme summary)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.45) including:
#####'EP_DISABL','E_SNGPNT','EP_PCI'.
###2.In the selected variables,the RPL_THEME2 is negatively correlated to EP_PCI, but is positively correlated to the others.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI2 RPL_THEME2 are most influenced by EP_DISABL(Disability estimate),E_SNGPNT(Single parent household with children under 18 estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.44 , which means positive autocorrelation in the residuals and it is acceptable.


######
###using SVI RPL_THEME3
data2_3<-data_clean(data1,SVI = "RPL_THEME3")
data2_3

EDA(data2_3)

#selected variables according to the correlations
selected_var <- c('RPL_THEME3','E_CROWD','EP_PCI','E_LIMENG',
                  'E_NOHSDP','E_MINRTY')
processed_data <- data2_3[,selected_var]
processed_data

expression2_3 = RPL_THEME3~E_CROWD+EP_PCI+E_LIMENG+E_NOHSDP+E_MINRTY

fit <- regression(processed_data,expression2_3)
summary(fit)

hyppthsis_test(fit,processed_data)
######
##Analysis according to results above:
###1.As the correlation matrix between the SVI3 'RPL_THEME3'(Percentile ranking for Minority Status/Language theme)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.50) including:
#####'E_CROWD','EP_PCI','E_LIMENG','E_NOHSDP','E_MINRTY'.
###2.In the selected variables,the RPL_THEME3 is negatively correlated to EP_PCI, but is positively correlated to the others.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI3 RPL_THEME3 are most influenced by E_LIMENG(Persons who speak English "less than well" estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.12, which means positive autocorrelation in the residuals and it is acceptable.



######
###using SVI RPL_THEME4
data2_4<-data_clean(data1,SVI = "RPL_THEME4")
data2_4

EDA(data2_4)

#selected variables according to the correlations
selected_var <- c('RPL_THEME4','EP_POV','E_CROWD','EP_NOVEH')
processed_data <- data2_4[,selected_var]
processed_data

expression2_4 = RPL_THEME4~EP_POV+E_CROWD+EP_NOVEH

fit <- regression(processed_data,expression2_4)
summary(fit)

hyppthsis_test(fit,processed_data)
######
##Analysis according to results above:
###1.As the correlation matrix between the SVI4 'RPL_THEME4'(Percentile ranking for Housing Type/Transportation theme)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.45) including:
#####'EP_POV',E_CROWD','EP_NOVEH'.
###2.In the selected variables,the RPL_THEME4 is positively correlated to all selected variables.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI4 RPL_THEME4 are equally influenced by EP_POV(Poverty estimate),E_CROWD(More people than rooms estimate),EP_NOVEH(No vehicle available estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.50, which means positive autocorrelation in the residuals and it is acceptable.


###Analyse for MICHIGAN###
data3<-rawdata(data_path,state = "MICHIGAN")
data3

######
###using SVI RPL_THEME1
data3_1<-data_clean(data3,SVI = "RPL_THEME1")
data3_1

EDA(data3_1)

#selected variables according to the correlations
selected_var <- c('RPL_THEME1','EP_UNINSUR','EP_NOHSDP','EP_POV',
                  'EP_UNEMP','EP_PCI','E_POV','E_NOHSDP')
processed_data <- data3_1[,selected_var]
processed_data

expression3_1 = RPL_THEME1~EP_UNINSUR+EP_NOHSDP+EP_POV+
  EP_UNEMP+EP_PCI+E_POV+E_NOHSDP

fit <- regression(processed_data,expression3_1)
summary(fit)

hyppthsis_test(fit,processed_data)
######
###1.As the correlation matrix between the SVI1 'RPL_THEME1'(Percentile ranking for Socioeconomic theme summary)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.6) including:
#####‘EP_UNINSUR','EP_NOHSDP','EP_POV','EP_UNEMP','EP_PCI','E_POV','E_NOHSDP'.
###2.In the selected variables,the RPL_THEME1 is negatively correlated to EP_PCI, but is positively correlated to the others.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI1 RPL_THEME1 are most influenced by EP_UNINSUR,EP_POV(poverty estimate),EP_UNEMP(Unemployment Rate estimate),EP_PCI(income estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.36, which means positive autocorrelation in the residuals and it is acceptable.


######
###using SVI RPL_THEME2
data3_2<-data_clean(data1,SVI = "RPL_THEME2")
data3_2

EDA(data3_2)

#selected variables according to the correlations
selected_var <- c('RPL_THEME2','EP_DISABL','E_SNGPNT')
processed_data <- data3_2[,selected_var]
processed_data

expression3_2 = RPL_THEME2~EP_DISABL+E_SNGPNT

fit <- regression(processed_data,expression3_2)
summary(fit)

hyppthsis_test(fit,processed_data)
######
##Analysis according to results above:
###1.As the correlation matrix between the SVI2 'RPL_THEME2'(Percentile ranking for Household Composition theme summary)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.50) including:
#####'EP_DISABL','E_SNGPNT'.
###2.In the selected variables,the RPL_THEME2 is both positively correlated to selected variables.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI2 RPL_THEME2 are equally influenced by EP_DISABL(Disability estimate),E_SNGPNT(Single parent household with children under 18 estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.44 , which means positive autocorrelation in the residuals and it is acceptable.


######
###using SVI RPL_THEME3
data3_3<-data_clean(data1,SVI = "RPL_THEME3")
data3_3

EDA(data3_3)

#selected variables according to the correlations
selected_var <- c('RPL_THEME3','E_CROWD','EP_PCI','E_LIMENG',
                  'E_NOHSDP','E_MINRTY')
processed_data <- data3_3[,selected_var]
processed_data

expression3_3 = RPL_THEME3~E_CROWD+EP_PCI+E_LIMENG+E_NOHSDP+E_MINRTY

fit <- regression(processed_data,expression3_3)
summary(fit)

hyppthsis_test(fit,processed_data)
######
##Analysis according to results above:
###1.As the correlation matrix between the SVI3 'RPL_THEME3'(Percentile ranking for Minority Status/Language theme)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.50) including:
#####'E_CROWD','EP_PCI','E_LIMENG','E_NOHSDP','E_MINRTY'.
###2.In the selected variables,the RPL_THEME3 is negatively correlated to EP_PCI, but is positively correlated to the others.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI3 RPL_THEME3 are most influenced by EP_PCI(income estimate),E_LIMENG(Persons who speak English "less than well" estimate),E_NOHSDP(Low educated estimate),E_MINRTY(Minority estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.12, which means positive autocorrelation in the residuals and it is acceptable.


######
###using SVI RPL_THEME4
data3_4<-data_clean(data1,SVI = "RPL_THEME4")
data3_4

EDA(data3_4)

#selected variables according to the correlations
selected_var <- c('RPL_THEME4','EP_POV','E_CROWD','EP_NOVEH')
processed_data <- data3_4[,selected_var]
processed_data

expression3_4 = RPL_THEME4~EP_POV+E_CROWD+EP_NOVEH

fit <- regression(processed_data,expression3_4)
summary(fit)

hyppthsis_test(fit,processed_data)
######
###1.As the correlation matrix between the SVI4 'RPL_THEME4'(Percentile ranking for Housing Type/Transportation theme)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.50) including:
#####'EP_POV',E_CROWD','EP_NOVEH'.
###2.In the selected variables,the RPL_THEME4 is positively correlated to all selected variables.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI4 RPL_THEME4 are equally influenced by EP_POV(Poverty estimate),E_CROWD(More people than rooms estimate),EP_NOVEH(No vehicle available estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.50, which means positive autocorrelation in the residuals and it is acceptable.



###Analyse for NEW YORK###
data4<-rawdata(data_path,state = "NEW YORK")
data4

######
###using SVI RPL_THEME1
data4_1<-data_clean(data4,SVI = "RPL_THEME1")
data4_1

EDA(data4_1)

#selected variables according to the correlations
selected_var <- c('RPL_THEME1','EP_UNINSUR','EP_NOHSDP','EP_POV',
                  'EP_UNEMP','EP_PCI','E_POV','E_NOHSDP')
processed_data <- data4_1[,selected_var]
processed_data

expression4_1 = RPL_THEME1~EP_UNINSUR+EP_NOHSDP+EP_POV+
  EP_UNEMP+EP_PCI+E_POV+E_NOHSDP

fit <- regression(processed_data,expression4_1)
summary(fit)

hyppthsis_test(fit,processed_data)
######
###1.As the correlation matrix between the SVI1 'RPL_THEME1'(Percentile ranking for Socioeconomic theme summary)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.6) including:
#####‘EP_UNINSUR','EP_NOHSDP','EP_POV','EP_UNEMP','EP_PCI','E_POV','E_NOHSDP'.
###2.In the selected variables,the RPL_THEME1 is negatively correlated to EP_PCI, but is positively correlated to the others.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI1 RPL_THEME1 are most influenced by EP_NOHSDP(Low Educated estimate),EP_POV(poverty estimate),EP_UNEMP(Unemployment Rate estimate),EP_PCI(income estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.3, which means positive autocorrelation in the residuals and it is acceptable.


######
###using SVI RPL_THEME2
data4_2<-data_clean(data1,SVI = "RPL_THEME2")
data4_2

EDA(data4_2)

#selected variables according to the correlations
selected_var <- c('RPL_THEME2','EP_DISABL','E_SNGPNT')
processed_data <- data4_2[,selected_var]
processed_data

expression4_2 = RPL_THEME2~EP_DISABL+E_SNGPNT

fit <- regression(processed_data,expression4_2)
summary(fit)

hyppthsis_test(fit,processed_data)
######
##Analysis according to results above:
###1.As the correlation matrix between the SVI2 'RPL_THEME2'(Percentile ranking for Household Composition theme summary)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.50) including:
#####'EP_DISABL','E_SNGPNT'.
###2.In the selected variables,the RPL_THEME2 is both positively correlated to selected variables.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI2 RPL_THEME2 are equally influenced by EP_DISABL(Disability estimate),E_SNGPNT(Single parent household with children under 18 estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.43 , which means positive autocorrelation in the residuals and it is acceptable.


######
###using SVI RPL_THEME3
data4_3<-data_clean(data1,SVI = "RPL_THEME3")
data4_3

EDA(data4_3)

#selected variables according to the correlations
selected_var <- c('RPL_THEME3','E_CROWD','EP_PCI','E_LIMENG',
                  'E_NOHSDP','E_MINRTY')
processed_data <- data4_3[,selected_var]
processed_data

expression4_3 = RPL_THEME3~E_CROWD+EP_PCI+E_LIMENG+E_NOHSDP+E_MINRTY

fit <- regression(processed_data,expression4_3)
summary(fit)

hyppthsis_test(fit,processed_data)
######
###1.As the correlation matrix between the SVI3 'RPL_THEME3'(Percentile ranking for Minority Status/Language theme)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.50) including:
#####'E_CROWD','EP_PCI','E_LIMENG','E_NOHSDP','E_MINRTY'.
###2.In the selected variables,the RPL_THEME3 is negatively correlated to EP_PCI, but is positively correlated to the others.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI3 RPL_THEME3 are most influenced by EP_PCI(income estimate),E_LIMENG(Persons who speak English "less than well" estimate),E_NOHSDP(Low educated estimate),E_MINRTY(Minority estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.12, which means positive autocorrelation in the residuals and it is acceptable.



######
###using SVI RPL_THEME4
data4_4<-data_clean(data1,SVI = "RPL_THEME4")
data4_4

EDA(data4_4)

#selected variables according to the correlations
selected_var <- c('RPL_THEME4','EP_POV','E_CROWD','EP_NOVEH')
processed_data <- data4_4[,selected_var]
processed_data

expression4_4 = RPL_THEME4~EP_POV+E_CROWD+EP_NOVEH

fit <- regression(processed_data,expression4_4)
summary(fit)

hyppthsis_test(fit,processed_data)
######
###1.As the correlation matrix between the SVI4 'RPL_THEME4'(Percentile ranking for Housing Type/Transportation theme)
#####and census variables, we can select the most correlated census variables(absolute pearson coefficient>0.50) including:
#####'EP_POV',E_CROWD','EP_NOVEH'.
###2.In the selected variables,the RPL_THEME4 is positively correlated to all selected variables.
###3. After multivariate regression on the data,according to the estimate of coefficients of each variables, we know that
######SVI4 RPL_THEME4 are equally influenced by EP_POV(Poverty estimate),E_CROWD(More people than rooms estimate),EP_NOVEH(No vehicle available estimate).
###4.As shown in the linear test figure, Linear regression can be good fit to all of the variables.
#####The results of Durbin Watson Test D-W Statistic is 1.49, which means positive autocorrelation in the residuals and it is acceptable.







