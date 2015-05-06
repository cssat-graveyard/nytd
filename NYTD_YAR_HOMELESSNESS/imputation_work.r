
# set.seed(4735)

library(RODBC)
library(Amelia)
library(maps)
library(corrplot)
library(caret)
library(dplyr)

con <- odbcConnect("POC")

dat <- sqlQuery(con, "
SELECT 
	npd.st
	,npd.stchid
	,npd.recnumbr
	,npd.dob
	,npd.dobyr
	,npd.dobmon
	,npd.sex
	,npd.amiakn
	,npd.asian
	,IIF(npd.blkafram = 3, 0, npd.blkafram) AS blkafram
	,npd.hawaiipi
	,npd.white
	,npd.racedcln
	,npd.hisorgin
	,IIF(npd.amiakn + npd.asian + IIF(npd.blkafram = 3, 0, npd.blkafram) + npd.hawaiipi + npd.white > 1, 1, 0) AS multirace
    ,IIF(nytd1.[currfte] IN (2, 77), NULL, nytd1.[currfte]) AS nytd1_currfte
    ,IIF(nytd1.[currpte] IN (2, 77), NULL, nytd1.[currpte]) AS nytd1_currpte
    ,IIF(nytd1.[emplysklls] IN (2, 77), NULL, nytd1.[emplysklls]) AS nytd1_emplysklls
    ,IIF(nytd1.[socsecrty] IN (2, 77), NULL, nytd1.[socsecrty]) AS nytd1_socsecrty
    ,IIF(nytd1.[educaid] IN (2, 77), NULL, nytd1.[educaid]) AS nytd1_educaid
    ,IIF(nytd1.[pubfinas] IN (2, 77), NULL, nytd1.[pubfinas]) AS nytd1_pubfinas
    ,IIF(nytd1.[pubfoodas] IN (2, 77), NULL, nytd1.[pubfoodas]) AS nytd1_pubfoodas
    ,IIF(nytd1.[pubhousas] IN (2, 77), NULL, nytd1.[pubhousas]) AS nytd1_pubhousas
    ,IIF(nytd1.[othrfinas] IN (2, 77), NULL, nytd1.[othrfinas]) AS nytd1_othrfinas
    ,IIF(nytd1.[highedcert] IN (8, 77, 78), NULL, nytd1.[highedcert]) AS nytd1_highedcert
    ,IIF(nytd1.[currenroll] IN (2, 77), NULL, nytd1.[currenroll]) AS nytd1_currenroll
    ,IIF(nytd1.[cnctadult] IN (2, 77), NULL, nytd1.[cnctadult]) AS nytd1_cnctadult
    ,IIF(nytd1.[homeless] IN (2, 77), NULL, nytd1.[homeless]) AS nytd1_homeless
    ,IIF(nytd1.[subabuse] IN (2, 77), NULL, nytd1.[subabuse]) AS nytd1_subabuse
    ,IIF(nytd1.[incarc] IN (2, 77), NULL, nytd1.[incarc]) AS nytd1_incarc
    ,IIF(nytd1.[children] IN (2, 77), NULL, nytd1.[children]) AS nytd1_children
    ,IIF(nytd1.[marriage] IN (2, 77), NULL, nytd1.[marriage]) AS nytd1_marriage
    ,IIF(nytd1.[medicaid] IN (2, 77), NULL, nytd1.[medicaid]) AS nytd1_medicaid
    ,IIF(nytd1.[othrhlthin] IN (2, 77), NULL, nytd1.[othrhlthin]) AS nytd1_othrhlthin
    ,IIF(nytd1.[medicalin] IN (2, 77), NULL, nytd1.[medicalin]) AS nytd1_medicalin
    ,IIF(nytd1.[mentlhlthin] IN (2, 77), NULL, nytd1.[mentlhlthin]) AS nytd1_mentlhlthin
    ,IIF(nytd1.[prescripin] IN (2, 77), NULL, nytd1.[prescripin]) AS nytd1_prescripin
    ,IIF(nytd2.[outcmfcs] IN (77), NULL, nytd2.[outcmfcs]) AS nytd2_outcmfcs -------
    ,IIF(nytd2.[currfte] IN (2, 77), NULL, nytd2.[currfte]) AS nytd2_currfte
    ,IIF(nytd2.[currpte] IN (2, 77), NULL, nytd2.[currpte]) AS nytd2_currpte
    ,IIF(nytd2.[emplysklls] IN (2, 77), NULL, nytd2.[emplysklls]) AS nytd2_emplysklls
    ,IIF(nytd2.[socsecrty] IN (2, 77), NULL, nytd2.[socsecrty]) AS nytd2_socsecrty
    ,IIF(nytd2.[educaid] IN (2, 77), NULL, nytd2.[educaid]) AS nytd2_educaid
    ,IIF(nytd2.[pubfinas] IN(2, 77), NULL, nytd2.[pubfinas]) AS nytd2_pubfinas
    ,IIF(nytd2.[pubfoodas] IN(2, 77), NULL, nytd2.[pubfoodas]) AS nytd2_pubfoodas
    ,IIF(nytd2.[pubhousas] IN(2, 77), NULL, nytd2.[pubhousas]) AS nytd2_pubhousas
    ,IIF(nytd2.[othrfinas] IN(2, 77), NULL, nytd2.[othrfinas]) AS nytd2_othrfinas
    ,IIF(nytd2.[highedcert] IN (8, 77, 78), NULL, nytd2.[highedcert]) AS nytd2_highedcert
    ,IIF(nytd2.[currenroll] IN (2, 77), NULL, nytd2.[currenroll]) AS nytd2_currenroll
    ,IIF(nytd2.[cnctadult] IN (2, 77), NULL, nytd2.[cnctadult]) AS nytd2_cnctadult
    ,IIF(nytd2.[homeless] IN (2, 77), NULL, nytd2.[homeless]) AS nytd2_homeless
    ,IIF(nytd2.[subabuse] IN (2, 77), NULL, nytd2.[subabuse]) AS nytd2_subabuse
    ,IIF(nytd2.[incarc] IN (2, 77), NULL, nytd2.[incarc]) AS nytd2_incarc
    ,IIF(nytd2.[children] IN (2, 77), NULL, nytd2.[children]) AS nytd2_children
    ,IIF(nytd2.[marriage] IN (2, 77), NULL, nytd2.[marriage]) AS nytd2_marriage
	,nytd2.[weight]
    ,IIF(nytd2.[medicaid] IN (2, 77), NULL, nytd2.[medicaid]) AS nytd2_medicaid
    ,IIF(nytd2.[othrhlthin] IN (2, 77), NULL, nytd2.[othrhlthin]) AS nytd2_othrhlthin
    ,IIF(nytd2.[medicalin] IN (2, 77), NULL, nytd2.[medicalin]) AS nytd2_medicalin
    ,IIF(nytd2.[mentlhlthin] IN (2, 77), NULL, nytd2.[mentlhlthin]) AS nytd2_mentlhlthin
    ,IIF(nytd2.[prescripin] IN (2, 77), NULL, nytd2.[prescripin]) AS nytd2_prescripin
	,s.[fcstatsv] AS s_fcstatsv	
    ,s.[tribesv] AS s_tribesv	
    ,s.[delinqntsv] AS s_delinqntsv	
    ,s.[edlevlsv] AS s_edlevlsv	
    ,ISNULL(s.[specedsv], 0) AS s_specedsv	
    ,ISNULL(s.[ilnasv], 0) AS s_ilnasv	
    ,ISNULL(s.[psedsuppsv], 0) AS s_psedsuppsv	
    ,ISNULL(s.[careersv], 0) AS s_careersv	
    ,ISNULL(s.[emplytrsv], 0) AS s_emplytrsv	
    ,ISNULL(s.[budgetsv], 0) AS s_budgetsv	
    ,ISNULL(s.[housedsv], 0) AS s_housedsv	
    ,ISNULL(s.[hlthedsv], 0) AS s_hlthedsv	
    ,ISNULL(s.[famsuppsv], 0) AS s_famsuppsv	
    ,ISNULL(s.[mentorsv], 0) AS s_mentorsv	
    ,ISNULL(s.[silsv], 0) AS s_silsv	
    ,ISNULL(s.[rmbrdfasv], 0) AS s_rmbrdfasv	
    ,ISNULL(s.[educfinasv], 0) AS s_educfinasv	
    ,ISNULL(s.[othrfinasv], 0) AS s_othrfinasv 
    ,IIF(fc.[clindis] = 'Yes', 1, IIF(fc.[clindis] = 'No', 0, IIF(fc.[clindis] = 'Not yet determined', 2, NULL))) AS fc_clindis 
    ,IIF(fc.[mr] = 'Yes', '1', IIF(fc.[mr] = 'No', '0', NULL)) AS fc_mr	
    ,IIF(fc.[vishear] = 'Yes', 1, IIF(fc.[vishear]= 'No', 0, NULL)) AS fc_vishear	
    ,IIF(fc.[phydis] = 'Yes', 1, IIF(fc.[phydis] = 'No', 0, NULL)) AS fc_phydis	
    ,IIF(fc.[dsmiii] = 'Yes', 1, IIF(fc.[dsmiii] = 'No', 0, NULL)) AS fc_dsmiii	
    ,IIF(fc.[othermed] = 'Yes', 1, IIF(fc.[othermed] = 'No', 0, NULL)) AS fc_othermed
    ,IIF(fc.[everadpt] IN ('Yes, child has been legally adopted', 'Yes'), 1, IIF(fc.[everadpt] IN ('No, has never been legally adopted', 'No'), 0, NULL)) AS fc_everadpt
    ,fc.[ageadopt] 
    ,fc.[totalrem] 
    ,fc.[numplep]
    ,fc.[manrem] 
	,IIF(fc.[phyabuse] = 'Yes', 1, IIF(fc.[phyabuse] = 'No', 0, NULL)) AS fc_phyabuse
    ,IIF(fc.[sexabuse] = 'Yes', 1, IIF(fc.[sexabuse] = 'No', 0, NULL)) AS fc_sexabuse
    ,IIF(fc.[neglect] = 'Yes', 1, IIF(fc.[neglect] = 'No', 0, NULL)) AS fc_neglect
    ,IIF(fc.[aaparent] = 'Yes', 1, IIF(fc.[aaparent] = 'No', 0, NULL)) AS fc_aaparent
    ,IIF(fc.[daparent] = 'Yes', 1, IIF(fc.[daparent] = 'No', 0, NULL)) AS fc_daparent
    ,IIF(fc.[aachild] = 'Yes', 1, IIF(fc.[aachild] = 'No', 0, NULL)) AS fc_aachild
    ,IIF(fc.[dachild] = 'Yes', 1, IIF(fc.[dachild] = 'No', 0, NULL)) AS fc_dachild
    ,IIF(fc.[childis] = 'Yes', 1, IIF(fc.[childis] = 'No', 0, NULL)) AS fc_childis
    ,IIF(fc.[chbehprb] = 'Yes', 1, IIF(fc.[chbehprb] = 'No', 0, NULL)) AS fc_chbehprb
    ,IIF(fc.[prtsdied] = 'Yes', 1, IIF(fc.[prtsdied] = 'No', 0, NULL)) AS fc_prtsdied
    ,IIF(fc.[prtsjail] = 'Yes', 1, IIF(fc.[prtsjail] = 'No', 0, NULL)) AS fc_prtsjail
    ,IIF(fc.[nocope] = 'Yes', 1, IIF(fc.[nocope] = 'No', 0, NULL)) AS fc_nocope
    ,IIF(fc.[abandmnt] = 'Yes', 1, IIF(fc.[abandmnt] = 'No', 0, NULL)) AS fc_abandmnt
    ,IIF(fc.[relinqsh] = 'Yes', 1, IIF(fc.[relinqsh] = 'No', 0, NULL)) AS fc_relinqsh
    ,IIF(fc.[housing] = 'Yes', 1, IIF(fc.[housing] = 'No', 0, NULL)) AS fc_housing
    ,IIF(fc.[placeout] = 'Yes', 1, IIF(fc.[placeout] = 'No', 0, NULL)) AS fc_placeout
    ,fc.[ctkfamst]
    ,fc.[fosfamst]
    ,IIF(fc.[ivefc] = 'Yes', 1, IIF(fc.[ivefc] = 'No', 0, NULL)) AS fc_ivefc   
    ,IIF(fc.[iveaa] = 'Yes', 1, IIF(fc.[iveaa] = 'No', 0, NULL)) AS fc_iveaa	
    ,IIF(fc.[ivaafdc] = 'Yes', 1, IIF(fc.[ivaafdc] = 'No', 0, NULL)) AS fc_ivaafdc	
    ,IIF(fc.[ivdchsup] = 'Yes', 1, IIF(fc.[ivdchsup] = 'No', 0, NULL)) AS fc_ivdchsup
    ,IIF(fc.[xixmedcd] = 'Yes', 1, IIF(fc.[xixmedcd] = 'No', 0, NULL)) AS fc_xixmedcd
    ,IIF(fc.[ssiother] = 'Yes', 1, IIF(fc.[ssiother] = 'No', 0, NULL)) AS fc_ssiother
    ,IIF(fc.[noa] = 'Yes', 1, IIF(fc.[noa] = 'No', 0, NULL)) AS fc_noa	
	,fc.[fcmntpay]
	,IIF(fc.[inatstart] = 'Yes', 1, IIF(fc.[inatstart] = 'No', 0, NULL)) AS fc_inatstart 
    ,IIF(fc.[inatend] = 'Yes', 1, IIF(fc.[inatend] = 'No', 0, NULL)) AS fc_inatend
    ,IIF(fc.[entered] = 'Yes', 1, IIF(fc.[entered] = 'No', 0, NULL)) AS fc_entered
    ,IIF(fc.[exited] = 'Yes', 1, IIF(fc.[exited] = 'No', 0, NULL)) AS fc_exited 
	,IIF(fc.[iswaiting] = 'Yes', 1, IIF(fc.[iswaiting] = 'No', 0, NULL)) AS fc_iswaiting 
    ,IIF(fc.[istpr] = 'Yes', 1, IIF(fc.[istpr] = 'No', 0, NULL)) AS fc_istpr
    ,fc.[latremlos]
    ,fc.[settinglos]
	,fc.[lifelos]
	,fc.[ageatend] AS fc_ageatstart 
    ,fc.[ageatend] AS fc_ageatlatrem 
    ,fc.[ageatend] AS fc_ageatend
    ,IIF(fc.[agedout] = 'Yes', 1, IIF(fc.[agedout] = 'No', 0, fc.[agedout])) AS fc_agedout  
FROM [CA_ODS].[ndacan].[NYTD_Outcomes_people_dim] AS npd
JOIN [CA_ODS].[ndacan].[NYTD_Outcomes_Waves_1_2] AS nytd1
	ON npd.stchid = nytd1.stchid
	AND nytd1.cd_wave = 1
JOIN [CA_ODS].[ndacan].[NYTD_Outcomes_Waves_1_2] AS nytd2
	ON npd.stchid = nytd2.stchid
	AND nytd2.cd_wave = 2
LEFT JOIN [CA_ODS].[ndacan].[NYTD_Services_2011_2012_2013_truncated] AS s 
	ON npd.stchid = s.stchid
	AND npd.sex = s.sex
	AND npd.dobyr = YEAR(CONVERT(date, s.dob))
	AND npd.dobmon = MONTH(CONVERT(date, s.dob))
LEFT JOIN 
	(SELECT 
		RANK() OVER(PARTITION BY fc.recnumbr, fc.st, npd.stchid ORDER BY fc.datayear DESC) AS r_order
		,fc.recnumbr AS recnumbr_fc
		,fc.st
		,npd.stchid
		,fc.datayear
	FROM [CA_ODS].[ndacan].[NYTD_Outcomes_people_dim] AS npd
		INNER JOIN [CA_ODS].[ndacan].[afcars_foster_care_00_13] AS fc
			ON npd.recnumbr = fc.RecNumbr
			AND npd.st = fc.St) AS fcid
	ON npd.stchid = fcid.stchid
	AND r_order = 1
LEFT JOIN [CA_ODS].[ndacan].[afcars_foster_care_00_13] AS fc
	ON npd.recnumbr = fc.recnumbr
	AND npd.st = fc.st
	AND fcid.datayear = fc.datayear
WHERE npd.st NOT IN ('NY', 'PR')
	--AND npd.st = 'WA'
	--AND s.fcstatsv IS NULL
ORDER BY 
	npd.stchid
")

missmap(dat)

head(dat)

# removing non numeric data and NA to checking correlations

cor_dat <- na.omit(dat[! names(dat) %in% c("st", "stchid", "recnumbr", "dob", "dobyr", "dobmon", "sex", "weight", "ageadopt", "ctkfamst", "fosfamst", "fips", "manrem")])

cor_dat <- na.omit(cor_dat[! names(cor_dat) %in% c("fc_ageatend", "nytd1_marriage", "nytd2_marriage", "fc_iswaiting", "nytd2_prescripin",
 			"nytd1_pubfoodas", "nytd2_prescripin", "nytd2_medicalin", "latremlos", "nytd1_prescripin", "nytd1_medicalin", "nytd1_pubhousas",
			"nytd2_pubhousas", "fc_ageatstart", "fc_exited", "fc_entered", "nytd2_pubfoodas", "nytd2_outcmfcs", "s_tribesv")])


# cor_dat <- dplyr::select(dat, -st, -stchid, -recnumbr, -dob, -sex, -ageadopt, -manrem, -ctkfamst, -fosfamst, -weight)

# cor_dat <- as.data.frame(sapply(cor_dat, as.numeric))

# checking for collinearity 

head(cor_dat)

cor_test <- cor(cor_dat)
cor_col <- findCorrelation(cor_test, cutoff = .70)

noquote(c(paste0(cor_col, ",")))

head(cor_dat[c(5, 8)],100)

table(cor_dat[[1]], cor_dat[[5]])

setwd("S:/Data Portal/erik/NYTD_YAR_HOMELESSNESS")
png(file = "correlation_plot.png", width = 480*5, height = 480*5)
corrplot(cor_test, order = "hclust")
dev.off()

# columns to remove based on collinearity 

dat <- dat[! names(dat) %in% c("fc_ageatend", "nytd1_marriage", "nytd2_marriage", "fc_iswaiting", "nytd2_prescripin",
 			"nytd1_pubfoodas", "nytd2_prescripin", "nytd2_medicalin", "latremlos", "nytd1_prescripin", "nytd1_medicalin", "nytd1_pubhousas",
			"nytd2_pubhousas", "fc_ageatstart", "fc_exited", "fc_entered", "nytd2_pubfoodas", "nytd2_outcmfcs", "dobyr", "dobmon", "manrem", "s_tribesv")]

# splitting the data
test_rf <- dat

set.seed(1984)

## 75% of the sample size
smp_size <- floor(0.66 * nrow(test_rf))

## set the seed to make your partition reproductible
train_ind <- sample(seq_len(nrow(test_rf)), size = smp_size)

train <- test_rf[train_ind, ]
validation <- test_rf[-train_ind, ]

dim(validation)

## spitting the training settinglos

train_df <- train

smp_size <- floor(0.5 * nrow(train_df))

train_ind <- sample(seq_len(nrow(train_df)), size = smp_size)

train <- train_df[train_ind, ]
test <- train_df[-train_ind, ]
dim(train)
dim(test)

library(data.table)

# read in your data, wrap in data.table(..., key="id") 
A <- data.table(read.table("vecA.csv",sep=",",header=T), key="id")
B <- data.table(read.table("vecB.csv",sep=",",header=T), key="id")

A <- data.table(noms_vars)
B <- data.table(names(dat))

setkey(A)
setkey(B)
\
# Then this is all you need
A[!B]







# # splitting the data
# test_rf <- dat

# set.seed(1983)

# ## 75% of the sample size
# smp_size <- floor(0.66 * nrow(test_rf))

# ## set the seed to make your partition reproductible
# train_ind <- sample(seq_len(nrow(test_rf)), size = smp_size)

# train <- test_rf[train_ind, ]
# validation <- test_rf[-train_ind, ]

# ## spitting the training settinglos

# train_df <- train

# smp_size <- floor(0.5 * nrow(train_df))

# train_ind <- sample(seq_len(nrow(train_df)), size = smp_size)

# train <- train_df[train_ind, ]
# test <- train_df[-train_ind, ]

# setting variables

id <- c("st", "stchid", "recnumbr", "dob", "weight")

noms_vars <- c("sex", "nytd2_homeless", "nytd2_subabuse", "nytd2_incarc", "nytd2_children", "s_fcstatsv", "s_delinqntsv", 
	"s_specedsv", "s_ilnasv", "s_psedsuppsv", "s_careersv", "s_emplytrsv", "s_budgetsv", "s_housedsv", "s_hlthedsv", "s_famsuppsv", 
	"s_mentorsv", "s_silsv", "s_rmbrdfasv", "s_educfinasv", "s_othrfinasv", "fc_clindis", "fc_mr", "fc_vishear", "fc_phydis", 
	"fc_dsmiii", "fc_othermed", "fc_everadpt", "fc_placeout", "ctkfamst", "fosfamst", "fc_ivefc", "fc_iveaa", "fc_ivaafdc", "fc_ivdchsup", 
	"fc_xixmedcd", "fc_ssiother", "fc_noa", "fc_istpr", "fc_agedout", "fc_chbehprb", "nytd2_medicaid", "nytd2_othrhlthin", "nytd2_currenroll",
	"nytd1_currfte", "nytd1_currpte", "nytd1_emplysklls", "nytd1_socsecrty", "nytd1_educaid", "nytd1_pubfinas", 
	"nytd1_othrfinas", "nytd1_currenroll", "nytd1_cnctadult", "nytd1_homeless", "nytd1_subabuse", "nytd1_incarc", "nytd1_medicaid",
	"nytd1_othrhlthin", "nytd2_currfte", "nytd2_currpte", "nytd2_emplysklls", "nytd2_socsecrty", "nytd2_educaid",
	"nytd2_pubfinas")

	
ord_vars <- c("s_edlevlsv", "ageadopt", "settinglos", "lifelos", "fcmntpay", "nytd1_highedcert", "nytd2_highedcert")  

# creating a dataframe for states

# st <- as.data.frame(unique(a.test$st))
# st_cd <- as.data.frame(cbind(1:length(unique(a.test$st)), st))
# names(st_cd) <- c("st_cd", "st")

# impute validation

# a.test <- left_join(a.test, st_cd)

validation_imp <- amelia(validation, idvars = id, m = 10, p2s = 2, noms = noms_vars, ord = ord_vars, empri = 1)

# impute test

# a.test <- left_join(a.test, st_cd) 

test_imp <- amelia(test, idvars = id, m = 1, p2s = 2, noms = noms_vars, ord = ord_vars, empri = 1)
 
# impute train

# a.test <- left_join(a.test, st_cd)

train.imp <- amelia(train, idvars = id, m = 1, p2s = 2, noms = noms_vars, ord = ord_vars, empri = 1)


























par(mfrow = c(2,3))
overimpute(test.am, "lifelos")
overimpute(test.am, "nytd1_highedcert")
overimpute(test.am, "s_edlevlsv")
overimpute(test.am, "nytd2_highedcert")
overimpute(test.am, "fcmntpay")

par(mfrow = c(2,2))
compare.density(test.am, "lifelos")
compare.density(test.am, "nytd1_highedcert")
compare.density(test.am, "s_edlevlsv")
compare.density(test.am, "fcmntpay")

head(test.am$imputations[[1]])

min(test.am$imputations[[4]]$lifelos)

head(test.am$imputations[[1]])

is(a.test$fcmntpay)

filter(a.test, is.na(asian))

yar_mod <- glm(nytd2_homeless ~ nytd2_children + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = a.test, family = binomial, weights = weight)

summary(yar_mod)

yar_mod_imp1 <- glm(nytd2_homeless ~ nytd2_children + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[1]], family = binomial, weights = weight)

summary(yar_mod_imp1)

yar_mod_imp2 <- glm(nytd2_homeless ~ nytd2_children + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[2]], family = binomial, weights = weight)

summary(yar_mod_imp2)

yar_mod_imp3 <- glm(nytd2_homeless ~ nytd2_children + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[3]], family = binomial, weights = weight)

summary(yar_mod_imp3)

yar_mod_imp4 <- glm(nytd2_homeless ~ nytd2_children + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[4]], family = binomial, weights = weight)

summary(yar_mod_imp4)

yar_mod_imp5 <- glm(nytd2_homeless ~ nytd2_children + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[5]], family = binomial, weights = weight)

summary(yar_mod_imp5) 

yar_mod_imp6 <- glm(nytd2_homeless ~ nytd2_children + as.factor(nytd2_pubhousas) + blkafram +  s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[6]], family = binomial, weights = weight)

summary(yar_mod_imp6)

yar_mod_imp7 <- glm(nytd2_homeless ~ nytd2_children + as.factor(nytd2_pubhousas) + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[7]], family = binomial, weights = weight)

summary(yar_mod_imp7)

yar_mod_imp8 <- glm(nytd2_homeless ~ nytd2_children + as.factor(nytd2_pubhousas) + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[8]], family = binomial, weights = weight)

summary(yar_mod_imp8)

yar_mod_imp9 <- glm(nytd2_homeless ~ nytd2_children + as.factor(nytd2_pubhousas) + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[9]], family = binomial, weights = weight)

summary(yar_mod_imp9)

yar_mod_imp10 <- glm(nytd2_homeless ~ nytd2_children + as.factor(nytd2_pubhousas) + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[10]], family = binomial, weights = weight)

summary(yar_mod_imp10)

# yar_mod_list <- list(NA)
# for (i in 1:10) {
	# paste0("yar_mod_imp",i)
	# yar_mod_imp[[i]] <- glm(nytd2_homeless ~ nytd2_children + nytd2_pubhousas + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[i]], family = binomial, weights = weight)
	# yar_mod_list <- list(list(yar_mod_list), list(yar_mod_imp[[i]]))

# }
# yar_mod_list

# paste0("yar_mod_imp",1)

# yar_mod_list[[1]]

# test_list <- list(yar_mod_imp1, yar_mod)



# par(mfrow = c(3,4))
# plot(coef(yar_mod_imp1), coef(yar_mod))
# plot(coef(yar_mod_imp2), coef(yar_mod))
# plot(coef(yar_mod_imp3), coef(yar_mod))
# plot(coef(yar_mod_imp3), coef(yar_mod))
# plot(coef(yar_mod_imp4), coef(yar_mod))
# plot(coef(yar_mod_imp5), coef(yar_mod))
# plot(coef(yar_mod_imp6), coef(yar_mod))
# plot(coef(yar_mod_imp7), coef(yar_mod))
# plot(coef(yar_mod_imp8), coef(yar_mod))
# plot(coef(yar_mod_imp9), coef(yar_mod))
# plot(coef(yar_mod_imp10), coef(yar_mod))

# par(mfrow = c(3,4))
# plot(coef(yar_mod))
# plot(coef(yar_mod_imp1))
# plot(coef(yar_mod_imp2))
# plot(coef(yar_mod_imp3))
# plot(coef(yar_mod_imp3))
# plot(coef(yar_mod_imp4))
# plot(coef(yar_mod_imp5))
# plot(coef(yar_mod_imp6))
# plot(coef(yar_mod_imp7))
# plot(coef(yar_mod_imp8))
# plot(coef(yar_mod_imp9))
# plot(coef(yar_mod_imp10))


yar_mod_list = lapply(ls(pattern = "yar_mod_imp"), get)
names(yar_mod_list) = ls(pattern = "yar_mod_imp")
yar_mod_list[["original"]] = yar_mod

yar_coef = sapply(yar_mod_list, coef)
yar_coef = as.data.frame(yar_coef)
yar_coef$coef = row.names(yar_coef)
yar_coef_long = reshape2::melt(yar_coef, id.vars = "coef")
library(ggplot2)
library(dplyr)
ggplot(filter(yar_coef_long, coef != "(Intercept)"), aes(x = 1, y = value)) + geom_boxplot() +
geom_point(aes(color = ifelse(variable == "original", "original", "imputed"))) +
facet_wrap(~ coef)

table(a.test$nytd2_homeless)
table(a.test$blkafram)
table(a.test$fc_chbehprb)
table(a.test$fc_everadpt)
table(a.test$nytd2_children)
table(a.test$s_delinqntsv)



































lapply(test.am$imputations[[i]], glm(nytd2_homeless ~ nytd2_children + nytd2_pubhousas + blkafram + s_delinqntsv + fc_everadpt + fc_chbehprb, data = test.am$imputations[[i]], family = binomial, weights = weight))

?list

summary(yar_mod_imp5)

coef(yar_mod_imp)
coef(yar_mod)
coef(yar_mod) - coef(yar_mod_imp)
(coef(yar_mod) - coef(yar_mod_imp)) / coef(yar_mod)
(coef(yar_mod) - coef(yar_mod_imp)) / coef(yar_mod) * 100













test.am$imputations[[1]][is.na(test.am$imputations[[1]]weight)]

filter(test.am$imputations[[1]], is.na(weight))

?is.na

length(na.omit(test.am$imputations[[1]]$fosfamst))


names(a.test)
summary(lm(a.test$blkafram ~ a.test$fc_neglect))

sapply(1:ncol(a.test), function(i) all(a.test$amiakn ==a.test[,i]))

# ('s1_edlevlsv', 'ageadopt', 'manrem', 'placeout', 'ctkfamst', 'fosfamst')

b.test <- sqlQuery(con, "SELECT
	npd.*
	,s1.[lclfipssv] AS s1_lclfipssv	-- probably don't need this
	,fc.[ageadopt]
    ,fc.[totalrem]
    ,fc.[numplep]
    ,fc.[manrem]
	,IIF(fc.[inatstart] = 'Yes', 1, IIF(fc.[inatstart] = 'No', 0, NULL)) AS fc_inatstart
	,IIF(fc.[exited] = 'Yes', 1, IIF(fc.[exited] = 'No', 0, NULL)) AS fc_exited
FROM [dbCoreAdministrativeTables].[public_data].[NYTD_Outcomes_people_dim] AS npd
JOIN [public_data].[NYTD_Outcomes_Waves_1_2] AS nytd1
	ON npd.stchid = nytd1.stchid
	AND nytd1.cd_wave = 1
JOIN [public_data].[NYTD_Outcomes_Waves_1_2] AS nytd2
	ON npd.stchid = nytd2.stchid
	AND nytd2.cd_wave = 2
LEFT JOIN [public_data].[NYTD_Services_2011_2012_2013_truncated] AS s1 
	ON npd.stchid = s1.stchid
	AND npd.sex = s1.sex
	AND npd.dobyr = YEAR(CONVERT(date, s1.dob))
	AND npd.dobmon = MONTH(CONVERT(date, s1.dob))
	AND s1.datayear = 2011
LEFT JOIN [public_data].[NYTD_Services_2011_2012_2013_truncated] AS s2 
	ON npd.stchid = s2.stchid
	AND npd.sex = s2.sex
	AND npd.dobyr = YEAR(CONVERT(date, s2.dob))
	AND npd.dobmon = MONTH(CONVERT(date, s2.dob))
	AND s2.datayear = 2012
LEFT JOIN [public_data].[NYTD_Services_2011_2012_2013_truncated] AS s3 
	ON npd.stchid = s3.stchid
	AND npd.sex = s3.sex
	AND npd.dobyr = YEAR(CONVERT(date, s3.dob))
	AND npd.dobmon = MONTH(CONVERT(date, s3.dob))
	AND s3.datayear = 2013
LEFT JOIN 
	(SELECT 
		RANK() OVER(PARTITION BY fc.recnumbr, fc.st, npd.stchid ORDER BY fc.datayear DESC) AS r_order
		,fc.recnumbr AS recnumbr_fc
		,fc.st
		,npd.stchid
		,fc.datayear
	FROM [dbCoreAdministrativeTables].[public_data].[NYTD_Outcomes_people_dim] AS npd
		INNER JOIN [public_data].[afcars_foster_care_00_13] AS fc
			ON npd.recnumbr = fc.RecNumbr
			AND npd.st = fc.St) AS fcid
	ON npd.stchid = fcid.stchid
	AND r_order = 1
LEFT JOIN [public_data].[afcars_foster_care_00_13] AS fc
	ON npd.recnumbr = fc.recnumbr
	AND npd.st = fc.st
	AND fcid.datayear = fc.datayear
ORDER BY 
	npd.stchid")

test.am <- amelia(b.test, idvars = c('stchid', 'recnumbr', 'dob', 'dobyr', 'dobmon', 'st'), m = 1, noms = c('sex', 'manrem'), ord = 'ageadopt')	

head(b.test)	
	
missmap(a.test)

dim(a.test)[[1]]/2

a.test$s1_lclfipssv

head(test.am$imputations[[1]], 50)

head(as.Date(a.test$dob))

hist(as.Date(a.test$dob), breaks = 365)

tail(arrange(a.test, dob), 100)

library(dplyr)

a.test %>% select(dob, s1_fcstatsv, s1_tribesv, s1_delinqntsv) %>% filter(is.na(s1_fcstatsv), is.na(s1_tribesv), is.na(s1_delinqntsv)) %>% arrange(dob)

						IIF(amiakn = 1 OR (asian = 1 OR blkafram = 1 OR hawaiipi = 1 OR white = 1), 6, 
							IIF(asian = 1 OR (amiakn = 1 OR blkafram = 1 OR hawaiipi = 1 OR white = 1), 6,
								IIF(blkafram = 1 OR (amiakn = 1 OR asian = 1 OR hawaiipi = 1 OR white = 1), 6,
									IIF(hawaiipi = 1 OR (amiakn = 1 OR asian = 1 OR blkafram = 1 OR white = 1), 6, 
										IIF(white = 1 OR (amiakn = 1 OR asian = 1 OR blkafram = 1 OR hawaiipi = 1), 6




