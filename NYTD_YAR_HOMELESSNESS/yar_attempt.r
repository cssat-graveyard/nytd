
library(Amelia)

set.seed(4735)

con <- odbcConnect("POC")

yar <- sqlQuery(con, "SELECT 
							O1.recnumbr
							,O1.st
							,IIF(O2.children = 2, NULL, O2.children) AS children
							,IIF(O2.homeless = 2, NULL, O2.homeless) AS homeless
							,IIF(O2.pubhousas IN (2, 77), NULL, IIF(O2.pubhousas = 88, 0, O2.pubhousas)) AS pubhousas
							,IIF(O2.blkafram = 3, NULL, O2.blkafram) AS blkafram
							,COALESCE(S1.delinqntsv, S2.delinqntsv, S3.delinqntsv) AS delinqntsv
							,IIF(FC.everadpt = 'No, has never been legally adopted' OR FC.everadpt = 'No', 0, 
								IIF(FC.everadpt = 'Yes, child has been legally adopted' OR FC.everadpt = 'Yes', 1, 
									IIF(FC.everadpt = 'Unable to determine', NULL, FC.everadpt))) AS everadpt
							,IIF(chbehprb = 'No', 0, IIF(chbehprb = 'Yes', 1, chbehprb)) AS chbehprb
							,O2.weight
						FROM
							(SELECT 
								RANK() OVER(PARTITION BY FC.recnumbr ORDER BY FC.datayear DESC) AS R_ORDER
								,FC.recnumbr AS recnumbr_fc
								,ID1.RecNumbr AS recnumber_nytd
								,FC.st AS st_fc
								,ID1.St AS st_nytd
								,ID1.stchid
								,datayear
								,ID1.sex
							FROM
								(SELECT 
									O.St
									,O.StChID
									,O.RecNumbr
									,O.sex
								FROM [public_data].[NYTD_Outcomes_Waves_1_2] AS O
								WHERE O.cd_wave = 2
								AND responded = 'Responded to Survey') AS ID1
							LEFT JOIN [public_data].[afcars_foster_care_00_12] AS FC
								ON FC.recnumbr = ID1.RecNumbr
								AND FC.st = ID1.St) AS ID 
						LEFT JOIN [public_data].[NYTD_Outcomes_Waves_1_2] AS O1
							ON ID.StChID = O1.StChID
							AND O1.cd_wave = 1
						LEFT JOIN [public_data].[NYTD_Outcomes_Waves_1_2] AS O2
							ON ID.StChID = O2.StChID
							AND O2.cd_wave = 2
						LEFT JOIN [public_data].[afcars_foster_care_00_12] AS FC
							ON ID.St_fc = FC.St
							AND ID.RecNumbr_fc = FC.recnumbr
							AND ID.datayear = FC.datayear
							AND ID.sex = FC.sex
						LEFT JOIN [public_data].[NYTD_Services_2011_2012_2013_truncated] AS S1 -- picking up a few records WHY?
							ON ID.stchid = S1.stchid
							AND ID.sex = S1.sex
							AND S1.datayear = 2011
						LEFT JOIN [public_data].[NYTD_Services_2011_2012_2013_truncated] AS S2 -- picking up a few more WHY?
							ON ID.stchid = S2.stchid
							AND ID.sex = S2.sex
							AND S2.datayear = 2012
						LEFT JOIN [public_data].[NYTD_Services_2011_2012_2013_truncated] AS S3 -- picking up a few more WHY?
							ON ID.stchid = S3.stchid
							AND ID.sex = S3.sex
							AND S3.datayear = 2012
						WHERE R_ORDER = 1 
							AND O1.recnumbr IS NOT NULL
						ORDER BY O1.st")

dim(yar)							

missmap(yar)

yar_mod <- glm(homeless ~ children + pubhousas + blkafram + delinqntsv + everadpt + chbehprb, data = yar, family = binomial, weights = weight)

summary(yar_mod)

plot(yar_mod)

plot(yar$weight)

table(test$homeless)
table(yar$children, test$homeless)
table(yar$pubhousas)
table(yar$delinqntsv)
table(yar$everadpt)
table(yar$chbehprb)
table(yar$blkafram)
  
for (i in 1:ncol(test)) {
	print(class(test[,i]))
} 
  
test <- select(yar, recnumbr, homeless, children, pubhousas, blkafram, delinqntsv, everadpt, chbehprb, weight) 

test.am <- amelia(test, cs = "recnumbr", m = 10, ord = c("homeless", "children", "pubhousas", "blkafram", "delinqntsv", "everadpt", "chbehprb"))  

summary(test.am$imputations[[2]])
							
yar_mod <- glm(homeless ~ children + pubhousas + blkafram + delinqntsv + everadpt + chbehprb, data = test.am$imputations[[10]], family = binomial, weights = weight)
summary(yar_mod)							

table(test.am$imputations[[2]]$children, test.am$imputations[[2]]$homeless)
table(test.am$imputations[[4]]$blkafram)

table()

# for lasso, use glmnet

install.packages("glmnet")
library(glmnet)

# for randomForest

install.packages("randomForest")
library(randomForest)

library(installr)

updateR()
























