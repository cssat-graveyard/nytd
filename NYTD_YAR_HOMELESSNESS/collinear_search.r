
library(corrplot)
library(caret)

test.a <- na.omit(a.test[! names(a.test) %in% c("st", "stchid", "recnumbr", "dob", "dobyr", "dobmon", "sex", "weight", "ageadopt", "ctkfamst", "fosfamst", "fips", "manrem")])

cor_test <- cor(test.a)
findCorrelation(cor_test, cutoff = .75)

setwd("S:/Data Portal/erik/NYTD_YAR_HOMELESSNESS")
png(file = "correlation_plot.png", width = 480*5, height = 480*5)
corrplot(cor_test, order = "hclust")
dev.off()


head(test.a[,c(95, 38)])

fc_test <- na.omit(a.test[names(a.test) %in% c("fc_clindis","fc_mr", "fc_vishear","fc_phydis", "fc_dsmiii", "fc_othermed", "fc_everadpt",  "totalrem", "numplep", "fc_phyabuse", "fc_sexabuse", "fc_neglect", "fc_aaparent", "fc_daparent", "fc_aachild", "fc_dachild", "fc_childis", "fc_chbehprb", "fc_prtsdied", "fc_prtsjail", "fc_nocope", "fc_abandmnt", "fc_relinqsh", "fc_housing", "fc_placeout", "fc_ivefc", "fc_iveaa", "fc_ivaafdc", "fc_ivdchsup", "fc_xixmedcd", "fc_ssiother", "fc_noa", "fcmntpay", "fc_inatstart", "fc_inatend", "fc_entered", "fc_exited", "fc_iswaiting", "fc_istpr", "latremlos", "settinglos", "lifelos", "fc_ageatend", "fc_agedout")])

fc_cor_test <- cor(fc_test)

corrplot(fc_cor_test, order = "hclust")

names(test.a)

nytd_test <- na.omit(a.test[names(a.test) %in% c("nytd1_currfte", "nytd1_currpte", "nytd1_emplysklls", "nytd1_socsecrty", "nytd1_educaid", "nytd1_pubfinas", "nytd1_pubfoodas", "nytd1_pubhousas", "nytd1_othrfinas", "nytd1_highedcert", "nytd1_currenroll", "nytd1_cnctadult", "nytd1_homeless", "nytd1_subabuse", "nytd1_incarc", "nytd1_children", "nytd1_marriage", "nytd1_medicaid", "nytd1_othrhlthin", "nytd1_medicalin", "nytd2_outcmfcs", "nytd2_currfte", "nytd2_currpte", "nytd2_emplysklls", "nytd2_socsecrty", "nytd2_educaid", "nytd2_pubfinas", "nytd2_pubfoodas", "nytd2_pubhousas", "nytd2_othrfinas", "nytd2_highedcert", "nytd2_currenroll", "nytd2_cnctadult", "nytd2_homeless", "nytd2_subabuse", "nytd2_incarc", "nytd2_children", "nytd2_marriage", "nytd2_medicaid", "nytd2_othrhlthin", "nytd2_medicalin")])

nytd_cor_test <- cor(nytd_test)

corrplot(nytd_cor_test, order = "hclust")







