lapply(c("sem","ggplot2", "psych", "RCurl", "irr", "nortest", 
         "moments","GPArotation","nFactors","boot","psy", "car",
         "vcd", "gridExtra","mi","VIM","gdata","sqldf",
         "reshape2","mclust","foreign","survival","memisc","lme4",
         "lmerTest","dplyr","QCA","VennDiagram","qgraph","igraph",
         "ltm","gmodels","eRm","mirt","dplyr","devtools","reshape",
         "mice"),
       library, character.only=T)


data<-read.csv("D:/Google Drive/[PES] life discussions/Tz_Validations/Tz_MHpostTBI_data.csv",sep=',')

data_validation<-subset(data[data$redcap_event_name=="enrollment_arm_1",])

#FIM 30 items
FIM_data30<-with(data_validation,data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30))
data_imputed <- mice(FIM_data30, seed = 2222, m=10)
FIM_data30<-mice::complete(data_imputed,4)
#FIM Motor
FIM_Motor30<-with(FIM_data30,data.frame(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16))
#FIM Cognitive
FIM_Cognitive30<-with(FIM_data30,data.frame(g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,g29,g30))


#MOCA
MOCA_data1<-with(data_validation,data.frame(f1a,f1c,f1d,f1e,f2c,f2d,f15,f16,f17,f18,f19,f20,f21,f21b,f22,f23))
#SUM f1+f2, f21 item
MOCA_data1$f1 <- rowSums(MOCA_data1[ , 1:5]) 
#MOCA_data1$f21 <- rowSums(MOCA_data1[ , 13:14]) 
MOCA_data1$f17 <- rowSums(MOCA_data1[ , 9:10]) 
MOCA_data1<-with(MOCA_data1,data.frame(f1,f15,f16,f17,f19,f20,f21,f21b,f22,f23))
data_imputed <- mice(MOCA_data1, seed = 2222, m=10)
MOCA_data<-mice::complete(data_imputed,4)

#Kessler
kessler_data1<-with(data_validation,data.frame(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10))
data_imputed <- mice(kessler_data1, seed = 2222, m=10)
kessler_data<-mice::complete(data_imputed,4)

#SF8
sf8_data1<-with(data_validation,data.frame(sf8_b1,
                                           sf8_b2,
                                           sf8_b3,
                                           sf8_b4,
                                           sf8_b5,
                                           sf8_b6,
                                           sf8_b7,
                                           sf8_b8))
data_imputed <- mice(sf8_data1, seed = 2222, m=10)
sf8_data<-complete(data_imputed,4)

#PHQ9
phq9_data1<-with(data_validation,data.frame(phq9_b11,
                                            phq9_b12,
                                            phq9_b13,
                                            phq9_b14,
                                            phq9_b15,
                                            phq9_b16,
                                            phq9_b17,
                                            phq9_b18,
                                            phq9_b19
))
data_imputed <- mice(phq9_data1, seed = 2222, m=10)
phq9_data<-mice::complete(data_imputed,4)

#AUDIT
audit_data<-with(data_validation,data.frame(h1,h2,h3,h4,h5,h6,h7,h8,h9,h10))
# audit_data1<-with(data_validation,data.frame(
# 	h1,h2,h3))
# audit_data2<-with(data_validation,data.frame(
# 	h4,h5,h6,h7,h8,h9,h10))
# audit_data2_3<-with(data_validation,data.frame(
# 	h4,h5,h6))
# audit_data3_3<-with(data_validation,data.frame(
# 	h7,h8,h9,h10))
data_imputed <- mice(audit_data, seed = 2222, m=10)
audit_data<-mice::complete(data_imputed,4)


#CONFIRMATORY FACTOR ANALYSIS

# FIM 30 itens
# 2 factor model
cfa_model <- '
MF =~ g1 + g2 + g3 + g4 + g5 + g6 + g8 + g9 + g10 + g11 + g12 + g13 + g14 + g15 + g16 + +g20 + g25
CF =~ g1 + g8 + g17 + g18 + g19 + g20 + g21 + g22 + g23 + g24 + g25 + g26 + g27 + g28 + g29 + g30
g19 ~~ g20
'

fit <- lavaan::cfa(cfa_model,
                   data = FIM_data30,
                   estimator="WLSMV",
                   ordered=colnames(FIM_data30)
)
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
)
)
# AIC(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")





#MOCA
# 1 factor model
cfa_model <- '
MOCA =~ f1 + f15 + f16 + f17 + f19 + f20 + f21 + f21b + f22 + f23
#f17 ~~ f21
'

fit <- lavaan::cfa(cfa_model,
                   data = MOCA_data,
                   estimator="WLSMV",
                   ordered=colnames(MOCA_data)
)
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
)
)
# AIC(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
#lavInspect(fit,what="th") 


#KESSLER
# 1 factor model
cfa_model <- '
Kessler =~  d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10
#
Kessler ~~ Kessler
#cov
# d2 ~~  d9
d5 ~~  d6
# d8 ~~  d10
'

fit <- lavaan::cfa(cfa_model,
                   data = kessler_data,
                   estimator="WLSMV",
                   ordered=colnames(kessler_data)
                   )
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
                                          )
                    )
# AIC(fit)
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
lavInspect(fit,what="th")

#KESSLER
# 2 factors model ###########################
cfa_model <- '
Depression =~  d1 + d4 + d7 + d8 + d9 + d10
Anxiety =~ d2 + d3 + d5 + d6
#
# Depression ~~ Depression
# Anxiety ~~ Anxiety
#cov
# d2 ~~  d9
# d5 ~~  d6
# d7 ~~  d8
'

fit <- lavaan::cfa(cfa_model,
                   data = kessler_data,
                   estimator="WLSMV",
                   ordered=colnames(kessler_data))
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled",
                                          "df.scaled"
))
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
                                  ci = TRUE,
                                  standardized = TRUE)
subset(Est, op == "=~")
lavInspect(fit,what="th")


#SF8
# 1 factor model
cfa_model <- '
SF8 =~  sf8_b1 + sf8_b2 + sf8_b3 + 
sf8_b4 + sf8_b5 + sf8_b6 + sf8_b7 + sf8_b8
#
#cov
sf8_b2 ~~ sf8_b8
sf8_b1 ~~ sf8_b5
'

fit <- lavaan::cfa(cfa_model,
                   data = sf8_data,
                   estimator="WLSMV",
                   ordered=colnames(sf8_data)
)
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
))
# AIC(fit)
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit, ci = TRUE, standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")
lavInspect(fit,what="th")

#SF8
# 2 factors model ###########################
cfa_model <- '
PHC =~  sf8_b1 + sf8_b2 + sf8_b3 + sf8_b4 + sf8_b5
MHC =~ sf8_b5 + sf8_b6 + sf8_b7 + sf8_b8
#
#cov
# sf8_b2 ~~ sf8_b8
sf8_b1 ~~ sf8_b5
'

fit <- lavaan::cfa(cfa_model,
                   data = sf8_data,
                   estimator="WLSMV",
                   ordered=colnames(sf8_data)
                   )
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
                                          ))
# AIC(fit)
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
                                  ci = TRUE,
                                  standardized = TRUE)
subset(Est, op == "=~")
lavInspect(fit,what="th")


#PHQ9
# 1 factor model
cfa_model <- '
PHQ9 =~  phq9_b11 + phq9_b12 + phq9_b13 + phq9_b14 + 
phq9_b15 + phq9_b16 + phq9_b17 + phq9_b18 + phq9_b19
'

fit <- lavaan::cfa(cfa_model,
                   data = phq9_data,
                   estimator="WLSMV",
                   ordered=colnames(phq9_data)
                   )
summary(fit, fit.measures=TRUE)
lavaan::fitMeasures(fit, fit.measures = c("rmsea.scaled",
                                          "rmsea.ci.lower.scaled",
                                          "rmsea.ci.upper.scaled",
                                          "cfi.scaled",
                                          "tli.scaled",
                                          "nnfi.scaled",
                                          "chisq.scaled",
                                          "pvalue.scaled"
                                          ))
# AIC(fit)
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
                                  ci = TRUE,
                                  standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")


#AUDIT
#1factor model ###########
audit_model <- '
Audit =~  h1 + h2 + h3 + h4 + h5 + h6 + h7 + h8 + h9 + h10
			 '
fit <- lavaan::cfa(audit_model,
                   data = audit_data,
                   estimator="WLSMV",
                   ordered=names(audit_data))
summary(fit,
        fit.measures=TRUE)
lavaan::fitMeasures(fit,
                    fit.measures = "all")
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
                                  ci = TRUE,
                                  standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")

#AUDIT
# 2 factor model ###########
audit_model2 <- '
Audit =~  h1 + h2 + h3
Audit2 =~ h4 + h5 + h6 + h7 + h8 + h9 + h10
'

fit <- lavaan::cfa(audit_model2,
                   data = audit_data,
                   estimator="WLSM",
                   ordered=names(audit_data))
summary(fit,
        fit.measures=TRUE)
lavaan::fitMeasures(fit,
                    fit.measures = "all")
parameterEstimates(fit)
Est <- lavaan::parameterEstimates(fit,
                                  ci = TRUE,
                                  standardized = TRUE)
subset(Est, op == "=~")
subset(Est, op == "~~")

