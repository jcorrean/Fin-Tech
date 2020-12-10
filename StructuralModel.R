library(readr)
Brandstat <- read_delim("RawDataBrandstat.csv", ",", escape_double = FALSE, trim_ws = TRUE)
automation <- Brandstat[14:20]
FacCond <- Brandstat[10:13]
library(lavaan);
modelData <- Brandstat
model<-"
! regressions 
   Automatization=~Aut__Aut1*Aut1
   Automatization=~Aut__Aut2*Aut2
   Automatization=~Aut__Aut3*Aut3
   Automatization=~Aut__Aut4*Aut4
   Automatization=~Aut__Aut5*Aut5
   Automatization=~Aut__Aut6*Aut6
   Automatization=~Aut__Aut7*Aut7
   Facilitating_Conditions=~Facilitating_Conditions__FC1*FC1
   Facilitating_Conditions=~Facilitating_Conditions__FC4*FC4
   Facilitating_Conditions=~Facilitating_Conditions__FC2*FC2
   Facilitating_Conditions=~Facilitating_Conditions__FC3*FC3
   Facilitating_Conditions=~Facilitating_Conditions__Automatization*Automatization
! residuals, variances and covariances
   Automatization ~~ 1.0*Automatization
   Aut1 ~~ VAR_Aut1*Aut1
   Aut2 ~~ VAR_Aut2*Aut2
   Aut3 ~~ VAR_Aut3*Aut3
   Aut4 ~~ VAR_Aut4*Aut4
   Aut5 ~~ VAR_Aut5*Aut5
   Aut6 ~~ VAR_Aut6*Aut6
   Aut7 ~~ VAR_Aut7*Aut7
   Facilitating_Conditions ~~ 1.0*Facilitating_Conditions
   FC1 ~~ VAR_FC1*FC1
   FC4 ~~ VAR_FC4*FC4
   FC2 ~~ VAR_FC2*FC2
   FC3 ~~ VAR_FC3*FC3
! observed means
   Aut1~1;
   Aut2~1;
   Aut3~1;
   Aut4~1;
   Aut5~1;
   Aut6~1;
   Aut7~1;
   FC1~1;
   FC4~1;
   FC2~1;
   FC3~1;
";
result<- sem(model, data=modelData);
summary(result, fit.measures=TRUE);
estadarizados <- standardizedSolution(result, output = "text")
write.csv(estadarizados, file = "/home/juan/PaperBrandstrat/estandarizados.csv")
library(semTools)
reliability(result)
