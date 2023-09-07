library(multcomp)
library(sjPlot)

############ Modelle berechnen ########################################
message("Models for accuracy as tempo_dev")
model_tempo_dev_1 <- lmerTest::lmer(tempo_dev ~ age_group * setting + tempo + (1|p_id),data=na.omit(iso_features))
model_tempo_dev_2 <- lmerTest::lmer(tempo_dev ~ age_group + setting + tempo + (1|p_id),data=na.omit(iso_features))
model_tempo_dev_3 <- lmerTest::lmer(tempo_dev ~ age_group + setting * tempo + (1|p_id),data=na.omit(iso_features))
model_tempo_dev_4 <- lmerTest::lmer(tempo_dev ~ age_group * tempo + setting + (1|p_id),data=na.omit(iso_features))


#bestes Modell  tempo_dev ohne Beat bestimmen

anova(model_tempo_dev_1, model_tempo_dev_2, model_tempo_dev_3, model_tempo_dev_4)

#Modell Accuracy mit tempo deviation mit Beat 
model_tempo_dev_1a <- lmerTest::lmer(tempo_dev ~ age_group * setting + tempo + beat_prod + (1|p_id),data=na.omit(iso_features))
model_tempo_dev_1b <- lmerTest::lmer(tempo_dev ~ age_group * setting + tempo + beat_perc + (1|p_id),data=na.omit(iso_features))
model_tempo_dev_1c <- lmerTest::lmer(tempo_dev ~ age_group * setting + tempo + beat_perc + beat_prod + (1|p_id),data=na.omit(iso_features)) 
model_tempo_dev_1d <- lmerTest::lmer(tempo_dev ~ age_group * setting + tempo + beat_prod + beat_prod:age_group + (1|p_id),data=na.omit(iso_features)) #%>% tab_model(file ="Modell_tempo_dev_Winner_beat.doc")


#bestes Modell  tempo_dev mit Beat bestimmen
anova(model_tempo_dev_1a, model_tempo_dev_1b, model_tempo_dev_1c, model_tempo_dev_1d)

#bestes Modell  tempo_dev mit Beat bestimmen
anova(model_tempo_dev_1, model_tempo_dev_1d)


message("Models for accuracy as log_tempo_abs_dev")
model_tempo_abs_dev_1 <- lmerTest::lmer(log_tempo_abs_dev ~ age_group * setting + tempo +(1|p_id),data=na.omit(iso_features))
model_tempo_abs_dev_2 <- lmerTest::lmer(log_tempo_abs_dev ~ age_group + setting + tempo +(1|p_id),data=na.omit(iso_features))
model_tempo_abs_dev_3 <- lmerTest::lmer(log_tempo_abs_dev ~ age_group + setting * tempo +(1|p_id),data=na.omit(iso_features))
model_tempo_abs_dev_4 <- lmerTest::lmer(log_tempo_abs_dev ~ age_group * tempo + setting + (1|p_id),data=na.omit(iso_features))

anova(model_tempo_abs_dev_1, model_tempo_abs_dev_2, model_tempo_abs_dev_3, model_tempo_abs_dev_4)


message("Models Accuracy wuth  log_tempo_abs_dev  and  beat covariates") 
model_tempo_abs_dev_4a <- lmerTest::lmer(log_tempo_abs_dev ~ age_group * tempo + setting + beat_prod + (1|p_id),data=na.omit(iso_features))
model_tempo_abs_dev_4b <- lmerTest::lmer(log_tempo_abs_dev ~ age_group * tempo + setting + beat_perc + (1|p_id),data=na.omit(iso_features))
model_tempo_abs_dev_4c <- lmerTest::lmer(log_tempo_abs_dev ~ age_group * tempo + setting + beat_perc + beat_prod + (1|p_id),data=na.omit(iso_features)) 
model_tempo_abs_dev_4d <- lmerTest::lmer(log_tempo_abs_dev ~ age_group * tempo + setting + beat_prod + beat_prod:age_group + (1|p_id), data = na.omit(iso_features)) %>% sjPlot::tab_model(file ="Modell_tempo_abs_dev_Winner_beat.doc")

anova(model_tempo_abs_dev_4a, model_tempo_abs_dev_4b, model_tempo_abs_dev_4c, model_tempo_abs_dev_4d)

anova(model_tempo_abs_dev_4, model_tempo_abs_dev_4d)

message("Models for  precision as log(circ_sd)") 

model_circ_sd_1 <- lmerTest::lmer(log(circ_sd)~age_group * setting + tempo +(1|p_id),data=na.omit(iso_features)) #%>% tab_model(file ="Modell_logCircSd_Winner_ohneBeat.doc")
model_circ_sd_2 <- lmerTest::lmer(log(circ_sd)~age_group + setting + tempo +(1|p_id),data=na.omit(iso_features)) #%>% tab_model()
model_circ_sd_3 <- lmerTest::lmer(log(circ_sd)~age_group + setting * tempo + (1|p_id),data=na.omit(iso_features))
model_circ_sd_4 <- lmerTest::lmer(log(circ_sd)~age_group * tempo + setting + (1|p_id),data=na.omit(iso_features))

#bestes Modell  Circ_SD ohne Beat bestimmen
anova(model_circ_sd_1, model_circ_sd_2, model_circ_sd_3, model_circ_sd_4)

#Modelle  Circ_SD mit Beat
model_circ_sd_1a <- lmerTest::lmer(log(circ_sd)~age_group * setting + tempo + beat_prod + (1|p_id),data=na.omit(iso_features))
model_circ_sd_1b <- lmerTest::lmer(log(circ_sd)~age_group * setting + tempo + beat_perc + (1|p_id),data=na.omit(iso_features))
model_circ_sd_1c <- lmerTest::lmer(log(circ_sd)~age_group * setting + tempo + beat_perc + beat_prod + (1|p_id),data=na.omit(iso_features)) #%>% tab_model(file ="Modell_logCircSd_Winner_beat.doc")
model_circ_sd_1d <- lmerTest::lmer(log(circ_sd)~age_group * setting + tempo + beat_prod + beat_prod:age_group + (1|p_id),data=na.omit(iso_features))
#bestes Modell  Circ_SD mit Beat bestimmen
anova(model_circ_sd_1a, model_circ_sd_1b, model_circ_sd_1c, model_circ_sd_1d)

#Synchronization mit oder ohne Beat
anova(model_circ_sd_1, model_circ_sd_1c)

#Ergebnisse Synchronization zu den Modellen ausgeben lassen
model_circ_sd_1c %>% tab_model()

###############################
message("Models for  phase as circ_mean") 

model_circ_mean_1 <- lmerTest::lmer(circ_mean~age_group * setting + tempo + (1|p_id),data=na.omit(iso_features))#%>% tab_model()
model_circ_mean_2 <- lmerTest::lmer(circ_mean~age_group + setting + tempo + (1|p_id),data=na.omit(iso_features)) #%>% tab_model()
model_circ_mean_3 <- lmerTest::lmer(circ_mean~age_group + setting * tempo + (1|p_id),data=na.omit(iso_features)) #%>% tab_model(file ="Modell_circ_mean_1stWinner_ohneBeat.doc")
model_circ_mean_4 <- lmerTest::lmer(circ_mean~age_group * tempo + setting + (1|p_id),data=na.omit(iso_features)) #%>% tab_model()

#bestes Modell circ_mean ohne Beat
anova(model_circ_mean_1, model_circ_mean_2, model_circ_mean_3, model_circ_mean_4)


message("Models for  phase as circ_mean with beat prod/pre covariates") 
model_circ_mean_3a <- lmerTest::lmer(circ_mean~age_group + setting * tempo + beat_prod + (1|p_id),data=na.omit(iso_features))
model_circ_mean_3b <- lmerTest::lmer(circ_mean~age_group + setting * tempo + beat_perc + (1|p_id),data=na.omit(iso_features))
model_circ_mean_3c <- lmerTest::lmer(circ_mean~age_group + setting * tempo + beat_prod + beat_perc + (1|p_id),data=na.omit(iso_features))# %>% tab_model(file ="Modell_circ_mean_2ndWinner_mit_Beat.doc")
model_circ_mean_3d <- lmerTest::lmer(circ_mean~age_group + setting * tempo + beat_prod:age_group + (1|p_id),data=na.omit(iso_features))

#bestes Modell circ_mean mit Beat
anova(model_circ_mean_3a, model_circ_mean_3b, model_circ_mean_3c, model_circ_mean_3d)


#Modell circ_mean mit oder ohne Beat
anova(model_circ_mean_3, model_circ_mean_3c)



########################################
#Modell med_ioi
message("Models for  accuracy as med_ioi") 

model_med_ioi_1 <- lmerTest::lmer(med_ioi~age_group * setting + tempo + (1|p_id),data=na.omit(iso_features))
model_med_ioi_2 <- lmerTest::lmer(med_ioi~age_group + setting + tempo + (1|p_id),data=na.omit(iso_features))
model_med_ioi_3 <- lmerTest::lmer(med_ioi~age_group + setting * tempo + (1|p_id),data=na.omit(iso_features))
model_med_ioi_4 <- lmerTest::lmer(med_ioi~age_group * tempo + setting + (1|p_id),data=na.omit(iso_features))

#bestes Modell ohne Beat med_ioi
anova(model_med_ioi_1, model_med_ioi_2, model_med_ioi_3, model_med_ioi_4)


#Modelle med_ioi mit beat für med_ioi
model_med_ioi_3a <- lmerTest::lmer(med_ioi~age_group + setting * tempo + beat_prod + (1|p_id),data=na.omit(iso_features))
model_med_ioi_3b <- lmerTest::lmer(med_ioi~age_group + setting * tempo + beat_perc + (1|p_id),data=na.omit(iso_features))
model_med_ioi_3c <- lmerTest::lmer(med_ioi~age_group + setting * tempo + beat_perc + beat_prod + (1|p_id),data=na.omit(iso_features))
model_med_ioi_3d <- lmerTest::lmer(med_ioi~age_group + setting * tempo + beat_prod:age_group + (1|p_id),data=na.omit(iso_features)) #%>% tab_model(file ="Modell_med_ioi_1stWinner_mit_Beat.doc")

#bestes Modell med_ioi mit Beat für med_ioi
anova(model_med_ioi_3a, model_med_ioi_3b, model_med_ioi_3c, model_med_ioi_3d) #%>% summary () # ergibt Fehler im Modell


#Alternative für Modell Tempo Adjustment mit beat für med_ioi basierend auf Modell 1
model_med_ioi_1a <- lmerTest::lmer(med_ioi~age_group * setting + tempo + beat_prod + (1|p_id),data=na.omit(iso_features))
model_med_ioi_1b <- lmerTest::lmer(med_ioi~age_group * setting + tempo + beat_perc + (1|p_id),data=na.omit(iso_features))
model_med_ioi_1c <- lmerTest::lmer(med_ioi~age_group * setting + tempo + beat_perc + beat_prod + (1|p_id),data=na.omit(iso_features))
model_med_ioi_1d <- lmerTest::lmer(med_ioi~age_group * setting + tempo + beat_prod:age_group + (1|p_id),data=na.omit(iso_features))


#bestes Modell mit Beat für med_ioi basierend auf Modell 1
anova(model_med_ioi_1a, model_med_ioi_1b, model_med_ioi_1c, model_med_ioi_1d) 

#Tempo Adjustment med_ioi Modell  mit oder ohne Beat
anova(model_med_ioi_1, model_med_ioi_1c, model_med_ioi_1d)



###########################################
message("Models for  precision as log(sd_ioi)") 

model_sd_ioi_1 <- lmerTest::lmer(log(sd_ioi)~age_group * setting + tempo + (1|p_id),data=na.omit(iso_features)) #%>% tab_model(file ="Modell_sd_ioi_2ndWinner.doc")
model_sd_ioi_2 <- lmerTest::lmer(log(sd_ioi)~age_group + setting + tempo + (1|p_id),data=na.omit(iso_features))
model_sd_ioi_3 <- lmerTest::lmer(log(sd_ioi)~age_group + setting * tempo + (1|p_id),data=na.omit(iso_features))
model_sd_ioi_4 <- lmerTest::lmer(log(sd_ioi)~age_group * tempo + setting + (1|p_id),data=na.omit(iso_features))

#bestes Modell ohne Beat sd_ioi
anova(model_sd_ioi_1, model_sd_ioi_2, model_sd_ioi_3, model_sd_ioi_4)  #REML = TRUE)%>%summary() refit = FALSE


#Modelle  mit beat für sd_ioi
model_sd_ioi_1a <- lmerTest::lmer(log(sd_ioi)~age_group * setting + tempo + beat_prod + (1|p_id), data=na.omit(iso_features))
model_sd_ioi_1b <- lmerTest::lmer(log(sd_ioi)~age_group * setting + tempo + beat_perc + (1|p_id), data=na.omit(iso_features))
model_sd_ioi_1c <- lmerTest::lmer(log(sd_ioi)~age_group * setting + tempo + beat_prod + beat_perc + (1|p_id), data=na.omit(iso_features))# %>% tab_model(file ="Modell_sd_ioi_1stWinner.doc")
model_sd_ioi_1d <- lmerTest::lmer(log(sd_ioi)~age_group * setting + tempo + beat_prod:age_group + (1|p_id), data=na.omit(iso_features))

#bestes Modell  mit Beat für sd_ioi
anova(model_sd_ioi_1a, model_sd_ioi_1b, model_sd_ioi_1c, model_sd_ioi_1d)

#sd_ioi Modell  mit oder ohne Beat
anova(model_sd_ioi_1, model_sd_ioi_1c)

#summary(model_sd_ioi_1c) %>% tab_model()


################## Post-hoc Vergleiche rechnen #########################################
#Accuracy
#### Modell fehlt noch

mod_accuracy_dev <- lmerTest::lmer(tempo_dev ~ age_group * tempo + setting + beat_prod + beat_prod:age_group + (1|p_id),data=na.omit(iso_features))
summary(glht(mod_accuracy_dev, linfct = mcp(age_group = "Tukey"), test = adjusted("holm")))

mod_accuracy_abs_dev <-  lmerTest::lmer(log_tempo_abs_dev ~ age_group * tempo + setting + beat_prod + beat_prod:age_group + (1|p_id),data=na.omit(iso_features))
summary(glht(mod_accuracy_abs_dev, linfct = mcp(age_group = "Tukey"), test = adjusted("holm")))


#Phase
mod_phase <- lmerTest::lmer(circ_mean~age_group + setting * tempo + beat_prod + beat_perc + (1|p_id),data=na.omit(iso_features))
# Tukey HSD für Altersgruppenverleiche mit Korrektur
summary(glht(mod_phase, linfct = mcp(age_group = "Tukey"), test = adjusted("holm")))


#SD_IOI
mod_sd_ioi <- lmerTest::lmer(log(sd_ioi)~age_group * setting + tempo + beat_prod + beat_perc + (1|p_id),data=na.omit(iso_features))
# Tukey HSD für Altersgruppenverleiche mit Korrektur
summary(glht(mod_sd_ioi, linfct = mcp(age_group = "Tukey"), test = adjusted("holm")))


#Circ_SD (Precision)
mod_circ_sd <- lmerTest::lmer(log(circ_sd)~age_group * setting + tempo + beat_perc + beat_prod + (1|p_id),data=na.omit(iso_features))
# Tukey HSD für Altersgruppenverleiche mit Korrektur
summary(glht(mod_circ_sd, linfct = mcp(age_group = "Tukey"), test = adjusted("holm")))

