#rct_data <- read_sav("C:/Users/lclem/Downloads/teens_OHAD 180717 - after typos treatment.sav")

#View(rct_data)

# age Age_CT1a
# gender Gender_CT1a
# hh size Persons_CT1a 
# physical/emotional abuse (ICASTf_m_C1) / ICASTf_m_C2
# poor parental supervision
# parenting depression/stress - DStot_CT1a

# hh size ---------------------------
hist(rct_data$Persons_CT1a)
min(rct_data$Persons_CT1a, na.rm = TRUE)
sd(rct_data$Persons_CT1a, na.rm = TRUE)
predict_hh_size <- glm(Persons_CT1a ~ Gender_CT1a + Age_CT1a,
                              family = "poisson",
                              data = rct_data)
summary(predict_hh_size)
newdata = with(rct_data, data.frame(Gender_CT1a, Age_CT1a))
hh_size <- Ecfun::simulate.glm(predict_hh_size, newdata = newdata)
hh_size <- hh_size$response$sim_1 + rnorm(551, 0, 2.2)
hh_size <- ifelse(hh_size < 2, 2, hh_size)
hh_size <- round(hh_size, 0)
hist(hh_size)


# PE abuse ------------------------
# ICAST16_CT1b (baseline); ICAST16_CT12 (post-test) - In the past month, how often did you hit, beat, slap or spank your teen  with bare hand?
# ICAST13_CT1b (baseline); ICAST13_CT12 (post-test) - In the past month, how often did you hit your teen with an object such as a stick, broom, switch or belt?
# ICAST15_CT1b (baseline); ICAST15_CT12 (post-test) - In the past month, how often did you push, grab or kick your teen ?
# ICAST19_CT1b (baseline); ICAST19_CT12 (post-test) - Shout or scream at your teen
# TODO: are these the right vars? What is _CT3?
rct_data$harsh_parenting <- with(rct_data, ICAST13_CT1b + ICAST15_CT1b + ICAST16_CT1b + ICAST19_CT1b)
rct_data$harsh_parenting_post <- with(rct_data, ICAST13_CT2 + ICAST15_CT2 + ICAST16_CT2 + ICAST19_CT2)

hist(rct_data$harsh_parenting)
predict_baseline_harsh <- glm(as.numeric(harsh_parenting) ~ Persons_CT1a + Gender_CT1a + Age_CT1a,
                              family = "poisson",
                              data = rct_data)
newdata = with(rct_data, data.frame(Persons_CT1a, Gender_CT1a, Age_CT1a))
baseline_PE_abuse <- simulate_data(newdata, model = predict_baseline_harsh, n = n, sd = sd(rct_data$harsh_parenting, na.rm = TRUE))
#hist(baseline_PE_abuse)

hist(rct_data$harsh_parenting_post)
sd(rct_data$harsh_parenting_post, na.rm = TRUE)
predict_post_harsh <- glm(as.numeric(harsh_parenting_post) ~ harsh_parenting,
                              family = "poisson",
                              data = rct_data)
newdata = with(rct_data, data.frame(harsh_parenting))
post_PE_abuse <- simulate_data(newdata, model = predict_post_harsh, n = n, sd = sd(rct_data$harsh_parenting_post, na.rm = TRUE))
#hist(post_PE_abuse)


## Poor Monitoring and Supervision: (sum items) ##############
rct_data$poor_monitoring <- with(rct_data, APQ10_CT1b + APQ31_CT1b + APQ30_CT1b)
# check: is it CT3???
rct_data$poor_monitoring_post <- with(rct_data, APQ10_CT2 + APQ31_CT3 + APQ30_CT2)
# total scale: APQ_Monit_CT1, APQ_Mon_C2

hist(rct_data$poor_monitoring)
# vars in model chosen via backwards selection
predict_baseline_monitoring <- glm(as.numeric(poor_monitoring) ~ harsh_parenting_post,
                              family = "poisson",
                              data = rct_data)
newdata = with(rct_data, data.frame(harsh_parenting))
baseline_poor_monitoring <- simulate_data(newdata, model = predict_baseline_monitoring, n = n, sd = sd(rct_data$poor_monitoring, na.rm = TRUE))
hist(baseline_poor_monitoring)

hist(rct_data$poor_monitoring_post)
# vars in model chosen via backwards selection
predict_post_monitoring <- glm(as.numeric(poor_monitoring_post) ~ poor_monitoring,
                                   family = "poisson",
                                   data = rct_data)
#summary(predict_post_monitoring)
newdata = with(rct_data, data.frame(poor_monitoring))
#n = nrow(newdata)
post_poor_monitoring <- simulate_data(newdata, model = predict_post_monitoring, n = n, sd = sd(rct_data$poor_monitoring_post, na.rm = TRUE))
hist(post_poor_monitoring)

# Parental Stress (sum of these 2 items) #
rct_data$parental_stress <- with(rct_data, PSS3_CT1a + PSS9_CT1a)
hist(rct_data$parental_stress)
predict_parental_stress <- glm(as.numeric(parental_stress) ~ Persons_CT1a,
                               family = "poisson",
                               data = rct_data)

# Caregiver depression: sum all three items
rct_data$caregiver_depression <- with(rct_data, DS6_CT1a + DS7_CT1a + DS8_CT1a_r_n)
hist(rct_data$caregiver_depression)
predict_caregiver_depression <- glm(as.numeric(caregiver_depression) ~ parental_stress,
                               family = "poisson",
                               data = rct_data)

           
# Food insecurity
hist(rct_data$Food_CT1a)
# vars in model chosen via backwards selection
predict_food_insecurity <- glm(as.numeric(Food_CT1a) ~ poor_monitoring_post + caregiver_depression,
                                family = "poisson",
                                data = rct_data)
summary(predict_food_insecurity)
newdata = with(rct_data, data.frame(poor_monitoring_post, caregiver_depression))
#n = nrow(newdata)
food_insecurity <- simulate_data(newdata, model = predict_food_insecurity, n = n, sd = sd(rct_data$Food_CT1a, na.rm = TRUE), round = TRUE, max = 7)
hist(food_insecurity)

# Financial stress:
hist(rct_data$ESSworry_CT1a)
# vars in model chosen via backwards selection
predict_financial_stress <- glm(as.numeric(ESSworry_CT1a) ~ caregiver_depression,
                               family = "poisson",
                               data = rct_data)
summary(predict_financial_stress)
newdata = with(rct_data, data.frame(caregiver_depression))
#n = nrow(newdata)
post_poor_monitoring <- simulate_data(newdata, model = predict_financial_stress, n = n, sd = sd(rct_data$ESSworry_CT1a, na.rm = TRUE), round = TRUE, max = 3)
hist(post_poor_monitoring)



# as.numeric(ESSworry_CT1a) ~ Persons_CT1a + Gender_CT1a + Age_CT1a + harsh_parenting + harsh_parenting_post + poor_monitoring + poor_monitoring_post,


### OLD CODE ###
# DStot
predict_stress <- glm(DStot_CT1a ~ Persons_CT1a + ICASTf_t_C1,
                      data = rct_data,
                      family = "gaussian")

# baseline - poor supervision
predict_baseline_apq <- glm(APQ_Mon_C1 ~ ICASTf_t_C1 + DStot_CT1a,
                            family = "poisson",
                            data = rct_data)

# post - poor supervision
predict_post_apq <- glm(APQ_Mon_C2 ~ APQ_Mon_C1 + DStot_CT1a + Persons_CT1a + Gender_CT1a + Age_CT1a,
                        family = "poisson",
                        data = rct_data)



# PARENTING STRESS #
newdata = data.frame(Persons_CT1a = hh_size, ICASTf_t_C1 = baseline_PE_abuse)
parenting_stress <- Ecfun::simulate.glm(predict_stress, newdata = newdata)
parenting_stress <- parenting_stress$response$sim_1 + rnorm(n, 0, 12)
parenting_stress <- ifelse(parenting_stress < 0, 0, parenting_stress)
parenting_stress <- round(parenting_stress, 0)
#hist(parenting_stress)

# PARENTING BASELINE SUPERVISION #
newdata = data.frame(ICASTf_t_C1 = baseline_PE_abuse, DStot_CT1a = parenting_stress)
baseline_supervision <- Ecfun::simulate.glm(predict_baseline_apq, newdata = newdata)
baseline_supervision <- baseline_supervision$response$sim_1 + rnorm(n=n, 0, 7.7) # TODO: should we vary the SD a bit?
baseline_supervision <- ifelse(baseline_supervision < 0, 0, baseline_supervision)
baseline_supervision <- round(baseline_supervision, 0)
#hist(baseline_supervision)

# PARENTING POST SUPERVISION #
newdata = with(rct_data, data.frame(APQ_Mon_C1 = baseline_supervision, DStot_CT1a = parenting_stress, Persons_CT1a = hh_size, Gender_CT1a = df_ppl$num_gender, Age_CT1a = age))
post_supervision <- Ecfun::simulate.glm(predict_post_apq, newdata = newdata)
post_supervision <- post_supervision$response$sim_1 + rnorm(n=n, 0, 7.6) # TODO: should we vary the SD a bit?
post_supervision <- ifelse(post_supervision < 0, 0, post_supervision)
post_supervision <- round(post_supervision, 0)
#hist(post_supervision)

 


















# -----------------------------------------
#rct_data1 <- rct_data %>% dplyr::select(c(ICASTf_m_C1, ICASTf_m_C2)) %>%
#  filter(complete.cases(ICASTf_m_C2))
#cor(as.numeric(rct_data1$ICASTf_m_C1), as.numeric(rct_data1$ICASTf_m_C2))
#ggplot(rct_data, aes(x = ICASTf_t_C1, y = ICASTf_t_C2)) + geom_point()
predict_post_icast <- glm(ICASTf_t_C2 ~ ICASTf_t_C1,
                              family = "poisson",
                              data = rct_data)
newdata = with(rct_data, data.frame(ICASTf_t_C1))
sd(rct_data$ICASTf_t_C2, na.rm = TRUE)
baseline_PE_abuse <- Ecfun::simulate.glm(predict_post_icast, newdata = newdata)
n <- length(baseline_PE_abuse$response$sim_1)
baseline_PE_abuse <- baseline_PE_abuse$response$sim_1 + rnorm(n=n, 0, 8.3)
baseline_PE_abuse <- ifelse(baseline_PE_abuse < 0, 0, baseline_PE_abuse)
hist(rct_data$ICASTf_t_C2)
hist(baseline_PE_abuse)

# TODO: create other variable/s

# parenting depression/stress - DStot_CT1a -------------------
hist(rct_data$DStot_CT1a)
predict_baseline <- glm(DStot_CT1a ~ Persons_CT1a + ICASTf_t_C1,
                       data = rct_data,
                       family = "gaussian")
summary(predict_baseline)
summary(rct_data$DStot_CT1a)
hist(rct_data$DStot_CT1a)
sd(rct_data$DStot_CT1a, na.rm = T)
mean(rct_data$DStot_CT1a, na.rm = T)

newdata = with(rct_data, data.frame(Persons_CT1a, ICASTf_t_C1))
parenting_stress <- Ecfun::simulate.glm(predict_baseline, newdata = newdata)
n <- length(parenting_stress$response$sim_1)
parenting_stress <- parenting_stress$response$sim_1 + rnorm(n=n, 0, 12)
parenting_stress <- ifelse(parenting_stress < 0, 0, parenting_stress)
parenting_stress <- round(parenting_stress, 0)
hist(parenting_stress)

#hist(Ecfun::simulate.glm(predict_baseline, newdata = newdata)$response$sim_1 + rnorm(nrow(rct_data), 0, 10))


# # poor parental supervision - baseline ----------------------
hist(rct_data$APQ_Mon_C1)
predict_baseline_apq <- glm(APQ_Mon_C1 ~ ICASTf_t_C1 + DStot_CT1a + Persons_CT1a + Gender_CT1a + Age_CT1a,
                        family = "poisson",
                        data = rct_data)
predict_baseline_apq <- glm(APQ_Mon_C1 ~ ICASTf_t_C1 + DStot_CT1a,
                        family = "poisson",
                        data = rct_data)
summary(predict_baseline_apq)
hist(rct_data$APQ_Mon_C1)
sd(rct_data$APQ_Mon_C1)

# there is an issue in the process or model, not just in our data. because the
# prediction issue is happening elsewhere
newdata = with(rct_data, data.frame(ICASTf_t_C1, DStot_CT1a))
baseline_APQ <- Ecfun::simulate.glm(predict_baseline_apq, newdata = newdata)
n <- length(baseline_APQ$response$sim_1)
baseline_APQ <- baseline_APQ$response$sim_1 + rnorm(n=n, 0, 7.7) # TODO: should we vary the SD a bit?
baseline_APQ <- ifelse(baseline_APQ < 0, 0, baseline_APQ)
baseline_APQ <- round(baseline_APQ, 0)
hist(baseline_APQ)



# # poor parental supervision - post --------------------
hist(rct_data$APQ_Mon_C2)
sd(rct_data$APQ_Mon_C2, na.rm = T)
predict_post_apq <- glm(APQ_Mon_C2 ~ APQ_Mon_C1 + ICASTf_t_C1 + DStot_CT1a + Persons_CT1a + Gender_CT1a + Age_CT1a,
                            family = "poisson",
                            data = rct_data)
predict_post_apq <- glm(APQ_Mon_C2 ~ APQ_Mon_C1 + DStot_CT1a + Persons_CT1a + Gender_CT1a + Age_CT1a,
                            family = "poisson",
                            data = rct_data)
summary(predict_post_apq)

# create model
newdata = with(rct_data, data.frame(APQ_Mon_C1, DStot_CT1a, Persons_CT1a, Gender_CT1a, Age_CT1a))
post_APQ <- Ecfun::simulate.glm(predict_post_apq, newdata = newdata)
n <- length(post_APQ$response$sim_1)
post_APQ <- post_APQ$response$sim_1 + rnorm(n=n, 0, 7.6) # TODO: should we vary the SD a bit?
post_APQ <- ifelse(post_APQ < 0, 0, post_APQ)
post_APQ <- round(post_APQ, 0)
hist(post_APQ)

