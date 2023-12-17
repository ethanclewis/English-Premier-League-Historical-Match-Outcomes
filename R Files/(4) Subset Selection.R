################################################################################

# Null and Full Models

LRFull <- glm(FTR ~ .-Date-Month, data = total_train, family = "binomial")
BIC(LRFull)

LRNull <- glm(FTR~1, data = total_train, family = "binomial")
summary(LRNull)

################################################################################

# EDA Results Models

# CV Model
LREDA <- glm(FTR~HomeTeam+AwayTeam+HTHG+HTAG+HTR+HS+AS+HST+AST+HY+AR, data = total_train, family = "binomial")
BIC(LREDA)

# Logit Model
LRReduced <- glm(FTR~Day+HomeTeam+AwayTeam+HTHG+HTAG+HTR+Referee+I(HS)+I(HS^2)+
                   AS+I(HST)+I(HST^2)+I(AST)+I(AST^2)+I(HC)+I(HC^2)+I(AC)+I(AC^2)+HY,
                 data = total_train, family = "binomial")
summary(LRReduced)

################################################################################
################################################################################
################################################################################

# Subset Selection

################################################################################

# Backwards (AIC)

# Full Model
LRStepBackwardAICFull <- step(LRFull, k=2)
summary(LRStepBackwardAICFull) # AIC = 5483

#CV Model
LRCVStepBackwardAIC <- step(LREDA, k=2)
formula(LRCVStepBackwardAIC) # AIC = 5602

# Logit Model
LRStepBackwardAIC <- step(LRReduced, k=2)
formula(LRStepBackwardAIC) # AIC = 5493.4

################################################################################

# Forwards (AIC)

# Full Model
LRStepForwardAICFull <- step(LRNull, scope = list(lower=LRNull, upper=LRFull), 
                             direction = "forward", k=2)
BIC(LRStepForwardAICFull) # AIC = 5483

# CV Model
LRCVStepForwardAIC <- step(LRNull, scope = list(lower=LRNull, upper=LREDA), 
                         direction = "forward", k=2)
formula(LRCVStepForwardAIC) # AIC = 5602.4

# Logit Model
LRStepForwardAIC <- step(LRNull, scope = list(lower=LRNull, upper=LRReduced), 
                         direction = "forward", k=2)
summary(LRStepForwardAIC) # AIC = 5494.4

################################################################################

# Both (AIC)

# Full Model
LRStepBothAICFull <- step(LRNull, scope = list(lower=LRNull, upper=LRFull), 
                          direction = "both", k=2)
summary(LRStepBothAICFull) # AIC = 5483

# CV Model
LRCVStepBothAIC <- step(LRNull, scope = list(lower=LRNull, upper=LREDA), 
                      direction = "both", k=2)
BIC(LRCVStepBothAIC) # AIC = 5602

# Logit Model
LRStepBothAIC <- step(LRNull, scope = list(lower=LRNull, upper=LRReduced), 
                      direction = "both", k=2)
summary(LRStepBothAIC) # AIC = 5494.4

################################################################################
################################################################################

# Backwards (BIC)

# Full Model
LRStepBackwardBICFull <- step(LRFull, k=log(6003))
formula(LRStepBackwardBICFull) # AIC = 5524.4 // BIC = 5584.723

#CV Model
LRCVStepBackwardBIC <- step(LREDA, k=log(6003))
BIC(LRCVStepBackwardBIC) # AIC = 5613.9 // BIC = 5647.374

# Logit Model
LRStepBackwardBIC <- step(LRReduced, k=log(6003))
formula(LRStepBackwardBIC) # AIC = 5531.3 // BIC = 5591.597

################################################################################

# Forwards (BIC)

# Full Model
LRStepForwardBICFull <- step(LRNull, scope = list(lower=LRNull, upper=LRFull), 
                             direction = "forward", k=log(6003))
formula(LRStepForwardBICFull) # AIC = 5524.7 // BIC = 5598.399

# CV Model
LRCVStepForwardBIC <- step(LRNull, scope = list(lower=LRNull, upper=LREDA), 
                         direction = "forward", k=log(6003))
BIC(LRCVStepForwardBIC) # AIC = 5614.7 // BIC = 5661.605

# Logit Model
LRStepForwardBIC <- step(LRNull, scope = list(lower=LRNull, upper=LRReduced), 
                         direction = "forward", k=log(6003))
formula(LRStepForwardBIC) # AIC = 5537.8 // BIC = 5604.758

################################################################################

# Both (BIC)

# Full Model
LRStepBothBICFull <- step(LRNull, scope = list(lower=LRNull, upper=LRFull), 
                          direction = "both", k=log(6003))
summary(LRStepBothBICFull) # AIC = 5524.4 // BIC = 5584.723

# CV Model
LRCVStepBothBIC <- step(LRNull, scope = list(lower=LRNull, upper=LREDA), 
                      direction = "both", k=log(6003))
summary(LRCVStepBothBIC) # AIC = 5613.9 // BIC = 5647.374

# Logit Model
LRStepBothBIC <- step(LRNull, scope = list(lower=LRNull, upper=LRReduced), 
                      direction = "both", k=log(6003))
formula(LRStepBothBIC) # AIC = 5537 // BIC = 5590.576

################################################################################

# SAVE: 'Subset Selection'

################################################################################