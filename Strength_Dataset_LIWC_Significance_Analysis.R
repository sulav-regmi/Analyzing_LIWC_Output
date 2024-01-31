# loading data (choose the LIWC dataset with all used dictionaries)
#usa_expanded_liwc <- read.csv(file.path("~/sulavregmi/Desktop/EA/YES_Files/LIWC_Strength/USA_Strength_LIWC_selected_dictionaries.csv"))
usa_expanded_liwc <- read.csv(file.choose())

View(usa_expanded_liwc)
A <- 2

# collapsed if (A >1) statements used for visual clarity (expand to view full code!)

# anovas
if (A > 1) {
# strength anovas
if (A > 1) {
  # WC
  anova_strength_WC <- aov(strength ~ WC, data = usa_expanded_liwc)
  pvalue_strength_WC <- summary(anova_strength_WC)[[1]][["Pr(>F)"]][1]
  
  # Analytic
  anova_strength_Analytic <- aov(strength ~ Analytic, data = usa_expanded_liwc)
  pvalue_strength_Analytic <- summary(anova_strength_Analytic)[[1]][["Pr(>F)"]][1]
  
  # Clout
  anova_strength_Clout <- aov(strength ~ Clout, data = usa_expanded_liwc)
  pvalue_strength_Clout <- summary(anova_strength_Clout)[[1]][["Pr(>F)"]][1]
  
  # Authentic
  anova_strength_Authentic <- aov(strength ~ Authentic, data = usa_expanded_liwc)
  pvalue_strength_Authentic <- summary(anova_strength_Authentic)[[1]][["Pr(>F)"]][1]
  
  # Tone
  anova_strength_Tone <- aov(strength ~ Tone, data = usa_expanded_liwc)
  pvalue_strength_Tone <- summary(anova_strength_Tone)[[1]][["Pr(>F)"]][1]
  
  # BigWords
  anova_strength_BigWords <- aov(strength ~ BigWords, data = usa_expanded_liwc)
  pvalue_strength_BigWords <- summary(anova_strength_BigWords)[[1]][["Pr(>F)"]][1]
  
  # Dic
  anova_strength_Dic <- aov(strength ~ Dic, data = usa_expanded_liwc)
  pvalue_strength_Dic <- summary(anova_strength_Dic)[[1]][["Pr(>F)"]][1]
  
  # Linguistic
  anova_strength_Linguistic <- aov(strength ~ Linguistic, data = usa_expanded_liwc)
  pvalue_strength_Linguistic <- summary(anova_strength_Linguistic)[[1]][["Pr(>F)"]][1]
  
  # Function
  anova_strength_function <- aov(strength ~ function., data = usa_expanded_liwc)
  pvalue_strength_function <- summary(anova_strength_function)[[1]][["Pr(>F)"]][1]
  
  # Pronoun
  anova_strength_pronoun <- aov(strength ~ pronoun, data = usa_expanded_liwc)
  pvalue_strength_pronoun <- summary(anova_strength_pronoun)[[1]][["Pr(>F)"]][1]
  
  # Ppron
  anova_strength_ppron <- aov(strength ~ ppron, data = usa_expanded_liwc)
  pvalue_strength_ppron <- summary(anova_strength_ppron)[[1]][["Pr(>F)"]][1]
  
  # i
  anova_strength_i <- aov(strength ~ i, data = usa_expanded_liwc)
  pvalue_strength_i <- summary(anova_strength_i)[[1]][["Pr(>F)"]][1]
  
  # we
  anova_strength_we <- aov(strength ~ we, data = usa_expanded_liwc)
  pvalue_strength_we <- summary(anova_strength_we)[[1]][["Pr(>F)"]][1]
  
  # you
  anova_strength_you <- aov(strength ~ you, data = usa_expanded_liwc)
  pvalue_strength_you <- summary(anova_strength_you)[[1]][["Pr(>F)"]][1]
  
  # shehe
  anova_strength_shehe <- aov(strength ~ shehe, data = usa_expanded_liwc)
  pvalue_strength_shehe <- summary(anova_strength_shehe)[[1]][["Pr(>F)"]][1]
  
  # they
  anova_strength_they <- aov(strength ~ they, data = usa_expanded_liwc)
  pvalue_strength_they <- summary(anova_strength_they)[[1]][["Pr(>F)"]][1]
  
  # Affect
  anova_strength_Affect <- aov(strength ~ Affect, data = usa_expanded_liwc)
  pvalue_strength_Affect <- summary(anova_strength_Affect)[[1]][["Pr(>F)"]][1]
  
  # tone_pos
  anova_strength_tone_pos <- aov(strength ~ tone_pos, data = usa_expanded_liwc)
  pvalue_strength_tone_pos <- summary(anova_strength_tone_pos)[[1]][["Pr(>F)"]][1]
  
  # tone_neg
  anova_strength_tone_neg <- aov(strength ~ tone_neg, data = usa_expanded_liwc)
  pvalue_strength_tone_neg <- summary(anova_strength_tone_neg)[[1]][["Pr(>F)"]][1]
  
  # emotion
  anova_strength_emotion <- aov(strength ~ emotion, data = usa_expanded_liwc)
  pvalue_strength_emotion <- summary(anova_strength_emotion)[[1]][["Pr(>F)"]][1]
  
  # emo_pos
  anova_strength_emo_pos <- aov(strength ~ emo_pos, data = usa_expanded_liwc)
  pvalue_strength_emo_pos <- summary(anova_strength_emo_pos)[[1]][["Pr(>F)"]][1]
  
  # emo_neg
  anova_strength_emo_neg <- aov(strength ~ emo_neg, data = usa_expanded_liwc)
  pvalue_strength_emo_neg <- summary(anova_strength_emo_neg)[[1]][["Pr(>F)"]][1]
  
  # emo_anx
  anova_strength_emo_anx <- aov(strength ~ emo_anx, data = usa_expanded_liwc)
  pvalue_strength_emo_anx <- summary(anova_strength_emo_anx)[[1]][["Pr(>F)"]][1]
  
  # emo_anger
  anova_strength_emo_anger <- aov(strength ~ emo_anger, data = usa_expanded_liwc)
  pvalue_strength_emo_anger <- summary(anova_strength_emo_anger)[[1]][["Pr(>F)"]][1]
  
  # emo_sad
  anova_strength_emo_sad <- aov(strength ~ emo_sad, data = usa_expanded_liwc)
  pvalue_strength_emo_sad <- summary(anova_strength_emo_sad)[[1]][["Pr(>F)"]][1]
  
  # swear
  anova_strength_swear <- aov(strength ~ swear, data = usa_expanded_liwc)
  pvalue_strength_swear <- summary(anova_strength_swear)[[1]][["Pr(>F)"]][1]
  
  # Social
  anova_strength_Social <- aov(strength ~ Social, data = usa_expanded_liwc)
  pvalue_strength_Social <- summary(anova_strength_Social)[[1]][["Pr(>F)"]][1]
  
  # socbehav
  anova_strength_socbehav <- aov(strength ~ socbehav, data = usa_expanded_liwc)
  pvalue_strength_socbehav <- summary(anova_strength_socbehav)[[1]][["Pr(>F)"]][1]
  
  # prosocial
  anova_strength_prosocial <- aov(strength ~ prosocial, data = usa_expanded_liwc)
  pvalue_strength_prosocial <- summary(anova_strength_prosocial)[[1]][["Pr(>F)"]][1]
  
  # polite
  anova_strength_polite <- aov(strength ~ polite, data = usa_expanded_liwc)
  pvalue_strength_polite <- summary(anova_strength_polite)[[1]][["Pr(>F)"]][1]
  
  # conflict
  anova_strength_conflict <- aov(strength ~ conflict, data = usa_expanded_liwc)
  pvalue_strength_conflict <- summary(anova_strength_conflict)[[1]][["Pr(>F)"]][1]
  
  # moral
  anova_strength_moral <- aov(strength ~ moral, data = usa_expanded_liwc)
  pvalue_strength_moral <- summary(anova_strength_moral)[[1]][["Pr(>F)"]][1]
  
  # comm
  anova_strength_comm <- aov(strength ~ comm, data = usa_expanded_liwc)
  pvalue_strength_comm <- summary(anova_strength_comm)[[1]][["Pr(>F)"]][1]
  
  # socrefs
  anova_strength_socrefs <- aov(strength ~ socrefs, data = usa_expanded_liwc)
  pvalue_strength_socrefs <- summary(anova_strength_socrefs)[[1]][["Pr(>F)"]][1]
  
  # family
  anova_strength_family <- aov(strength ~ family, data = usa_expanded_liwc)
  pvalue_strength_family <- summary(anova_strength_family)[[1]][["Pr(>F)"]][1]
  
  # friend
  anova_strength_friend <- aov(strength ~ friend, data = usa_expanded_liwc)
  pvalue_strength_friend <- summary(anova_strength_friend)[[1]][["Pr(>F)"]][1]
  
  # female
  anova_strength_female <- aov(strength ~ female, data = usa_expanded_liwc)
  pvalue_strength_female <- summary(anova_strength_female)[[1]][["Pr(>F)"]][1]
  
  # male
  anova_strength_male <- aov(strength ~ male, data = usa_expanded_liwc)
  pvalue_strength_male <- summary(anova_strength_male)[[1]][["Pr(>F)"]][1]
  
  # health
  anova_strength_health <- aov(strength ~ health, data = usa_expanded_liwc)
  pvalue_strength_health <- summary(anova_strength_health)[[1]][["Pr(>F)"]][1]
  
  # illness
  anova_strength_illness <- aov(strength ~ illness, data = usa_expanded_liwc)
  pvalue_strength_illness <- summary(anova_strength_illness)[[1]][["Pr(>F)"]][1]
  
  # wellness
  anova_strength_wellness <- aov(strength ~ wellness, data = usa_expanded_liwc)
  pvalue_strength_wellness <- summary(anova_strength_wellness)[[1]][["Pr(>F)"]][1]
  
  # mental
  anova_strength_mental <- aov(strength ~ mental, data = usa_expanded_liwc)
  pvalue_strength_mental <- summary(anova_strength_mental)[[1]][["Pr(>F)"]][1]
  
  # need
  anova_strength_need <- aov(strength ~ need, data = usa_expanded_liwc)
  pvalue_strength_need <- summary(anova_strength_need)[[1]][["Pr(>F)"]][1]
  
  # want
  anova_strength_want <- aov(strength ~ want, data = usa_expanded_liwc)
  pvalue_strength_want <- summary(anova_strength_want)[[1]][["Pr(>F)"]][1]
  
  # Acquire
  anova_strength_acquire <- aov(strength ~ acquire, data = usa_expanded_liwc)
  pvalue_strength_acquire <- summary(anova_strength_acquire)[[1]][["Pr(>F)"]][1]
  
  # Lack
  anova_strength_lack <- aov(strength ~ lack, data = usa_expanded_liwc)
  pvalue_strength_lack <- summary(anova_strength_lack)[[1]][["Pr(>F)"]][1]
  
  # Fulfill
  anova_strength_fulfill <- aov(strength ~ fulfill, data = usa_expanded_liwc)
  pvalue_strength_fulfill <- summary(anova_strength_fulfill)[[1]][["Pr(>F)"]][1]
  
  # Fatigue
  anova_strength_fatigue <- aov(strength ~ fatigue, data = usa_expanded_liwc)
  pvalue_strength_fatigue <- summary(anova_strength_fatigue)[[1]][["Pr(>F)"]][1]
}

# justified anovas
if (A > 1) {
  # WC
  anova_justified_WC <- aov(justified ~ WC, data = usa_expanded_liwc)
  pvalue_justified_WC <- summary(anova_justified_WC)[[1]][["Pr(>F)"]][1]
  
  # Analytic
  anova_justified_Analytic <- aov(justified ~ Analytic, data = usa_expanded_liwc)
  pvalue_justified_Analytic <- summary(anova_justified_Analytic)[[1]][["Pr(>F)"]][1]
  
  # Clout
  anova_justified_Clout <- aov(justified ~ Clout, data = usa_expanded_liwc)
  pvalue_justified_Clout <- summary(anova_justified_Clout)[[1]][["Pr(>F)"]][1]
  
  # Authentic
  anova_justified_Authentic <- aov(justified ~ Authentic, data = usa_expanded_liwc)
  pvalue_justified_Authentic <- summary(anova_justified_Authentic)[[1]][["Pr(>F)"]][1]
  
  # Tone
  anova_justified_Tone <- aov(justified ~ Tone, data = usa_expanded_liwc)
  pvalue_justified_Tone <- summary(anova_justified_Tone)[[1]][["Pr(>F)"]][1]
  
  # BigWords
  anova_justified_BigWords <- aov(justified ~ BigWords, data = usa_expanded_liwc)
  pvalue_justified_BigWords <- summary(anova_justified_BigWords)[[1]][["Pr(>F)"]][1]
  
  # Dic
  anova_justified_Dic <- aov(justified ~ Dic, data = usa_expanded_liwc)
  pvalue_justified_Dic <- summary(anova_justified_Dic)[[1]][["Pr(>F)"]][1]
  
  # Linguistic
  anova_justified_Linguistic <- aov(justified ~ Linguistic, data = usa_expanded_liwc)
  pvalue_justified_Linguistic <- summary(anova_justified_Linguistic)[[1]][["Pr(>F)"]][1]
  
  # Function
  anova_justified_function <- aov(justified ~ function., data = usa_expanded_liwc)
  pvalue_justified_function <- summary(anova_justified_function)[[1]][["Pr(>F)"]][1]
  
  # Pronoun
  anova_justified_pronoun <- aov(justified ~ pronoun, data = usa_expanded_liwc)
  pvalue_justified_pronoun <- summary(anova_justified_pronoun)[[1]][["Pr(>F)"]][1]
  
  # Ppron
  anova_justified_ppron <- aov(justified ~ ppron, data = usa_expanded_liwc)
  pvalue_justified_ppron <- summary(anova_justified_ppron)[[1]][["Pr(>F)"]][1]
  
  # i
  anova_justified_i <- aov(justified ~ i, data = usa_expanded_liwc)
  pvalue_justified_i <- summary(anova_justified_i)[[1]][["Pr(>F)"]][1]
  
  # we
  anova_justified_we <- aov(justified ~ we, data = usa_expanded_liwc)
  pvalue_justified_we <- summary(anova_justified_we)[[1]][["Pr(>F)"]][1]
  
  # you
  anova_justified_you <- aov(justified ~ you, data = usa_expanded_liwc)
  pvalue_justified_you <- summary(anova_justified_you)[[1]][["Pr(>F)"]][1]
  
  # shehe
  anova_justified_shehe <- aov(justified ~ shehe, data = usa_expanded_liwc)
  pvalue_justified_shehe <- summary(anova_justified_shehe)[[1]][["Pr(>F)"]][1]
  
  # they
  anova_justified_they <- aov(justified ~ they, data = usa_expanded_liwc)
  pvalue_justified_they <- summary(anova_justified_they)[[1]][["Pr(>F)"]][1]
  
  # Affect
  anova_justified_Affect <- aov(justified ~ Affect, data = usa_expanded_liwc)
  pvalue_justified_Affect <- summary(anova_justified_Affect)[[1]][["Pr(>F)"]][1]
  
  # tone_pos
  anova_justified_tone_pos <- aov(justified ~ tone_pos, data = usa_expanded_liwc)
  pvalue_justified_tone_pos <- summary(anova_justified_tone_pos)[[1]][["Pr(>F)"]][1]
  
  # tone_neg
  anova_justified_tone_neg <- aov(justified ~ tone_neg, data = usa_expanded_liwc)
  pvalue_justified_tone_neg <- summary(anova_justified_tone_neg)[[1]][["Pr(>F)"]][1]
  
  # emotion
  anova_justified_emotion <- aov(justified ~ emotion, data = usa_expanded_liwc)
  pvalue_justified_emotion <- summary(anova_justified_emotion)[[1]][["Pr(>F)"]][1]
  
  # emo_pos
  anova_justified_emo_pos <- aov(justified ~ emo_pos, data = usa_expanded_liwc)
  pvalue_justified_emo_pos <- summary(anova_justified_emo_pos)[[1]][["Pr(>F)"]][1]
  
  # emo_neg
  anova_justified_emo_neg <- aov(justified ~ emo_neg, data = usa_expanded_liwc)
  pvalue_justified_emo_neg <- summary(anova_justified_emo_neg)[[1]][["Pr(>F)"]][1]
  
  # emo_anx
  anova_justified_emo_anx <- aov(justified ~ emo_anx, data = usa_expanded_liwc)
  pvalue_justified_emo_anx <- summary(anova_justified_emo_anx)[[1]][["Pr(>F)"]][1]
  
  # emo_anger
  anova_justified_emo_anger <- aov(justified ~ emo_anger, data = usa_expanded_liwc)
  pvalue_justified_emo_anger <- summary(anova_justified_emo_anger)[[1]][["Pr(>F)"]][1]
  
  # emo_sad
  anova_justified_emo_sad <- aov(justified ~ emo_sad, data = usa_expanded_liwc)
  pvalue_justified_emo_sad <- summary(anova_justified_emo_sad)[[1]][["Pr(>F)"]][1]
  
  # swear
  anova_justified_swear <- aov(justified ~ swear, data = usa_expanded_liwc)
  pvalue_justified_swear <- summary(anova_justified_swear)[[1]][["Pr(>F)"]][1]
  
  # Social
  anova_justified_Social <- aov(justified ~ Social, data = usa_expanded_liwc)
  pvalue_justified_Social <- summary(anova_justified_Social)[[1]][["Pr(>F)"]][1]
  
  # socbehav
  anova_justified_socbehav <- aov(justified ~ socbehav, data = usa_expanded_liwc)
  pvalue_justified_socbehav <- summary(anova_justified_socbehav)[[1]][["Pr(>F)"]][1]
  
  # prosocial
  anova_justified_prosocial <- aov(justified ~ prosocial, data = usa_expanded_liwc)
  pvalue_justified_prosocial <- summary(anova_justified_prosocial)[[1]][["Pr(>F)"]][1]
  
  # polite
  anova_justified_polite <- aov(justified ~ polite, data = usa_expanded_liwc)
  pvalue_justified_polite <- summary(anova_justified_polite)[[1]][["Pr(>F)"]][1]
  
  # conflict
  anova_justified_conflict <- aov(justified ~ conflict, data = usa_expanded_liwc)
  pvalue_justified_conflict <- summary(anova_justified_conflict)[[1]][["Pr(>F)"]][1]
  
  # moral
  anova_justified_moral <- aov(justified ~ moral, data = usa_expanded_liwc)
  pvalue_justified_moral <- summary(anova_justified_moral)[[1]][["Pr(>F)"]][1]
  
  # comm
  anova_justified_comm <- aov(justified ~ comm, data = usa_expanded_liwc)
  pvalue_justified_comm <- summary(anova_justified_comm)[[1]][["Pr(>F)"]][1]
  
  # socrefs
  anova_justified_socrefs <- aov(justified ~ socrefs, data = usa_expanded_liwc)
  pvalue_justified_socrefs <- summary(anova_justified_socrefs)[[1]][["Pr(>F)"]][1]
  
  # family
  anova_justified_family <- aov(justified ~ family, data = usa_expanded_liwc)
  pvalue_justified_family <- summary(anova_justified_family)[[1]][["Pr(>F)"]][1]
  
  # friend
  anova_justified_friend <- aov(justified ~ friend, data = usa_expanded_liwc)
  pvalue_justified_friend <- summary(anova_justified_friend)[[1]][["Pr(>F)"]][1]
  
  # female
  anova_justified_female <- aov(justified ~ female, data = usa_expanded_liwc)
  pvalue_justified_female <- summary(anova_justified_female)[[1]][["Pr(>F)"]][1]
  
  # male
  anova_justified_male <- aov(justified ~ male, data = usa_expanded_liwc)
  pvalue_justified_male <- summary(anova_justified_male)[[1]][["Pr(>F)"]][1]
  
  # health
  anova_justified_health <- aov(justified ~ health, data = usa_expanded_liwc)
  pvalue_justified_health <- summary(anova_justified_health)[[1]][["Pr(>F)"]][1]
  
  # illness
  anova_justified_illness <- aov(justified ~ illness, data = usa_expanded_liwc)
  pvalue_justified_illness <- summary(anova_justified_illness)[[1]][["Pr(>F)"]][1]
  
  # wellness
  anova_justified_wellness <- aov(justified ~ wellness, data = usa_expanded_liwc)
  pvalue_justified_wellness <- summary(anova_justified_wellness)[[1]][["Pr(>F)"]][1]
  
  # mental
  anova_justified_mental <- aov(justified ~ mental, data = usa_expanded_liwc)
  pvalue_justified_mental <- summary(anova_justified_mental)[[1]][["Pr(>F)"]][1]
  
  # need
  anova_justified_need <- aov(justified ~ need, data = usa_expanded_liwc)
  pvalue_justified_need <- summary(anova_justified_need)[[1]][["Pr(>F)"]][1]
  
  # want
  anova_justified_want <- aov(justified ~ want, data = usa_expanded_liwc)
  pvalue_justified_want <- summary(anova_justified_want)[[1]][["Pr(>F)"]][1]
  
  # Acquire
  anova_justified_acquire <- aov(justified ~ acquire, data = usa_expanded_liwc)
  pvalue_justified_acquire <- summary(anova_justified_acquire)[[1]][["Pr(>F)"]][1]
  
  # Lack
  anova_justified_lack <- aov(justified ~ lack, data = usa_expanded_liwc)
  pvalue_justified_lack <- summary(anova_justified_lack)[[1]][["Pr(>F)"]][1]
  
  # Fulfill
  anova_justified_fulfill <- aov(justified ~ fulfill, data = usa_expanded_liwc)
  pvalue_justified_fulfill <- summary(anova_justified_fulfill)[[1]][["Pr(>F)"]][1]
  
  # Fatigue
  anova_justified_fatigue <- aov(justified ~ fatigue, data = usa_expanded_liwc)
  pvalue_justified_fatigue <- summary(anova_justified_fatigue)[[1]][["Pr(>F)"]][1]
}

# guilt anovas
if (A > 1) {
  # WC
  anova_guilt_WC <- aov(guilt ~ WC, data = usa_expanded_liwc)
  pvalue_guilt_WC <- summary(anova_guilt_WC)[[1]][["Pr(>F)"]][1]
  
  # Analytic
  anova_guilt_Analytic <- aov(guilt ~ Analytic, data = usa_expanded_liwc)
  pvalue_guilt_Analytic <- summary(anova_guilt_Analytic)[[1]][["Pr(>F)"]][1]
  
  # Clout
  anova_guilt_Clout <- aov(guilt ~ Clout, data = usa_expanded_liwc)
  pvalue_guilt_Clout <- summary(anova_guilt_Clout)[[1]][["Pr(>F)"]][1]
  
  # Authentic
  anova_guilt_Authentic <- aov(guilt ~ Authentic, data = usa_expanded_liwc)
  pvalue_guilt_Authentic <- summary(anova_guilt_Authentic)[[1]][["Pr(>F)"]][1]
  
  # Tone
  anova_guilt_Tone <- aov(guilt ~ Tone, data = usa_expanded_liwc)
  pvalue_guilt_Tone <- summary(anova_guilt_Tone)[[1]][["Pr(>F)"]][1]
  
  # BigWords
  anova_guilt_BigWords <- aov(guilt ~ BigWords, data = usa_expanded_liwc)
  pvalue_guilt_BigWords <- summary(anova_guilt_BigWords)[[1]][["Pr(>F)"]][1]
  
  # Dic
  anova_guilt_Dic <- aov(guilt ~ Dic, data = usa_expanded_liwc)
  pvalue_guilt_Dic <- summary(anova_guilt_Dic)[[1]][["Pr(>F)"]][1]
  
  # Linguistic
  anova_guilt_Linguistic <- aov(guilt ~ Linguistic, data = usa_expanded_liwc)
  pvalue_guilt_Linguistic <- summary(anova_guilt_Linguistic)[[1]][["Pr(>F)"]][1]
  
  # Function
  anova_guilt_function <- aov(guilt ~ function., data = usa_expanded_liwc)
  pvalue_guilt_function <- summary(anova_guilt_function)[[1]][["Pr(>F)"]][1]
  
  # Pronoun
  anova_guilt_pronoun <- aov(guilt ~ pronoun, data = usa_expanded_liwc)
  pvalue_guilt_pronoun <- summary(anova_guilt_pronoun)[[1]][["Pr(>F)"]][1]
  
  # Ppron
  anova_guilt_ppron <- aov(guilt ~ ppron, data = usa_expanded_liwc)
  pvalue_guilt_ppron <- summary(anova_guilt_ppron)[[1]][["Pr(>F)"]][1]
  
  # i
  anova_guilt_i <- aov(guilt ~ i, data = usa_expanded_liwc)
  pvalue_guilt_i <- summary(anova_guilt_i)[[1]][["Pr(>F)"]][1]
  
  # we
  anova_guilt_we <- aov(guilt ~ we, data = usa_expanded_liwc)
  pvalue_guilt_we <- summary(anova_guilt_we)[[1]][["Pr(>F)"]][1]
  
  # you
  anova_guilt_you <- aov(guilt ~ you, data = usa_expanded_liwc)
  pvalue_guilt_you <- summary(anova_guilt_you)[[1]][["Pr(>F)"]][1]
  
  # shehe
  anova_guilt_shehe <- aov(guilt ~ shehe, data = usa_expanded_liwc)
  pvalue_guilt_shehe <- summary(anova_guilt_shehe)[[1]][["Pr(>F)"]][1]
  
  # they
  anova_guilt_they <- aov(guilt ~ they, data = usa_expanded_liwc)
  pvalue_guilt_they <- summary(anova_guilt_they)[[1]][["Pr(>F)"]][1]
  
  # Affect
  anova_guilt_Affect <- aov(guilt ~ Affect, data = usa_expanded_liwc)
  pvalue_guilt_Affect <- summary(anova_guilt_Affect)[[1]][["Pr(>F)"]][1]
  
  # tone_pos
  anova_guilt_tone_pos <- aov(guilt ~ tone_pos, data = usa_expanded_liwc)
  pvalue_guilt_tone_pos <- summary(anova_guilt_tone_pos)[[1]][["Pr(>F)"]][1]
  
  # tone_neg
  anova_guilt_tone_neg <- aov(guilt ~ tone_neg, data = usa_expanded_liwc)
  pvalue_guilt_tone_neg <- summary(anova_guilt_tone_neg)[[1]][["Pr(>F)"]][1]
  
  # emotion
  anova_guilt_emotion <- aov(guilt ~ emotion, data = usa_expanded_liwc)
  pvalue_guilt_emotion <- summary(anova_guilt_emotion)[[1]][["Pr(>F)"]][1]
  
  # emo_pos
  anova_guilt_emo_pos <- aov(guilt ~ emo_pos, data = usa_expanded_liwc)
  pvalue_guilt_emo_pos <- summary(anova_guilt_emo_pos)[[1]][["Pr(>F)"]][1]
  
  # emo_neg
  anova_guilt_emo_neg <- aov(guilt ~ emo_neg, data = usa_expanded_liwc)
  pvalue_guilt_emo_neg <- summary(anova_guilt_emo_neg)[[1]][["Pr(>F)"]][1]
  
  # emo_anx
  anova_guilt_emo_anx <- aov(guilt ~ emo_anx, data = usa_expanded_liwc)
  pvalue_guilt_emo_anx <- summary(anova_guilt_emo_anx)[[1]][["Pr(>F)"]][1]
  
  # emo_anger
  anova_guilt_emo_anger <- aov(guilt ~ emo_anger, data = usa_expanded_liwc)
  pvalue_guilt_emo_anger <- summary(anova_guilt_emo_anger)[[1]][["Pr(>F)"]][1]
  
  # emo_sad
  anova_guilt_emo_sad <- aov(guilt ~ emo_sad, data = usa_expanded_liwc)
  pvalue_guilt_emo_sad <- summary(anova_guilt_emo_sad)[[1]][["Pr(>F)"]][1]
  
  # swear
  anova_guilt_swear <- aov(guilt ~ swear, data = usa_expanded_liwc)
  pvalue_guilt_swear <- summary(anova_guilt_swear)[[1]][["Pr(>F)"]][1]
  
  # Social
  anova_guilt_Social <- aov(guilt ~ Social, data = usa_expanded_liwc)
  pvalue_guilt_Social <- summary(anova_guilt_Social)[[1]][["Pr(>F)"]][1]
  
  # socbehav
  anova_guilt_socbehav <- aov(guilt ~ socbehav, data = usa_expanded_liwc)
  pvalue_guilt_socbehav <- summary(anova_guilt_socbehav)[[1]][["Pr(>F)"]][1]
  
  # prosocial
  anova_guilt_prosocial <- aov(guilt ~ prosocial, data = usa_expanded_liwc)
  pvalue_guilt_prosocial <- summary(anova_guilt_prosocial)[[1]][["Pr(>F)"]][1]
  
  # polite
  anova_guilt_polite <- aov(guilt ~ polite, data = usa_expanded_liwc)
  pvalue_guilt_polite <- summary(anova_guilt_polite)[[1]][["Pr(>F)"]][1]
  
  # conflict
  anova_guilt_conflict <- aov(guilt ~ conflict, data = usa_expanded_liwc)
  pvalue_guilt_conflict <- summary(anova_guilt_conflict)[[1]][["Pr(>F)"]][1]
  
  # moral
  anova_guilt_moral <- aov(guilt ~ moral, data = usa_expanded_liwc)
  pvalue_guilt_moral <- summary(anova_guilt_moral)[[1]][["Pr(>F)"]][1]
  
  # comm
  anova_guilt_comm <- aov(guilt ~ comm, data = usa_expanded_liwc)
  pvalue_guilt_comm <- summary(anova_guilt_comm)[[1]][["Pr(>F)"]][1]
  
  # socrefs
  anova_guilt_socrefs <- aov(guilt ~ socrefs, data = usa_expanded_liwc)
  pvalue_guilt_socrefs <- summary(anova_guilt_socrefs)[[1]][["Pr(>F)"]][1]
  
  # family
  anova_guilt_family <- aov(guilt ~ family, data = usa_expanded_liwc)
  pvalue_guilt_family <- summary(anova_guilt_family)[[1]][["Pr(>F)"]][1]
  
  # friend
  anova_guilt_friend <- aov(guilt ~ friend, data = usa_expanded_liwc)
  pvalue_guilt_friend <- summary(anova_guilt_friend)[[1]][["Pr(>F)"]][1]
  
  # female
  anova_guilt_female <- aov(guilt ~ female, data = usa_expanded_liwc)
  pvalue_guilt_female <- summary(anova_guilt_female)[[1]][["Pr(>F)"]][1]
  
  # male
  anova_guilt_male <- aov(guilt ~ male, data = usa_expanded_liwc)
  pvalue_guilt_male <- summary(anova_guilt_male)[[1]][["Pr(>F)"]][1]
  
  # health
  anova_guilt_health <- aov(guilt ~ health, data = usa_expanded_liwc)
  pvalue_guilt_health <- summary(anova_guilt_health)[[1]][["Pr(>F)"]][1]
  
  # illness
  anova_guilt_illness <- aov(guilt ~ illness, data = usa_expanded_liwc)
  pvalue_guilt_illness <- summary(anova_guilt_illness)[[1]][["Pr(>F)"]][1]
  
  # wellness
  anova_guilt_wellness <- aov(guilt ~ wellness, data = usa_expanded_liwc)
  pvalue_guilt_wellness <- summary(anova_guilt_wellness)[[1]][["Pr(>F)"]][1]
  
  # mental
  anova_guilt_mental <- aov(guilt ~ mental, data = usa_expanded_liwc)
  pvalue_guilt_mental <- summary(anova_guilt_mental)[[1]][["Pr(>F)"]][1]
  
  # need
  anova_guilt_need <- aov(guilt ~ need, data = usa_expanded_liwc)
  pvalue_guilt_need <- summary(anova_guilt_need)[[1]][["Pr(>F)"]][1]
  
  # want
  anova_guilt_want <- aov(guilt ~ want, data = usa_expanded_liwc)
  pvalue_guilt_want <- summary(anova_guilt_want)[[1]][["Pr(>F)"]][1]
  
  # Acquire
  anova_guilt_acquire <- aov(guilt ~ acquire, data = usa_expanded_liwc)
  pvalue_guilt_acquire <- summary(anova_guilt_acquire)[[1]][["Pr(>F)"]][1]
  
  # Lack
  anova_guilt_lack <- aov(guilt ~ lack, data = usa_expanded_liwc)
  pvalue_guilt_lack <- summary(anova_guilt_lack)[[1]][["Pr(>F)"]][1]
  
  # Fulfill
  anova_guilt_fulfill <- aov(guilt ~ fulfill, data = usa_expanded_liwc)
  pvalue_guilt_fulfill <- summary(anova_guilt_fulfill)[[1]][["Pr(>F)"]][1]
  
  # Fatigue
  anova_guilt_fatigue <- aov(guilt ~ fatigue, data = usa_expanded_liwc)
  pvalue_guilt_fatigue <- summary(anova_guilt_fatigue)[[1]][["Pr(>F)"]][1]
}

# depressed anovas
if (A > 1) {
  # WC
  anova_depressed_WC <- aov(depressed ~ WC, data = usa_expanded_liwc)
  pvalue_depressed_WC <- summary(anova_depressed_WC)[[1]][["Pr(>F)"]][1]
  
  # Analytic
  anova_depressed_Analytic <- aov(depressed ~ Analytic, data = usa_expanded_liwc)
  pvalue_depressed_Analytic <- summary(anova_depressed_Analytic)[[1]][["Pr(>F)"]][1]
  
  # Clout
  anova_depressed_Clout <- aov(depressed ~ Clout, data = usa_expanded_liwc)
  pvalue_depressed_Clout <- summary(anova_depressed_Clout)[[1]][["Pr(>F)"]][1]
  
  # Authentic
  anova_depressed_Authentic <- aov(depressed ~ Authentic, data = usa_expanded_liwc)
  pvalue_depressed_Authentic <- summary(anova_depressed_Authentic)[[1]][["Pr(>F)"]][1]
  
  # Tone
  anova_depressed_Tone <- aov(depressed ~ Tone, data = usa_expanded_liwc)
  pvalue_depressed_Tone <- summary(anova_depressed_Tone)[[1]][["Pr(>F)"]][1]
  
  # BigWords
  anova_depressed_BigWords <- aov(depressed ~ BigWords, data = usa_expanded_liwc)
  pvalue_depressed_BigWords <- summary(anova_depressed_BigWords)[[1]][["Pr(>F)"]][1]
  
  # Dic
  anova_depressed_Dic <- aov(depressed ~ Dic, data = usa_expanded_liwc)
  pvalue_depressed_Dic <- summary(anova_depressed_Dic)[[1]][["Pr(>F)"]][1]
  
  # Linguistic
  anova_depressed_Linguistic <- aov(depressed ~ Linguistic, data = usa_expanded_liwc)
  pvalue_depressed_Linguistic <- summary(anova_depressed_Linguistic)[[1]][["Pr(>F)"]][1]
  
  # Function
  anova_depressed_function <- aov(depressed ~ function., data = usa_expanded_liwc)
  pvalue_depressed_function <- summary(anova_depressed_function)[[1]][["Pr(>F)"]][1]
  
  # Pronoun
  anova_depressed_pronoun <- aov(depressed ~ pronoun, data = usa_expanded_liwc)
  pvalue_depressed_pronoun <- summary(anova_depressed_pronoun)[[1]][["Pr(>F)"]][1]
  
  # Ppron
  anova_depressed_ppron <- aov(depressed ~ ppron, data = usa_expanded_liwc)
  pvalue_depressed_ppron <- summary(anova_depressed_ppron)[[1]][["Pr(>F)"]][1]
  
  # i
  anova_depressed_i <- aov(depressed ~ i, data = usa_expanded_liwc)
  pvalue_depressed_i <- summary(anova_depressed_i)[[1]][["Pr(>F)"]][1]
  
  # we
  anova_depressed_we <- aov(depressed ~ we, data = usa_expanded_liwc)
  pvalue_depressed_we <- summary(anova_depressed_we)[[1]][["Pr(>F)"]][1]
  
  # you
  anova_depressed_you <- aov(depressed ~ you, data = usa_expanded_liwc)
  pvalue_depressed_you <- summary(anova_depressed_you)[[1]][["Pr(>F)"]][1]
  
  # shehe
  anova_depressed_shehe <- aov(depressed ~ shehe, data = usa_expanded_liwc)
  pvalue_depressed_shehe <- summary(anova_depressed_shehe)[[1]][["Pr(>F)"]][1]
  
  # they
  anova_depressed_they <- aov(depressed ~ they, data = usa_expanded_liwc)
  pvalue_depressed_they <- summary(anova_depressed_they)[[1]][["Pr(>F)"]][1]
  
  # Affect
  anova_depressed_Affect <- aov(depressed ~ Affect, data = usa_expanded_liwc)
  pvalue_depressed_Affect <- summary(anova_depressed_Affect)[[1]][["Pr(>F)"]][1]
  
  # tone_pos
  anova_depressed_tone_pos <- aov(depressed ~ tone_pos, data = usa_expanded_liwc)
  pvalue_depressed_tone_pos <- summary(anova_depressed_tone_pos)[[1]][["Pr(>F)"]][1]
  
  # tone_neg
  anova_depressed_tone_neg <- aov(depressed ~ tone_neg, data = usa_expanded_liwc)
  pvalue_depressed_tone_neg <- summary(anova_depressed_tone_neg)[[1]][["Pr(>F)"]][1]
  
  # emotion
  anova_depressed_emotion <- aov(depressed ~ emotion, data = usa_expanded_liwc)
  pvalue_depressed_emotion <- summary(anova_depressed_emotion)[[1]][["Pr(>F)"]][1]
  
  # emo_pos
  anova_depressed_emo_pos <- aov(depressed ~ emo_pos, data = usa_expanded_liwc)
  pvalue_depressed_emo_pos <- summary(anova_depressed_emo_pos)[[1]][["Pr(>F)"]][1]
  
  # emo_neg
  anova_depressed_emo_neg <- aov(depressed ~ emo_neg, data = usa_expanded_liwc)
  pvalue_depressed_emo_neg <- summary(anova_depressed_emo_neg)[[1]][["Pr(>F)"]][1]
  
  # emo_anx
  anova_depressed_emo_anx <- aov(depressed ~ emo_anx, data = usa_expanded_liwc)
  pvalue_depressed_emo_anx <- summary(anova_depressed_emo_anx)[[1]][["Pr(>F)"]][1]
  
  # emo_anger
  anova_depressed_emo_anger <- aov(depressed ~ emo_anger, data = usa_expanded_liwc)
  pvalue_depressed_emo_anger <- summary(anova_depressed_emo_anger)[[1]][["Pr(>F)"]][1]
  
  # emo_sad
  anova_depressed_emo_sad <- aov(depressed ~ emo_sad, data = usa_expanded_liwc)
  pvalue_depressed_emo_sad <- summary(anova_depressed_emo_sad)[[1]][["Pr(>F)"]][1]
  
  # swear
  anova_depressed_swear <- aov(depressed ~ swear, data = usa_expanded_liwc)
  pvalue_depressed_swear <- summary(anova_depressed_swear)[[1]][["Pr(>F)"]][1]
  
  # Social
  anova_depressed_Social <- aov(depressed ~ Social, data = usa_expanded_liwc)
  pvalue_depressed_Social <- summary(anova_depressed_Social)[[1]][["Pr(>F)"]][1]
  
  # socbehav
  anova_depressed_socbehav <- aov(depressed ~ socbehav, data = usa_expanded_liwc)
  pvalue_depressed_socbehav <- summary(anova_depressed_socbehav)[[1]][["Pr(>F)"]][1]
  
  # prosocial
  anova_depressed_prosocial <- aov(depressed ~ prosocial, data = usa_expanded_liwc)
  pvalue_depressed_prosocial <- summary(anova_depressed_prosocial)[[1]][["Pr(>F)"]][1]
  
  # polite
  anova_depressed_polite <- aov(depressed ~ polite, data = usa_expanded_liwc)
  pvalue_depressed_polite <- summary(anova_depressed_polite)[[1]][["Pr(>F)"]][1]
  
  # conflict
  anova_depressed_conflict <- aov(depressed ~ conflict, data = usa_expanded_liwc)
  pvalue_depressed_conflict <- summary(anova_depressed_conflict)[[1]][["Pr(>F)"]][1]
  
  # moral
  anova_depressed_moral <- aov(depressed ~ moral, data = usa_expanded_liwc)
  pvalue_depressed_moral <- summary(anova_depressed_moral)[[1]][["Pr(>F)"]][1]
  
  # comm
  anova_depressed_comm <- aov(depressed ~ comm, data = usa_expanded_liwc)
  pvalue_depressed_comm <- summary(anova_depressed_comm)[[1]][["Pr(>F)"]][1]
  
  # socrefs
  anova_depressed_socrefs <- aov(depressed ~ socrefs, data = usa_expanded_liwc)
  pvalue_depressed_socrefs <- summary(anova_depressed_socrefs)[[1]][["Pr(>F)"]][1]
  
  # family
  anova_depressed_family <- aov(depressed ~ family, data = usa_expanded_liwc)
  pvalue_depressed_family <- summary(anova_depressed_family)[[1]][["Pr(>F)"]][1]
  
  # friend
  anova_depressed_friend <- aov(depressed ~ friend, data = usa_expanded_liwc)
  pvalue_depressed_friend <- summary(anova_depressed_friend)[[1]][["Pr(>F)"]][1]
  
  # female
  anova_depressed_female <- aov(depressed ~ female, data = usa_expanded_liwc)
  pvalue_depressed_female <- summary(anova_depressed_female)[[1]][["Pr(>F)"]][1]
  
  # male
  anova_depressed_male <- aov(depressed ~ male, data = usa_expanded_liwc)
  pvalue_depressed_male <- summary(anova_depressed_male)[[1]][["Pr(>F)"]][1]
  
  # health
  anova_depressed_health <- aov(depressed ~ health, data = usa_expanded_liwc)
  pvalue_depressed_health <- summary(anova_depressed_health)[[1]][["Pr(>F)"]][1]
  
  # illness
  anova_depressed_illness <- aov(depressed ~ illness, data = usa_expanded_liwc)
  pvalue_depressed_illness <- summary(anova_depressed_illness)[[1]][["Pr(>F)"]][1]
  
  # wellness
  anova_depressed_wellness <- aov(depressed ~ wellness, data = usa_expanded_liwc)
  pvalue_depressed_wellness <- summary(anova_depressed_wellness)[[1]][["Pr(>F)"]][1]
  
  # mental
  anova_depressed_mental <- aov(depressed ~ mental, data = usa_expanded_liwc)
  pvalue_depressed_mental <- summary(anova_depressed_mental)[[1]][["Pr(>F)"]][1]
  
  # need
  anova_depressed_need <- aov(depressed ~ need, data = usa_expanded_liwc)
  pvalue_depressed_need <- summary(anova_depressed_need)[[1]][["Pr(>F)"]][1]
  
  # want
  anova_depressed_want <- aov(depressed ~ want, data = usa_expanded_liwc)
  pvalue_depressed_want <- summary(anova_depressed_want)[[1]][["Pr(>F)"]][1]
  
  # Acquire
  anova_depressed_acquire <- aov(depressed ~ acquire, data = usa_expanded_liwc)
  pvalue_depressed_acquire <- summary(anova_depressed_acquire)[[1]][["Pr(>F)"]][1]
  
  # Lack
  anova_depressed_lack <- aov(depressed ~ lack, data = usa_expanded_liwc)
  pvalue_depressed_lack <- summary(anova_depressed_lack)[[1]][["Pr(>F)"]][1]
  
  # Fulfill
  anova_depressed_fulfill <- aov(depressed ~ fulfill, data = usa_expanded_liwc)
  pvalue_depressed_fulfill <- summary(anova_depressed_fulfill)[[1]][["Pr(>F)"]][1]
  
  # Fatigue
  anova_depressed_fatigue <- aov(depressed ~ fatigue, data = usa_expanded_liwc)
  pvalue_depressed_fatigue <- summary(anova_depressed_fatigue)[[1]][["Pr(>F)"]][1]
}

# fulfill_1 anovas
if (A > 1) {
  # WC
  anova_fulfill_1_WC <- aov(fulfill_1 ~ WC, data = usa_expanded_liwc)
  pvalue_fulfill_1_WC <- summary(anova_fulfill_1_WC)[[1]][["Pr(>F)"]][1]
  
  # Analytic
  anova_fulfill_1_Analytic <- aov(fulfill_1 ~ Analytic, data = usa_expanded_liwc)
  pvalue_fulfill_1_Analytic <- summary(anova_fulfill_1_Analytic)[[1]][["Pr(>F)"]][1]
  
  # Clout
  anova_fulfill_1_Clout <- aov(fulfill_1 ~ Clout, data = usa_expanded_liwc)
  pvalue_fulfill_1_Clout <- summary(anova_fulfill_1_Clout)[[1]][["Pr(>F)"]][1]
  
  # Authentic
  anova_fulfill_1_Authentic <- aov(fulfill_1 ~ Authentic, data = usa_expanded_liwc)
  pvalue_fulfill_1_Authentic <- summary(anova_fulfill_1_Authentic)[[1]][["Pr(>F)"]][1]
  
  # Tone
  anova_fulfill_1_Tone <- aov(fulfill_1 ~ Tone, data = usa_expanded_liwc)
  pvalue_fulfill_1_Tone <- summary(anova_fulfill_1_Tone)[[1]][["Pr(>F)"]][1]
  
  # BigWords
  anova_fulfill_1_BigWords <- aov(fulfill_1 ~ BigWords, data = usa_expanded_liwc)
  pvalue_fulfill_1_BigWords <- summary(anova_fulfill_1_BigWords)[[1]][["Pr(>F)"]][1]
  
  # Dic
  anova_fulfill_1_Dic <- aov(fulfill_1 ~ Dic, data = usa_expanded_liwc)
  pvalue_fulfill_1_Dic <- summary(anova_fulfill_1_Dic)[[1]][["Pr(>F)"]][1]
  
  # Linguistic
  anova_fulfill_1_Linguistic <- aov(fulfill_1 ~ Linguistic, data = usa_expanded_liwc)
  pvalue_fulfill_1_Linguistic <- summary(anova_fulfill_1_Linguistic)[[1]][["Pr(>F)"]][1]
  
  # Function
  anova_fulfill_1_function <- aov(fulfill_1 ~ function., data = usa_expanded_liwc)
  pvalue_fulfill_1_function <- summary(anova_fulfill_1_function)[[1]][["Pr(>F)"]][1]
  
  # Pronoun
  anova_fulfill_1_pronoun <- aov(fulfill_1 ~ pronoun, data = usa_expanded_liwc)
  pvalue_fulfill_1_pronoun <- summary(anova_fulfill_1_pronoun)[[1]][["Pr(>F)"]][1]
  
  # Ppron
  anova_fulfill_1_ppron <- aov(fulfill_1 ~ ppron, data = usa_expanded_liwc)
  pvalue_fulfill_1_ppron <- summary(anova_fulfill_1_ppron)[[1]][["Pr(>F)"]][1]
  
  # i
  anova_fulfill_1_i <- aov(fulfill_1 ~ i, data = usa_expanded_liwc)
  pvalue_fulfill_1_i <- summary(anova_fulfill_1_i)[[1]][["Pr(>F)"]][1]
  
  # we
  anova_fulfill_1_we <- aov(fulfill_1 ~ we, data = usa_expanded_liwc)
  pvalue_fulfill_1_we <- summary(anova_fulfill_1_we)[[1]][["Pr(>F)"]][1]
  
  # you
  anova_fulfill_1_you <- aov(fulfill_1 ~ you, data = usa_expanded_liwc)
  pvalue_fulfill_1_you <- summary(anova_fulfill_1_you)[[1]][["Pr(>F)"]][1]
  
  # shehe
  anova_fulfill_1_shehe <- aov(fulfill_1 ~ shehe, data = usa_expanded_liwc)
  pvalue_fulfill_1_shehe <- summary(anova_fulfill_1_shehe)[[1]][["Pr(>F)"]][1]
  
  # they
  anova_fulfill_1_they <- aov(fulfill_1 ~ they, data = usa_expanded_liwc)
  pvalue_fulfill_1_they <- summary(anova_fulfill_1_they)[[1]][["Pr(>F)"]][1]
  
  # Affect
  anova_fulfill_1_Affect <- aov(fulfill_1 ~ Affect, data = usa_expanded_liwc)
  pvalue_fulfill_1_Affect <- summary(anova_fulfill_1_Affect)[[1]][["Pr(>F)"]][1]
  
  # tone_pos
  anova_fulfill_1_tone_pos <- aov(fulfill_1 ~ tone_pos, data = usa_expanded_liwc)
  pvalue_fulfill_1_tone_pos <- summary(anova_fulfill_1_tone_pos)[[1]][["Pr(>F)"]][1]
  
  # tone_neg
  anova_fulfill_1_tone_neg <- aov(fulfill_1 ~ tone_neg, data = usa_expanded_liwc)
  pvalue_fulfill_1_tone_neg <- summary(anova_fulfill_1_tone_neg)[[1]][["Pr(>F)"]][1]
  
  # emotion
  anova_fulfill_1_emotion <- aov(fulfill_1 ~ emotion, data = usa_expanded_liwc)
  pvalue_fulfill_1_emotion <- summary(anova_fulfill_1_emotion)[[1]][["Pr(>F)"]][1]
  
  # emo_pos
  anova_fulfill_1_emo_pos <- aov(fulfill_1 ~ emo_pos, data = usa_expanded_liwc)
  pvalue_fulfill_1_emo_pos <- summary(anova_fulfill_1_emo_pos)[[1]][["Pr(>F)"]][1]
  
  # emo_neg
  anova_fulfill_1_emo_neg <- aov(fulfill_1 ~ emo_neg, data = usa_expanded_liwc)
  pvalue_fulfill_1_emo_neg <- summary(anova_fulfill_1_emo_neg)[[1]][["Pr(>F)"]][1]
  
  # emo_anx
  anova_fulfill_1_emo_anx <- aov(fulfill_1 ~ emo_anx, data = usa_expanded_liwc)
  pvalue_fulfill_1_emo_anx <- summary(anova_fulfill_1_emo_anx)[[1]][["Pr(>F)"]][1]
  
  # emo_anger
  anova_fulfill_1_emo_anger <- aov(fulfill_1 ~ emo_anger, data = usa_expanded_liwc)
  pvalue_fulfill_1_emo_anger <- summary(anova_fulfill_1_emo_anger)[[1]][["Pr(>F)"]][1]
  
  # emo_sad
  anova_fulfill_1_emo_sad <- aov(fulfill_1 ~ emo_sad, data = usa_expanded_liwc)
  pvalue_fulfill_1_emo_sad <- summary(anova_fulfill_1_emo_sad)[[1]][["Pr(>F)"]][1]
  
  # swear
  anova_fulfill_1_swear <- aov(fulfill_1 ~ swear, data = usa_expanded_liwc)
  pvalue_fulfill_1_swear <- summary(anova_fulfill_1_swear)[[1]][["Pr(>F)"]][1]
  
  # Social
  anova_fulfill_1_Social <- aov(fulfill_1 ~ Social, data = usa_expanded_liwc)
  pvalue_fulfill_1_Social <- summary(anova_fulfill_1_Social)[[1]][["Pr(>F)"]][1]
  
  # socbehav
  anova_fulfill_1_socbehav <- aov(fulfill_1 ~ socbehav, data = usa_expanded_liwc)
  pvalue_fulfill_1_socbehav <- summary(anova_fulfill_1_socbehav)[[1]][["Pr(>F)"]][1]
  
  # prosocial
  anova_fulfill_1_prosocial <- aov(fulfill_1 ~ prosocial, data = usa_expanded_liwc)
  pvalue_fulfill_1_prosocial <- summary(anova_fulfill_1_prosocial)[[1]][["Pr(>F)"]][1]
  
  # polite
  anova_fulfill_1_polite <- aov(fulfill_1 ~ polite, data = usa_expanded_liwc)
  pvalue_fulfill_1_polite <- summary(anova_fulfill_1_polite)[[1]][["Pr(>F)"]][1]
  
  # conflict
  anova_fulfill_1_conflict <- aov(fulfill_1 ~ conflict, data = usa_expanded_liwc)
  pvalue_fulfill_1_conflict <- summary(anova_fulfill_1_conflict)[[1]][["Pr(>F)"]][1]
  
  # moral
  anova_fulfill_1_moral <- aov(fulfill_1 ~ moral, data = usa_expanded_liwc)
  pvalue_fulfill_1_moral <- summary(anova_fulfill_1_moral)[[1]][["Pr(>F)"]][1]
  
  # comm
  anova_fulfill_1_comm <- aov(fulfill_1 ~ comm, data = usa_expanded_liwc)
  pvalue_fulfill_1_comm <- summary(anova_fulfill_1_comm)[[1]][["Pr(>F)"]][1]
  
  # socrefs
  anova_fulfill_1_socrefs <- aov(fulfill_1 ~ socrefs, data = usa_expanded_liwc)
  pvalue_fulfill_1_socrefs <- summary(anova_fulfill_1_socrefs)[[1]][["Pr(>F)"]][1]
  
  # family
  anova_fulfill_1_family <- aov(fulfill_1 ~ family, data = usa_expanded_liwc)
  pvalue_fulfill_1_family <- summary(anova_fulfill_1_family)[[1]][["Pr(>F)"]][1]
  
  # friend
  anova_fulfill_1_friend <- aov(fulfill_1 ~ friend, data = usa_expanded_liwc)
  pvalue_fulfill_1_friend <- summary(anova_fulfill_1_friend)[[1]][["Pr(>F)"]][1]
  
  # female
  anova_fulfill_1_female <- aov(fulfill_1 ~ female, data = usa_expanded_liwc)
  pvalue_fulfill_1_female <- summary(anova_fulfill_1_female)[[1]][["Pr(>F)"]][1]
  
  # male
  anova_fulfill_1_male <- aov(fulfill_1 ~ male, data = usa_expanded_liwc)
  pvalue_fulfill_1_male <- summary(anova_fulfill_1_male)[[1]][["Pr(>F)"]][1]
  
  # health
  anova_fulfill_1_health <- aov(fulfill_1 ~ health, data = usa_expanded_liwc)
  pvalue_fulfill_1_health <- summary(anova_fulfill_1_health)[[1]][["Pr(>F)"]][1]
  
  # illness
  anova_fulfill_1_illness <- aov(fulfill_1 ~ illness, data = usa_expanded_liwc)
  pvalue_fulfill_1_illness <- summary(anova_fulfill_1_illness)[[1]][["Pr(>F)"]][1]
  
  # wellness
  anova_fulfill_1_wellness <- aov(fulfill_1 ~ wellness, data = usa_expanded_liwc)
  pvalue_fulfill_1_wellness <- summary(anova_fulfill_1_wellness)[[1]][["Pr(>F)"]][1]
  
  # mental
  anova_fulfill_1_mental <- aov(fulfill_1 ~ mental, data = usa_expanded_liwc)
  pvalue_fulfill_1_mental <- summary(anova_fulfill_1_mental)[[1]][["Pr(>F)"]][1]
  
  # need
  anova_fulfill_1_need <- aov(fulfill_1 ~ need, data = usa_expanded_liwc)
  pvalue_fulfill_1_need <- summary(anova_fulfill_1_need)[[1]][["Pr(>F)"]][1]
  
  # want
  anova_fulfill_1_want <- aov(fulfill_1 ~ want, data = usa_expanded_liwc)
  pvalue_fulfill_1_want <- summary(anova_fulfill_1_want)[[1]][["Pr(>F)"]][1]
  
  # Acquire
  anova_fulfill_1_acquire <- aov(fulfill_1 ~ acquire, data = usa_expanded_liwc)
  pvalue_fulfill_1_acquire <- summary(anova_fulfill_1_acquire)[[1]][["Pr(>F)"]][1]
  
  # Lack
  anova_fulfill_1_lack <- aov(fulfill_1 ~ lack, data = usa_expanded_liwc)
  pvalue_fulfill_1_lack <- summary(anova_fulfill_1_lack)[[1]][["Pr(>F)"]][1]
  
  # Fulfill
  anova_fulfill_1_fulfill <- aov(fulfill_1 ~ fulfill, data = usa_expanded_liwc)
  pvalue_fulfill_1_fulfill <- summary(anova_fulfill_1_fulfill)[[1]][["Pr(>F)"]][1]
  
  # Fatigue
  anova_fulfill_1_fatigue <- aov(fulfill_1 ~ fatigue, data = usa_expanded_liwc)
  pvalue_fulfill_1_fatigue <- summary(anova_fulfill_1_fatigue)[[1]][["Pr(>F)"]][1]
}

# effort anovas
if (A > 1) {
  # WC
  anova_effort_WC <- aov(effort ~ WC, data = usa_expanded_liwc)
  pvalue_effort_WC <- summary(anova_effort_WC)[[1]][["Pr(>F)"]][1]
  
  # Analytic
  anova_effort_Analytic <- aov(effort ~ Analytic, data = usa_expanded_liwc)
  pvalue_effort_Analytic <- summary(anova_effort_Analytic)[[1]][["Pr(>F)"]][1]
  
  # Clout
  anova_effort_Clout <- aov(effort ~ Clout, data = usa_expanded_liwc)
  pvalue_effort_Clout <- summary(anova_effort_Clout)[[1]][["Pr(>F)"]][1]
  
  # Authentic
  anova_effort_Authentic <- aov(effort ~ Authentic, data = usa_expanded_liwc)
  pvalue_effort_Authentic <- summary(anova_effort_Authentic)[[1]][["Pr(>F)"]][1]
  
  # Tone
  anova_effort_Tone <- aov(effort ~ Tone, data = usa_expanded_liwc)
  pvalue_effort_Tone <- summary(anova_effort_Tone)[[1]][["Pr(>F)"]][1]
  
  # BigWords
  anova_effort_BigWords <- aov(effort ~ BigWords, data = usa_expanded_liwc)
  pvalue_effort_BigWords <- summary(anova_effort_BigWords)[[1]][["Pr(>F)"]][1]
  
  # Dic
  anova_effort_Dic <- aov(effort ~ Dic, data = usa_expanded_liwc)
  pvalue_effort_Dic <- summary(anova_effort_Dic)[[1]][["Pr(>F)"]][1]
  
  # Linguistic
  anova_effort_Linguistic <- aov(effort ~ Linguistic, data = usa_expanded_liwc)
  pvalue_effort_Linguistic <- summary(anova_effort_Linguistic)[[1]][["Pr(>F)"]][1]
  
  # Function
  anova_effort_function <- aov(effort ~ function., data = usa_expanded_liwc)
  pvalue_effort_function <- summary(anova_effort_function)[[1]][["Pr(>F)"]][1]
  
  # Pronoun
  anova_effort_pronoun <- aov(effort ~ pronoun, data = usa_expanded_liwc)
  pvalue_effort_pronoun <- summary(anova_effort_pronoun)[[1]][["Pr(>F)"]][1]
  
  # Ppron
  anova_effort_ppron <- aov(effort ~ ppron, data = usa_expanded_liwc)
  pvalue_effort_ppron <- summary(anova_effort_ppron)[[1]][["Pr(>F)"]][1]
  
  # i
  anova_effort_i <- aov(effort ~ i, data = usa_expanded_liwc)
  pvalue_effort_i <- summary(anova_effort_i)[[1]][["Pr(>F)"]][1]
  
  # we
  anova_effort_we <- aov(effort ~ we, data = usa_expanded_liwc)
  pvalue_effort_we <- summary(anova_effort_we)[[1]][["Pr(>F)"]][1]
  
  # you
  anova_effort_you <- aov(effort ~ you, data = usa_expanded_liwc)
  pvalue_effort_you <- summary(anova_effort_you)[[1]][["Pr(>F)"]][1]
  
  # shehe
  anova_effort_shehe <- aov(effort ~ shehe, data = usa_expanded_liwc)
  pvalue_effort_shehe <- summary(anova_effort_shehe)[[1]][["Pr(>F)"]][1]
  
  # they
  anova_effort_they <- aov(effort ~ they, data = usa_expanded_liwc)
  pvalue_effort_they <- summary(anova_effort_they)[[1]][["Pr(>F)"]][1]
  
  # Affect
  anova_effort_Affect <- aov(effort ~ Affect, data = usa_expanded_liwc)
  pvalue_effort_Affect <- summary(anova_effort_Affect)[[1]][["Pr(>F)"]][1]
  
  # tone_pos
  anova_effort_tone_pos <- aov(effort ~ tone_pos, data = usa_expanded_liwc)
  pvalue_effort_tone_pos <- summary(anova_effort_tone_pos)[[1]][["Pr(>F)"]][1]
  
  # tone_neg
  anova_effort_tone_neg <- aov(effort ~ tone_neg, data = usa_expanded_liwc)
  pvalue_effort_tone_neg <- summary(anova_effort_tone_neg)[[1]][["Pr(>F)"]][1]
  
  # emotion
  anova_effort_emotion <- aov(effort ~ emotion, data = usa_expanded_liwc)
  pvalue_effort_emotion <- summary(anova_effort_emotion)[[1]][["Pr(>F)"]][1]
  
  # emo_pos
  anova_effort_emo_pos <- aov(effort ~ emo_pos, data = usa_expanded_liwc)
  pvalue_effort_emo_pos <- summary(anova_effort_emo_pos)[[1]][["Pr(>F)"]][1]
  
  # emo_neg
  anova_effort_emo_neg <- aov(effort ~ emo_neg, data = usa_expanded_liwc)
  pvalue_effort_emo_neg <- summary(anova_effort_emo_neg)[[1]][["Pr(>F)"]][1]
  
  # emo_anx
  anova_effort_emo_anx <- aov(effort ~ emo_anx, data = usa_expanded_liwc)
  pvalue_effort_emo_anx <- summary(anova_effort_emo_anx)[[1]][["Pr(>F)"]][1]
  
  # emo_anger
  anova_effort_emo_anger <- aov(effort ~ emo_anger, data = usa_expanded_liwc)
  pvalue_effort_emo_anger <- summary(anova_effort_emo_anger)[[1]][["Pr(>F)"]][1]
  
  # emo_sad
  anova_effort_emo_sad <- aov(effort ~ emo_sad, data = usa_expanded_liwc)
  pvalue_effort_emo_sad <- summary(anova_effort_emo_sad)[[1]][["Pr(>F)"]][1]
  
  # swear
  anova_effort_swear <- aov(effort ~ swear, data = usa_expanded_liwc)
  pvalue_effort_swear <- summary(anova_effort_swear)[[1]][["Pr(>F)"]][1]
  
  # Social
  anova_effort_Social <- aov(effort ~ Social, data = usa_expanded_liwc)
  pvalue_effort_Social <- summary(anova_effort_Social)[[1]][["Pr(>F)"]][1]
  
  # socbehav
  anova_effort_socbehav <- aov(effort ~ socbehav, data = usa_expanded_liwc)
  pvalue_effort_socbehav <- summary(anova_effort_socbehav)[[1]][["Pr(>F)"]][1]
  
  # prosocial
  anova_effort_prosocial <- aov(effort ~ prosocial, data = usa_expanded_liwc)
  pvalue_effort_prosocial <- summary(anova_effort_prosocial)[[1]][["Pr(>F)"]][1]
  
  # polite
  anova_effort_polite <- aov(effort ~ polite, data = usa_expanded_liwc)
  pvalue_effort_polite <- summary(anova_effort_polite)[[1]][["Pr(>F)"]][1]
  
  # conflict
  anova_effort_conflict <- aov(effort ~ conflict, data = usa_expanded_liwc)
  pvalue_effort_conflict <- summary(anova_effort_conflict)[[1]][["Pr(>F)"]][1]
  
  # moral
  anova_effort_moral <- aov(effort ~ moral, data = usa_expanded_liwc)
  pvalue_effort_moral <- summary(anova_effort_moral)[[1]][["Pr(>F)"]][1]
  
  # comm
  anova_effort_comm <- aov(effort ~ comm, data = usa_expanded_liwc)
  pvalue_effort_comm <- summary(anova_effort_comm)[[1]][["Pr(>F)"]][1]
  
  # socrefs
  anova_effort_socrefs <- aov(effort ~ socrefs, data = usa_expanded_liwc)
  pvalue_effort_socrefs <- summary(anova_effort_socrefs)[[1]][["Pr(>F)"]][1]
  
  # family
  anova_effort_family <- aov(effort ~ family, data = usa_expanded_liwc)
  pvalue_effort_family <- summary(anova_effort_family)[[1]][["Pr(>F)"]][1]
  
  # friend
  anova_effort_friend <- aov(effort ~ friend, data = usa_expanded_liwc)
  pvalue_effort_friend <- summary(anova_effort_friend)[[1]][["Pr(>F)"]][1]
  
  # female
  anova_effort_female <- aov(effort ~ female, data = usa_expanded_liwc)
  pvalue_effort_female <- summary(anova_effort_female)[[1]][["Pr(>F)"]][1]
  
  # male
  anova_effort_male <- aov(effort ~ male, data = usa_expanded_liwc)
  pvalue_effort_male <- summary(anova_effort_male)[[1]][["Pr(>F)"]][1]
  
  # health
  anova_effort_health <- aov(effort ~ health, data = usa_expanded_liwc)
  pvalue_effort_health <- summary(anova_effort_health)[[1]][["Pr(>F)"]][1]
  
  # illness
  anova_effort_illness <- aov(effort ~ illness, data = usa_expanded_liwc)
  pvalue_effort_illness <- summary(anova_effort_illness)[[1]][["Pr(>F)"]][1]
  
  # wellness
  anova_effort_wellness <- aov(effort ~ wellness, data = usa_expanded_liwc)
  pvalue_effort_wellness <- summary(anova_effort_wellness)[[1]][["Pr(>F)"]][1]
  
  # mental
  anova_effort_mental <- aov(effort ~ mental, data = usa_expanded_liwc)
  pvalue_effort_mental <- summary(anova_effort_mental)[[1]][["Pr(>F)"]][1]
  
  # need
  anova_effort_need <- aov(effort ~ need, data = usa_expanded_liwc)
  pvalue_effort_need <- summary(anova_effort_need)[[1]][["Pr(>F)"]][1]
  
  # want
  anova_effort_want <- aov(effort ~ want, data = usa_expanded_liwc)
  pvalue_effort_want <- summary(anova_effort_want)[[1]][["Pr(>F)"]][1]
  
  # Acquire
  anova_effort_acquire <- aov(effort ~ acquire, data = usa_expanded_liwc)
  pvalue_effort_acquire <- summary(anova_effort_acquire)[[1]][["Pr(>F)"]][1]
  
  # Lack
  anova_effort_lack <- aov(effort ~ lack, data = usa_expanded_liwc)
  pvalue_effort_lack <- summary(anova_effort_lack)[[1]][["Pr(>F)"]][1]
  
  # Fulfill
  anova_effort_fulfill <- aov(effort ~ fulfill, data = usa_expanded_liwc)
  pvalue_effort_fulfill <- summary(anova_effort_fulfill)[[1]][["Pr(>F)"]][1]
  
  # Fatigue
  anova_effort_fatigue <- aov(effort ~ fatigue, data = usa_expanded_liwc)
  pvalue_effort_fatigue <- summary(anova_effort_fatigue)[[1]][["Pr(>F)"]][1]
  
}

# sex anovas
if (A > 1) {
  # WC
  anova_sex_WC <- aov(sex ~ WC, data = usa_expanded_liwc)
  pvalue_sex_WC <- summary(anova_sex_WC)[[1]][["Pr(>F)"]][1]
  
  # Analytic
  anova_sex_Analytic <- aov(sex ~ Analytic, data = usa_expanded_liwc)
  pvalue_sex_Analytic <- summary(anova_sex_Analytic)[[1]][["Pr(>F)"]][1]
  
  # Clout
  anova_sex_Clout <- aov(sex ~ Clout, data = usa_expanded_liwc)
  pvalue_sex_Clout <- summary(anova_sex_Clout)[[1]][["Pr(>F)"]][1]
  
  # Authentic
  anova_sex_Authentic <- aov(sex ~ Authentic, data = usa_expanded_liwc)
  pvalue_sex_Authentic <- summary(anova_sex_Authentic)[[1]][["Pr(>F)"]][1]
  
  # Tone
  anova_sex_Tone <- aov(sex ~ Tone, data = usa_expanded_liwc)
  pvalue_sex_Tone <- summary(anova_sex_Tone)[[1]][["Pr(>F)"]][1]
  
  # BigWords
  anova_sex_BigWords <- aov(sex ~ BigWords, data = usa_expanded_liwc)
  pvalue_sex_BigWords <- summary(anova_sex_BigWords)[[1]][["Pr(>F)"]][1]
  
  # Dic
  anova_sex_Dic <- aov(sex ~ Dic, data = usa_expanded_liwc)
  pvalue_sex_Dic <- summary(anova_sex_Dic)[[1]][["Pr(>F)"]][1]
  
  # Linguistic
  anova_sex_Linguistic <- aov(sex ~ Linguistic, data = usa_expanded_liwc)
  pvalue_sex_Linguistic <- summary(anova_sex_Linguistic)[[1]][["Pr(>F)"]][1]
  
  # Function
  anova_sex_function <- aov(sex ~ function., data = usa_expanded_liwc)
  pvalue_sex_function <- summary(anova_sex_function)[[1]][["Pr(>F)"]][1]
  
  # Pronoun
  anova_sex_pronoun <- aov(sex ~ pronoun, data = usa_expanded_liwc)
  pvalue_sex_pronoun <- summary(anova_sex_pronoun)[[1]][["Pr(>F)"]][1]
  
  # Ppron
  anova_sex_ppron <- aov(sex ~ ppron, data = usa_expanded_liwc)
  pvalue_sex_ppron <- summary(anova_sex_ppron)[[1]][["Pr(>F)"]][1]
  
  # i
  anova_sex_i <- aov(sex ~ i, data = usa_expanded_liwc)
  pvalue_sex_i <- summary(anova_sex_i)[[1]][["Pr(>F)"]][1]
  
  # we
  anova_sex_we <- aov(sex ~ we, data = usa_expanded_liwc)
  pvalue_sex_we <- summary(anova_sex_we)[[1]][["Pr(>F)"]][1]
  
  # you
  anova_sex_you <- aov(sex ~ you, data = usa_expanded_liwc)
  pvalue_sex_you <- summary(anova_sex_you)[[1]][["Pr(>F)"]][1]
  
  # shehe
  anova_sex_shehe <- aov(sex ~ shehe, data = usa_expanded_liwc)
  pvalue_sex_shehe <- summary(anova_sex_shehe)[[1]][["Pr(>F)"]][1]
  
  # they
  anova_sex_they <- aov(sex ~ they, data = usa_expanded_liwc)
  pvalue_sex_they <- summary(anova_sex_they)[[1]][["Pr(>F)"]][1]
  
  # Affect
  anova_sex_Affect <- aov(sex ~ Affect, data = usa_expanded_liwc)
  pvalue_sex_Affect <- summary(anova_sex_Affect)[[1]][["Pr(>F)"]][1]
  
  # tone_pos
  anova_sex_tone_pos <- aov(sex ~ tone_pos, data = usa_expanded_liwc)
  pvalue_sex_tone_pos <- summary(anova_sex_tone_pos)[[1]][["Pr(>F)"]][1]
  
  # tone_neg
  anova_sex_tone_neg <- aov(sex ~ tone_neg, data = usa_expanded_liwc)
  pvalue_sex_tone_neg <- summary(anova_sex_tone_neg)[[1]][["Pr(>F)"]][1]
  
  # emotion
  anova_sex_emotion <- aov(sex ~ emotion, data = usa_expanded_liwc)
  pvalue_sex_emotion <- summary(anova_sex_emotion)[[1]][["Pr(>F)"]][1]
  
  # emo_pos
  anova_sex_emo_pos <- aov(sex ~ emo_pos, data = usa_expanded_liwc)
  pvalue_sex_emo_pos <- summary(anova_sex_emo_pos)[[1]][["Pr(>F)"]][1]
  
  # emo_neg
  anova_sex_emo_neg <- aov(sex ~ emo_neg, data = usa_expanded_liwc)
  pvalue_sex_emo_neg <- summary(anova_sex_emo_neg)[[1]][["Pr(>F)"]][1]
  
  # emo_anx
  anova_sex_emo_anx <- aov(sex ~ emo_anx, data = usa_expanded_liwc)
  pvalue_sex_emo_anx <- summary(anova_sex_emo_anx)[[1]][["Pr(>F)"]][1]
  
  # emo_anger
  anova_sex_emo_anger <- aov(sex ~ emo_anger, data = usa_expanded_liwc)
  pvalue_sex_emo_anger <- summary(anova_sex_emo_anger)[[1]][["Pr(>F)"]][1]
  
  # emo_sad
  anova_sex_emo_sad <- aov(sex ~ emo_sad, data = usa_expanded_liwc)
  pvalue_sex_emo_sad <- summary(anova_sex_emo_sad)[[1]][["Pr(>F)"]][1]
  
  # swear
  anova_sex_swear <- aov(sex ~ swear, data = usa_expanded_liwc)
  pvalue_sex_swear <- summary(anova_sex_swear)[[1]][["Pr(>F)"]][1]
  
  # Social
  anova_sex_Social <- aov(sex ~ Social, data = usa_expanded_liwc)
  pvalue_sex_Social <- summary(anova_sex_Social)[[1]][["Pr(>F)"]][1]
  
  # socbehav
  anova_sex_socbehav <- aov(sex ~ socbehav, data = usa_expanded_liwc)
  pvalue_sex_socbehav <- summary(anova_sex_socbehav)[[1]][["Pr(>F)"]][1]
  
  # prosocial
  anova_sex_prosocial <- aov(sex ~ prosocial, data = usa_expanded_liwc)
  pvalue_sex_prosocial <- summary(anova_sex_prosocial)[[1]][["Pr(>F)"]][1]
  
  # polite
  anova_sex_polite <- aov(sex ~ polite, data = usa_expanded_liwc)
  pvalue_sex_polite <- summary(anova_sex_polite)[[1]][["Pr(>F)"]][1]
  
  # conflict
  anova_sex_conflict <- aov(sex ~ conflict, data = usa_expanded_liwc)
  pvalue_sex_conflict <- summary(anova_sex_conflict)[[1]][["Pr(>F)"]][1]
  
  # moral
  anova_sex_moral <- aov(sex ~ moral, data = usa_expanded_liwc)
  pvalue_sex_moral <- summary(anova_sex_moral)[[1]][["Pr(>F)"]][1]
  
  # comm
  anova_sex_comm <- aov(sex ~ comm, data = usa_expanded_liwc)
  pvalue_sex_comm <- summary(anova_sex_comm)[[1]][["Pr(>F)"]][1]
  
  # socrefs
  anova_sex_socrefs <- aov(sex ~ socrefs, data = usa_expanded_liwc)
  pvalue_sex_socrefs <- summary(anova_sex_socrefs)[[1]][["Pr(>F)"]][1]
  
  # family
  anova_sex_family <- aov(sex ~ family, data = usa_expanded_liwc)
  pvalue_sex_family <- summary(anova_sex_family)[[1]][["Pr(>F)"]][1]
  
  # friend
  anova_sex_friend <- aov(sex ~ friend, data = usa_expanded_liwc)
  pvalue_sex_friend <- summary(anova_sex_friend)[[1]][["Pr(>F)"]][1]
  
  # female
  anova_sex_female <- aov(sex ~ female, data = usa_expanded_liwc)
  pvalue_sex_female <- summary(anova_sex_female)[[1]][["Pr(>F)"]][1]
  
  # male
  anova_sex_male <- aov(sex ~ male, data = usa_expanded_liwc)
  pvalue_sex_male <- summary(anova_sex_male)[[1]][["Pr(>F)"]][1]
  
  # health
  anova_sex_health <- aov(sex ~ health, data = usa_expanded_liwc)
  pvalue_sex_health <- summary(anova_sex_health)[[1]][["Pr(>F)"]][1]
  
  # illness
  anova_sex_illness <- aov(sex ~ illness, data = usa_expanded_liwc)
  pvalue_sex_illness <- summary(anova_sex_illness)[[1]][["Pr(>F)"]][1]
  
  # wellness
  anova_sex_wellness <- aov(sex ~ wellness, data = usa_expanded_liwc)
  pvalue_sex_wellness <- summary(anova_sex_wellness)[[1]][["Pr(>F)"]][1]
  
  # mental
  anova_sex_mental <- aov(sex ~ mental, data = usa_expanded_liwc)
  pvalue_sex_mental <- summary(anova_sex_mental)[[1]][["Pr(>F)"]][1]
  
  # need
  anova_sex_need <- aov(sex ~ need, data = usa_expanded_liwc)
  pvalue_sex_need <- summary(anova_sex_need)[[1]][["Pr(>F)"]][1]
  
  # want
  anova_sex_want <- aov(sex ~ want, data = usa_expanded_liwc)
  pvalue_sex_want <- summary(anova_sex_want)[[1]][["Pr(>F)"]][1]
  
  # Acquire
  anova_sex_acquire <- aov(sex ~ acquire, data = usa_expanded_liwc)
  pvalue_sex_acquire <- summary(anova_sex_acquire)[[1]][["Pr(>F)"]][1]
  
  # Lack
  anova_sex_lack <- aov(sex ~ lack, data = usa_expanded_liwc)
  pvalue_sex_lack <- summary(anova_sex_lack)[[1]][["Pr(>F)"]][1]
  
  # Fulfill
  anova_sex_fulfill <- aov(sex ~ fulfill, data = usa_expanded_liwc)
  pvalue_sex_fulfill <- summary(anova_sex_fulfill)[[1]][["Pr(>F)"]][1]
  
  # Fatigue
  anova_sex_fatigue <- aov(sex ~ fatigue, data = usa_expanded_liwc)
  pvalue_sex_fatigue <- summary(anova_sex_fatigue)[[1]][["Pr(>F)"]][1]
  
}

# age anovas
if (A > 1) {
  # WC
  anova_age_WC <- aov(age ~ WC, data = usa_expanded_liwc)
  pvalue_age_WC <- summary(anova_age_WC)[[1]][["Pr(>F)"]][1]
  
  # Analytic
  anova_age_Analytic <- aov(age ~ Analytic, data = usa_expanded_liwc)
  pvalue_age_Analytic <- summary(anova_age_Analytic)[[1]][["Pr(>F)"]][1]
  
  # Clout
  anova_age_Clout <- aov(age ~ Clout, data = usa_expanded_liwc)
  pvalue_age_Clout <- summary(anova_age_Clout)[[1]][["Pr(>F)"]][1]
  
  # Authentic
  anova_age_Authentic <- aov(age ~ Authentic, data = usa_expanded_liwc)
  pvalue_age_Authentic <- summary(anova_age_Authentic)[[1]][["Pr(>F)"]][1]
  
  # Tone
  anova_age_Tone <- aov(age ~ Tone, data = usa_expanded_liwc)
  pvalue_age_Tone <- summary(anova_age_Tone)[[1]][["Pr(>F)"]][1]
  
  # BigWords
  anova_age_BigWords <- aov(age ~ BigWords, data = usa_expanded_liwc)
  pvalue_age_BigWords <- summary(anova_age_BigWords)[[1]][["Pr(>F)"]][1]
  
  # Dic
  anova_age_Dic <- aov(age ~ Dic, data = usa_expanded_liwc)
  pvalue_age_Dic <- summary(anova_age_Dic)[[1]][["Pr(>F)"]][1]
  
  # Linguistic
  anova_age_Linguistic <- aov(age ~ Linguistic, data = usa_expanded_liwc)
  pvalue_age_Linguistic <- summary(anova_age_Linguistic)[[1]][["Pr(>F)"]][1]
  
  # Function
  anova_age_function <- aov(age ~ function., data = usa_expanded_liwc)
  pvalue_age_function <- summary(anova_age_function)[[1]][["Pr(>F)"]][1]
  
  # Pronoun
  anova_age_pronoun <- aov(age ~ pronoun, data = usa_expanded_liwc)
  pvalue_age_pronoun <- summary(anova_age_pronoun)[[1]][["Pr(>F)"]][1]
  
  # Ppron
  anova_age_ppron <- aov(age ~ ppron, data = usa_expanded_liwc)
  pvalue_age_ppron <- summary(anova_age_ppron)[[1]][["Pr(>F)"]][1]
  
  # i
  anova_age_i <- aov(age ~ i, data = usa_expanded_liwc)
  pvalue_age_i <- summary(anova_age_i)[[1]][["Pr(>F)"]][1]
  
  # we
  anova_age_we <- aov(age ~ we, data = usa_expanded_liwc)
  pvalue_age_we <- summary(anova_age_we)[[1]][["Pr(>F)"]][1]
  
  # you
  anova_age_you <- aov(age ~ you, data = usa_expanded_liwc)
  pvalue_age_you <- summary(anova_age_you)[[1]][["Pr(>F)"]][1]
  
  # shehe
  anova_age_shehe <- aov(age ~ shehe, data = usa_expanded_liwc)
  pvalue_age_shehe <- summary(anova_age_shehe)[[1]][["Pr(>F)"]][1]
  
  # they
  anova_age_they <- aov(age ~ they, data = usa_expanded_liwc)
  pvalue_age_they <- summary(anova_age_they)[[1]][["Pr(>F)"]][1]
  
  # Affect
  anova_age_Affect <- aov(age ~ Affect, data = usa_expanded_liwc)
  pvalue_age_Affect <- summary(anova_age_Affect)[[1]][["Pr(>F)"]][1]
  
  # tone_pos
  anova_age_tone_pos <- aov(age ~ tone_pos, data = usa_expanded_liwc)
  pvalue_age_tone_pos <- summary(anova_age_tone_pos)[[1]][["Pr(>F)"]][1]
  
  # tone_neg
  anova_age_tone_neg <- aov(age ~ tone_neg, data = usa_expanded_liwc)
  pvalue_age_tone_neg <- summary(anova_age_tone_neg)[[1]][["Pr(>F)"]][1]
  
  # emotion
  anova_age_emotion <- aov(age ~ emotion, data = usa_expanded_liwc)
  pvalue_age_emotion <- summary(anova_age_emotion)[[1]][["Pr(>F)"]][1]
  
  # emo_pos
  anova_age_emo_pos <- aov(age ~ emo_pos, data = usa_expanded_liwc)
  pvalue_age_emo_pos <- summary(anova_age_emo_pos)[[1]][["Pr(>F)"]][1]
  
  # emo_neg
  anova_age_emo_neg <- aov(age ~ emo_neg, data = usa_expanded_liwc)
  pvalue_age_emo_neg <- summary(anova_age_emo_neg)[[1]][["Pr(>F)"]][1]
  
  # emo_anx
  anova_age_emo_anx <- aov(age ~ emo_anx, data = usa_expanded_liwc)
  pvalue_age_emo_anx <- summary(anova_age_emo_anx)[[1]][["Pr(>F)"]][1]
  
  # emo_anger
  anova_age_emo_anger <- aov(age ~ emo_anger, data = usa_expanded_liwc)
  pvalue_age_emo_anger <- summary(anova_age_emo_anger)[[1]][["Pr(>F)"]][1]
  
  # emo_sad
  anova_age_emo_sad <- aov(age ~ emo_sad, data = usa_expanded_liwc)
  pvalue_age_emo_sad <- summary(anova_age_emo_sad)[[1]][["Pr(>F)"]][1]
  
  # swear
  anova_age_swear <- aov(age ~ swear, data = usa_expanded_liwc)
  pvalue_age_swear <- summary(anova_age_swear)[[1]][["Pr(>F)"]][1]
  
  # Social
  anova_age_Social <- aov(age ~ Social, data = usa_expanded_liwc)
  pvalue_age_Social <- summary(anova_age_Social)[[1]][["Pr(>F)"]][1]
  
  # socbehav
  anova_age_socbehav <- aov(age ~ socbehav, data = usa_expanded_liwc)
  pvalue_age_socbehav <- summary(anova_age_socbehav)[[1]][["Pr(>F)"]][1]
  
  # prosocial
  anova_age_prosocial <- aov(age ~ prosocial, data = usa_expanded_liwc)
  pvalue_age_prosocial <- summary(anova_age_prosocial)[[1]][["Pr(>F)"]][1]
  
  # polite
  anova_age_polite <- aov(age ~ polite, data = usa_expanded_liwc)
  pvalue_age_polite <- summary(anova_age_polite)[[1]][["Pr(>F)"]][1]
  
  # conflict
  anova_age_conflict <- aov(age ~ conflict, data = usa_expanded_liwc)
  pvalue_age_conflict <- summary(anova_age_conflict)[[1]][["Pr(>F)"]][1]
  
  # moral
  anova_age_moral <- aov(age ~ moral, data = usa_expanded_liwc)
  pvalue_age_moral <- summary(anova_age_moral)[[1]][["Pr(>F)"]][1]
  
  # comm
  anova_age_comm <- aov(age ~ comm, data = usa_expanded_liwc)
  pvalue_age_comm <- summary(anova_age_comm)[[1]][["Pr(>F)"]][1]
  
  # socrefs
  anova_age_socrefs <- aov(age ~ socrefs, data = usa_expanded_liwc)
  pvalue_age_socrefs <- summary(anova_age_socrefs)[[1]][["Pr(>F)"]][1]
  
  # family
  anova_age_family <- aov(age ~ family, data = usa_expanded_liwc)
  pvalue_age_family <- summary(anova_age_family)[[1]][["Pr(>F)"]][1]
  
  # friend
  anova_age_friend <- aov(age ~ friend, data = usa_expanded_liwc)
  pvalue_age_friend <- summary(anova_age_friend)[[1]][["Pr(>F)"]][1]
  
  # female
  anova_age_female <- aov(age ~ female, data = usa_expanded_liwc)
  pvalue_age_female <- summary(anova_age_female)[[1]][["Pr(>F)"]][1]
  
  # male
  anova_age_male <- aov(age ~ male, data = usa_expanded_liwc)
  pvalue_age_male <- summary(anova_age_male)[[1]][["Pr(>F)"]][1]
  
  # health
  anova_age_health <- aov(age ~ health, data = usa_expanded_liwc)
  pvalue_age_health <- summary(anova_age_health)[[1]][["Pr(>F)"]][1]
  
  # illness
  anova_age_illness <- aov(age ~ illness, data = usa_expanded_liwc)
  pvalue_age_illness <- summary(anova_age_illness)[[1]][["Pr(>F)"]][1]
  
  # wellness
  anova_age_wellness <- aov(age ~ wellness, data = usa_expanded_liwc)
  pvalue_age_wellness <- summary(anova_age_wellness)[[1]][["Pr(>F)"]][1]
  
  # mental
  anova_age_mental <- aov(age ~ mental, data = usa_expanded_liwc)
  pvalue_age_mental <- summary(anova_age_mental)[[1]][["Pr(>F)"]][1]
  
  # need
  anova_age_need <- aov(age ~ need, data = usa_expanded_liwc)
  pvalue_age_need <- summary(anova_age_need)[[1]][["Pr(>F)"]][1]
  
  # want
  anova_age_want <- aov(age ~ want, data = usa_expanded_liwc)
  pvalue_age_want <- summary(anova_age_want)[[1]][["Pr(>F)"]][1]
  
  # Acquire
  anova_age_acquire <- aov(age ~ acquire, data = usa_expanded_liwc)
  pvalue_age_acquire <- summary(anova_age_acquire)[[1]][["Pr(>F)"]][1]
  
  # Lack
  anova_age_lack <- aov(age ~ lack, data = usa_expanded_liwc)
  pvalue_age_lack <- summary(anova_age_lack)[[1]][["Pr(>F)"]][1]
  
  # Fulfill
  anova_age_fulfill <- aov(age ~ fulfill, data = usa_expanded_liwc)
  pvalue_age_fulfill <- summary(anova_age_fulfill)[[1]][["Pr(>F)"]][1]
  
  # Fatigue
  anova_age_fatigue <- aov(age ~ fatigue, data = usa_expanded_liwc)
  pvalue_age_fatigue <- summary(anova_age_fatigue)[[1]][["Pr(>F)"]][1]
  
}

# ses anovas
if (A > 1) {
  # WC
  anova_ses_WC <- aov(ses ~ WC, data = usa_expanded_liwc)
  pvalue_ses_WC <- summary(anova_ses_WC)[[1]][["Pr(>F)"]][1]
  
  # Analytic
  anova_ses_Analytic <- aov(ses ~ Analytic, data = usa_expanded_liwc)
  pvalue_ses_Analytic <- summary(anova_ses_Analytic)[[1]][["Pr(>F)"]][1]
  
  # Clout
  anova_ses_Clout <- aov(ses ~ Clout, data = usa_expanded_liwc)
  pvalue_ses_Clout <- summary(anova_ses_Clout)[[1]][["Pr(>F)"]][1]
  
  # Authentic
  anova_ses_Authentic <- aov(ses ~ Authentic, data = usa_expanded_liwc)
  pvalue_ses_Authentic <- summary(anova_ses_Authentic)[[1]][["Pr(>F)"]][1]
  
  # Tone
  anova_ses_Tone <- aov(ses ~ Tone, data = usa_expanded_liwc)
  pvalue_ses_Tone <- summary(anova_ses_Tone)[[1]][["Pr(>F)"]][1]
  
  # BigWords
  anova_ses_BigWords <- aov(ses ~ BigWords, data = usa_expanded_liwc)
  pvalue_ses_BigWords <- summary(anova_ses_BigWords)[[1]][["Pr(>F)"]][1]
  
  # Dic
  anova_ses_Dic <- aov(ses ~ Dic, data = usa_expanded_liwc)
  pvalue_ses_Dic <- summary(anova_ses_Dic)[[1]][["Pr(>F)"]][1]
  
  # Linguistic
  anova_ses_Linguistic <- aov(ses ~ Linguistic, data = usa_expanded_liwc)
  pvalue_ses_Linguistic <- summary(anova_ses_Linguistic)[[1]][["Pr(>F)"]][1]
  
  # Function
  anova_ses_function <- aov(ses ~ function., data = usa_expanded_liwc)
  pvalue_ses_function <- summary(anova_ses_function)[[1]][["Pr(>F)"]][1]
  
  # Pronoun
  anova_ses_pronoun <- aov(ses ~ pronoun, data = usa_expanded_liwc)
  pvalue_ses_pronoun <- summary(anova_ses_pronoun)[[1]][["Pr(>F)"]][1]
  
  # Ppron
  anova_ses_ppron <- aov(ses ~ ppron, data = usa_expanded_liwc)
  pvalue_ses_ppron <- summary(anova_ses_ppron)[[1]][["Pr(>F)"]][1]
  
  # i
  anova_ses_i <- aov(ses ~ i, data = usa_expanded_liwc)
  pvalue_ses_i <- summary(anova_ses_i)[[1]][["Pr(>F)"]][1]
  
  # we
  anova_ses_we <- aov(ses ~ we, data = usa_expanded_liwc)
  pvalue_ses_we <- summary(anova_ses_we)[[1]][["Pr(>F)"]][1]
  
  # you
  anova_ses_you <- aov(ses ~ you, data = usa_expanded_liwc)
  pvalue_ses_you <- summary(anova_ses_you)[[1]][["Pr(>F)"]][1]
  
  # shehe
  anova_ses_shehe <- aov(ses ~ shehe, data = usa_expanded_liwc)
  pvalue_ses_shehe <- summary(anova_ses_shehe)[[1]][["Pr(>F)"]][1]
  
  # they
  anova_ses_they <- aov(ses ~ they, data = usa_expanded_liwc)
  pvalue_ses_they <- summary(anova_ses_they)[[1]][["Pr(>F)"]][1]
  
  # Affect
  anova_ses_Affect <- aov(ses ~ Affect, data = usa_expanded_liwc)
  pvalue_ses_Affect <- summary(anova_ses_Affect)[[1]][["Pr(>F)"]][1]
  
  # tone_pos
  anova_ses_tone_pos <- aov(ses ~ tone_pos, data = usa_expanded_liwc)
  pvalue_ses_tone_pos <- summary(anova_ses_tone_pos)[[1]][["Pr(>F)"]][1]
  
  # tone_neg
  anova_ses_tone_neg <- aov(ses ~ tone_neg, data = usa_expanded_liwc)
  pvalue_ses_tone_neg <- summary(anova_ses_tone_neg)[[1]][["Pr(>F)"]][1]
  
  # emotion
  anova_ses_emotion <- aov(ses ~ emotion, data = usa_expanded_liwc)
  pvalue_ses_emotion <- summary(anova_ses_emotion)[[1]][["Pr(>F)"]][1]
  
  # emo_pos
  anova_ses_emo_pos <- aov(ses ~ emo_pos, data = usa_expanded_liwc)
  pvalue_ses_emo_pos <- summary(anova_ses_emo_pos)[[1]][["Pr(>F)"]][1]
  
  # emo_neg
  anova_ses_emo_neg <- aov(ses ~ emo_neg, data = usa_expanded_liwc)
  pvalue_ses_emo_neg <- summary(anova_ses_emo_neg)[[1]][["Pr(>F)"]][1]
  
  # emo_anx
  anova_ses_emo_anx <- aov(ses ~ emo_anx, data = usa_expanded_liwc)
  pvalue_ses_emo_anx <- summary(anova_ses_emo_anx)[[1]][["Pr(>F)"]][1]
  
  # emo_anger
  anova_ses_emo_anger <- aov(ses ~ emo_anger, data = usa_expanded_liwc)
  pvalue_ses_emo_anger <- summary(anova_ses_emo_anger)[[1]][["Pr(>F)"]][1]
  
  # emo_sad
  anova_ses_emo_sad <- aov(ses ~ emo_sad, data = usa_expanded_liwc)
  pvalue_ses_emo_sad <- summary(anova_ses_emo_sad)[[1]][["Pr(>F)"]][1]
  
  # swear
  anova_ses_swear <- aov(ses ~ swear, data = usa_expanded_liwc)
  pvalue_ses_swear <- summary(anova_ses_swear)[[1]][["Pr(>F)"]][1]
  
  # Social
  anova_ses_Social <- aov(ses ~ Social, data = usa_expanded_liwc)
  pvalue_ses_Social <- summary(anova_ses_Social)[[1]][["Pr(>F)"]][1]
  
  # socbehav
  anova_ses_socbehav <- aov(ses ~ socbehav, data = usa_expanded_liwc)
  pvalue_ses_socbehav <- summary(anova_ses_socbehav)[[1]][["Pr(>F)"]][1]
  
  # prosocial
  anova_ses_prosocial <- aov(ses ~ prosocial, data = usa_expanded_liwc)
  pvalue_ses_prosocial <- summary(anova_ses_prosocial)[[1]][["Pr(>F)"]][1]
  
  # polite
  anova_ses_polite <- aov(ses ~ polite, data = usa_expanded_liwc)
  pvalue_ses_polite <- summary(anova_ses_polite)[[1]][["Pr(>F)"]][1]
  
  # conflict
  anova_ses_conflict <- aov(ses ~ conflict, data = usa_expanded_liwc)
  pvalue_ses_conflict <- summary(anova_ses_conflict)[[1]][["Pr(>F)"]][1]
  
  # moral
  anova_ses_moral <- aov(ses ~ moral, data = usa_expanded_liwc)
  pvalue_ses_moral <- summary(anova_ses_moral)[[1]][["Pr(>F)"]][1]
  
  # comm
  anova_ses_comm <- aov(ses ~ comm, data = usa_expanded_liwc)
  pvalue_ses_comm <- summary(anova_ses_comm)[[1]][["Pr(>F)"]][1]
  
  # socrefs
  anova_ses_socrefs <- aov(ses ~ socrefs, data = usa_expanded_liwc)
  pvalue_ses_socrefs <- summary(anova_ses_socrefs)[[1]][["Pr(>F)"]][1]
  
  # family
  anova_ses_family <- aov(ses ~ family, data = usa_expanded_liwc)
  pvalue_ses_family <- summary(anova_ses_family)[[1]][["Pr(>F)"]][1]
  
  # friend
  anova_ses_friend <- aov(ses ~ friend, data = usa_expanded_liwc)
  pvalue_ses_friend <- summary(anova_ses_friend)[[1]][["Pr(>F)"]][1]
  
  # female
  anova_ses_female <- aov(ses ~ female, data = usa_expanded_liwc)
  pvalue_ses_female <- summary(anova_ses_female)[[1]][["Pr(>F)"]][1]
  
  # male
  anova_ses_male <- aov(ses ~ male, data = usa_expanded_liwc)
  pvalue_ses_male <- summary(anova_ses_male)[[1]][["Pr(>F)"]][1]
  
  # health
  anova_ses_health <- aov(ses ~ health, data = usa_expanded_liwc)
  pvalue_ses_health <- summary(anova_ses_health)[[1]][["Pr(>F)"]][1]
  
  # illness
  anova_ses_illness <- aov(ses ~ illness, data = usa_expanded_liwc)
  pvalue_ses_illness <- summary(anova_ses_illness)[[1]][["Pr(>F)"]][1]
  
  # wellness
  anova_ses_wellness <- aov(ses ~ wellness, data = usa_expanded_liwc)
  pvalue_ses_wellness <- summary(anova_ses_wellness)[[1]][["Pr(>F)"]][1]
  
  # mental
  anova_ses_mental <- aov(ses ~ mental, data = usa_expanded_liwc)
  pvalue_ses_mental <- summary(anova_ses_mental)[[1]][["Pr(>F)"]][1]
  
  # need
  anova_ses_need <- aov(ses ~ need, data = usa_expanded_liwc)
  pvalue_ses_need <- summary(anova_ses_need)[[1]][["Pr(>F)"]][1]
  
  # want
  anova_ses_want <- aov(ses ~ want, data = usa_expanded_liwc)
  pvalue_ses_want <- summary(anova_ses_want)[[1]][["Pr(>F)"]][1]
  
  # Acquire
  anova_ses_acquire <- aov(ses ~ acquire, data = usa_expanded_liwc)
  pvalue_ses_acquire <- summary(anova_ses_acquire)[[1]][["Pr(>F)"]][1]
  
  # Lack
  anova_ses_lack <- aov(ses ~ lack, data = usa_expanded_liwc)
  pvalue_ses_lack <- summary(anova_ses_lack)[[1]][["Pr(>F)"]][1]
  
  # Fulfill
  anova_ses_fulfill <- aov(ses ~ fulfill, data = usa_expanded_liwc)
  pvalue_ses_fulfill <- summary(anova_ses_fulfill)[[1]][["Pr(>F)"]][1]
  
  # Fatigue
  anova_ses_fatigue <- aov(ses ~ fatigue, data = usa_expanded_liwc)
  pvalue_ses_fatigue <- summary(anova_ses_fatigue)[[1]][["Pr(>F)"]][1]
  
}
}

# assigning significance values
if (A > 1) {

# strength
if (A > 1) {

  # WC
  if (pvalue_strength_WC < 0.001) {
    significance_strength_WC <- 3
  } else if (pvalue_strength_WC < 0.01) {
    significance_strength_WC <- 2
  } else if (pvalue_strength_WC < 0.05) {
    significance_strength_WC <- 1
  } else {
    significance_strength_WC <- 0
  }
  
  # Analytic
  if (pvalue_strength_Analytic < 0.001) {
    significance_strength_Analytic <- 3
  } else if (pvalue_strength_Analytic < 0.01) {
    significance_strength_Analytic <- 2
  } else if (pvalue_strength_Analytic < 0.05) {
    significance_strength_Analytic <- 1
  } else {
    significance_strength_Analytic <- 0
  }
  
  # Clout
  if (pvalue_strength_Clout < 0.001) {
    significance_strength_Clout <- 3
  } else if (pvalue_strength_Clout < 0.01) {
    significance_strength_Clout <- 2
  } else if (pvalue_strength_Clout < 0.05) {
    significance_strength_Clout <- 1
  } else {
    significance_strength_Clout <- 0
  }
  
  # Authentic
  if (pvalue_strength_Authentic < 0.001) {
    significance_strength_Authentic <- 3
  } else if (pvalue_strength_Authentic < 0.01) {
    significance_strength_Authentic <- 2
  } else if (pvalue_strength_Authentic < 0.05) {
    significance_strength_Authentic <- 1
  } else {
    significance_strength_Authentic <- 0
  }
  
  # Tone
  if (pvalue_strength_Tone < 0.001) {
    significance_strength_Tone <- 3
  } else if (pvalue_strength_Tone < 0.01) {
    significance_strength_Tone <- 2
  } else if (pvalue_strength_Tone < 0.05) {
    significance_strength_Tone <- 1
  } else {
    significance_strength_Tone <- 0
  }
  
  # BigWords
  if (pvalue_strength_BigWords < 0.001) {
    significance_strength_BigWords <- 3
  } else if (pvalue_strength_BigWords < 0.01) {
    significance_strength_BigWords <- 2
  } else if (pvalue_strength_BigWords < 0.05) {
    significance_strength_BigWords <- 1
  } else {
    significance_strength_BigWords <- 0
  }
  
  # Dic
  if (pvalue_strength_Dic < 0.001) {
    significance_strength_Dic <- 3
  } else if (pvalue_strength_Dic < 0.01) {
    significance_strength_Dic <- 2
  } else if (pvalue_strength_Dic < 0.05) {
    significance_strength_Dic <- 1
  } else {
    significance_strength_Dic <- 0
  }
  
  # Linguistic
  if (pvalue_strength_Linguistic < 0.001) {
    significance_strength_Linguistic <- 3
  } else if (pvalue_strength_Linguistic < 0.01) {
    significance_strength_Linguistic <- 2
  } else if (pvalue_strength_Linguistic < 0.05) {
    significance_strength_Linguistic <- 1
  } else {
    significance_strength_Linguistic <- 0
  }
  
  # function
  if (pvalue_strength_function < 0.001) {
    significance_strength_function <- 3
  } else if (pvalue_strength_function < 0.01) {
    significance_strength_function <- 2
  } else if (pvalue_strength_function < 0.05) {
    significance_strength_function <- 1
  } else {
    significance_strength_function <- 0
  }
  
  # pronoun
  if (pvalue_strength_pronoun < 0.001) {
    significance_strength_pronoun <- 3
  } else if (pvalue_strength_pronoun < 0.01) {
    significance_strength_pronoun <- 2
  } else if (pvalue_strength_pronoun < 0.05) {
    significance_strength_pronoun <- 1
  } else {
    significance_strength_pronoun <- 0
  }
  
  # ppron
  if (pvalue_strength_ppron < 0.001) {
    significance_strength_ppron <- 3
  } else if (pvalue_strength_ppron < 0.01) {
    significance_strength_ppron <- 2
  } else if (pvalue_strength_ppron < 0.05) {
    significance_strength_ppron <- 1
  } else {
    significance_strength_ppron <- 0
  }
  
  # i
  if (pvalue_strength_i < 0.001) {
    significance_strength_i <- 3
  } else if (pvalue_strength_i < 0.01) {
    significance_strength_i <- 2
  } else if (pvalue_strength_i < 0.05) {
    significance_strength_i <- 1
  } else {
    significance_strength_i <- 0
  }
  
  # we
  if (pvalue_strength_we < 0.001) {
    significance_strength_we <- 3
  } else if (pvalue_strength_we < 0.01) {
    significance_strength_we <- 2
  } else if (pvalue_strength_we < 0.05) {
    significance_strength_we <- 1
  } else {
    significance_strength_we <- 0
  }
  
  # you
  if (pvalue_strength_you < 0.001) {
    significance_strength_you <- 3
  } else if (pvalue_strength_you < 0.01) {
    significance_strength_you <- 2
  } else if (pvalue_strength_you < 0.05) {
    significance_strength_you <- 1
  } else {
    significance_strength_you <- 0
  }
  
  # shehe
  if (pvalue_strength_shehe < 0.001) {
    significance_strength_shehe <- 3
  } else if (pvalue_strength_shehe < 0.01) {
    significance_strength_shehe <- 2
  } else if (pvalue_strength_shehe < 0.05) {
    significance_strength_shehe <- 1
  } else {
    significance_strength_shehe <- 0
  }
  
  
  # they
  if (pvalue_strength_they < 0.001) {
    significance_strength_they <- 3
  } else if (pvalue_strength_they < 0.01) {
    significance_strength_they <- 2
  } else if (pvalue_strength_they < 0.05) {
    significance_strength_they <- 1
  } else {
    significance_strength_they <- 0
  }
  
  # Affect
  if (pvalue_strength_Affect < 0.001) {
    significance_strength_Affect <- 3
  } else if (pvalue_strength_Affect < 0.01) {
    significance_strength_Affect <- 2
  } else if (pvalue_strength_Affect < 0.05) {
    significance_strength_Affect <- 1
  } else {
    significance_strength_Affect <- 0
  }
  
  # tone_pos
  if (pvalue_strength_tone_pos < 0.001) {
    significance_strength_tone_pos <- 3
  } else if (pvalue_strength_tone_pos < 0.01) {
    significance_strength_tone_pos <- 2
  } else if (pvalue_strength_tone_pos < 0.05) {
    significance_strength_tone_pos <- 1
  } else {
    significance_strength_tone_pos <- 0
  }
  
  # tone_neg
  if (pvalue_strength_tone_neg < 0.001) {
    significance_strength_tone_neg <- 3
  } else if (pvalue_strength_tone_neg < 0.01) {
    significance_strength_tone_neg <- 2
  } else if (pvalue_strength_tone_neg < 0.05) {
    significance_strength_tone_neg <- 1
  } else {
    significance_strength_tone_neg <- 0
  }
  
  # emotion
  if (pvalue_strength_emotion < 0.001) {
    significance_strength_emotion <- 3
  } else if (pvalue_strength_emotion < 0.01) {
    significance_strength_emotion <- 2
  } else if (pvalue_strength_emotion < 0.05) {
    significance_strength_emotion <- 1
  } else {
    significance_strength_emotion <- 0
  }
  
  # emo_pos
  if (pvalue_strength_emo_pos < 0.001) {
    significance_strength_emo_pos <- 3
  } else if (pvalue_strength_emo_pos < 0.01) {
    significance_strength_emo_pos <- 2
  } else if (pvalue_strength_emo_pos < 0.05) {
    significance_strength_emo_pos <- 1
  } else {
    significance_strength_emo_pos <- 0
  }
  
  # emo_neg
  if (pvalue_strength_emo_neg < 0.001) {
    significance_strength_emo_neg <- 3
  } else if (pvalue_strength_emo_neg < 0.01) {
    significance_strength_emo_neg <- 2
  } else if (pvalue_strength_emo_neg < 0.05) {
    significance_strength_emo_neg <- 1
  } else {
    significance_strength_emo_neg <- 0
  }
  
  # emo_anx
  if (pvalue_strength_emo_anx < 0.001) {
    significance_strength_emo_anx <- 3
  } else if (pvalue_strength_emo_anx < 0.01) {
    significance_strength_emo_anx <- 2
  } else if (pvalue_strength_emo_anx < 0.05) {
    significance_strength_emo_anx <- 1
  } else {
    significance_strength_emo_anx <- 0
  }
  
  # emo_anger
  if (pvalue_strength_emo_anger < 0.001) {
    significance_strength_emo_anger <- 3
  } else if (pvalue_strength_emo_anger < 0.01) {
    significance_strength_emo_anger <- 2
  } else if (pvalue_strength_emo_anger < 0.05) {
    significance_strength_emo_anger <- 1
  } else {
    significance_strength_emo_anger <- 0
  }
  
  # emo_sad
  if (pvalue_strength_emo_sad < 0.001) {
    significance_strength_emo_sad <- 3
  } else if (pvalue_strength_emo_sad < 0.01) {
    significance_strength_emo_sad <- 2
  } else if (pvalue_strength_emo_sad < 0.05) {
    significance_strength_emo_sad <- 1
  } else {
    significance_strength_emo_sad <- 0
  }
  
  # Social
  if (pvalue_strength_Social < 0.001) {
    significance_strength_Social <- 3
  } else if (pvalue_strength_Social < 0.01) {
    significance_strength_Social <- 2
  } else if (pvalue_strength_Social < 0.05) {
    significance_strength_Social <- 1
  } else {
    significance_strength_Social <- 0
  }
  
  # socbehav
  if (pvalue_strength_socbehav < 0.001) {
    significance_strength_socbehav <- 3
  } else if (pvalue_strength_socbehav < 0.01) {
    significance_strength_socbehav <- 2
  } else if (pvalue_strength_socbehav < 0.05) {
    significance_strength_socbehav <- 1
  } else {
    significance_strength_socbehav <- 0
  }
  
  # prosocial
  if (pvalue_strength_prosocial < 0.001) {
    significance_strength_prosocial <- 3
  } else if (pvalue_strength_prosocial < 0.01) {
    significance_strength_prosocial <- 2
  } else if (pvalue_strength_prosocial < 0.05) {
    significance_strength_prosocial <- 1
  } else {
    significance_strength_prosocial <- 0
  }
  
  # polite
  if (pvalue_strength_polite < 0.001) {
    significance_strength_polite <- 3
  } else if (pvalue_strength_polite < 0.01) {
    significance_strength_polite <- 2
  } else if (pvalue_strength_polite < 0.05) {
    significance_strength_polite <- 1
  } else {
    significance_strength_polite <- 0
  }
  
  # conflict
  if (pvalue_strength_conflict < 0.001) {
    significance_strength_conflict <- 3
  } else if (pvalue_strength_conflict < 0.01) {
    significance_strength_conflict <- 2
  } else if (pvalue_strength_conflict < 0.05) {
    significance_strength_conflict <- 1
  } else {
    significance_strength_conflict <- 0
  }
  
  # moral
  if (pvalue_strength_moral < 0.001) {
    significance_strength_moral <- 3
  } else if (pvalue_strength_moral < 0.01) {
    significance_strength_moral <- 2
  } else if (pvalue_strength_moral < 0.05) {
    significance_strength_moral <- 1
  } else {
    significance_strength_moral <- 0
  }
  
  # comm
  if (pvalue_strength_comm < 0.001) {
    significance_strength_comm <- 3
  } else if (pvalue_strength_comm < 0.01) {
    significance_strength_comm <- 2
  } else if (pvalue_strength_comm < 0.05) {
    significance_strength_comm <- 1
  } else {
    significance_strength_comm <- 0
  }
  
  # socrefs
  if (pvalue_strength_socrefs < 0.001) {
    significance_strength_socrefs <- 3
  } else if (pvalue_strength_socrefs < 0.01) {
    significance_strength_socrefs <- 2
  } else if (pvalue_strength_socrefs < 0.05) {
    significance_strength_socrefs <- 1
  } else {
    significance_strength_socrefs <- 0
  }
  
  # family
  if (pvalue_strength_family < 0.001) {
    significance_strength_family <- 3
  } else if (pvalue_strength_family < 0.01) {
    significance_strength_family <- 2
  } else if (pvalue_strength_family < 0.05) {
    significance_strength_family <- 1
  } else {
    significance_strength_family <- 0
  }
  
  # friend
  if (pvalue_strength_friend < 0.001) {
    significance_strength_friend <- 3
  } else if (pvalue_strength_friend < 0.01) {
    significance_strength_friend <- 2
  } else if (pvalue_strength_friend < 0.05) {
    significance_strength_friend <- 1
  } else {
    significance_strength_friend <- 0
  }
  
  # female
  if (pvalue_strength_female < 0.001) {
    significance_strength_female <- 3
  } else if (pvalue_strength_female < 0.01) {
    significance_strength_female <- 2
  } else if (pvalue_strength_female < 0.05) {
    significance_strength_female <- 1
  } else {
    significance_strength_female <- 0
  }
  
  # male
  if (pvalue_strength_male < 0.001) {
    significance_strength_male <- 3
  } else if (pvalue_strength_male < 0.01) {
    significance_strength_male <- 2
  } else if (pvalue_strength_male < 0.05) {
    significance_strength_male <- 1
  } else {
    significance_strength_male <- 0
  }
  
  # health
  if (pvalue_strength_health < 0.001) {
    significance_strength_health <- 3
  } else if (pvalue_strength_health < 0.01) {
    significance_strength_health <- 2
  } else if (pvalue_strength_health < 0.05) {
    significance_strength_health <- 1
  } else {
    significance_strength_health <- 0
  }
  
  # illness
  if (pvalue_strength_illness < 0.001) {
    significance_strength_illness <- 3
  } else if (pvalue_strength_illness < 0.01) {
    significance_strength_illness <- 2
  } else if (pvalue_strength_illness < 0.05) {
    significance_strength_illness <- 1
  } else {
    significance_strength_illness <- 0
  }
  
  # wellness
  if (pvalue_strength_wellness < 0.001) {
    significance_strength_wellness <- 3
  } else if (pvalue_strength_wellness < 0.01) {
    significance_strength_wellness <- 2
  } else if (pvalue_strength_wellness < 0.05) {
    significance_strength_wellness <- 1
  } else {
    significance_strength_wellness <- 0
  }
  
  # mental
  if (pvalue_strength_mental < 0.001) {
    significance_strength_mental <- 3
  } else if (pvalue_strength_mental < 0.01) {
    significance_strength_mental <- 2
  } else if (pvalue_strength_mental < 0.05) {
    significance_strength_mental <- 1
  } else {
    significance_strength_mental <- 0
  }
  
  # need
  if (pvalue_strength_need < 0.001) {
    significance_strength_need <- 3
  } else if (pvalue_strength_need < 0.01) {
    significance_strength_need <- 2
  } else if (pvalue_strength_need < 0.05) {
    significance_strength_need <- 1
  } else {
    significance_strength_need <- 0
  }
  
  # want
  if (pvalue_strength_want < 0.001) {
    significance_strength_want <- 3
  } else if (pvalue_strength_want < 0.01) {
    significance_strength_want <- 2
  } else if (pvalue_strength_want < 0.05) {
    significance_strength_want <- 1
  } else {
    significance_strength_want <- 0
  }
  
  # acquire
  if (pvalue_strength_acquire < 0.001) {
    significance_strength_acquire <- 3
  } else if (pvalue_strength_acquire < 0.01) {
    significance_strength_acquire <- 2
  } else if (pvalue_strength_acquire < 0.05) {
    significance_strength_acquire <- 1
  } else {
    significance_strength_acquire <- 0
  }
  
  # lack
  if (pvalue_strength_lack < 0.001) {
    significance_strength_lack <- 3
  } else if (pvalue_strength_lack < 0.01) {
    significance_strength_lack <- 2
  } else if (pvalue_strength_lack < 0.05) {
    significance_strength_lack <- 1
  } else {
    significance_strength_lack <- 0
  }
  
  # fulfill
  if (pvalue_strength_fulfill < 0.001) {
    significance_strength_fulfill <- 3
  } else if (pvalue_strength_fulfill < 0.01) {
    significance_strength_fulfill <- 2
  } else if (pvalue_strength_fulfill < 0.05) {
    significance_strength_fulfill <- 1
  } else {
    significance_strength_fulfill <- 0
  }
  
  # fatigue
  if (pvalue_strength_fatigue < 0.001) {
    significance_strength_fatigue <- 3
  } else if (pvalue_strength_fatigue < 0.01) {
    significance_strength_fatigue <- 2
  } else if (pvalue_strength_fatigue < 0.05) {
    significance_strength_fatigue <- 1
  } else {
    significance_strength_fatigue <- 0
  }
  
}

# justified
if (A > 1) {

  # WC
  if (pvalue_justified_WC < 0.001) {
    significance_justified_WC <- 3
  } else if (pvalue_justified_WC < 0.01) {
    significance_justified_WC <- 2
  } else if (pvalue_justified_WC < 0.05) {
    significance_justified_WC <- 1
  } else {
    significance_justified_WC <- 0
  }
  
  # Analytic
  if (pvalue_justified_Analytic < 0.001) {
    significance_justified_Analytic <- 3
  } else if (pvalue_justified_Analytic < 0.01) {
    significance_justified_Analytic <- 2
  } else if (pvalue_justified_Analytic < 0.05) {
    significance_justified_Analytic <- 1
  } else {
    significance_justified_Analytic <- 0
  }
  
  # Clout
  if (pvalue_justified_Clout < 0.001) {
    significance_justified_Clout <- 3
  } else if (pvalue_justified_Clout < 0.01) {
    significance_justified_Clout <- 2
  } else if (pvalue_justified_Clout < 0.05) {
    significance_justified_Clout <- 1
  } else {
    significance_justified_Clout <- 0
  }
  
  # Authentic
  if (pvalue_justified_Authentic < 0.001) {
    significance_justified_Authentic <- 3
  } else if (pvalue_justified_Authentic < 0.01) {
    significance_justified_Authentic <- 2
  } else if (pvalue_justified_Authentic < 0.05) {
    significance_justified_Authentic <- 1
  } else {
    significance_justified_Authentic <- 0
  }
  
  # Tone
  if (pvalue_justified_Tone < 0.001) {
    significance_justified_Tone <- 3
  } else if (pvalue_justified_Tone < 0.01) {
    significance_justified_Tone <- 2
  } else if (pvalue_justified_Tone < 0.05) {
    significance_justified_Tone <- 1
  } else {
    significance_justified_Tone <- 0
  }
  
  # BigWords
  if (pvalue_justified_BigWords < 0.001) {
    significance_justified_BigWords <- 3
  } else if (pvalue_justified_BigWords < 0.01) {
    significance_justified_BigWords <- 2
  } else if (pvalue_justified_BigWords < 0.05) {
    significance_justified_BigWords <- 1
  } else {
    significance_justified_BigWords <- 0
  }
  
  # Dic
  if (pvalue_justified_Dic < 0.001) {
    significance_justified_Dic <- 3
  } else if (pvalue_justified_Dic < 0.01) {
    significance_justified_Dic <- 2
  } else if (pvalue_justified_Dic < 0.05) {
    significance_justified_Dic <- 1
  } else {
    significance_justified_Dic <- 0
  }
  
  # Linguistic
  if (pvalue_justified_Linguistic < 0.001) {
    significance_justified_Linguistic <- 3
  } else if (pvalue_justified_Linguistic < 0.01) {
    significance_justified_Linguistic <- 2
  } else if (pvalue_justified_Linguistic < 0.05) {
    significance_justified_Linguistic <- 1
  } else {
    significance_justified_Linguistic <- 0
  }
  
  # function
  if (pvalue_justified_function < 0.001) {
    significance_justified_function <- 3
  } else if (pvalue_justified_function < 0.01) {
    significance_justified_function <- 2
  } else if (pvalue_justified_function < 0.05) {
    significance_justified_function <- 1
  } else {
    significance_justified_function <- 0
  }
  
  # pronoun
  if (pvalue_justified_pronoun < 0.001) {
    significance_justified_pronoun <- 3
  } else if (pvalue_justified_pronoun < 0.01) {
    significance_justified_pronoun <- 2
  } else if (pvalue_justified_pronoun < 0.05) {
    significance_justified_pronoun <- 1
  } else {
    significance_justified_pronoun <- 0
  }
  
  # ppron
  if (pvalue_justified_ppron < 0.001) {
    significance_justified_ppron <- 3
  } else if (pvalue_justified_ppron < 0.01) {
    significance_justified_ppron <- 2
  } else if (pvalue_justified_ppron < 0.05) {
    significance_justified_ppron <- 1
  } else {
    significance_justified_ppron <- 0
  }
  
  # i
  if (pvalue_justified_i < 0.001) {
    significance_justified_i <- 3
  } else if (pvalue_justified_i < 0.01) {
    significance_justified_i <- 2
  } else if (pvalue_justified_i < 0.05) {
    significance_justified_i <- 1
  } else {
    significance_justified_i <- 0
  }
  
  # we
  if (pvalue_justified_we < 0.001) {
    significance_justified_we <- 3
  } else if (pvalue_justified_we < 0.01) {
    significance_justified_we <- 2
  } else if (pvalue_justified_we < 0.05) {
    significance_justified_we <- 1
  } else {
    significance_justified_we <- 0
  }
  
  # you
  if (pvalue_justified_you < 0.001) {
    significance_justified_you <- 3
  } else if (pvalue_justified_you < 0.01) {
    significance_justified_you <- 2
  } else if (pvalue_justified_you < 0.05) {
    significance_justified_you <- 1
  } else {
    significance_justified_you <- 0
  }
  
  # shehe
  if (pvalue_justified_shehe < 0.001) {
    significance_justified_shehe <- 3
  } else if (pvalue_justified_shehe < 0.01) {
    significance_justified_shehe <- 2
  } else if (pvalue_justified_shehe < 0.05) {
    significance_justified_shehe <- 1
  } else {
    significance_justified_shehe <- 0
  }
  
  # they
  if (pvalue_justified_they < 0.001) {
    significance_justified_they <- 3
  } else if (pvalue_justified_they < 0.01) {
    significance_justified_they <- 2
  } else if (pvalue_justified_they < 0.05) {
    significance_justified_they <- 1
  } else {
    significance_justified_they <- 0
  }
  
  # Affect
  if (pvalue_justified_Affect < 0.001) {
    significance_justified_Affect <- 3
  } else if (pvalue_justified_Affect < 0.01) {
    significance_justified_Affect <- 2
  } else if (pvalue_justified_Affect < 0.05) {
    significance_justified_Affect <- 1
  } else {
    significance_justified_Affect <- 0
  }
  
  # tone_pos
  if (pvalue_justified_tone_pos < 0.001) {
    significance_justified_tone_pos <- 3
  } else if (pvalue_justified_tone_pos < 0.01) {
    significance_justified_tone_pos <- 2
  } else if (pvalue_justified_tone_pos < 0.05) {
    significance_justified_tone_pos <- 1
  } else {
    significance_justified_tone_pos <- 0
  }
  
  # tone_neg
  if (pvalue_justified_tone_neg < 0.001) {
    significance_justified_tone_neg <- 3
  } else if (pvalue_justified_tone_neg < 0.01) {
    significance_justified_tone_neg <- 2
  } else if (pvalue_justified_tone_neg < 0.05) {
    significance_justified_tone_neg <- 1
  } else {
    significance_justified_tone_neg <- 0
  }
  
  # emotion
  if (pvalue_justified_emotion < 0.001) {
    significance_justified_emotion <- 3
  } else if (pvalue_justified_emotion < 0.01) {
    significance_justified_emotion <- 2
  } else if (pvalue_justified_emotion < 0.05) {
    significance_justified_emotion <- 1
  } else {
    significance_justified_emotion <- 0
  }
  
  # emo_pos
  if (pvalue_justified_emo_pos < 0.001) {
    significance_justified_emo_pos <- 3
  } else if (pvalue_justified_emo_pos < 0.01) {
    significance_justified_emo_pos <- 2
  } else if (pvalue_justified_emo_pos < 0.05) {
    significance_justified_emo_pos <- 1
  } else {
    significance_justified_emo_pos <- 0
  }
  
  # emo_neg
  if (pvalue_justified_emo_neg < 0.001) {
    significance_justified_emo_neg <- 3
  } else if (pvalue_justified_emo_neg < 0.01) {
    significance_justified_emo_neg <- 2
  } else if (pvalue_justified_emo_neg < 0.05) {
    significance_justified_emo_neg <- 1
  } else {
    significance_justified_emo_neg <- 0
  }
  
  # emo_anx
  if (pvalue_justified_emo_anx < 0.001) {
    significance_justified_emo_anx <- 3
  } else if (pvalue_justified_emo_anx < 0.01) {
    significance_justified_emo_anx <- 2
  } else if (pvalue_justified_emo_anx < 0.05) {
    significance_justified_emo_anx <- 1
  } else {
    significance_justified_emo_anx <- 0
  }
  
  # emo_anger
  if (pvalue_justified_emo_anger < 0.001) {
    significance_justified_emo_anger <- 3
  } else if (pvalue_justified_emo_anger < 0.01) {
    significance_justified_emo_anger <- 2
  } else if (pvalue_justified_emo_anger < 0.05) {
    significance_justified_emo_anger <- 1
  } else {
    significance_justified_emo_anger <- 0
  }
  
  # emo_sad
  if (pvalue_justified_emo_sad < 0.001) {
    significance_justified_emo_sad <- 3
  } else if (pvalue_justified_emo_sad < 0.01) {
    significance_justified_emo_sad <- 2
  } else if (pvalue_justified_emo_sad < 0.05) {
    significance_justified_emo_sad <- 1
  } else {
    significance_justified_emo_sad <- 0
  }
  
  # Social
  if (pvalue_justified_Social < 0.001) {
    significance_justified_Social <- 3
  } else if (pvalue_justified_Social < 0.01) {
    significance_justified_Social <- 2
  } else if (pvalue_justified_Social < 0.05) {
    significance_justified_Social <- 1
  } else {
    significance_justified_Social <- 0
  }
  
  # socbehav
  if (pvalue_justified_socbehav < 0.001) {
    significance_justified_socbehav <- 3
  } else if (pvalue_justified_socbehav < 0.01) {
    significance_justified_socbehav <- 2
  } else if (pvalue_justified_socbehav < 0.05) {
    significance_justified_socbehav <- 1
  } else {
    significance_justified_socbehav <- 0
  }
  
  # prosocial
  if (pvalue_justified_prosocial < 0.001) {
    significance_justified_prosocial <- 3
  } else if (pvalue_justified_prosocial < 0.01) {
    significance_justified_prosocial <- 2
  } else if (pvalue_justified_prosocial < 0.05) {
    significance_justified_prosocial <- 1
  } else {
    significance_justified_prosocial <- 0
  }
  
  # polite
  if (pvalue_justified_polite < 0.001) {
    significance_justified_polite <- 3
  } else if (pvalue_justified_polite < 0.01) {
    significance_justified_polite <- 2
  } else if (pvalue_justified_polite < 0.05) {
    significance_justified_polite <- 1
  } else {
    significance_justified_polite <- 0
  }
  
  # conflict
  if (pvalue_justified_conflict < 0.001) {
    significance_justified_conflict <- 3
  } else if (pvalue_justified_conflict < 0.01) {
    significance_justified_conflict <- 2
  } else if (pvalue_justified_conflict < 0.05) {
    significance_justified_conflict <- 1
  } else {
    significance_justified_conflict <- 0
  }
  
  # moral
  if (pvalue_justified_moral < 0.001) {
    significance_justified_moral <- 3
  } else if (pvalue_justified_moral < 0.01) {
    significance_justified_moral <- 2
  } else if (pvalue_justified_moral < 0.05) {
    significance_justified_moral <- 1
  } else {
    significance_justified_moral <- 0
  }
  
  # comm
  if (pvalue_justified_comm < 0.001) {
    significance_justified_comm <- 3
  } else if (pvalue_justified_comm < 0.01) {
    significance_justified_comm <- 2
  } else if (pvalue_justified_comm < 0.05) {
    significance_justified_comm <- 1
  } else {
    significance_justified_comm <- 0
  }
  
  # socrefs
  if (pvalue_justified_socrefs < 0.001) {
    significance_justified_socrefs <- 3
  } else if (pvalue_justified_socrefs < 0.01) {
    significance_justified_socrefs <- 2
  } else if (pvalue_justified_socrefs < 0.05) {
    significance_justified_socrefs <- 1
  } else {
    significance_justified_socrefs <- 0
  }
  
  # family
  if (pvalue_justified_family < 0.001) {
    significance_justified_family <- 3
  } else if (pvalue_justified_family < 0.01) {
    significance_justified_family <- 2
  } else if (pvalue_justified_family < 0.05) {
    significance_justified_family <- 1
  } else {
    significance_justified_family <- 0
  }
  
  # friend
  if (pvalue_justified_friend < 0.001) {
    significance_justified_friend <- 3
  } else if (pvalue_justified_friend < 0.01) {
    significance_justified_friend <- 2
  } else if (pvalue_justified_friend < 0.05) {
    significance_justified_friend <- 1
  } else {
    significance_justified_friend <- 0
  }
  
  # female
  if (pvalue_justified_female < 0.001) {
    significance_justified_female <- 3
  } else if (pvalue_justified_female < 0.01) {
    significance_justified_female <- 2
  } else if (pvalue_justified_female < 0.05) {
    significance_justified_female <- 1
  } else {
    significance_justified_female <- 0
  }
  
  # male
  if (pvalue_justified_male < 0.001) {
    significance_justified_male <- 3
  } else if (pvalue_justified_male < 0.01) {
    significance_justified_male <- 2
  } else if (pvalue_justified_male < 0.05) {
    significance_justified_male <- 1
  } else {
    significance_justified_male <- 0
  }
  
  # health
  if (pvalue_justified_health < 0.001) {
    significance_justified_health <- 3
  } else if (pvalue_justified_health < 0.01) {
    significance_justified_health <- 2
  } else if (pvalue_justified_health < 0.05) {
    significance_justified_health <- 1
  } else {
    significance_justified_health <- 0
  }
  
  # illness
  if (pvalue_justified_illness < 0.001) {
    significance_justified_illness <- 3
  } else if (pvalue_justified_illness < 0.01) {
    significance_justified_illness <- 2
  } else if (pvalue_justified_illness < 0.05) {
    significance_justified_illness <- 1
  } else {
    significance_justified_illness <- 0
  }
  
  # wellness
  if (pvalue_justified_wellness < 0.001) {
    significance_justified_wellness <- 3
  } else if (pvalue_justified_wellness < 0.01) {
    significance_justified_wellness <- 2
  } else if (pvalue_justified_wellness < 0.05) {
    significance_justified_wellness <- 1
  } else {
    significance_justified_wellness <- 0
  }
  
  # mental
  if (pvalue_justified_mental < 0.001) {
    significance_justified_mental <- 3
  } else if (pvalue_justified_mental < 0.01) {
    significance_justified_mental <- 2
  } else if (pvalue_justified_mental < 0.05) {
    significance_justified_mental <- 1
  } else {
    significance_justified_mental <- 0
  }
  
  # need
  if (pvalue_justified_need < 0.001) {
    significance_justified_need <- 3
  } else if (pvalue_justified_need < 0.01) {
    significance_justified_need <- 2
  } else if (pvalue_justified_need < 0.05) {
    significance_justified_need <- 1
  } else {
    significance_justified_need <- 0
  }
  
  # want
  if (pvalue_justified_want < 0.001) {
    significance_justified_want <- 3
  } else if (pvalue_justified_want < 0.01) {
    significance_justified_want <- 2
  } else if (pvalue_justified_want < 0.05) {
    significance_justified_want <- 1
  } else {
    significance_justified_want <- 0
  }
  
  # acquire
  if (pvalue_justified_acquire < 0.001) {
    significance_justified_acquire <- 3
  } else if (pvalue_justified_acquire < 0.01) {
    significance_justified_acquire <- 2
  } else if (pvalue_justified_acquire < 0.05) {
    significance_justified_acquire <- 1
  } else {
    significance_justified_acquire <- 0
  }
  
  # lack
  if (pvalue_justified_lack < 0.001) {
    significance_justified_lack <- 3
  } else if (pvalue_justified_lack < 0.01) {
    significance_justified_lack <- 2
  } else if (pvalue_justified_lack < 0.05) {
    significance_justified_lack <- 1
  } else {
    significance_justified_lack <- 0
  }
  
  # fulfill
  if (pvalue_justified_fulfill < 0.001) {
    significance_justified_fulfill <- 3
  } else if (pvalue_justified_fulfill < 0.01) {
    significance_justified_fulfill <- 2
  } else if (pvalue_justified_fulfill < 0.05) {
    significance_justified_fulfill <- 1
  } else {
    significance_justified_fulfill <- 0
  }
  
  # fatigue
  if (pvalue_justified_fatigue < 0.001) {
    significance_justified_fatigue <- 3
  } else if (pvalue_justified_fatigue < 0.01) {
    significance_justified_fatigue <- 2
  } else if (pvalue_justified_fatigue < 0.05) {
    significance_justified_fatigue <- 1
  } else {
    significance_justified_fatigue <- 0
  }
  
}

# guilt
if (A > 1) {
  # WC
  if (pvalue_guilt_WC < 0.001) {
    significance_guilt_WC <- 3
  } else if (pvalue_guilt_WC < 0.01) {
    significance_guilt_WC <- 2
  } else if (pvalue_guilt_WC < 0.05) {
    significance_guilt_WC <- 1
  } else {
    significance_guilt_WC <- 0
  }
  
  # Analytic
  if (pvalue_guilt_Analytic < 0.001) {
    significance_guilt_Analytic <- 3
  } else if (pvalue_guilt_Analytic < 0.01) {
    significance_guilt_Analytic <- 2
  } else if (pvalue_guilt_Analytic < 0.05) {
    significance_guilt_Analytic <- 1
  } else {
    significance_guilt_Analytic <- 0
  }
  
  # Clout
  if (pvalue_guilt_Clout < 0.001) {
    significance_guilt_Clout <- 3
  } else if (pvalue_guilt_Clout < 0.01) {
    significance_guilt_Clout <- 2
  } else if (pvalue_guilt_Clout < 0.05) {
    significance_guilt_Clout <- 1
  } else {
    significance_guilt_Clout <- 0
  }
  
  # Authentic
  if (pvalue_guilt_Authentic < 0.001) {
    significance_guilt_Authentic <- 3
  } else if (pvalue_guilt_Authentic < 0.01) {
    significance_guilt_Authentic <- 2
  } else if (pvalue_guilt_Authentic < 0.05) {
    significance_guilt_Authentic <- 1
  } else {
    significance_guilt_Authentic <- 0
  }
  
  # Tone
  if (pvalue_guilt_Tone < 0.001) {
    significance_guilt_Tone <- 3
  } else if (pvalue_guilt_Tone < 0.01) {
    significance_guilt_Tone <- 2
  } else if (pvalue_guilt_Tone < 0.05) {
    significance_guilt_Tone <- 1
  } else {
    significance_guilt_Tone <- 0
  }
  
  # BigWords
  if (pvalue_guilt_BigWords < 0.001) {
    significance_guilt_BigWords <- 3
  } else if (pvalue_guilt_BigWords < 0.01) {
    significance_guilt_BigWords <- 2
  } else if (pvalue_guilt_BigWords < 0.05) {
    significance_guilt_BigWords <- 1
  } else {
    significance_guilt_BigWords <- 0
  }
  
  # Dic
  if (pvalue_guilt_Dic < 0.001) {
    significance_guilt_Dic <- 3
  } else if (pvalue_guilt_Dic < 0.01) {
    significance_guilt_Dic <- 2
  } else if (pvalue_guilt_Dic < 0.05) {
    significance_guilt_Dic <- 1
  } else {
    significance_guilt_Dic <- 0
  }
  
  # Linguistic
  if (pvalue_guilt_Linguistic < 0.001) {
    significance_guilt_Linguistic <- 3
  } else if (pvalue_guilt_Linguistic < 0.01) {
    significance_guilt_Linguistic <- 2
  } else if (pvalue_guilt_Linguistic < 0.05) {
    significance_guilt_Linguistic <- 1
  } else {
    significance_guilt_Linguistic <- 0
  }
  
  # function
  if (pvalue_guilt_function < 0.001) {
    significance_guilt_function <- 3
  } else if (pvalue_guilt_function < 0.01) {
    significance_guilt_function <- 2
  } else if (pvalue_guilt_function < 0.05) {
    significance_guilt_function <- 1
  } else {
    significance_guilt_function <- 0
  }
  
  # pronoun
  if (pvalue_guilt_pronoun < 0.001) {
    significance_guilt_pronoun <- 3
  } else if (pvalue_guilt_pronoun < 0.01) {
    significance_guilt_pronoun <- 2
  } else if (pvalue_guilt_pronoun < 0.05) {
    significance_guilt_pronoun <- 1
  } else {
    significance_guilt_pronoun <- 0
  }
  
  # ppron
  if (pvalue_guilt_ppron < 0.001) {
    significance_guilt_ppron <- 3
  } else if (pvalue_guilt_ppron < 0.01) {
    significance_guilt_ppron <- 2
  } else if (pvalue_guilt_ppron < 0.05) {
    significance_guilt_ppron <- 1
  } else {
    significance_guilt_ppron <- 0
  }
  
  # i
  if (pvalue_guilt_i < 0.001) {
    significance_guilt_i <- 3
  } else if (pvalue_guilt_i < 0.01) {
    significance_guilt_i <- 2
  } else if (pvalue_guilt_i < 0.05) {
    significance_guilt_i <- 1
  } else {
    significance_guilt_i <- 0
  }
  
  # we
  if (pvalue_guilt_we < 0.001) {
    significance_guilt_we <- 3
  } else if (pvalue_guilt_we < 0.01) {
    significance_guilt_we <- 2
  } else if (pvalue_guilt_we < 0.05) {
    significance_guilt_we <- 1
  } else {
    significance_guilt_we <- 0
  }
  
  # you
  if (pvalue_guilt_you < 0.001) {
    significance_guilt_you <- 3
  } else if (pvalue_guilt_you < 0.01) {
    significance_guilt_you <- 2
  } else if (pvalue_guilt_you < 0.05) {
    significance_guilt_you <- 1
  } else {
    significance_guilt_you <- 0
  }
  
  # shehe
  if (pvalue_guilt_shehe < 0.001) {
    significance_guilt_shehe <- 3
  } else if (pvalue_guilt_shehe < 0.01) {
    significance_guilt_shehe <- 2
  } else if (pvalue_guilt_shehe < 0.05) {
    significance_guilt_shehe <- 1
  } else {
    significance_guilt_shehe <- 0
  }
  
  # they
  if (pvalue_guilt_they < 0.001) {
    significance_guilt_they <- 3
  } else if (pvalue_guilt_they < 0.01) {
    significance_guilt_they <- 2
  } else if (pvalue_guilt_they < 0.05) {
    significance_guilt_they <- 1
  } else {
    significance_guilt_they <- 0
  }
  
  # Affect
  if (pvalue_guilt_Affect < 0.001) {
    significance_guilt_Affect <- 3
  } else if (pvalue_guilt_Affect < 0.01) {
    significance_guilt_Affect <- 2
  } else if (pvalue_guilt_Affect < 0.05) {
    significance_guilt_Affect <- 1
  } else {
    significance_guilt_Affect <- 0
  }
  
  # tone_pos
  if (pvalue_guilt_tone_pos < 0.001) {
    significance_guilt_tone_pos <- 3
  } else if (pvalue_guilt_tone_pos < 0.01) {
    significance_guilt_tone_pos <- 2
  } else if (pvalue_guilt_tone_pos < 0.05) {
    significance_guilt_tone_pos <- 1
  } else {
    significance_guilt_tone_pos <- 0
  }
  
  # tone_neg
  if (pvalue_guilt_tone_neg < 0.001) {
    significance_guilt_tone_neg <- 3
  } else if (pvalue_guilt_tone_neg < 0.01) {
    significance_guilt_tone_neg <- 2
  } else if (pvalue_guilt_tone_neg < 0.05) {
    significance_guilt_tone_neg <- 1
  } else {
    significance_guilt_tone_neg <- 0
  }
  
  # emotion
  if (pvalue_guilt_emotion < 0.001) {
    significance_guilt_emotion <- 3
  } else if (pvalue_guilt_emotion < 0.01) {
    significance_guilt_emotion <- 2
  } else if (pvalue_guilt_emotion < 0.05) {
    significance_guilt_emotion <- 1
  } else {
    significance_guilt_emotion <- 0
  }
  
  # emo_pos
  if (pvalue_guilt_emo_pos < 0.001) {
    significance_guilt_emo_pos <- 3
  } else if (pvalue_guilt_emo_pos < 0.01) {
    significance_guilt_emo_pos <- 2
  } else if (pvalue_guilt_emo_pos < 0.05) {
    significance_guilt_emo_pos <- 1
  } else {
    significance_guilt_emo_pos <- 0
  }
  
  # emo_neg
  if (pvalue_guilt_emo_neg < 0.001) {
    significance_guilt_emo_neg <- 3
  } else if (pvalue_guilt_emo_neg < 0.01) {
    significance_guilt_emo_neg <- 2
  } else if (pvalue_guilt_emo_neg < 0.05) {
    significance_guilt_emo_neg <- 1
  } else {
    significance_guilt_emo_neg <- 0
  }
  
  # emo_anx
  if (pvalue_guilt_emo_anx < 0.001) {
    significance_guilt_emo_anx <- 3
  } else if (pvalue_guilt_emo_anx < 0.01) {
    significance_guilt_emo_anx <- 2
  } else if (pvalue_guilt_emo_anx < 0.05) {
    significance_guilt_emo_anx <- 1
  } else {
    significance_guilt_emo_anx <- 0
  }
  
  # emo_anger
  if (pvalue_guilt_emo_anger < 0.001) {
    significance_guilt_emo_anger <- 3
  } else if (pvalue_guilt_emo_anger < 0.01) {
    significance_guilt_emo_anger <- 2
  } else if (pvalue_guilt_emo_anger < 0.05) {
    significance_guilt_emo_anger <- 1
  } else {
    significance_guilt_emo_anger <- 0
  }
  
  # emo_sad
  if (pvalue_guilt_emo_sad < 0.001) {
    significance_guilt_emo_sad <- 3
  } else if (pvalue_guilt_emo_sad < 0.01) {
    significance_guilt_emo_sad <- 2
  } else if (pvalue_guilt_emo_sad < 0.05) {
    significance_guilt_emo_sad <- 1
  } else {
    significance_guilt_emo_sad <- 0
  }
  
  # Social
  if (pvalue_guilt_Social < 0.001) {
    significance_guilt_Social <- 3
  } else if (pvalue_guilt_Social < 0.01) {
    significance_guilt_Social <- 2
  } else if (pvalue_guilt_Social < 0.05) {
    significance_guilt_Social <- 1
  } else {
    significance_guilt_Social <- 0
  }
  
  # socbehav
  if (pvalue_guilt_socbehav < 0.001) {
    significance_guilt_socbehav <- 3
  } else if (pvalue_guilt_socbehav < 0.01) {
    significance_guilt_socbehav <- 2
  } else if (pvalue_guilt_socbehav < 0.05) {
    significance_guilt_socbehav <- 1
  } else {
    significance_guilt_socbehav <- 0
  }
  
  # prosocial
  if (pvalue_guilt_prosocial < 0.001) {
    significance_guilt_prosocial <- 3
  } else if (pvalue_guilt_prosocial < 0.01) {
    significance_guilt_prosocial <- 2
  } else if (pvalue_guilt_prosocial < 0.05) {
    significance_guilt_prosocial <- 1
  } else {
    significance_guilt_prosocial <- 0
  }
  
  # polite
  if (pvalue_guilt_polite < 0.001) {
    significance_guilt_polite <- 3
  } else if (pvalue_guilt_polite < 0.01) {
    significance_guilt_polite <- 2
  } else if (pvalue_guilt_polite < 0.05) {
    significance_guilt_polite <- 1
  } else {
    significance_guilt_polite <- 0
  }
  
  # conflict
  if (pvalue_guilt_conflict < 0.001) {
    significance_guilt_conflict <- 3
  } else if (pvalue_guilt_conflict < 0.01) {
    significance_guilt_conflict <- 2
  } else if (pvalue_guilt_conflict < 0.05) {
    significance_guilt_conflict <- 1
  } else {
    significance_guilt_conflict <- 0
  }
  
  # moral
  if (pvalue_guilt_moral < 0.001) {
    significance_guilt_moral <- 3
  } else if (pvalue_guilt_moral < 0.01) {
    significance_guilt_moral <- 2
  } else if (pvalue_guilt_moral < 0.05) {
    significance_guilt_moral <- 1
  } else {
    significance_guilt_moral <- 0
  }
  
  # comm
  if (pvalue_guilt_comm < 0.001) {
    significance_guilt_comm <- 3
  } else if (pvalue_guilt_comm < 0.01) {
    significance_guilt_comm <- 2
  } else if (pvalue_guilt_comm < 0.05) {
    significance_guilt_comm <- 1
  } else {
    significance_guilt_comm <- 0
  }
  
  # socrefs
  if (pvalue_guilt_socrefs < 0.001) {
    significance_guilt_socrefs <- 3
  } else if (pvalue_guilt_socrefs < 0.01) {
    significance_guilt_socrefs <- 2
  } else if (pvalue_guilt_socrefs < 0.05) {
    significance_guilt_socrefs <- 1
  } else {
    significance_guilt_socrefs <- 0
  }
  
  # family
  if (pvalue_guilt_family < 0.001) {
    significance_guilt_family <- 3
  } else if (pvalue_guilt_family < 0.01) {
    significance_guilt_family <- 2
  } else if (pvalue_guilt_family < 0.05) {
    significance_guilt_family <- 1
  } else {
    significance_guilt_family <- 0
  }
  
  # friend
  if (pvalue_guilt_friend < 0.001) {
    significance_guilt_friend <- 3
  } else if (pvalue_guilt_friend < 0.01) {
    significance_guilt_friend <- 2
  } else if (pvalue_guilt_friend < 0.05) {
    significance_guilt_friend <- 1
  } else {
    significance_guilt_friend <- 0
  }
  
  # female
  if (pvalue_guilt_female < 0.001) {
    significance_guilt_female <- 3
  } else if (pvalue_guilt_female < 0.01) {
    significance_guilt_female <- 2
  } else if (pvalue_guilt_female < 0.05) {
    significance_guilt_female <- 1
  } else {
    significance_guilt_female <- 0
  }
  
  # male
  if (pvalue_guilt_male < 0.001) {
    significance_guilt_male <- 3
  } else if (pvalue_guilt_male < 0.01) {
    significance_guilt_male <- 2
  } else if (pvalue_guilt_male < 0.05) {
    significance_guilt_male <- 1
  } else {
    significance_guilt_male <- 0
  }
  
  # health
  if (pvalue_guilt_health < 0.001) {
    significance_guilt_health <- 3
  } else if (pvalue_guilt_health < 0.01) {
    significance_guilt_health <- 2
  } else if (pvalue_guilt_health < 0.05) {
    significance_guilt_health <- 1
  } else {
    significance_guilt_health <- 0
  }
  
  # illness
  if (pvalue_guilt_illness < 0.001) {
    significance_guilt_illness <- 3
  } else if (pvalue_guilt_illness < 0.01) {
    significance_guilt_illness <- 2
  } else if (pvalue_guilt_illness < 0.05) {
    significance_guilt_illness <- 1
  } else {
    significance_guilt_illness <- 0
  }
  
  # wellness
  if (pvalue_guilt_wellness < 0.001) {
    significance_guilt_wellness <- 3
  } else if (pvalue_guilt_wellness < 0.01) {
    significance_guilt_wellness <- 2
  } else if (pvalue_guilt_wellness < 0.05) {
    significance_guilt_wellness <- 1
  } else {
    significance_guilt_wellness <- 0
  }
  
  # mental
  if (pvalue_guilt_mental < 0.001) {
    significance_guilt_mental <- 3
  } else if (pvalue_guilt_mental < 0.01) {
    significance_guilt_mental <- 2
  } else if (pvalue_guilt_mental < 0.05) {
    significance_guilt_mental <- 1
  } else {
    significance_guilt_mental <- 0
  }
  
  # need
  if (pvalue_guilt_need < 0.001) {
    significance_guilt_need <- 3
  } else if (pvalue_guilt_need < 0.01) {
    significance_guilt_need <- 2
  } else if (pvalue_guilt_need < 0.05) {
    significance_guilt_need <- 1
  } else {
    significance_guilt_need <- 0
  }
  
  # want
  if (pvalue_guilt_want < 0.001) {
    significance_guilt_want <- 3
  } else if (pvalue_guilt_want < 0.01) {
    significance_guilt_want <- 2
  } else if (pvalue_guilt_want < 0.05) {
    significance_guilt_want <- 1
  } else {
    significance_guilt_want <- 0
  }
  
  # acquire
  if (pvalue_guilt_acquire < 0.001) {
    significance_guilt_acquire <- 3
  } else if (pvalue_guilt_acquire < 0.01) {
    significance_guilt_acquire <- 2
  } else if (pvalue_guilt_acquire < 0.05) {
    significance_guilt_acquire <- 1
  } else {
    significance_guilt_acquire <- 0
  }
  
  # lack
  if (pvalue_guilt_lack < 0.001) {
    significance_guilt_lack <- 3
  } else if (pvalue_guilt_lack < 0.01) {
    significance_guilt_lack <- 2
  } else if (pvalue_guilt_lack < 0.05) {
    significance_guilt_lack <- 1
  } else {
    significance_guilt_lack <- 0
  }
  
  # fulfill
  if (pvalue_guilt_fulfill < 0.001) {
    significance_guilt_fulfill <- 3
  } else if (pvalue_guilt_fulfill < 0.01) {
    significance_guilt_fulfill <- 2
  } else if (pvalue_guilt_fulfill < 0.05) {
    significance_guilt_fulfill <- 1
  } else {
    significance_guilt_fulfill <- 0
  }
  
  # fatigue
  if (pvalue_guilt_fatigue < 0.001) {
    significance_guilt_fatigue <- 3
  } else if (pvalue_guilt_fatigue < 0.01) {
    significance_guilt_fatigue <- 2
  } else if (pvalue_guilt_fatigue < 0.05) {
    significance_guilt_fatigue <- 1
  } else {
    significance_guilt_fatigue <- 0
  }
  
}

# depressed
if (A > 1) {
  # WC
  if (pvalue_depressed_WC < 0.001) {
    significance_depressed_WC <- 3
  } else if (pvalue_depressed_WC < 0.01) {
    significance_depressed_WC <- 2
  } else if (pvalue_depressed_WC < 0.05) {
    significance_depressed_WC <- 1
  } else {
    significance_depressed_WC <- 0
  }
  
  # Analytic
  if (pvalue_depressed_Analytic < 0.001) {
    significance_depressed_Analytic <- 3
  } else if (pvalue_depressed_Analytic < 0.01) {
    significance_depressed_Analytic <- 2
  } else if (pvalue_depressed_Analytic < 0.05) {
    significance_depressed_Analytic <- 1
  } else {
    significance_depressed_Analytic <- 0
  }
  
  # Clout
  if (pvalue_depressed_Clout < 0.001) {
    significance_depressed_Clout <- 3
  } else if (pvalue_depressed_Clout < 0.01) {
    significance_depressed_Clout <- 2
  } else if (pvalue_depressed_Clout < 0.05) {
    significance_depressed_Clout <- 1
  } else {
    significance_depressed_Clout <- 0
  }
  
  # Authentic
  if (pvalue_depressed_Authentic < 0.001) {
    significance_depressed_Authentic <- 3
  } else if (pvalue_depressed_Authentic < 0.01) {
    significance_depressed_Authentic <- 2
  } else if (pvalue_depressed_Authentic < 0.05) {
    significance_depressed_Authentic <- 1
  } else {
    significance_depressed_Authentic <- 0
  }
  
  # Tone
  if (pvalue_depressed_Tone < 0.001) {
    significance_depressed_Tone <- 3
  } else if (pvalue_depressed_Tone < 0.01) {
    significance_depressed_Tone <- 2
  } else if (pvalue_depressed_Tone < 0.05) {
    significance_depressed_Tone <- 1
  } else {
    significance_depressed_Tone <- 0
  }
  
  # BigWords
  if (pvalue_depressed_BigWords < 0.001) {
    significance_depressed_BigWords <- 3
  } else if (pvalue_depressed_BigWords < 0.01) {
    significance_depressed_BigWords <- 2
  } else if (pvalue_depressed_BigWords < 0.05) {
    significance_depressed_BigWords <- 1
  } else {
    significance_depressed_BigWords <- 0
  }
  
  # Dic
  if (pvalue_depressed_Dic < 0.001) {
    significance_depressed_Dic <- 3
  } else if (pvalue_depressed_Dic < 0.01) {
    significance_depressed_Dic <- 2
  } else if (pvalue_depressed_Dic < 0.05) {
    significance_depressed_Dic <- 1
  } else {
    significance_depressed_Dic <- 0
  }
  
  # Linguistic
  if (pvalue_depressed_Linguistic < 0.001) {
    significance_depressed_Linguistic <- 3
  } else if (pvalue_depressed_Linguistic < 0.01) {
    significance_depressed_Linguistic <- 2
  } else if (pvalue_depressed_Linguistic < 0.05) {
    significance_depressed_Linguistic <- 1
  } else {
    significance_depressed_Linguistic <- 0
  }
  
  # function
  if (pvalue_depressed_function < 0.001) {
    significance_depressed_function <- 3
  } else if (pvalue_depressed_function < 0.01) {
    significance_depressed_function <- 2
  } else if (pvalue_depressed_function < 0.05) {
    significance_depressed_function <- 1
  } else {
    significance_depressed_function <- 0
  }
  
  # pronoun
  if (pvalue_depressed_pronoun < 0.001) {
    significance_depressed_pronoun <- 3
  } else if (pvalue_depressed_pronoun < 0.01) {
    significance_depressed_pronoun <- 2
  } else if (pvalue_depressed_pronoun < 0.05) {
    significance_depressed_pronoun <- 1
  } else {
    significance_depressed_pronoun <- 0
  }
  
  # ppron
  if (pvalue_depressed_ppron < 0.001) {
    significance_depressed_ppron <- 3
  } else if (pvalue_depressed_ppron < 0.01) {
    significance_depressed_ppron <- 2
  } else if (pvalue_depressed_ppron < 0.05) {
    significance_depressed_ppron <- 1
  } else {
    significance_depressed_ppron <- 0
  }
  
  # i
  if (pvalue_depressed_i < 0.001) {
    significance_depressed_i <- 3
  } else if (pvalue_depressed_i < 0.01) {
    significance_depressed_i <- 2
  } else if (pvalue_depressed_i < 0.05) {
    significance_depressed_i <- 1
  } else {
    significance_depressed_i <- 0
  }
  
  # we
  if (pvalue_depressed_we < 0.001) {
    significance_depressed_we <- 3
  } else if (pvalue_depressed_we < 0.01) {
    significance_depressed_we <- 2
  } else if (pvalue_depressed_we < 0.05) {
    significance_depressed_we <- 1
  } else {
    significance_depressed_we <- 0
  }
  
  # you
  if (pvalue_depressed_you < 0.001) {
    significance_depressed_you <- 3
  } else if (pvalue_depressed_you < 0.01) {
    significance_depressed_you <- 2
  } else if (pvalue_depressed_you < 0.05) {
    significance_depressed_you <- 1
  } else {
    significance_depressed_you <- 0
  }
  
  # shehe
  if (pvalue_depressed_shehe < 0.001) {
    significance_depressed_shehe <- 3
  } else if (pvalue_depressed_shehe < 0.01) {
    significance_depressed_shehe <- 2
  } else if (pvalue_depressed_shehe < 0.05) {
    significance_depressed_shehe <- 1
  } else {
    significance_depressed_shehe <- 0
  }
  
  # they
  if (pvalue_depressed_they < 0.001) {
    significance_depressed_they <- 3
  } else if (pvalue_depressed_they < 0.01) {
    significance_depressed_they <- 2
  } else if (pvalue_depressed_they < 0.05) {
    significance_depressed_they <- 1
  } else {
    significance_depressed_they <- 0
  }
  
  # Affect
  if (pvalue_depressed_Affect < 0.001) {
    significance_depressed_Affect <- 3
  } else if (pvalue_depressed_Affect < 0.01) {
    significance_depressed_Affect <- 2
  } else if (pvalue_depressed_Affect < 0.05) {
    significance_depressed_Affect <- 1
  } else {
    significance_depressed_Affect <- 0
  }
  
  # tone_pos
  if (pvalue_depressed_tone_pos < 0.001) {
    significance_depressed_tone_pos <- 3
  } else if (pvalue_depressed_tone_pos < 0.01) {
    significance_depressed_tone_pos <- 2
  } else if (pvalue_depressed_tone_pos < 0.05) {
    significance_depressed_tone_pos <- 1
  } else {
    significance_depressed_tone_pos <- 0
  }
  
  # tone_neg
  if (pvalue_depressed_tone_neg < 0.001) {
    significance_depressed_tone_neg <- 3
  } else if (pvalue_depressed_tone_neg < 0.01) {
    significance_depressed_tone_neg <- 2
  } else if (pvalue_depressed_tone_neg < 0.05) {
    significance_depressed_tone_neg <- 1
  } else {
    significance_depressed_tone_neg <- 0
  }
  
  # emotion
  if (pvalue_depressed_emotion < 0.001) {
    significance_depressed_emotion <- 3
  } else if (pvalue_depressed_emotion < 0.01) {
    significance_depressed_emotion <- 2
  } else if (pvalue_depressed_emotion < 0.05) {
    significance_depressed_emotion <- 1
  } else {
    significance_depressed_emotion <- 0
  }
  
  # emo_pos
  if (pvalue_depressed_emo_pos < 0.001) {
    significance_depressed_emo_pos <- 3
  } else if (pvalue_depressed_emo_pos < 0.01) {
    significance_depressed_emo_pos <- 2
  } else if (pvalue_depressed_emo_pos < 0.05) {
    significance_depressed_emo_pos <- 1
  } else {
    significance_depressed_emo_pos <- 0
  }
  
  # emo_neg
  if (pvalue_depressed_emo_neg < 0.001) {
    significance_depressed_emo_neg <- 3
  } else if (pvalue_depressed_emo_neg < 0.01) {
    significance_depressed_emo_neg <- 2
  } else if (pvalue_depressed_emo_neg < 0.05) {
    significance_depressed_emo_neg <- 1
  } else {
    significance_depressed_emo_neg <- 0
  }
  
  # emo_anx
  if (pvalue_depressed_emo_anx < 0.001) {
    significance_depressed_emo_anx <- 3
  } else if (pvalue_depressed_emo_anx < 0.01) {
    significance_depressed_emo_anx <- 2
  } else if (pvalue_depressed_emo_anx < 0.05) {
    significance_depressed_emo_anx <- 1
  } else {
    significance_depressed_emo_anx <- 0
  }
  
  # emo_anger
  if (pvalue_depressed_emo_anger < 0.001) {
    significance_depressed_emo_anger <- 3
  } else if (pvalue_depressed_emo_anger < 0.01) {
    significance_depressed_emo_anger <- 2
  } else if (pvalue_depressed_emo_anger < 0.05) {
    significance_depressed_emo_anger <- 1
  } else {
    significance_depressed_emo_anger <- 0
  }
  
  # emo_sad
  if (pvalue_depressed_emo_sad < 0.001) {
    significance_depressed_emo_sad <- 3
  } else if (pvalue_depressed_emo_sad < 0.01) {
    significance_depressed_emo_sad <- 2
  } else if (pvalue_depressed_emo_sad < 0.05) {
    significance_depressed_emo_sad <- 1
  } else {
    significance_depressed_emo_sad <- 0
  }
  
  # Social
  if (pvalue_depressed_Social < 0.001) {
    significance_depressed_Social <- 3
  } else if (pvalue_depressed_Social < 0.01) {
    significance_depressed_Social <- 2
  } else if (pvalue_depressed_Social < 0.05) {
    significance_depressed_Social <- 1
  } else {
    significance_depressed_Social <- 0
  }
  
  # socbehav
  if (pvalue_depressed_socbehav < 0.001) {
    significance_depressed_socbehav <- 3
  } else if (pvalue_depressed_socbehav < 0.01) {
    significance_depressed_socbehav <- 2
  } else if (pvalue_depressed_socbehav < 0.05) {
    significance_depressed_socbehav <- 1
  } else {
    significance_depressed_socbehav <- 0
  }
  
  # prosocial
  if (pvalue_depressed_prosocial < 0.001) {
    significance_depressed_prosocial <- 3
  } else if (pvalue_depressed_prosocial < 0.01) {
    significance_depressed_prosocial <- 2
  } else if (pvalue_depressed_prosocial < 0.05) {
    significance_depressed_prosocial <- 1
  } else {
    significance_depressed_prosocial <- 0
  }
  
  # polite
  if (pvalue_depressed_polite < 0.001) {
    significance_depressed_polite <- 3
  } else if (pvalue_depressed_polite < 0.01) {
    significance_depressed_polite <- 2
  } else if (pvalue_depressed_polite < 0.05) {
    significance_depressed_polite <- 1
  } else {
    significance_depressed_polite <- 0
  }
  
  # conflict
  if (pvalue_depressed_conflict < 0.001) {
    significance_depressed_conflict <- 3
  } else if (pvalue_depressed_conflict < 0.01) {
    significance_depressed_conflict <- 2
  } else if (pvalue_depressed_conflict < 0.05) {
    significance_depressed_conflict <- 1
  } else {
    significance_depressed_conflict <- 0
  }
  
  # moral
  if (pvalue_depressed_moral < 0.001) {
    significance_depressed_moral <- 3
  } else if (pvalue_depressed_moral < 0.01) {
    significance_depressed_moral <- 2
  } else if (pvalue_depressed_moral < 0.05) {
    significance_depressed_moral <- 1
  } else {
    significance_depressed_moral <- 0
  }
  
  # comm
  if (pvalue_depressed_comm < 0.001) {
    significance_depressed_comm <- 3
  } else if (pvalue_depressed_comm < 0.01) {
    significance_depressed_comm <- 2
  } else if (pvalue_depressed_comm < 0.05) {
    significance_depressed_comm <- 1
  } else {
    significance_depressed_comm <- 0
  }
  
  # socrefs
  if (pvalue_depressed_socrefs < 0.001) {
    significance_depressed_socrefs <- 3
  } else if (pvalue_depressed_socrefs < 0.01) {
    significance_depressed_socrefs <- 2
  } else if (pvalue_depressed_socrefs < 0.05) {
    significance_depressed_socrefs <- 1
  } else {
    significance_depressed_socrefs <- 0
  }
  
  # family
  if (pvalue_depressed_family < 0.001) {
    significance_depressed_family <- 3
  } else if (pvalue_depressed_family < 0.01) {
    significance_depressed_family <- 2
  } else if (pvalue_depressed_family < 0.05) {
    significance_depressed_family <- 1
  } else {
    significance_depressed_family <- 0
  }
  
  # friend
  if (pvalue_depressed_friend < 0.001) {
    significance_depressed_friend <- 3
  } else if (pvalue_depressed_friend < 0.01) {
    significance_depressed_friend <- 2
  } else if (pvalue_depressed_friend < 0.05) {
    significance_depressed_friend <- 1
  } else {
    significance_depressed_friend <- 0
  }
  
  # female
  if (pvalue_depressed_female < 0.001) {
    significance_depressed_female <- 3
  } else if (pvalue_depressed_female < 0.01) {
    significance_depressed_female <- 2
  } else if (pvalue_depressed_female < 0.05) {
    significance_depressed_female <- 1
  } else {
    significance_depressed_female <- 0
  }
  
  # male
  if (pvalue_depressed_male < 0.001) {
    significance_depressed_male <- 3
  } else if (pvalue_depressed_male < 0.01) {
    significance_depressed_male <- 2
  } else if (pvalue_depressed_male < 0.05) {
    significance_depressed_male <- 1
  } else {
    significance_depressed_male <- 0
  }
  
  # health
  if (pvalue_depressed_health < 0.001) {
    significance_depressed_health <- 3
  } else if (pvalue_depressed_health < 0.01) {
    significance_depressed_health <- 2
  } else if (pvalue_depressed_health < 0.05) {
    significance_depressed_health <- 1
  } else {
    significance_depressed_health <- 0
  }
  
  # illness
  if (pvalue_depressed_illness < 0.001) {
    significance_depressed_illness <- 3
  } else if (pvalue_depressed_illness < 0.01) {
    significance_depressed_illness <- 2
  } else if (pvalue_depressed_illness < 0.05) {
    significance_depressed_illness <- 1
  } else {
    significance_depressed_illness <- 0
  }
  
  # wellness
  if (pvalue_depressed_wellness < 0.001) {
    significance_depressed_wellness <- 3
  } else if (pvalue_depressed_wellness < 0.01) {
    significance_depressed_wellness <- 2
  } else if (pvalue_depressed_wellness < 0.05) {
    significance_depressed_wellness <- 1
  } else {
    significance_depressed_wellness <- 0
  }
  
  # mental
  if (pvalue_depressed_mental < 0.001) {
    significance_depressed_mental <- 3
  } else if (pvalue_depressed_mental < 0.01) {
    significance_depressed_mental <- 2
  } else if (pvalue_depressed_mental < 0.05) {
    significance_depressed_mental <- 1
  } else {
    significance_depressed_mental <- 0
  }
  
  # need
  if (pvalue_depressed_need < 0.001) {
    significance_depressed_need <- 3
  } else if (pvalue_depressed_need < 0.01) {
    significance_depressed_need <- 2
  } else if (pvalue_depressed_need < 0.05) {
    significance_depressed_need <- 1
  } else {
    significance_depressed_need <- 0
  }
  
  # want
  if (pvalue_depressed_want < 0.001) {
    significance_depressed_want <- 3
  } else if (pvalue_depressed_want < 0.01) {
    significance_depressed_want <- 2
  } else if (pvalue_depressed_want < 0.05) {
    significance_depressed_want <- 1
  } else {
    significance_depressed_want <- 0
  }
  
  # acquire
  if (pvalue_depressed_acquire < 0.001) {
    significance_depressed_acquire <- 3
  } else if (pvalue_depressed_acquire < 0.01) {
    significance_depressed_acquire <- 2
  } else if (pvalue_depressed_acquire < 0.05) {
    significance_depressed_acquire <- 1
  } else {
    significance_depressed_acquire <- 0
  }
  
  # lack
  if (pvalue_depressed_lack < 0.001) {
    significance_depressed_lack <- 3
  } else if (pvalue_depressed_lack < 0.01) {
    significance_depressed_lack <- 2
  } else if (pvalue_depressed_lack < 0.05) {
    significance_depressed_lack <- 1
  } else {
    significance_depressed_lack <- 0
  }
  
  # fulfill
  if (pvalue_depressed_fulfill < 0.001) {
    significance_depressed_fulfill <- 3
  } else if (pvalue_depressed_fulfill < 0.01) {
    significance_depressed_fulfill <- 2
  } else if (pvalue_depressed_fulfill < 0.05) {
    significance_depressed_fulfill <- 1
  } else {
    significance_depressed_fulfill <- 0
  }
  
  # fatigue
  if (pvalue_depressed_fatigue < 0.001) {
    significance_depressed_fatigue <- 3
  } else if (pvalue_depressed_fatigue < 0.01) {
    significance_depressed_fatigue <- 2
  } else if (pvalue_depressed_fatigue < 0.05) {
    significance_depressed_fatigue <- 1
  } else {
    significance_depressed_fatigue <- 0
  }
  
}

# fulfill_1
if (A > 1) {
  # WC
  if (pvalue_fulfill_1_WC < 0.001) {
    significance_fulfill_1_WC <- 3
  } else if (pvalue_fulfill_1_WC < 0.01) {
    significance_fulfill_1_WC <- 2
  } else if (pvalue_fulfill_1_WC < 0.05) {
    significance_fulfill_1_WC <- 1
  } else {
    significance_fulfill_1_WC <- 0
  }
  
  # Analytic
  if (pvalue_fulfill_1_Analytic < 0.001) {
    significance_fulfill_1_Analytic <- 3
  } else if (pvalue_fulfill_1_Analytic < 0.01) {
    significance_fulfill_1_Analytic <- 2
  } else if (pvalue_fulfill_1_Analytic < 0.05) {
    significance_fulfill_1_Analytic <- 1
  } else {
    significance_fulfill_1_Analytic <- 0
  }
  
  # Clout
  if (pvalue_fulfill_1_Clout < 0.001) {
    significance_fulfill_1_Clout <- 3
  } else if (pvalue_fulfill_1_Clout < 0.01) {
    significance_fulfill_1_Clout <- 2
  } else if (pvalue_fulfill_1_Clout < 0.05) {
    significance_fulfill_1_Clout <- 1
  } else {
    significance_fulfill_1_Clout <- 0
  }
  
  # Authentic
  if (pvalue_fulfill_1_Authentic < 0.001) {
    significance_fulfill_1_Authentic <- 3
  } else if (pvalue_fulfill_1_Authentic < 0.01) {
    significance_fulfill_1_Authentic <- 2
  } else if (pvalue_fulfill_1_Authentic < 0.05) {
    significance_fulfill_1_Authentic <- 1
  } else {
    significance_fulfill_1_Authentic <- 0
  }
  
  # Tone
  if (pvalue_fulfill_1_Tone < 0.001) {
    significance_fulfill_1_Tone <- 3
  } else if (pvalue_fulfill_1_Tone < 0.01) {
    significance_fulfill_1_Tone <- 2
  } else if (pvalue_fulfill_1_Tone < 0.05) {
    significance_fulfill_1_Tone <- 1
  } else {
    significance_fulfill_1_Tone <- 0
  }
  
  # BigWords
  if (pvalue_fulfill_1_BigWords < 0.001) {
    significance_fulfill_1_BigWords <- 3
  } else if (pvalue_fulfill_1_BigWords < 0.01) {
    significance_fulfill_1_BigWords <- 2
  } else if (pvalue_fulfill_1_BigWords < 0.05) {
    significance_fulfill_1_BigWords <- 1
  } else {
    significance_fulfill_1_BigWords <- 0
  }
  
  # Dic
  if (pvalue_fulfill_1_Dic < 0.001) {
    significance_fulfill_1_Dic <- 3
  } else if (pvalue_fulfill_1_Dic < 0.01) {
    significance_fulfill_1_Dic <- 2
  } else if (pvalue_fulfill_1_Dic < 0.05) {
    significance_fulfill_1_Dic <- 1
  } else {
    significance_fulfill_1_Dic <- 0
  }
  
  # Linguistic
  if (pvalue_fulfill_1_Linguistic < 0.001) {
    significance_fulfill_1_Linguistic <- 3
  } else if (pvalue_fulfill_1_Linguistic < 0.01) {
    significance_fulfill_1_Linguistic <- 2
  } else if (pvalue_fulfill_1_Linguistic < 0.05) {
    significance_fulfill_1_Linguistic <- 1
  } else {
    significance_fulfill_1_Linguistic <- 0
  }
  
  # function
  if (pvalue_fulfill_1_function < 0.001) {
    significance_fulfill_1_function <- 3
  } else if (pvalue_fulfill_1_function < 0.01) {
    significance_fulfill_1_function <- 2
  } else if (pvalue_fulfill_1_function < 0.05) {
    significance_fulfill_1_function <- 1
  } else {
    significance_fulfill_1_function <- 0
  }
  
  # pronoun
  if (pvalue_fulfill_1_pronoun < 0.001) {
    significance_fulfill_1_pronoun <- 3
  } else if (pvalue_fulfill_1_pronoun < 0.01) {
    significance_fulfill_1_pronoun <- 2
  } else if (pvalue_fulfill_1_pronoun < 0.05) {
    significance_fulfill_1_pronoun <- 1
  } else {
    significance_fulfill_1_pronoun <- 0
  }
  
  # ppron
  if (pvalue_fulfill_1_ppron < 0.001) {
    significance_fulfill_1_ppron <- 3
  } else if (pvalue_fulfill_1_ppron < 0.01) {
    significance_fulfill_1_ppron <- 2
  } else if (pvalue_fulfill_1_ppron < 0.05) {
    significance_fulfill_1_ppron <- 1
  } else {
    significance_fulfill_1_ppron <- 0
  }
  
  # i
  if (pvalue_fulfill_1_i < 0.001) {
    significance_fulfill_1_i <- 3
  } else if (pvalue_fulfill_1_i < 0.01) {
    significance_fulfill_1_i <- 2
  } else if (pvalue_fulfill_1_i < 0.05) {
    significance_fulfill_1_i <- 1
  } else {
    significance_fulfill_1_i <- 0
  }
  
  # we
  if (pvalue_fulfill_1_we < 0.001) {
    significance_fulfill_1_we <- 3
  } else if (pvalue_fulfill_1_we < 0.01) {
    significance_fulfill_1_we <- 2
  } else if (pvalue_fulfill_1_we < 0.05) {
    significance_fulfill_1_we <- 1
  } else {
    significance_fulfill_1_we <- 0
  }
  
  # you
  if (pvalue_fulfill_1_you < 0.001) {
    significance_fulfill_1_you <- 3
  } else if (pvalue_fulfill_1_you < 0.01) {
    significance_fulfill_1_you <- 2
  } else if (pvalue_fulfill_1_you < 0.05) {
    significance_fulfill_1_you <- 1
  } else {
    significance_fulfill_1_you <- 0
  }
  
  # shehe
  if (pvalue_fulfill_1_shehe < 0.001) {
    significance_fulfill_1_shehe <- 3
  } else if (pvalue_fulfill_1_shehe < 0.01) {
    significance_fulfill_1_shehe <- 2
  } else if (pvalue_fulfill_1_shehe < 0.05) {
    significance_fulfill_1_shehe <- 1
  } else {
    significance_fulfill_1_shehe <- 0
  }
  
  # they
  if (pvalue_fulfill_1_they < 0.001) {
    significance_fulfill_1_they <- 3
  } else if (pvalue_fulfill_1_they < 0.01) {
    significance_fulfill_1_they <- 2
  } else if (pvalue_fulfill_1_they < 0.05) {
    significance_fulfill_1_they <- 1
  } else {
    significance_fulfill_1_they <- 0
  }
  
  # Affect
  if (pvalue_fulfill_1_Affect < 0.001) {
    significance_fulfill_1_Affect <- 3
  } else if (pvalue_fulfill_1_Affect < 0.01) {
    significance_fulfill_1_Affect <- 2
  } else if (pvalue_fulfill_1_Affect < 0.05) {
    significance_fulfill_1_Affect <- 1
  } else {
    significance_fulfill_1_Affect <- 0
  }
  
  # tone_pos
  if (pvalue_fulfill_1_tone_pos < 0.001) {
    significance_fulfill_1_tone_pos <- 3
  } else if (pvalue_fulfill_1_tone_pos < 0.01) {
    significance_fulfill_1_tone_pos <- 2
  } else if (pvalue_fulfill_1_tone_pos < 0.05) {
    significance_fulfill_1_tone_pos <- 1
  } else {
    significance_fulfill_1_tone_pos <- 0
  }
  
  # tone_neg
  if (pvalue_fulfill_1_tone_neg < 0.001) {
    significance_fulfill_1_tone_neg <- 3
  } else if (pvalue_fulfill_1_tone_neg < 0.01) {
    significance_fulfill_1_tone_neg <- 2
  } else if (pvalue_fulfill_1_tone_neg < 0.05) {
    significance_fulfill_1_tone_neg <- 1
  } else {
    significance_fulfill_1_tone_neg <- 0
  }
  
  # emotion
  if (pvalue_fulfill_1_emotion < 0.001) {
    significance_fulfill_1_emotion <- 3
  } else if (pvalue_fulfill_1_emotion < 0.01) {
    significance_fulfill_1_emotion <- 2
  } else if (pvalue_fulfill_1_emotion < 0.05) {
    significance_fulfill_1_emotion <- 1
  } else {
    significance_fulfill_1_emotion <- 0
  }
  
  # emo_pos
  if (pvalue_fulfill_1_emo_pos < 0.001) {
    significance_fulfill_1_emo_pos <- 3
  } else if (pvalue_fulfill_1_emo_pos < 0.01) {
    significance_fulfill_1_emo_pos <- 2
  } else if (pvalue_fulfill_1_emo_pos < 0.05) {
    significance_fulfill_1_emo_pos <- 1
  } else {
    significance_fulfill_1_emo_pos <- 0
  }
  
  # emo_neg
  if (pvalue_fulfill_1_emo_neg < 0.001) {
    significance_fulfill_1_emo_neg <- 3
  } else if (pvalue_fulfill_1_emo_neg < 0.01) {
    significance_fulfill_1_emo_neg <- 2
  } else if (pvalue_fulfill_1_emo_neg < 0.05) {
    significance_fulfill_1_emo_neg <- 1
  } else {
    significance_fulfill_1_emo_neg <- 0
  }
  
  # emo_anx
  if (pvalue_fulfill_1_emo_anx < 0.001) {
    significance_fulfill_1_emo_anx <- 3
  } else if (pvalue_fulfill_1_emo_anx < 0.01) {
    significance_fulfill_1_emo_anx <- 2
  } else if (pvalue_fulfill_1_emo_anx < 0.05) {
    significance_fulfill_1_emo_anx <- 1
  } else {
    significance_fulfill_1_emo_anx <- 0
  }
  
  # emo_anger
  if (pvalue_fulfill_1_emo_anger < 0.001) {
    significance_fulfill_1_emo_anger <- 3
  } else if (pvalue_fulfill_1_emo_anger < 0.01) {
    significance_fulfill_1_emo_anger <- 2
  } else if (pvalue_fulfill_1_emo_anger < 0.05) {
    significance_fulfill_1_emo_anger <- 1
  } else {
    significance_fulfill_1_emo_anger <- 0
  }
  
  # emo_sad
  if (pvalue_fulfill_1_emo_sad < 0.001) {
    significance_fulfill_1_emo_sad <- 3
  } else if (pvalue_fulfill_1_emo_sad < 0.01) {
    significance_fulfill_1_emo_sad <- 2
  } else if (pvalue_fulfill_1_emo_sad < 0.05) {
    significance_fulfill_1_emo_sad <- 1
  } else {
    significance_fulfill_1_emo_sad <- 0
  }
  
  # Social
  if (pvalue_fulfill_1_Social < 0.001) {
    significance_fulfill_1_Social <- 3
  } else if (pvalue_fulfill_1_Social < 0.01) {
    significance_fulfill_1_Social <- 2
  } else if (pvalue_fulfill_1_Social < 0.05) {
    significance_fulfill_1_Social <- 1
  } else {
    significance_fulfill_1_Social <- 0
  }
  
  # socbehav
  if (pvalue_fulfill_1_socbehav < 0.001) {
    significance_fulfill_1_socbehav <- 3
  } else if (pvalue_fulfill_1_socbehav < 0.01) {
    significance_fulfill_1_socbehav <- 2
  } else if (pvalue_fulfill_1_socbehav < 0.05) {
    significance_fulfill_1_socbehav <- 1
  } else {
    significance_fulfill_1_socbehav <- 0
  }
  
  # prosocial
  if (pvalue_fulfill_1_prosocial < 0.001) {
    significance_fulfill_1_prosocial <- 3
  } else if (pvalue_fulfill_1_prosocial < 0.01) {
    significance_fulfill_1_prosocial <- 2
  } else if (pvalue_fulfill_1_prosocial < 0.05) {
    significance_fulfill_1_prosocial <- 1
  } else {
    significance_fulfill_1_prosocial <- 0
  }
  
  # polite
  if (pvalue_fulfill_1_polite < 0.001) {
    significance_fulfill_1_polite <- 3
  } else if (pvalue_fulfill_1_polite < 0.01) {
    significance_fulfill_1_polite <- 2
  } else if (pvalue_fulfill_1_polite < 0.05) {
    significance_fulfill_1_polite <- 1
  } else {
    significance_fulfill_1_polite <- 0
  }
  
  # conflict
  if (pvalue_fulfill_1_conflict < 0.001) {
    significance_fulfill_1_conflict <- 3
  } else if (pvalue_fulfill_1_conflict < 0.01) {
    significance_fulfill_1_conflict <- 2
  } else if (pvalue_fulfill_1_conflict < 0.05) {
    significance_fulfill_1_conflict <- 1
  } else {
    significance_fulfill_1_conflict <- 0
  }
  
  # moral
  if (pvalue_fulfill_1_moral < 0.001) {
    significance_fulfill_1_moral <- 3
  } else if (pvalue_fulfill_1_moral < 0.01) {
    significance_fulfill_1_moral <- 2
  } else if (pvalue_fulfill_1_moral < 0.05) {
    significance_fulfill_1_moral <- 1
  } else {
    significance_fulfill_1_moral <- 0
  }
  
  # comm
  if (pvalue_fulfill_1_comm < 0.001) {
    significance_fulfill_1_comm <- 3
  } else if (pvalue_fulfill_1_comm < 0.01) {
    significance_fulfill_1_comm <- 2
  } else if (pvalue_fulfill_1_comm < 0.05) {
    significance_fulfill_1_comm <- 1
  } else {
    significance_fulfill_1_comm <- 0
  }
  
  # socrefs
  if (pvalue_fulfill_1_socrefs < 0.001) {
    significance_fulfill_1_socrefs <- 3
  } else if (pvalue_fulfill_1_socrefs < 0.01) {
    significance_fulfill_1_socrefs <- 2
  } else if (pvalue_fulfill_1_socrefs < 0.05) {
    significance_fulfill_1_socrefs <- 1
  } else {
    significance_fulfill_1_socrefs <- 0
  }
  
  # family
  if (pvalue_fulfill_1_family < 0.001) {
    significance_fulfill_1_family <- 3
  } else if (pvalue_fulfill_1_family < 0.01) {
    significance_fulfill_1_family <- 2
  } else if (pvalue_fulfill_1_family < 0.05) {
    significance_fulfill_1_family <- 1
  } else {
    significance_fulfill_1_family <- 0
  }
  
  # friend
  if (pvalue_fulfill_1_friend < 0.001) {
    significance_fulfill_1_friend <- 3
  } else if (pvalue_fulfill_1_friend < 0.01) {
    significance_fulfill_1_friend <- 2
  } else if (pvalue_fulfill_1_friend < 0.05) {
    significance_fulfill_1_friend <- 1
  } else {
    significance_fulfill_1_friend <- 0
  }
  
  # female
  if (pvalue_fulfill_1_female < 0.001) {
    significance_fulfill_1_female <- 3
  } else if (pvalue_fulfill_1_female < 0.01) {
    significance_fulfill_1_female <- 2
  } else if (pvalue_fulfill_1_female < 0.05) {
    significance_fulfill_1_female <- 1
  } else {
    significance_fulfill_1_female <- 0
  }
  
  # male
  if (pvalue_fulfill_1_male < 0.001) {
    significance_fulfill_1_male <- 3
  } else if (pvalue_fulfill_1_male < 0.01) {
    significance_fulfill_1_male <- 2
  } else if (pvalue_fulfill_1_male < 0.05) {
    significance_fulfill_1_male <- 1
  } else {
    significance_fulfill_1_male <- 0
  }
  
  # health
  if (pvalue_fulfill_1_health < 0.001) {
    significance_fulfill_1_health <- 3
  } else if (pvalue_fulfill_1_health < 0.01) {
    significance_fulfill_1_health <- 2
  } else if (pvalue_fulfill_1_health < 0.05) {
    significance_fulfill_1_health <- 1
  } else {
    significance_fulfill_1_health <- 0
  }
  
  # illness
  if (pvalue_fulfill_1_illness < 0.001) {
    significance_fulfill_1_illness <- 3
  } else if (pvalue_fulfill_1_illness < 0.01) {
    significance_fulfill_1_illness <- 2
  } else if (pvalue_fulfill_1_illness < 0.05) {
    significance_fulfill_1_illness <- 1
  } else {
    significance_fulfill_1_illness <- 0
  }
  
  # wellness
  if (pvalue_fulfill_1_wellness < 0.001) {
    significance_fulfill_1_wellness <- 3
  } else if (pvalue_fulfill_1_wellness < 0.01) {
    significance_fulfill_1_wellness <- 2
  } else if (pvalue_fulfill_1_wellness < 0.05) {
    significance_fulfill_1_wellness <- 1
  } else {
    significance_fulfill_1_wellness <- 0
  }
  
  # mental
  if (pvalue_fulfill_1_mental < 0.001) {
    significance_fulfill_1_mental <- 3
  } else if (pvalue_fulfill_1_mental < 0.01) {
    significance_fulfill_1_mental <- 2
  } else if (pvalue_fulfill_1_mental < 0.05) {
    significance_fulfill_1_mental <- 1
  } else {
    significance_fulfill_1_mental <- 0
  }
  
  # need
  if (pvalue_fulfill_1_need < 0.001) {
    significance_fulfill_1_need <- 3
  } else if (pvalue_fulfill_1_need < 0.01) {
    significance_fulfill_1_need <- 2
  } else if (pvalue_fulfill_1_need < 0.05) {
    significance_fulfill_1_need <- 1
  } else {
    significance_fulfill_1_need <- 0
  }
  
  # want
  if (pvalue_fulfill_1_want < 0.001) {
    significance_fulfill_1_want <- 3
  } else if (pvalue_fulfill_1_want < 0.01) {
    significance_fulfill_1_want <- 2
  } else if (pvalue_fulfill_1_want < 0.05) {
    significance_fulfill_1_want <- 1
  } else {
    significance_fulfill_1_want <- 0
  }
  
  # acquire
  if (pvalue_fulfill_1_acquire < 0.001) {
    significance_fulfill_1_acquire <- 3
  } else if (pvalue_fulfill_1_acquire < 0.01) {
    significance_fulfill_1_acquire <- 2
  } else if (pvalue_fulfill_1_acquire < 0.05) {
    significance_fulfill_1_acquire <- 1
  } else {
    significance_fulfill_1_acquire <- 0
  }
  
  # lack
  if (pvalue_fulfill_1_lack < 0.001) {
    significance_fulfill_1_lack <- 3
  } else if (pvalue_fulfill_1_lack < 0.01) {
    significance_fulfill_1_lack <- 2
  } else if (pvalue_fulfill_1_lack < 0.05) {
    significance_fulfill_1_lack <- 1
  } else {
    significance_fulfill_1_lack <- 0
  }
  
  # fulfill
  if (pvalue_fulfill_1_fulfill < 0.001) {
    significance_fulfill_1_fulfill <- 3
  } else if (pvalue_fulfill_1_fulfill < 0.01) {
    significance_fulfill_1_fulfill <- 2
  } else if (pvalue_fulfill_1_fulfill < 0.05) {
    significance_fulfill_1_fulfill <- 1
  } else {
    significance_fulfill_1_fulfill <- 0
  }
  
  # fatigue
  if (pvalue_fulfill_1_fatigue < 0.001) {
    significance_fulfill_1_fatigue <- 3
  } else if (pvalue_fulfill_1_fatigue < 0.01) {
    significance_fulfill_1_fatigue <- 2
  } else if (pvalue_fulfill_1_fatigue < 0.05) {
    significance_fulfill_1_fatigue <- 1
  } else {
    significance_fulfill_1_fatigue <- 0
  }
  
}

# effort
if (A > 1) {
  # WC
  if (pvalue_effort_WC < 0.001) {
    significance_effort_WC <- 3
  } else if (pvalue_effort_WC < 0.01) {
    significance_effort_WC <- 2
  } else if (pvalue_effort_WC < 0.05) {
    significance_effort_WC <- 1
  } else {
    significance_effort_WC <- 0
  }
  
  # Analytic
  if (pvalue_effort_Analytic < 0.001) {
    significance_effort_Analytic <- 3
  } else if (pvalue_effort_Analytic < 0.01) {
    significance_effort_Analytic <- 2
  } else if (pvalue_effort_Analytic < 0.05) {
    significance_effort_Analytic <- 1
  } else {
    significance_effort_Analytic <- 0
  }
  
  # Clout
  if (pvalue_effort_Clout < 0.001) {
    significance_effort_Clout <- 3
  } else if (pvalue_effort_Clout < 0.01) {
    significance_effort_Clout <- 2
  } else if (pvalue_effort_Clout < 0.05) {
    significance_effort_Clout <- 1
  } else {
    significance_effort_Clout <- 0
  }
  
  # Authentic
  if (pvalue_effort_Authentic < 0.001) {
    significance_effort_Authentic <- 3
  } else if (pvalue_effort_Authentic < 0.01) {
    significance_effort_Authentic <- 2
  } else if (pvalue_effort_Authentic < 0.05) {
    significance_effort_Authentic <- 1
  } else {
    significance_effort_Authentic <- 0
  }
  
  # Tone
  if (pvalue_effort_Tone < 0.001) {
    significance_effort_Tone <- 3
  } else if (pvalue_effort_Tone < 0.01) {
    significance_effort_Tone <- 2
  } else if (pvalue_effort_Tone < 0.05) {
    significance_effort_Tone <- 1
  } else {
    significance_effort_Tone <- 0
  }
  
  # BigWords
  if (pvalue_effort_BigWords < 0.001) {
    significance_effort_BigWords <- 3
  } else if (pvalue_effort_BigWords < 0.01) {
    significance_effort_BigWords <- 2
  } else if (pvalue_effort_BigWords < 0.05) {
    significance_effort_BigWords <- 1
  } else {
    significance_effort_BigWords <- 0
  }
  
  # Dic
  if (pvalue_effort_Dic < 0.001) {
    significance_effort_Dic <- 3
  } else if (pvalue_effort_Dic < 0.01) {
    significance_effort_Dic <- 2
  } else if (pvalue_effort_Dic < 0.05) {
    significance_effort_Dic <- 1
  } else {
    significance_effort_Dic <- 0
  }
  
  # Linguistic
  if (pvalue_effort_Linguistic < 0.001) {
    significance_effort_Linguistic <- 3
  } else if (pvalue_effort_Linguistic < 0.01) {
    significance_effort_Linguistic <- 2
  } else if (pvalue_effort_Linguistic < 0.05) {
    significance_effort_Linguistic <- 1
  } else {
    significance_effort_Linguistic <- 0
  }
  
  # function
  if (pvalue_effort_function < 0.001) {
    significance_effort_function <- 3
  } else if (pvalue_effort_function < 0.01) {
    significance_effort_function <- 2
  } else if (pvalue_effort_function < 0.05) {
    significance_effort_function <- 1
  } else {
    significance_effort_function <- 0
  }
  
  # pronoun
  if (pvalue_effort_pronoun < 0.001) {
    significance_effort_pronoun <- 3
  } else if (pvalue_effort_pronoun < 0.01) {
    significance_effort_pronoun <- 2
  } else if (pvalue_effort_pronoun < 0.05) {
    significance_effort_pronoun <- 1
  } else {
    significance_effort_pronoun <- 0
  }
  
  # ppron
  if (pvalue_effort_ppron < 0.001) {
    significance_effort_ppron <- 3
  } else if (pvalue_effort_ppron < 0.01) {
    significance_effort_ppron <- 2
  } else if (pvalue_effort_ppron < 0.05) {
    significance_effort_ppron <- 1
  } else {
    significance_effort_ppron <- 0
  }
  
  # i
  if (pvalue_effort_i < 0.001) {
    significance_effort_i <- 3
  } else if (pvalue_effort_i < 0.01) {
    significance_effort_i <- 2
  } else if (pvalue_effort_i < 0.05) {
    significance_effort_i <- 1
  } else {
    significance_effort_i <- 0
  }
  
  # we
  if (pvalue_effort_we < 0.001) {
    significance_effort_we <- 3
  } else if (pvalue_effort_we < 0.01) {
    significance_effort_we <- 2
  } else if (pvalue_effort_we < 0.05) {
    significance_effort_we <- 1
  } else {
    significance_effort_we <- 0
  }
  
  # you
  if (pvalue_effort_you < 0.001) {
    significance_effort_you <- 3
  } else if (pvalue_effort_you < 0.01) {
    significance_effort_you <- 2
  } else if (pvalue_effort_you < 0.05) {
    significance_effort_you <- 1
  } else {
    significance_effort_you <- 0
  }
  
  # shehe
  if (pvalue_effort_shehe < 0.001) {
    significance_effort_shehe <- 3
  } else if (pvalue_effort_shehe < 0.01) {
    significance_effort_shehe <- 2
  } else if (pvalue_effort_shehe < 0.05) {
    significance_effort_shehe <- 1
  } else {
    significance_effort_shehe <- 0
  }
  
  # they
  if (pvalue_effort_they < 0.001) {
    significance_effort_they <- 3
  } else if (pvalue_effort_they < 0.01) {
    significance_effort_they <- 2
  } else if (pvalue_effort_they < 0.05) {
    significance_effort_they <- 1
  } else {
    significance_effort_they <- 0
  }
  
  # Affect
  if (pvalue_effort_Affect < 0.001) {
    significance_effort_Affect <- 3
  } else if (pvalue_effort_Affect < 0.01) {
    significance_effort_Affect <- 2
  } else if (pvalue_effort_Affect < 0.05) {
    significance_effort_Affect <- 1
  } else {
    significance_effort_Affect <- 0
  }
  
  # tone_pos
  if (pvalue_effort_tone_pos < 0.001) {
    significance_effort_tone_pos <- 3
  } else if (pvalue_effort_tone_pos < 0.01) {
    significance_effort_tone_pos <- 2
  } else if (pvalue_effort_tone_pos < 0.05) {
    significance_effort_tone_pos <- 1
  } else {
    significance_effort_tone_pos <- 0
  }
  
  # tone_neg
  if (pvalue_effort_tone_neg < 0.001) {
    significance_effort_tone_neg <- 3
  } else if (pvalue_effort_tone_neg < 0.01) {
    significance_effort_tone_neg <- 2
  } else if (pvalue_effort_tone_neg < 0.05) {
    significance_effort_tone_neg <- 1
  } else {
    significance_effort_tone_neg <- 0
  }
  
  # emotion
  if (pvalue_effort_emotion < 0.001) {
    significance_effort_emotion <- 3
  } else if (pvalue_effort_emotion < 0.01) {
    significance_effort_emotion <- 2
  } else if (pvalue_effort_emotion < 0.05) {
    significance_effort_emotion <- 1
  } else {
    significance_effort_emotion <- 0
  }
  
  # emo_pos
  if (pvalue_effort_emo_pos < 0.001) {
    significance_effort_emo_pos <- 3
  } else if (pvalue_effort_emo_pos < 0.01) {
    significance_effort_emo_pos <- 2
  } else if (pvalue_effort_emo_pos < 0.05) {
    significance_effort_emo_pos <- 1
  } else {
    significance_effort_emo_pos <- 0
  }
  
  # emo_neg
  if (pvalue_effort_emo_neg < 0.001) {
    significance_effort_emo_neg <- 3
  } else if (pvalue_effort_emo_neg < 0.01) {
    significance_effort_emo_neg <- 2
  } else if (pvalue_effort_emo_neg < 0.05) {
    significance_effort_emo_neg <- 1
  } else {
    significance_effort_emo_neg <- 0
  }
  
  # emo_anx
  if (pvalue_effort_emo_anx < 0.001) {
    significance_effort_emo_anx <- 3
  } else if (pvalue_effort_emo_anx < 0.01) {
    significance_effort_emo_anx <- 2
  } else if (pvalue_effort_emo_anx < 0.05) {
    significance_effort_emo_anx <- 1
  } else {
    significance_effort_emo_anx <- 0
  }
  
  # emo_anger
  if (pvalue_effort_emo_anger < 0.001) {
    significance_effort_emo_anger <- 3
  } else if (pvalue_effort_emo_anger < 0.01) {
    significance_effort_emo_anger <- 2
  } else if (pvalue_effort_emo_anger < 0.05) {
    significance_effort_emo_anger <- 1
  } else {
    significance_effort_emo_anger <- 0
  }
  
  # emo_sad
  if (pvalue_effort_emo_sad < 0.001) {
    significance_effort_emo_sad <- 3
  } else if (pvalue_effort_emo_sad < 0.01) {
    significance_effort_emo_sad <- 2
  } else if (pvalue_effort_emo_sad < 0.05) {
    significance_effort_emo_sad <- 1
  } else {
    significance_effort_emo_sad <- 0
  }
  
  # Social
  if (pvalue_effort_Social < 0.001) {
    significance_effort_Social <- 3
  } else if (pvalue_effort_Social < 0.01) {
    significance_effort_Social <- 2
  } else if (pvalue_effort_Social < 0.05) {
    significance_effort_Social <- 1
  } else {
    significance_effort_Social <- 0
  }
  
  # socbehav
  if (pvalue_effort_socbehav < 0.001) {
    significance_effort_socbehav <- 3
  } else if (pvalue_effort_socbehav < 0.01) {
    significance_effort_socbehav <- 2
  } else if (pvalue_effort_socbehav < 0.05) {
    significance_effort_socbehav <- 1
  } else {
    significance_effort_socbehav <- 0
  }
  
  # prosocial
  if (pvalue_effort_prosocial < 0.001) {
    significance_effort_prosocial <- 3
  } else if (pvalue_effort_prosocial < 0.01) {
    significance_effort_prosocial <- 2
  } else if (pvalue_effort_prosocial < 0.05) {
    significance_effort_prosocial <- 1
  } else {
    significance_effort_prosocial <- 0
  }
  
  # polite
  if (pvalue_effort_polite < 0.001) {
    significance_effort_polite <- 3
  } else if (pvalue_effort_polite < 0.01) {
    significance_effort_polite <- 2
  } else if (pvalue_effort_polite < 0.05) {
    significance_effort_polite <- 1
  } else {
    significance_effort_polite <- 0
  }
  
  # conflict
  if (pvalue_effort_conflict < 0.001) {
    significance_effort_conflict <- 3
  } else if (pvalue_effort_conflict < 0.01) {
    significance_effort_conflict <- 2
  } else if (pvalue_effort_conflict < 0.05) {
    significance_effort_conflict <- 1
  } else {
    significance_effort_conflict <- 0
  }
  
  # moral
  if (pvalue_effort_moral < 0.001) {
    significance_effort_moral <- 3
  } else if (pvalue_effort_moral < 0.01) {
    significance_effort_moral <- 2
  } else if (pvalue_effort_moral < 0.05) {
    significance_effort_moral <- 1
  } else {
    significance_effort_moral <- 0
  }
  
  # comm
  if (pvalue_effort_comm < 0.001) {
    significance_effort_comm <- 3
  } else if (pvalue_effort_comm < 0.01) {
    significance_effort_comm <- 2
  } else if (pvalue_effort_comm < 0.05) {
    significance_effort_comm <- 1
  } else {
    significance_effort_comm <- 0
  }
  
  # socrefs
  if (pvalue_effort_socrefs < 0.001) {
    significance_effort_socrefs <- 3
  } else if (pvalue_effort_socrefs < 0.01) {
    significance_effort_socrefs <- 2
  } else if (pvalue_effort_socrefs < 0.05) {
    significance_effort_socrefs <- 1
  } else {
    significance_effort_socrefs <- 0
  }
  
  # family
  if (pvalue_effort_family < 0.001) {
    significance_effort_family <- 3
  } else if (pvalue_effort_family < 0.01) {
    significance_effort_family <- 2
  } else if (pvalue_effort_family < 0.05) {
    significance_effort_family <- 1
  } else {
    significance_effort_family <- 0
  }
  
  # friend
  if (pvalue_effort_friend < 0.001) {
    significance_effort_friend <- 3
  } else if (pvalue_effort_friend < 0.01) {
    significance_effort_friend <- 2
  } else if (pvalue_effort_friend < 0.05) {
    significance_effort_friend <- 1
  } else {
    significance_effort_friend <- 0
  }
  
  # female
  if (pvalue_effort_female < 0.001) {
    significance_effort_female <- 3
  } else if (pvalue_effort_female < 0.01) {
    significance_effort_female <- 2
  } else if (pvalue_effort_female < 0.05) {
    significance_effort_female <- 1
  } else {
    significance_effort_female <- 0
  }
  
  # male
  if (pvalue_effort_male < 0.001) {
    significance_effort_male <- 3
  } else if (pvalue_effort_male < 0.01) {
    significance_effort_male <- 2
  } else if (pvalue_effort_male < 0.05) {
    significance_effort_male <- 1
  } else {
    significance_effort_male <- 0
  }
  
  # health
  if (pvalue_effort_health < 0.001) {
    significance_effort_health <- 3
  } else if (pvalue_effort_health < 0.01) {
    significance_effort_health <- 2
  } else if (pvalue_effort_health < 0.05) {
    significance_effort_health <- 1
  } else {
    significance_effort_health <- 0
  }
  
  # illness
  if (pvalue_effort_illness < 0.001) {
    significance_effort_illness <- 3
  } else if (pvalue_effort_illness < 0.01) {
    significance_effort_illness <- 2
  } else if (pvalue_effort_illness < 0.05) {
    significance_effort_illness <- 1
  } else {
    significance_effort_illness <- 0
  }
  
  # wellness
  if (pvalue_effort_wellness < 0.001) {
    significance_effort_wellness <- 3
  } else if (pvalue_effort_wellness < 0.01) {
    significance_effort_wellness <- 2
  } else if (pvalue_effort_wellness < 0.05) {
    significance_effort_wellness <- 1
  } else {
    significance_effort_wellness <- 0
  }
  
  # mental
  if (pvalue_effort_mental < 0.001) {
    significance_effort_mental <- 3
  } else if (pvalue_effort_mental < 0.01) {
    significance_effort_mental <- 2
  } else if (pvalue_effort_mental < 0.05) {
    significance_effort_mental <- 1
  } else {
    significance_effort_mental <- 0
  }
  
  # need
  if (pvalue_effort_need < 0.001) {
    significance_effort_need <- 3
  } else if (pvalue_effort_need < 0.01) {
    significance_effort_need <- 2
  } else if (pvalue_effort_need < 0.05) {
    significance_effort_need <- 1
  } else {
    significance_effort_need <- 0
  }
  
  # want
  if (pvalue_effort_want < 0.001) {
    significance_effort_want <- 3
  } else if (pvalue_effort_want < 0.01) {
    significance_effort_want <- 2
  } else if (pvalue_effort_want < 0.05) {
    significance_effort_want <- 1
  } else {
    significance_effort_want <- 0
  }
  
  # acquire
  if (pvalue_effort_acquire < 0.001) {
    significance_effort_acquire <- 3
  } else if (pvalue_effort_acquire < 0.01) {
    significance_effort_acquire <- 2
  } else if (pvalue_effort_acquire < 0.05) {
    significance_effort_acquire <- 1
  } else {
    significance_effort_acquire <- 0
  }
  
  # lack
  if (pvalue_effort_lack < 0.001) {
    significance_effort_lack <- 3
  } else if (pvalue_effort_lack < 0.01) {
    significance_effort_lack <- 2
  } else if (pvalue_effort_lack < 0.05) {
    significance_effort_lack <- 1
  } else {
    significance_effort_lack <- 0
  }
  
  # fulfill
  if (pvalue_effort_fulfill < 0.001) {
    significance_effort_fulfill <- 3
  } else if (pvalue_effort_fulfill < 0.01) {
    significance_effort_fulfill <- 2
  } else if (pvalue_effort_fulfill < 0.05) {
    significance_effort_fulfill <- 1
  } else {
    significance_effort_fulfill <- 0
  }
  
  # fatigue
  if (pvalue_effort_fatigue < 0.001) {
    significance_effort_fatigue <- 3
  } else if (pvalue_effort_fatigue < 0.01) {
    significance_effort_fatigue <- 2
  } else if (pvalue_effort_fatigue < 0.05) {
    significance_effort_fatigue <- 1
  } else {
    significance_effort_fatigue <- 0
  }
  
}

# sex
if (A > 1) {
  # WC
  if (pvalue_sex_WC < 0.001) {
    significance_sex_WC <- 3
  } else if (pvalue_sex_WC < 0.01) {
    significance_sex_WC <- 2
  } else if (pvalue_sex_WC < 0.05) {
    significance_sex_WC <- 1
  } else {
    significance_sex_WC <- 0
  }
  
  # Analytic
  if (pvalue_sex_Analytic < 0.001) {
    significance_sex_Analytic <- 3
  } else if (pvalue_sex_Analytic < 0.01) {
    significance_sex_Analytic <- 2
  } else if (pvalue_sex_Analytic < 0.05) {
    significance_sex_Analytic <- 1
  } else {
    significance_sex_Analytic <- 0
  }
  
  # Clout
  if (pvalue_sex_Clout < 0.001) {
    significance_sex_Clout <- 3
  } else if (pvalue_sex_Clout < 0.01) {
    significance_sex_Clout <- 2
  } else if (pvalue_sex_Clout < 0.05) {
    significance_sex_Clout <- 1
  } else {
    significance_sex_Clout <- 0
  }
  
  # Authentic
  if (pvalue_sex_Authentic < 0.001) {
    significance_sex_Authentic <- 3
  } else if (pvalue_sex_Authentic < 0.01) {
    significance_sex_Authentic <- 2
  } else if (pvalue_sex_Authentic < 0.05) {
    significance_sex_Authentic <- 1
  } else {
    significance_sex_Authentic <- 0
  }
  
  # Tone
  if (pvalue_sex_Tone < 0.001) {
    significance_sex_Tone <- 3
  } else if (pvalue_sex_Tone < 0.01) {
    significance_sex_Tone <- 2
  } else if (pvalue_sex_Tone < 0.05) {
    significance_sex_Tone <- 1
  } else {
    significance_sex_Tone <- 0
  }
  
  # BigWords
  if (pvalue_sex_BigWords < 0.001) {
    significance_sex_BigWords <- 3
  } else if (pvalue_sex_BigWords < 0.01) {
    significance_sex_BigWords <- 2
  } else if (pvalue_sex_BigWords < 0.05) {
    significance_sex_BigWords <- 1
  } else {
    significance_sex_BigWords <- 0
  }
  
  # Dic
  if (pvalue_sex_Dic < 0.001) {
    significance_sex_Dic <- 3
  } else if (pvalue_sex_Dic < 0.01) {
    significance_sex_Dic <- 2
  } else if (pvalue_sex_Dic < 0.05) {
    significance_sex_Dic <- 1
  } else {
    significance_sex_Dic <- 0
  }
  
  # Linguistic
  if (pvalue_sex_Linguistic < 0.001) {
    significance_sex_Linguistic <- 3
  } else if (pvalue_sex_Linguistic < 0.01) {
    significance_sex_Linguistic <- 2
  } else if (pvalue_sex_Linguistic < 0.05) {
    significance_sex_Linguistic <- 1
  } else {
    significance_sex_Linguistic <- 0
  }
  
  # function
  if (pvalue_sex_function < 0.001) {
    significance_sex_function <- 3
  } else if (pvalue_sex_function < 0.01) {
    significance_sex_function <- 2
  } else if (pvalue_sex_function < 0.05) {
    significance_sex_function <- 1
  } else {
    significance_sex_function <- 0
  }
  
  # pronoun
  if (pvalue_sex_pronoun < 0.001) {
    significance_sex_pronoun <- 3
  } else if (pvalue_sex_pronoun < 0.01) {
    significance_sex_pronoun <- 2
  } else if (pvalue_sex_pronoun < 0.05) {
    significance_sex_pronoun <- 1
  } else {
    significance_sex_pronoun <- 0
  }
  
  # ppron
  if (pvalue_sex_ppron < 0.001) {
    significance_sex_ppron <- 3
  } else if (pvalue_sex_ppron < 0.01) {
    significance_sex_ppron <- 2
  } else if (pvalue_sex_ppron < 0.05) {
    significance_sex_ppron <- 1
  } else {
    significance_sex_ppron <- 0
  }
  
  # i
  if (pvalue_sex_i < 0.001) {
    significance_sex_i <- 3
  } else if (pvalue_sex_i < 0.01) {
    significance_sex_i <- 2
  } else if (pvalue_sex_i < 0.05) {
    significance_sex_i <- 1
  } else {
    significance_sex_i <- 0
  }
  
  # we
  if (pvalue_sex_we < 0.001) {
    significance_sex_we <- 3
  } else if (pvalue_sex_we < 0.01) {
    significance_sex_we <- 2
  } else if (pvalue_sex_we < 0.05) {
    significance_sex_we <- 1
  } else {
    significance_sex_we <- 0
  }
  
  # you
  if (pvalue_sex_you < 0.001) {
    significance_sex_you <- 3
  } else if (pvalue_sex_you < 0.01) {
    significance_sex_you <- 2
  } else if (pvalue_sex_you < 0.05) {
    significance_sex_you <- 1
  } else {
    significance_sex_you <- 0
  }
  
  # shehe
  if (pvalue_sex_shehe < 0.001) {
    significance_sex_shehe <- 3
  } else if (pvalue_sex_shehe < 0.01) {
    significance_sex_shehe <- 2
  } else if (pvalue_sex_shehe < 0.05) {
    significance_sex_shehe <- 1
  } else {
    significance_sex_shehe <- 0
  }
  
  # they
  if (pvalue_sex_they < 0.001) {
    significance_sex_they <- 3
  } else if (pvalue_sex_they < 0.01) {
    significance_sex_they <- 2
  } else if (pvalue_sex_they < 0.05) {
    significance_sex_they <- 1
  } else {
    significance_sex_they <- 0
  }
  
  # Affect
  if (pvalue_sex_Affect < 0.001) {
    significance_sex_Affect <- 3
  } else if (pvalue_sex_Affect < 0.01) {
    significance_sex_Affect <- 2
  } else if (pvalue_sex_Affect < 0.05) {
    significance_sex_Affect <- 1
  } else {
    significance_sex_Affect <- 0
  }
  
  # tone_pos
  if (pvalue_sex_tone_pos < 0.001) {
    significance_sex_tone_pos <- 3
  } else if (pvalue_sex_tone_pos < 0.01) {
    significance_sex_tone_pos <- 2
  } else if (pvalue_sex_tone_pos < 0.05) {
    significance_sex_tone_pos <- 1
  } else {
    significance_sex_tone_pos <- 0
  }
  
  # tone_neg
  if (pvalue_sex_tone_neg < 0.001) {
    significance_sex_tone_neg <- 3
  } else if (pvalue_sex_tone_neg < 0.01) {
    significance_sex_tone_neg <- 2
  } else if (pvalue_sex_tone_neg < 0.05) {
    significance_sex_tone_neg <- 1
  } else {
    significance_sex_tone_neg <- 0
  }
  
  # emotion
  if (pvalue_sex_emotion < 0.001) {
    significance_sex_emotion <- 3
  } else if (pvalue_sex_emotion < 0.01) {
    significance_sex_emotion <- 2
  } else if (pvalue_sex_emotion < 0.05) {
    significance_sex_emotion <- 1
  } else {
    significance_sex_emotion <- 0
  }
  
  # emo_pos
  if (pvalue_sex_emo_pos < 0.001) {
    significance_sex_emo_pos <- 3
  } else if (pvalue_sex_emo_pos < 0.01) {
    significance_sex_emo_pos <- 2
  } else if (pvalue_sex_emo_pos < 0.05) {
    significance_sex_emo_pos <- 1
  } else {
    significance_sex_emo_pos <- 0
  }
  
  # emo_neg
  if (pvalue_sex_emo_neg < 0.001) {
    significance_sex_emo_neg <- 3
  } else if (pvalue_sex_emo_neg < 0.01) {
    significance_sex_emo_neg <- 2
  } else if (pvalue_sex_emo_neg < 0.05) {
    significance_sex_emo_neg <- 1
  } else {
    significance_sex_emo_neg <- 0
  }
  
  # emo_anx
  if (pvalue_sex_emo_anx < 0.001) {
    significance_sex_emo_anx <- 3
  } else if (pvalue_sex_emo_anx < 0.01) {
    significance_sex_emo_anx <- 2
  } else if (pvalue_sex_emo_anx < 0.05) {
    significance_sex_emo_anx <- 1
  } else {
    significance_sex_emo_anx <- 0
  }
  
  # emo_anger
  if (pvalue_sex_emo_anger < 0.001) {
    significance_sex_emo_anger <- 3
  } else if (pvalue_sex_emo_anger < 0.01) {
    significance_sex_emo_anger <- 2
  } else if (pvalue_sex_emo_anger < 0.05) {
    significance_sex_emo_anger <- 1
  } else {
    significance_sex_emo_anger <- 0
  }
  
  # emo_sad
  if (pvalue_sex_emo_sad < 0.001) {
    significance_sex_emo_sad <- 3
  } else if (pvalue_sex_emo_sad < 0.01) {
    significance_sex_emo_sad <- 2
  } else if (pvalue_sex_emo_sad < 0.05) {
    significance_sex_emo_sad <- 1
  } else {
    significance_sex_emo_sad <- 0
  }
  
  # Social
  if (pvalue_sex_Social < 0.001) {
    significance_sex_Social <- 3
  } else if (pvalue_sex_Social < 0.01) {
    significance_sex_Social <- 2
  } else if (pvalue_sex_Social < 0.05) {
    significance_sex_Social <- 1
  } else {
    significance_sex_Social <- 0
  }
  
  # socbehav
  if (pvalue_sex_socbehav < 0.001) {
    significance_sex_socbehav <- 3
  } else if (pvalue_sex_socbehav < 0.01) {
    significance_sex_socbehav <- 2
  } else if (pvalue_sex_socbehav < 0.05) {
    significance_sex_socbehav <- 1
  } else {
    significance_sex_socbehav <- 0
  }
  
  # prosocial
  if (pvalue_sex_prosocial < 0.001) {
    significance_sex_prosocial <- 3
  } else if (pvalue_sex_prosocial < 0.01) {
    significance_sex_prosocial <- 2
  } else if (pvalue_sex_prosocial < 0.05) {
    significance_sex_prosocial <- 1
  } else {
    significance_sex_prosocial <- 0
  }
  
  # polite
  if (pvalue_sex_polite < 0.001) {
    significance_sex_polite <- 3
  } else if (pvalue_sex_polite < 0.01) {
    significance_sex_polite <- 2
  } else if (pvalue_sex_polite < 0.05) {
    significance_sex_polite <- 1
  } else {
    significance_sex_polite <- 0
  }
  
  # conflict
  if (pvalue_sex_conflict < 0.001) {
    significance_sex_conflict <- 3
  } else if (pvalue_sex_conflict < 0.01) {
    significance_sex_conflict <- 2
  } else if (pvalue_sex_conflict < 0.05) {
    significance_sex_conflict <- 1
  } else {
    significance_sex_conflict <- 0
  }
  
  # moral
  if (pvalue_sex_moral < 0.001) {
    significance_sex_moral <- 3
  } else if (pvalue_sex_moral < 0.01) {
    significance_sex_moral <- 2
  } else if (pvalue_sex_moral < 0.05) {
    significance_sex_moral <- 1
  } else {
    significance_sex_moral <- 0
  }
  
  # comm
  if (pvalue_sex_comm < 0.001) {
    significance_sex_comm <- 3
  } else if (pvalue_sex_comm < 0.01) {
    significance_sex_comm <- 2
  } else if (pvalue_sex_comm < 0.05) {
    significance_sex_comm <- 1
  } else {
    significance_sex_comm <- 0
  }
  
  # socrefs
  if (pvalue_sex_socrefs < 0.001) {
    significance_sex_socrefs <- 3
  } else if (pvalue_sex_socrefs < 0.01) {
    significance_sex_socrefs <- 2
  } else if (pvalue_sex_socrefs < 0.05) {
    significance_sex_socrefs <- 1
  } else {
    significance_sex_socrefs <- 0
  }
  
  # family
  if (pvalue_sex_family < 0.001) {
    significance_sex_family <- 3
  } else if (pvalue_sex_family < 0.01) {
    significance_sex_family <- 2
  } else if (pvalue_sex_family < 0.05) {
    significance_sex_family <- 1
  } else {
    significance_sex_family <- 0
  }
  
  # friend
  if (pvalue_sex_friend < 0.001) {
    significance_sex_friend <- 3
  } else if (pvalue_sex_friend < 0.01) {
    significance_sex_friend <- 2
  } else if (pvalue_sex_friend < 0.05) {
    significance_sex_friend <- 1
  } else {
    significance_sex_friend <- 0
  }
  
  # female
  if (pvalue_sex_female < 0.001) {
    significance_sex_female <- 3
  } else if (pvalue_sex_female < 0.01) {
    significance_sex_female <- 2
  } else if (pvalue_sex_female < 0.05) {
    significance_sex_female <- 1
  } else {
    significance_sex_female <- 0
  }
  
  # male
  if (pvalue_sex_male < 0.001) {
    significance_sex_male <- 3
  } else if (pvalue_sex_male < 0.01) {
    significance_sex_male <- 2
  } else if (pvalue_sex_male < 0.05) {
    significance_sex_male <- 1
  } else {
    significance_sex_male <- 0
  }
  
  # health
  if (pvalue_sex_health < 0.001) {
    significance_sex_health <- 3
  } else if (pvalue_sex_health < 0.01) {
    significance_sex_health <- 2
  } else if (pvalue_sex_health < 0.05) {
    significance_sex_health <- 1
  } else {
    significance_sex_health <- 0
  }
  
  # illness
  if (pvalue_sex_illness < 0.001) {
    significance_sex_illness <- 3
  } else if (pvalue_sex_illness < 0.01) {
    significance_sex_illness <- 2
  } else if (pvalue_sex_illness < 0.05) {
    significance_sex_illness <- 1
  } else {
    significance_sex_illness <- 0
  }
  
  # wellness
  if (pvalue_sex_wellness < 0.001) {
    significance_sex_wellness <- 3
  } else if (pvalue_sex_wellness < 0.01) {
    significance_sex_wellness <- 2
  } else if (pvalue_sex_wellness < 0.05) {
    significance_sex_wellness <- 1
  } else {
    significance_sex_wellness <- 0
  }
  
  # mental
  if (pvalue_sex_mental < 0.001) {
    significance_sex_mental <- 3
  } else if (pvalue_sex_mental < 0.01) {
    significance_sex_mental <- 2
  } else if (pvalue_sex_mental < 0.05) {
    significance_sex_mental <- 1
  } else {
    significance_sex_mental <- 0
  }
  
  # need
  if (pvalue_sex_need < 0.001) {
    significance_sex_need <- 3
  } else if (pvalue_sex_need < 0.01) {
    significance_sex_need <- 2
  } else if (pvalue_sex_need < 0.05) {
    significance_sex_need <- 1
  } else {
    significance_sex_need <- 0
  }
  
  # want
  if (pvalue_sex_want < 0.001) {
    significance_sex_want <- 3
  } else if (pvalue_sex_want < 0.01) {
    significance_sex_want <- 2
  } else if (pvalue_sex_want < 0.05) {
    significance_sex_want <- 1
  } else {
    significance_sex_want <- 0
  }
  
  # acquire
  if (pvalue_sex_acquire < 0.001) {
    significance_sex_acquire <- 3
  } else if (pvalue_sex_acquire < 0.01) {
    significance_sex_acquire <- 2
  } else if (pvalue_sex_acquire < 0.05) {
    significance_sex_acquire <- 1
  } else {
    significance_sex_acquire <- 0
  }
  
  # lack
  if (pvalue_sex_lack < 0.001) {
    significance_sex_lack <- 3
  } else if (pvalue_sex_lack < 0.01) {
    significance_sex_lack <- 2
  } else if (pvalue_sex_lack < 0.05) {
    significance_sex_lack <- 1
  } else {
    significance_sex_lack <- 0
  }
  
  # fulfill
  if (pvalue_sex_fulfill < 0.001) {
    significance_sex_fulfill <- 3
  } else if (pvalue_sex_fulfill < 0.01) {
    significance_sex_fulfill <- 2
  } else if (pvalue_sex_fulfill < 0.05) {
    significance_sex_fulfill <- 1
  } else {
    significance_sex_fulfill <- 0
  }
  
  # fatigue
  if (pvalue_sex_fatigue < 0.001) {
    significance_sex_fatigue <- 3
  } else if (pvalue_sex_fatigue < 0.01) {
    significance_sex_fatigue <- 2
  } else if (pvalue_sex_fatigue < 0.05) {
    significance_sex_fatigue <- 1
  } else {
    significance_sex_fatigue <- 0
  }
  
}

# age
if (A > 1) {
  # WC
  if (pvalue_age_WC < 0.001) {
    significance_age_WC <- 3
  } else if (pvalue_age_WC < 0.01) {
    significance_age_WC <- 2
  } else if (pvalue_age_WC < 0.05) {
    significance_age_WC <- 1
  } else {
    significance_age_WC <- 0
  }
  
  # Analytic
  if (pvalue_age_Analytic < 0.001) {
    significance_age_Analytic <- 3
  } else if (pvalue_age_Analytic < 0.01) {
    significance_age_Analytic <- 2
  } else if (pvalue_age_Analytic < 0.05) {
    significance_age_Analytic <- 1
  } else {
    significance_age_Analytic <- 0
  }
  
  # Clout
  if (pvalue_age_Clout < 0.001) {
    significance_age_Clout <- 3
  } else if (pvalue_age_Clout < 0.01) {
    significance_age_Clout <- 2
  } else if (pvalue_age_Clout < 0.05) {
    significance_age_Clout <- 1
  } else {
    significance_age_Clout <- 0
  }
  
  # Authentic
  if (pvalue_age_Authentic < 0.001) {
    significance_age_Authentic <- 3
  } else if (pvalue_age_Authentic < 0.01) {
    significance_age_Authentic <- 2
  } else if (pvalue_age_Authentic < 0.05) {
    significance_age_Authentic <- 1
  } else {
    significance_age_Authentic <- 0
  }
  
  # Tone
  if (pvalue_age_Tone < 0.001) {
    significance_age_Tone <- 3
  } else if (pvalue_age_Tone < 0.01) {
    significance_age_Tone <- 2
  } else if (pvalue_age_Tone < 0.05) {
    significance_age_Tone <- 1
  } else {
    significance_age_Tone <- 0
  }
  
  # BigWords
  if (pvalue_age_BigWords < 0.001) {
    significance_age_BigWords <- 3
  } else if (pvalue_age_BigWords < 0.01) {
    significance_age_BigWords <- 2
  } else if (pvalue_age_BigWords < 0.05) {
    significance_age_BigWords <- 1
  } else {
    significance_age_BigWords <- 0
  }
  
  # Dic
  if (pvalue_age_Dic < 0.001) {
    significance_age_Dic <- 3
  } else if (pvalue_age_Dic < 0.01) {
    significance_age_Dic <- 2
  } else if (pvalue_age_Dic < 0.05) {
    significance_age_Dic <- 1
  } else {
    significance_age_Dic <- 0
  }
  
  # Linguistic
  if (pvalue_age_Linguistic < 0.001) {
    significance_age_Linguistic <- 3
  } else if (pvalue_age_Linguistic < 0.01) {
    significance_age_Linguistic <- 2
  } else if (pvalue_age_Linguistic < 0.05) {
    significance_age_Linguistic <- 1
  } else {
    significance_age_Linguistic <- 0
  }
  
  # function
  if (pvalue_age_function < 0.001) {
    significance_age_function <- 3
  } else if (pvalue_age_function < 0.01) {
    significance_age_function <- 2
  } else if (pvalue_age_function < 0.05) {
    significance_age_function <- 1
  } else {
    significance_age_function <- 0
  }
  
  # pronoun
  if (pvalue_age_pronoun < 0.001) {
    significance_age_pronoun <- 3
  } else if (pvalue_age_pronoun < 0.01) {
    significance_age_pronoun <- 2
  } else if (pvalue_age_pronoun < 0.05) {
    significance_age_pronoun <- 1
  } else {
    significance_age_pronoun <- 0
  }
  
  # ppron
  if (pvalue_age_ppron < 0.001) {
    significance_age_ppron <- 3
  } else if (pvalue_age_ppron < 0.01) {
    significance_age_ppron <- 2
  } else if (pvalue_age_ppron < 0.05) {
    significance_age_ppron <- 1
  } else {
    significance_age_ppron <- 0
  }
  
  # i
  if (pvalue_age_i < 0.001) {
    significance_age_i <- 3
  } else if (pvalue_age_i < 0.01) {
    significance_age_i <- 2
  } else if (pvalue_age_i < 0.05) {
    significance_age_i <- 1
  } else {
    significance_age_i <- 0
  }
  
  # we
  if (pvalue_age_we < 0.001) {
    significance_age_we <- 3
  } else if (pvalue_age_we < 0.01) {
    significance_age_we <- 2
  } else if (pvalue_age_we < 0.05) {
    significance_age_we <- 1
  } else {
    significance_age_we <- 0
  }
  
  # you
  if (pvalue_age_you < 0.001) {
    significance_age_you <- 3
  } else if (pvalue_age_you < 0.01) {
    significance_age_you <- 2
  } else if (pvalue_age_you < 0.05) {
    significance_age_you <- 1
  } else {
    significance_age_you <- 0
  }
  
  # shehe
  if (pvalue_age_shehe < 0.001) {
    significance_age_shehe <- 3
  } else if (pvalue_age_shehe < 0.01) {
    significance_age_shehe <- 2
  } else if (pvalue_age_shehe < 0.05) {
    significance_age_shehe <- 1
  } else {
    significance_age_shehe <- 0
  }
  
  # they
  if (pvalue_age_they < 0.001) {
    significance_age_they <- 3
  } else if (pvalue_age_they < 0.01) {
    significance_age_they <- 2
  } else if (pvalue_age_they < 0.05) {
    significance_age_they <- 1
  } else {
    significance_age_they <- 0
  }
  
  # Affect
  if (pvalue_age_Affect < 0.001) {
    significance_age_Affect <- 3
  } else if (pvalue_age_Affect < 0.01) {
    significance_age_Affect <- 2
  } else if (pvalue_age_Affect < 0.05) {
    significance_age_Affect <- 1
  } else {
    significance_age_Affect <- 0
  }
  
  # tone_pos
  if (pvalue_age_tone_pos < 0.001) {
    significance_age_tone_pos <- 3
  } else if (pvalue_age_tone_pos < 0.01) {
    significance_age_tone_pos <- 2
  } else if (pvalue_age_tone_pos < 0.05) {
    significance_age_tone_pos <- 1
  } else {
    significance_age_tone_pos <- 0
  }
  
  # tone_neg
  if (pvalue_age_tone_neg < 0.001) {
    significance_age_tone_neg <- 3
  } else if (pvalue_age_tone_neg < 0.01) {
    significance_age_tone_neg <- 2
  } else if (pvalue_age_tone_neg < 0.05) {
    significance_age_tone_neg <- 1
  } else {
    significance_age_tone_neg <- 0
  }
  
  # emotion
  if (pvalue_age_emotion < 0.001) {
    significance_age_emotion <- 3
  } else if (pvalue_age_emotion < 0.01) {
    significance_age_emotion <- 2
  } else if (pvalue_age_emotion < 0.05) {
    significance_age_emotion <- 1
  } else {
    significance_age_emotion <- 0
  }
  
  # emo_pos
  if (pvalue_age_emo_pos < 0.001) {
    significance_age_emo_pos <- 3
  } else if (pvalue_age_emo_pos < 0.01) {
    significance_age_emo_pos <- 2
  } else if (pvalue_age_emo_pos < 0.05) {
    significance_age_emo_pos <- 1
  } else {
    significance_age_emo_pos <- 0
  }
  
  # emo_neg
  if (pvalue_age_emo_neg < 0.001) {
    significance_age_emo_neg <- 3
  } else if (pvalue_age_emo_neg < 0.01) {
    significance_age_emo_neg <- 2
  } else if (pvalue_age_emo_neg < 0.05) {
    significance_age_emo_neg <- 1
  } else {
    significance_age_emo_neg <- 0
  }
  
  # emo_anx
  if (pvalue_age_emo_anx < 0.001) {
    significance_age_emo_anx <- 3
  } else if (pvalue_age_emo_anx < 0.01) {
    significance_age_emo_anx <- 2
  } else if (pvalue_age_emo_anx < 0.05) {
    significance_age_emo_anx <- 1
  } else {
    significance_age_emo_anx <- 0
  }
  
  # emo_anger
  if (pvalue_age_emo_anger < 0.001) {
    significance_age_emo_anger <- 3
  } else if (pvalue_age_emo_anger < 0.01) {
    significance_age_emo_anger <- 2
  } else if (pvalue_age_emo_anger < 0.05) {
    significance_age_emo_anger <- 1
  } else {
    significance_age_emo_anger <- 0
  }
  
  # emo_sad
  if (pvalue_age_emo_sad < 0.001) {
    significance_age_emo_sad <- 3
  } else if (pvalue_age_emo_sad < 0.01) {
    significance_age_emo_sad <- 2
  } else if (pvalue_age_emo_sad < 0.05) {
    significance_age_emo_sad <- 1
  } else {
    significance_age_emo_sad <- 0
  }
  
  # Social
  if (pvalue_age_Social < 0.001) {
    significance_age_Social <- 3
  } else if (pvalue_age_Social < 0.01) {
    significance_age_Social <- 2
  } else if (pvalue_age_Social < 0.05) {
    significance_age_Social <- 1
  } else {
    significance_age_Social <- 0
  }
  
  # socbehav
  if (pvalue_age_socbehav < 0.001) {
    significance_age_socbehav <- 3
  } else if (pvalue_age_socbehav < 0.01) {
    significance_age_socbehav <- 2
  } else if (pvalue_age_socbehav < 0.05) {
    significance_age_socbehav <- 1
  } else {
    significance_age_socbehav <- 0
  }
  
  # prosocial
  if (pvalue_age_prosocial < 0.001) {
    significance_age_prosocial <- 3
  } else if (pvalue_age_prosocial < 0.01) {
    significance_age_prosocial <- 2
  } else if (pvalue_age_prosocial < 0.05) {
    significance_age_prosocial <- 1
  } else {
    significance_age_prosocial <- 0
  }
  
  # polite
  if (pvalue_age_polite < 0.001) {
    significance_age_polite <- 3
  } else if (pvalue_age_polite < 0.01) {
    significance_age_polite <- 2
  } else if (pvalue_age_polite < 0.05) {
    significance_age_polite <- 1
  } else {
    significance_age_polite <- 0
  }
  
  # conflict
  if (pvalue_age_conflict < 0.001) {
    significance_age_conflict <- 3
  } else if (pvalue_age_conflict < 0.01) {
    significance_age_conflict <- 2
  } else if (pvalue_age_conflict < 0.05) {
    significance_age_conflict <- 1
  } else {
    significance_age_conflict <- 0
  }
  
  # moral
  if (pvalue_age_moral < 0.001) {
    significance_age_moral <- 3
  } else if (pvalue_age_moral < 0.01) {
    significance_age_moral <- 2
  } else if (pvalue_age_moral < 0.05) {
    significance_age_moral <- 1
  } else {
    significance_age_moral <- 0
  }
  
  # comm
  if (pvalue_age_comm < 0.001) {
    significance_age_comm <- 3
  } else if (pvalue_age_comm < 0.01) {
    significance_age_comm <- 2
  } else if (pvalue_age_comm < 0.05) {
    significance_age_comm <- 1
  } else {
    significance_age_comm <- 0
  }
  
  # socrefs
  if (pvalue_age_socrefs < 0.001) {
    significance_age_socrefs <- 3
  } else if (pvalue_age_socrefs < 0.01) {
    significance_age_socrefs <- 2
  } else if (pvalue_age_socrefs < 0.05) {
    significance_age_socrefs <- 1
  } else {
    significance_age_socrefs <- 0
  }
  
  # family
  if (pvalue_age_family < 0.001) {
    significance_age_family <- 3
  } else if (pvalue_age_family < 0.01) {
    significance_age_family <- 2
  } else if (pvalue_age_family < 0.05) {
    significance_age_family <- 1
  } else {
    significance_age_family <- 0
  }
  
  # friend
  if (pvalue_age_friend < 0.001) {
    significance_age_friend <- 3
  } else if (pvalue_age_friend < 0.01) {
    significance_age_friend <- 2
  } else if (pvalue_age_friend < 0.05) {
    significance_age_friend <- 1
  } else {
    significance_age_friend <- 0
  }
  
  # female
  if (pvalue_age_female < 0.001) {
    significance_age_female <- 3
  } else if (pvalue_age_female < 0.01) {
    significance_age_female <- 2
  } else if (pvalue_age_female < 0.05) {
    significance_age_female <- 1
  } else {
    significance_age_female <- 0
  }
  
  # male
  if (pvalue_age_male < 0.001) {
    significance_age_male <- 3
  } else if (pvalue_age_male < 0.01) {
    significance_age_male <- 2
  } else if (pvalue_age_male < 0.05) {
    significance_age_male <- 1
  } else {
    significance_age_male <- 0
  }
  
  # health
  if (pvalue_age_health < 0.001) {
    significance_age_health <- 3
  } else if (pvalue_age_health < 0.01) {
    significance_age_health <- 2
  } else if (pvalue_age_health < 0.05) {
    significance_age_health <- 1
  } else {
    significance_age_health <- 0
  }
  
  # illness
  if (pvalue_age_illness < 0.001) {
    significance_age_illness <- 3
  } else if (pvalue_age_illness < 0.01) {
    significance_age_illness <- 2
  } else if (pvalue_age_illness < 0.05) {
    significance_age_illness <- 1
  } else {
    significance_age_illness <- 0
  }
  
  # wellness
  if (pvalue_age_wellness < 0.001) {
    significance_age_wellness <- 3
  } else if (pvalue_age_wellness < 0.01) {
    significance_age_wellness <- 2
  } else if (pvalue_age_wellness < 0.05) {
    significance_age_wellness <- 1
  } else {
    significance_age_wellness <- 0
  }
  
  # mental
  if (pvalue_age_mental < 0.001) {
    significance_age_mental <- 3
  } else if (pvalue_age_mental < 0.01) {
    significance_age_mental <- 2
  } else if (pvalue_age_mental < 0.05) {
    significance_age_mental <- 1
  } else {
    significance_age_mental <- 0
  }
  
  # need
  if (pvalue_age_need < 0.001) {
    significance_age_need <- 3
  } else if (pvalue_age_need < 0.01) {
    significance_age_need <- 2
  } else if (pvalue_age_need < 0.05) {
    significance_age_need <- 1
  } else {
    significance_age_need <- 0
  }
  
  # want
  if (pvalue_age_want < 0.001) {
    significance_age_want <- 3
  } else if (pvalue_age_want < 0.01) {
    significance_age_want <- 2
  } else if (pvalue_age_want < 0.05) {
    significance_age_want <- 1
  } else {
    significance_age_want <- 0
  }
  
  # acquire
  if (pvalue_age_acquire < 0.001) {
    significance_age_acquire <- 3
  } else if (pvalue_age_acquire < 0.01) {
    significance_age_acquire <- 2
  } else if (pvalue_age_acquire < 0.05) {
    significance_age_acquire <- 1
  } else {
    significance_age_acquire <- 0
  }
  
  # lack
  if (pvalue_age_lack < 0.001) {
    significance_age_lack <- 3
  } else if (pvalue_age_lack < 0.01) {
    significance_age_lack <- 2
  } else if (pvalue_age_lack < 0.05) {
    significance_age_lack <- 1
  } else {
    significance_age_lack <- 0
  }
  
  # fulfill
  if (pvalue_age_fulfill < 0.001) {
    significance_age_fulfill <- 3
  } else if (pvalue_age_fulfill < 0.01) {
    significance_age_fulfill <- 2
  } else if (pvalue_age_fulfill < 0.05) {
    significance_age_fulfill <- 1
  } else {
    significance_age_fulfill <- 0
  }
  
  # fatigue
  if (pvalue_age_fatigue < 0.001) {
    significance_age_fatigue <- 3
  } else if (pvalue_age_fatigue < 0.01) {
    significance_age_fatigue <- 2
  } else if (pvalue_age_fatigue < 0.05) {
    significance_age_fatigue <- 1
  } else {
    significance_age_fatigue <- 0
  }
  
}

# ses
if (A > 1) {
  # WC
  if (pvalue_ses_WC < 0.001) {
    significance_ses_WC <- 3
  } else if (pvalue_ses_WC < 0.01) {
    significance_ses_WC <- 2
  } else if (pvalue_ses_WC < 0.05) {
    significance_ses_WC <- 1
  } else {
    significance_ses_WC <- 0
  }
  
  # Analytic
  if (pvalue_ses_Analytic < 0.001) {
    significance_ses_Analytic <- 3
  } else if (pvalue_ses_Analytic < 0.01) {
    significance_ses_Analytic <- 2
  } else if (pvalue_ses_Analytic < 0.05) {
    significance_ses_Analytic <- 1
  } else {
    significance_ses_Analytic <- 0
  }
  
  # Clout
  if (pvalue_ses_Clout < 0.001) {
    significance_ses_Clout <- 3
  } else if (pvalue_ses_Clout < 0.01) {
    significance_ses_Clout <- 2
  } else if (pvalue_ses_Clout < 0.05) {
    significance_ses_Clout <- 1
  } else {
    significance_ses_Clout <- 0
  }
  
  # Authentic
  if (pvalue_ses_Authentic < 0.001) {
    significance_ses_Authentic <- 3
  } else if (pvalue_ses_Authentic < 0.01) {
    significance_ses_Authentic <- 2
  } else if (pvalue_ses_Authentic < 0.05) {
    significance_ses_Authentic <- 1
  } else {
    significance_ses_Authentic <- 0
  }
  
  # Tone
  if (pvalue_ses_Tone < 0.001) {
    significance_ses_Tone <- 3
  } else if (pvalue_ses_Tone < 0.01) {
    significance_ses_Tone <- 2
  } else if (pvalue_ses_Tone < 0.05) {
    significance_ses_Tone <- 1
  } else {
    significance_ses_Tone <- 0
  }
  
  # BigWords
  if (pvalue_ses_BigWords < 0.001) {
    significance_ses_BigWords <- 3
  } else if (pvalue_ses_BigWords < 0.01) {
    significance_ses_BigWords <- 2
  } else if (pvalue_ses_BigWords < 0.05) {
    significance_ses_BigWords <- 1
  } else {
    significance_ses_BigWords <- 0
  }
  
  # Dic
  if (pvalue_ses_Dic < 0.001) {
    significance_ses_Dic <- 3
  } else if (pvalue_ses_Dic < 0.01) {
    significance_ses_Dic <- 2
  } else if (pvalue_ses_Dic < 0.05) {
    significance_ses_Dic <- 1
  } else {
    significance_ses_Dic <- 0
  }
  
  # Linguistic
  if (pvalue_ses_Linguistic < 0.001) {
    significance_ses_Linguistic <- 3
  } else if (pvalue_ses_Linguistic < 0.01) {
    significance_ses_Linguistic <- 2
  } else if (pvalue_ses_Linguistic < 0.05) {
    significance_ses_Linguistic <- 1
  } else {
    significance_ses_Linguistic <- 0
  }
  
  # function
  if (pvalue_ses_function < 0.001) {
    significance_ses_function <- 3
  } else if (pvalue_ses_function < 0.01) {
    significance_ses_function <- 2
  } else if (pvalue_ses_function < 0.05) {
    significance_ses_function <- 1
  } else {
    significance_ses_function <- 0
  }
  
  # pronoun
  if (pvalue_ses_pronoun < 0.001) {
    significance_ses_pronoun <- 3
  } else if (pvalue_ses_pronoun < 0.01) {
    significance_ses_pronoun <- 2
  } else if (pvalue_ses_pronoun < 0.05) {
    significance_ses_pronoun <- 1
  } else {
    significance_ses_pronoun <- 0
  }
  
  # ppron
  if (pvalue_ses_ppron < 0.001) {
    significance_ses_ppron <- 3
  } else if (pvalue_ses_ppron < 0.01) {
    significance_ses_ppron <- 2
  } else if (pvalue_ses_ppron < 0.05) {
    significance_ses_ppron <- 1
  } else {
    significance_ses_ppron <- 0
  }
  
  # i
  if (pvalue_ses_i < 0.001) {
    significance_ses_i <- 3
  } else if (pvalue_ses_i < 0.01) {
    significance_ses_i <- 2
  } else if (pvalue_ses_i < 0.05) {
    significance_ses_i <- 1
  } else {
    significance_ses_i <- 0
  }
  
  # we
  if (pvalue_ses_we < 0.001) {
    significance_ses_we <- 3
  } else if (pvalue_ses_we < 0.01) {
    significance_ses_we <- 2
  } else if (pvalue_ses_we < 0.05) {
    significance_ses_we <- 1
  } else {
    significance_ses_we <- 0
  }
  
  # you
  if (pvalue_ses_you < 0.001) {
    significance_ses_you <- 3
  } else if (pvalue_ses_you < 0.01) {
    significance_ses_you <- 2
  } else if (pvalue_ses_you < 0.05) {
    significance_ses_you <- 1
  } else {
    significance_ses_you <- 0
  }
  
  # shehe
  if (pvalue_ses_shehe < 0.001) {
    significance_ses_shehe <- 3
  } else if (pvalue_ses_shehe < 0.01) {
    significance_ses_shehe <- 2
  } else if (pvalue_ses_shehe < 0.05) {
    significance_ses_shehe <- 1
  } else {
    significance_ses_shehe <- 0
  }
  
  # they
  if (pvalue_ses_they < 0.001) {
    significance_ses_they <- 3
  } else if (pvalue_ses_they < 0.01) {
    significance_ses_they <- 2
  } else if (pvalue_ses_they < 0.05) {
    significance_ses_they <- 1
  } else {
    significance_ses_they <- 0
  }
  
  # Affect
  if (pvalue_ses_Affect < 0.001) {
    significance_ses_Affect <- 3
  } else if (pvalue_ses_Affect < 0.01) {
    significance_ses_Affect <- 2
  } else if (pvalue_ses_Affect < 0.05) {
    significance_ses_Affect <- 1
  } else {
    significance_ses_Affect <- 0
  }
  
  # tone_pos
  if (pvalue_ses_tone_pos < 0.001) {
    significance_ses_tone_pos <- 3
  } else if (pvalue_ses_tone_pos < 0.01) {
    significance_ses_tone_pos <- 2
  } else if (pvalue_ses_tone_pos < 0.05) {
    significance_ses_tone_pos <- 1
  } else {
    significance_ses_tone_pos <- 0
  }
  
  # tone_neg
  if (pvalue_ses_tone_neg < 0.001) {
    significance_ses_tone_neg <- 3
  } else if (pvalue_ses_tone_neg < 0.01) {
    significance_ses_tone_neg <- 2
  } else if (pvalue_ses_tone_neg < 0.05) {
    significance_ses_tone_neg <- 1
  } else {
    significance_ses_tone_neg <- 0
  }
  
  # emotion
  if (pvalue_ses_emotion < 0.001) {
    significance_ses_emotion <- 3
  } else if (pvalue_ses_emotion < 0.01) {
    significance_ses_emotion <- 2
  } else if (pvalue_ses_emotion < 0.05) {
    significance_ses_emotion <- 1
  } else {
    significance_ses_emotion <- 0
  }
  
  # emo_pos
  if (pvalue_ses_emo_pos < 0.001) {
    significance_ses_emo_pos <- 3
  } else if (pvalue_ses_emo_pos < 0.01) {
    significance_ses_emo_pos <- 2
  } else if (pvalue_ses_emo_pos < 0.05) {
    significance_ses_emo_pos <- 1
  } else {
    significance_ses_emo_pos <- 0
  }
  
  # emo_neg
  if (pvalue_ses_emo_neg < 0.001) {
    significance_ses_emo_neg <- 3
  } else if (pvalue_ses_emo_neg < 0.01) {
    significance_ses_emo_neg <- 2
  } else if (pvalue_ses_emo_neg < 0.05) {
    significance_ses_emo_neg <- 1
  } else {
    significance_ses_emo_neg <- 0
  }
  
  # emo_anx
  if (pvalue_ses_emo_anx < 0.001) {
    significance_ses_emo_anx <- 3
  } else if (pvalue_ses_emo_anx < 0.01) {
    significance_ses_emo_anx <- 2
  } else if (pvalue_ses_emo_anx < 0.05) {
    significance_ses_emo_anx <- 1
  } else {
    significance_ses_emo_anx <- 0
  }
  
  # emo_anger
  if (pvalue_ses_emo_anger < 0.001) {
    significance_ses_emo_anger <- 3
  } else if (pvalue_ses_emo_anger < 0.01) {
    significance_ses_emo_anger <- 2
  } else if (pvalue_ses_emo_anger < 0.05) {
    significance_ses_emo_anger <- 1
  } else {
    significance_ses_emo_anger <- 0
  }
  
  # emo_sad
  if (pvalue_ses_emo_sad < 0.001) {
    significance_ses_emo_sad <- 3
  } else if (pvalue_ses_emo_sad < 0.01) {
    significance_ses_emo_sad <- 2
  } else if (pvalue_ses_emo_sad < 0.05) {
    significance_ses_emo_sad <- 1
  } else {
    significance_ses_emo_sad <- 0
  }
  
  # Social
  if (pvalue_ses_Social < 0.001) {
    significance_ses_Social <- 3
  } else if (pvalue_ses_Social < 0.01) {
    significance_ses_Social <- 2
  } else if (pvalue_ses_Social < 0.05) {
    significance_ses_Social <- 1
  } else {
    significance_ses_Social <- 0
  }
  
  # socbehav
  if (pvalue_ses_socbehav < 0.001) {
    significance_ses_socbehav <- 3
  } else if (pvalue_ses_socbehav < 0.01) {
    significance_ses_socbehav <- 2
  } else if (pvalue_ses_socbehav < 0.05) {
    significance_ses_socbehav <- 1
  } else {
    significance_ses_socbehav <- 0
  }
  
  # prosocial
  if (pvalue_ses_prosocial < 0.001) {
    significance_ses_prosocial <- 3
  } else if (pvalue_ses_prosocial < 0.01) {
    significance_ses_prosocial <- 2
  } else if (pvalue_ses_prosocial < 0.05) {
    significance_ses_prosocial <- 1
  } else {
    significance_ses_prosocial <- 0
  }
  
  # polite
  if (pvalue_ses_polite < 0.001) {
    significance_ses_polite <- 3
  } else if (pvalue_ses_polite < 0.01) {
    significance_ses_polite <- 2
  } else if (pvalue_ses_polite < 0.05) {
    significance_ses_polite <- 1
  } else {
    significance_ses_polite <- 0
  }
  
  # conflict
  if (pvalue_ses_conflict < 0.001) {
    significance_ses_conflict <- 3
  } else if (pvalue_ses_conflict < 0.01) {
    significance_ses_conflict <- 2
  } else if (pvalue_ses_conflict < 0.05) {
    significance_ses_conflict <- 1
  } else {
    significance_ses_conflict <- 0
  }
  
  # moral
  if (pvalue_ses_moral < 0.001) {
    significance_ses_moral <- 3
  } else if (pvalue_ses_moral < 0.01) {
    significance_ses_moral <- 2
  } else if (pvalue_ses_moral < 0.05) {
    significance_ses_moral <- 1
  } else {
    significance_ses_moral <- 0
  }
  
  # comm
  if (pvalue_ses_comm < 0.001) {
    significance_ses_comm <- 3
  } else if (pvalue_ses_comm < 0.01) {
    significance_ses_comm <- 2
  } else if (pvalue_ses_comm < 0.05) {
    significance_ses_comm <- 1
  } else {
    significance_ses_comm <- 0
  }
  
  # socrefs
  if (pvalue_ses_socrefs < 0.001) {
    significance_ses_socrefs <- 3
  } else if (pvalue_ses_socrefs < 0.01) {
    significance_ses_socrefs <- 2
  } else if (pvalue_ses_socrefs < 0.05) {
    significance_ses_socrefs <- 1
  } else {
    significance_ses_socrefs <- 0
  }
  
  # family
  if (pvalue_ses_family < 0.001) {
    significance_ses_family <- 3
  } else if (pvalue_ses_family < 0.01) {
    significance_ses_family <- 2
  } else if (pvalue_ses_family < 0.05) {
    significance_ses_family <- 1
  } else {
    significance_ses_family <- 0
  }
  
  # friend
  if (pvalue_ses_friend < 0.001) {
    significance_ses_friend <- 3
  } else if (pvalue_ses_friend < 0.01) {
    significance_ses_friend <- 2
  } else if (pvalue_ses_friend < 0.05) {
    significance_ses_friend <- 1
  } else {
    significance_ses_friend <- 0
  }
  
  # female
  if (pvalue_ses_female < 0.001) {
    significance_ses_female <- 3
  } else if (pvalue_ses_female < 0.01) {
    significance_ses_female <- 2
  } else if (pvalue_ses_female < 0.05) {
    significance_ses_female <- 1
  } else {
    significance_ses_female <- 0
  }
  
  # male
  if (pvalue_ses_male < 0.001) {
    significance_ses_male <- 3
  } else if (pvalue_ses_male < 0.01) {
    significance_ses_male <- 2
  } else if (pvalue_ses_male < 0.05) {
    significance_ses_male <- 1
  } else {
    significance_ses_male <- 0
  }
  
  # health
  if (pvalue_ses_health < 0.001) {
    significance_ses_health <- 3
  } else if (pvalue_ses_health < 0.01) {
    significance_ses_health <- 2
  } else if (pvalue_ses_health < 0.05) {
    significance_ses_health <- 1
  } else {
    significance_ses_health <- 0
  }
  
  # illness
  if (pvalue_ses_illness < 0.001) {
    significance_ses_illness <- 3
  } else if (pvalue_ses_illness < 0.01) {
    significance_ses_illness <- 2
  } else if (pvalue_ses_illness < 0.05) {
    significance_ses_illness <- 1
  } else {
    significance_ses_illness <- 0
  }
  
  # wellness
  if (pvalue_ses_wellness < 0.001) {
    significance_ses_wellness <- 3
  } else if (pvalue_ses_wellness < 0.01) {
    significance_ses_wellness <- 2
  } else if (pvalue_ses_wellness < 0.05) {
    significance_ses_wellness <- 1
  } else {
    significance_ses_wellness <- 0
  }
  
  # mental
  if (pvalue_ses_mental < 0.001) {
    significance_ses_mental <- 3
  } else if (pvalue_ses_mental < 0.01) {
    significance_ses_mental <- 2
  } else if (pvalue_ses_mental < 0.05) {
    significance_ses_mental <- 1
  } else {
    significance_ses_mental <- 0
  }
  
  # need
  if (pvalue_ses_need < 0.001) {
    significance_ses_need <- 3
  } else if (pvalue_ses_need < 0.01) {
    significance_ses_need <- 2
  } else if (pvalue_ses_need < 0.05) {
    significance_ses_need <- 1
  } else {
    significance_ses_need <- 0
  }
  
  # want
  if (pvalue_ses_want < 0.001) {
    significance_ses_want <- 3
  } else if (pvalue_ses_want < 0.01) {
    significance_ses_want <- 2
  } else if (pvalue_ses_want < 0.05) {
    significance_ses_want <- 1
  } else {
    significance_ses_want <- 0
  }
  
  # acquire
  if (pvalue_ses_acquire < 0.001) {
    significance_ses_acquire <- 3
  } else if (pvalue_ses_acquire < 0.01) {
    significance_ses_acquire <- 2
  } else if (pvalue_ses_acquire < 0.05) {
    significance_ses_acquire <- 1
  } else {
    significance_ses_acquire <- 0
  }
  
  # lack
  if (pvalue_ses_lack < 0.001) {
    significance_ses_lack <- 3
  } else if (pvalue_ses_lack < 0.01) {
    significance_ses_lack <- 2
  } else if (pvalue_ses_lack < 0.05) {
    significance_ses_lack <- 1
  } else {
    significance_ses_lack <- 0
  }
  
  # fulfill
  if (pvalue_ses_fulfill < 0.001) {
    significance_ses_fulfill <- 3
  } else if (pvalue_ses_fulfill < 0.01) {
    significance_ses_fulfill <- 2
  } else if (pvalue_ses_fulfill < 0.05) {
    significance_ses_fulfill <- 1
  } else {
    significance_ses_fulfill <- 0
  }
  
  # fatigue
  if (pvalue_ses_fatigue < 0.001) {
    significance_ses_fatigue <- 3
  } else if (pvalue_ses_fatigue < 0.01) {
    significance_ses_fatigue <- 2
  } else if (pvalue_ses_fatigue < 0.05) {
    significance_ses_fatigue <- 1
  } else {
    significance_ses_fatigue <- 0
  }
  
}

}

# table of significance values
if (A > 1) {
  
significant_interactions <- data.frame(
  strength = c(significance_strength_WC, significance_strength_Analytic, significance_strength_Clout, significance_strength_Authentic, significance_strength_Tone, significance_strength_BigWords, significance_strength_Dic, significance_strength_Linguistic, significance_strength_function, significance_strength_pronoun, significance_strength_ppron, significance_strength_i, significance_strength_we, significance_strength_you, significance_strength_shehe, significance_strength_they, significance_strength_Affect, significance_strength_tone_pos, significance_strength_tone_neg, significance_strength_emotion, significance_strength_emo_pos, significance_strength_emo_neg, significance_strength_emo_anx, significance_strength_emo_anger, significance_strength_emo_sad, significance_strength_Social, significance_strength_socbehav, significance_strength_prosocial, significance_strength_polite, significance_strength_conflict, significance_strength_moral, significance_strength_comm, significance_strength_socrefs, significance_strength_family, significance_strength_friend, significance_strength_female, significance_strength_male, significance_strength_health, significance_strength_illness, significance_strength_wellness, significance_strength_mental, significance_strength_need, significance_strength_want, significance_strength_acquire, significance_strength_lack, significance_strength_fulfill, significance_strength_fatigue),
  justified = c(significance_justified_WC, significance_justified_Analytic, significance_justified_Clout, significance_justified_Authentic, significance_justified_Tone, significance_justified_BigWords, significance_justified_Dic, significance_justified_Linguistic, significance_justified_function, significance_justified_pronoun, significance_justified_ppron, significance_justified_i, significance_justified_we, significance_justified_you, significance_justified_shehe, significance_justified_they, significance_justified_Affect, significance_justified_tone_pos, significance_justified_tone_neg, significance_justified_emotion, significance_justified_emo_pos, significance_justified_emo_neg, significance_justified_emo_anx, significance_justified_emo_anger, significance_justified_emo_sad, significance_justified_Social, significance_justified_socbehav, significance_justified_prosocial, significance_justified_polite, significance_justified_conflict, significance_justified_moral, significance_justified_comm, significance_justified_socrefs, significance_justified_family, significance_justified_friend, significance_justified_female, significance_justified_male, significance_justified_health, significance_justified_illness, significance_justified_wellness, significance_justified_mental, significance_justified_need, significance_justified_want, significance_justified_acquire, significance_justified_lack, significance_justified_fulfill, significance_justified_fatigue),
  guilt = c(significance_guilt_WC, significance_guilt_Analytic, significance_guilt_Clout, significance_guilt_Authentic, significance_guilt_Tone, significance_guilt_BigWords, significance_guilt_Dic, significance_guilt_Linguistic, significance_guilt_function, significance_guilt_pronoun, significance_guilt_ppron, significance_guilt_i, significance_guilt_we, significance_guilt_you, significance_guilt_shehe, significance_guilt_they, significance_guilt_Affect, significance_guilt_tone_pos, significance_guilt_tone_neg, significance_guilt_emotion, significance_guilt_emo_pos, significance_guilt_emo_neg, significance_guilt_emo_anx, significance_guilt_emo_anger, significance_guilt_emo_sad, significance_guilt_Social, significance_guilt_socbehav, significance_guilt_prosocial, significance_guilt_polite, significance_guilt_conflict, significance_guilt_moral, significance_guilt_comm, significance_guilt_socrefs, significance_guilt_family, significance_guilt_friend, significance_guilt_female, significance_guilt_male, significance_guilt_health, significance_guilt_illness, significance_guilt_wellness, significance_guilt_mental, significance_guilt_need, significance_guilt_want, significance_guilt_acquire, significance_guilt_lack, significance_guilt_fulfill, significance_guilt_fatigue),
  depressed = c(significance_depressed_WC, significance_depressed_Analytic, significance_depressed_Clout, significance_depressed_Authentic, significance_depressed_Tone, significance_depressed_BigWords, significance_depressed_Dic, significance_depressed_Linguistic, significance_depressed_function, significance_depressed_pronoun, significance_depressed_ppron, significance_depressed_i, significance_depressed_we, significance_depressed_you, significance_depressed_shehe, significance_depressed_they, significance_depressed_Affect, significance_depressed_tone_pos, significance_depressed_tone_neg, significance_depressed_emotion, significance_depressed_emo_pos, significance_depressed_emo_neg, significance_depressed_emo_anx, significance_depressed_emo_anger, significance_depressed_emo_sad, significance_depressed_Social, significance_depressed_socbehav, significance_depressed_prosocial, significance_depressed_polite, significance_depressed_conflict, significance_depressed_moral, significance_depressed_comm, significance_depressed_socrefs, significance_depressed_family, significance_depressed_friend, significance_depressed_female, significance_depressed_male, significance_depressed_health, significance_depressed_illness, significance_depressed_wellness, significance_depressed_mental, significance_depressed_need, significance_depressed_want, significance_depressed_acquire, significance_depressed_lack, significance_depressed_fulfill, significance_depressed_fatigue),
  fulfill_1 = c(significance_fulfill_1_WC, significance_fulfill_1_Analytic, significance_fulfill_1_Clout, significance_fulfill_1_Authentic, significance_fulfill_1_Tone, significance_fulfill_1_BigWords, significance_fulfill_1_Dic, significance_fulfill_1_Linguistic, significance_fulfill_1_function, significance_fulfill_1_pronoun, significance_fulfill_1_ppron, significance_fulfill_1_i, significance_fulfill_1_we, significance_fulfill_1_you, significance_fulfill_1_shehe, significance_fulfill_1_they, significance_fulfill_1_Affect, significance_fulfill_1_tone_pos, significance_fulfill_1_tone_neg, significance_fulfill_1_emotion, significance_fulfill_1_emo_pos, significance_fulfill_1_emo_neg, significance_fulfill_1_emo_anx, significance_fulfill_1_emo_anger, significance_fulfill_1_emo_sad, significance_fulfill_1_Social, significance_fulfill_1_socbehav, significance_fulfill_1_prosocial, significance_fulfill_1_polite, significance_fulfill_1_conflict, significance_fulfill_1_moral, significance_fulfill_1_comm, significance_fulfill_1_socrefs, significance_fulfill_1_family, significance_fulfill_1_friend, significance_fulfill_1_female, significance_fulfill_1_male, significance_fulfill_1_health, significance_fulfill_1_illness, significance_fulfill_1_wellness, significance_fulfill_1_mental, significance_fulfill_1_need, significance_fulfill_1_want, significance_fulfill_1_acquire, significance_fulfill_1_lack, significance_fulfill_1_fulfill, significance_fulfill_1_fatigue),
  effort = c(significance_effort_WC, significance_effort_Analytic, significance_effort_Clout, significance_effort_Authentic, significance_effort_Tone, significance_effort_BigWords, significance_effort_Dic, significance_effort_Linguistic, significance_effort_function, significance_effort_pronoun, significance_effort_ppron, significance_effort_i, significance_effort_we, significance_effort_you, significance_effort_shehe, significance_effort_they, significance_effort_Affect, significance_effort_tone_pos, significance_effort_tone_neg, significance_effort_emotion, significance_effort_emo_pos, significance_effort_emo_neg, significance_effort_emo_anx, significance_effort_emo_anger, significance_effort_emo_sad, significance_effort_Social, significance_effort_socbehav, significance_effort_prosocial, significance_effort_polite, significance_effort_conflict, significance_effort_moral, significance_effort_comm, significance_effort_socrefs, significance_effort_family, significance_effort_friend, significance_effort_female, significance_effort_male, significance_effort_health, significance_effort_illness, significance_effort_wellness, significance_effort_mental, significance_effort_need, significance_effort_want, significance_effort_acquire, significance_effort_lack, significance_effort_fulfill, significance_effort_fatigue),
  sex = c(significance_sex_WC, significance_sex_Analytic, significance_sex_Clout, significance_sex_Authentic, significance_sex_Tone, significance_sex_BigWords, significance_sex_Dic, significance_sex_Linguistic, significance_sex_function, significance_sex_pronoun, significance_sex_ppron, significance_sex_i, significance_sex_we, significance_sex_you, significance_sex_shehe, significance_sex_they, significance_sex_Affect, significance_sex_tone_pos, significance_sex_tone_neg, significance_sex_emotion, significance_sex_emo_pos, significance_sex_emo_neg, significance_sex_emo_anx, significance_sex_emo_anger, significance_sex_emo_sad, significance_sex_Social, significance_sex_socbehav, significance_sex_prosocial, significance_sex_polite, significance_sex_conflict, significance_sex_moral, significance_sex_comm, significance_sex_socrefs, significance_sex_family, significance_sex_friend, significance_sex_female, significance_sex_male, significance_sex_health, significance_sex_illness, significance_sex_wellness, significance_sex_mental, significance_sex_need, significance_sex_want, significance_sex_acquire, significance_sex_lack, significance_sex_fulfill, significance_sex_fatigue),
  age = c(significance_age_WC, significance_age_Analytic, significance_age_Clout, significance_age_Authentic, significance_age_Tone, significance_age_BigWords, significance_age_Dic, significance_age_Linguistic, significance_age_function, significance_age_pronoun, significance_age_ppron, significance_age_i, significance_age_we, significance_age_you, significance_age_shehe, significance_age_they, significance_age_Affect, significance_age_tone_pos, significance_age_tone_neg, significance_age_emotion, significance_age_emo_pos, significance_age_emo_neg, significance_age_emo_anx, significance_age_emo_anger, significance_age_emo_sad, significance_age_Social, significance_age_socbehav, significance_age_prosocial, significance_age_polite, significance_age_conflict, significance_age_moral, significance_age_comm, significance_age_socrefs, significance_age_family, significance_age_friend, significance_age_female, significance_age_male, significance_age_health, significance_age_illness, significance_age_wellness, significance_age_mental, significance_age_need, significance_age_want, significance_age_acquire, significance_age_lack, significance_age_fulfill, significance_age_fatigue),
  ses = c(significance_ses_WC, significance_ses_Analytic, significance_ses_Clout, significance_ses_Authentic, significance_ses_Tone, significance_ses_BigWords, significance_ses_Dic, significance_ses_Linguistic, significance_ses_function, significance_ses_pronoun, significance_ses_ppron, significance_ses_i, significance_ses_we, significance_ses_you, significance_ses_shehe, significance_ses_they, significance_ses_Affect, significance_ses_tone_pos, significance_ses_tone_neg, significance_ses_emotion, significance_ses_emo_pos, significance_ses_emo_neg, significance_ses_emo_anx, significance_ses_emo_anger, significance_ses_emo_sad, significance_ses_Social, significance_ses_socbehav, significance_ses_prosocial, significance_ses_polite, significance_ses_conflict, significance_ses_moral, significance_ses_comm, significance_ses_socrefs, significance_ses_family, significance_ses_friend, significance_ses_female, significance_ses_male, significance_ses_health, significance_ses_illness, significance_ses_wellness, significance_ses_mental, significance_ses_need, significance_ses_want, significance_ses_acquire, significance_ses_lack, significance_ses_fulfill, significance_ses_fatigue)
)

rownames(significant_interactions) <- c("WC", "Analytic", "Clout", "Authentic", "Tone", "BigWords", "Dic", "Linguistic", "function", "pronoun", "ppron", "i", "we", "you", "shehe", "they", "Affect", "tone_pos", "tone_neg", "emotion", "emo_pos", "emo_neg", "emo_anx", "emo_anger", "emo_sad", "Social", "socbehav", "prosocial", "polite", "conflict", "moral", "comm", "socrefs", "family", "friend", "female", "male", "health", "illness", "wellness", "mental", "need", "want", "acquire", "lack", "fulfill", "fatigue")

}

View(significant_interactions)
write.csv(significant_interactions, file = "LIWCdictionaries_anova_significance_values.csv", row.names = T)

