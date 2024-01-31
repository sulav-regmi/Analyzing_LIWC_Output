testlist <- names(usa_strength_liwc)

print(testlist)
p_test <- summary(anovatest)[[1]][["Pr(>F)"]][[1]]


anova_strength_WC <- aov(strength ~ WC, data = usa_strength_liwc)
pvalue_strength_WC <- summary(anova_strength_WC)[[1]][["Pr(>F)"]][[1]]



if (pvalue_strength_WC < 0.001) {
  significance_strength_WC <- 3
} else if (pvalue_strength_WC <0.01) {
  significance_strength_WC <- 2
} else if (pvalue_strength_WC < 0.05) {
  significance_strength_WC <- 1
} else {
  significance_strength_WC <- 0
}

dummy <- 2
if (dummy > 1) {
print("test1")
print("test2")
print("test3")
# test
print("test4")
print("test5")
}


print(significance_strength_WC)


mean(usa_strength_liwc$Guilt)

test <- aov(strength ~ function., data = usa_strength_liwc)
summary(test)


if (A > 1) {
  
}

# significance value format
# significance_[metric]_[dictionary]
# e.g. significance_age_Clout

table_test <- data.frame(
  strength = c("strength1", "str2", "str3"),
  justified = c("j1", "j2", "j3"),
  guilt = c("g1","g2", "g3")
)

rownames(table_test) <- c("dic1", "dic2", "dic3")

View(table_test)








