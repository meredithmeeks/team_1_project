



setwd("C:/Users/HK/Desktop/Columbia/2017 Spring/G5069 Topics in Applied Data Science/team_1_project")
gss <- read.csv("GSS.2006.csv")


gss_sub <- gss[which(gss$partyid < 7), ]


library(ggplot2)

# partyid vs income
p_income <- ggplot(gss_sub, aes(x = as.factor(partyid), y = income06)) + 
  geom_boxplot() + theme_classic() + xlab("PartyID") + ylab("Income") + 
  ggtitle("Income Distribution by PartyID")
p_income

# partyid vs union
p_union1 <- ggplot(gss_sub, aes(x = as.factor(partyid), fill = as.factor(union))) + 
  geom_bar() + theme_classic() + xlab("PartyID") + ylab("Count") + 
  ggtitle("PartyID Distribution with Union Participation")
p_union1

p_union2 <- ggplot(gss_sub, aes(x = as.factor(union), fill = as.factor(partyid))) + 
  geom_bar(position = "fill") + theme_classic() + xlab("Union Status") + ylab("Count") + 
  ggtitle("PartyID % in different Union Status")
p_union2

# partyid vs childs
p_childs <- ggplot(gss_sub, aes(x = as.factor(partyid), y = childs)) + 
  geom_boxplot() + theme_classic() + xlab("PartyID") + ylab("Num of Children") + 
  ggtitle("Distribution of Children Num by PartyID")
p_childs


# regression?
gss_sub$partyid2 <- ifelse(gss_sub$partyid == 6, 1, 0)
lm1 <- glm(partyid2 ~ income06 + union + childs, data = gss_sub, family = binomial(link = "logit"))
summary(lm1)

# Call:
#   glm(formula = partyid2 ~ income06 + union + childs, family = binomial(link = "logit"), 
#       data = gss_sub)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.9718  -0.5291  -0.4451  -0.3496   2.6976  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.63990    0.38630 -12.011  < 2e-16 ***
#   income06     0.07699    0.01199   6.420 1.36e-10 ***
#   union        0.23774    0.07436   3.197  0.00139 ** 
#   childs       0.17664    0.03411   5.179 2.23e-07 ***
#   ---
#   Signif. codes:  0 ¡®***¡¯ 0.001 ¡®**¡¯ 0.01 ¡®*¡¯ 0.05 ¡®.¡¯ 0.1 ¡® ¡¯ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2069.2  on 2957  degrees of freedom
# Residual deviance: 1997.2  on 2954  degrees of freedom
# (1461 observations deleted due to missingness)
# AIC: 2005.2
# 
# Number of Fisher Scoring iterations: 5


