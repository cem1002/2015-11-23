library(rio)
library(dplyr)
library(lubridate)
library(ggplot2)

service <- tbl_df(import("311_daily_contact_centre_trends.xlsx",
                         sheet="all"))

# Extract 2010:2013 and remove 2010 outliers - see last week for details
service %>% filter(year(Date) %in% 2010:2013, 
                   `Average Talk Time (sec)` > 100) -> service_full


# ANOVA for all the complete years
anova_full <- aov(`Average Talk Time (sec)` ~ factor(year(Date)), 
                  data=service_full)
summary(anova_full)

# Normality
anova_full %>% ggplot(aes(sample = .resid)) + stat_qq()

# Equal variance
service_full %>% group_by(year(Date)) %>% 
  summarise(sd=sd(`Average Talk Time (sec)`))

# Regression

service_full %>% lm(`Service Level %` ~ `Calls Offered`, data = .) -> lm_fit
summary(lm_fit)

# residuals
lm_fit %>% ggplot(aes(sample = .resid)) + stat_qq()
lm_fit %>% ggplot(aes(x=.fitted, y=.resid)) + geom_point()

# Hmmm...
service_full %>% 
  ggplot(aes(x=`Calls Offered`, y=`Service Level %`)) + geom_point()

# Arbitrarily restrict to < 10000 calls offered
service_full %>% filter(`Calls Offered` < 10000) -> serv_10k

serv_10k %>% lm(`Service Level %` ~ `Calls Offered` + `Weekday/ Weekend`,
                data = .) -> lm_fit_10k
summary(lm_fit_10k)

lm_fit_10k %>% ggplot(aes(sample = .resid)) + stat_qq()
lm_fit_10k %>% ggplot(aes(x=.fitted, y=.resid)) + geom_point()
# So not really a good model fit. 

