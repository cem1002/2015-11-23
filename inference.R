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

# Two way
anova_2way <- aov(`Average Talk Time (sec)` ~ factor(year(Date)) + `Day of Week`, data=service_full)
summary(anova_2way)




# Normality
anova_full %>% ggplot(aes(sample = .resid)) + stat_qq()

# Equal variance
service_full %>% group_by(year(Date)) %>% 
summarise(ave = mean(`Average Talk Time (sec)`), sd=sd(`Average Talk Time (sec)`))

# Regression

service_full %>% lm(`Service Level %` ~ `Calls Offered`, data = .) ->
  lm_fit

summary(lm_fit)

# residuals
lm_fit %>% ggplot(aes(sample = .resid)) + stat_qq()
lm_fit %>% ggplot(aes(x=.fitted, y=.resid)) + geom_point()

# Hmmm...
service_full %>% 
  ggplot(aes(x=`Calls Offered`, y=`Service Level %`, color=`Weekday/ Weekend`)) + geom_point()

# Arbitrarily restrict to < 10000 calls offered
service_full %>% filter(`Calls Offered` < 10000) -> serv_10k

serv_10k %>% lm(`Service Level %` ~ `Calls Offered`,
                data = .) -> lm_fit_10k
summary(lm_fit_10k)

# From the above - R-squared is 0.287. Here is the correlation coefficient, 
# which is exactly the (negative) square root:
serv_10k %>% summarise(cor=cor(`Service Level %`, `Calls Offered`))

lm_fit_10k %>% ggplot(aes(sample = .resid)) + stat_qq()
lm_fit_10k %>% ggplot(aes(x=.fitted, y=.resid)) + geom_point()
# So not really a good model fit. 

serv_10k %>% filter(year(Date)==2011, month(Date)==6) %>% 
  mutate(Unanswered = `Calls Offered` - `Calls Answered`) ->
  june_2011

