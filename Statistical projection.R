#raw data
Num_of_referrals <- c(1725,1948,2080,2197,3880)
Section_47s <- c(849,1132,978,815,1498)
Strategy_discussions <- c(1448,1778,1684,1577,2261)
date <- c(2015,2016,2017,2018,2019)
pred_date <- c(2020,2021,2022,2023,2024,2025)
date2 <- date^2

#linear model setup
referral_mod <- lm(Num_of_referrals ~ date)
section_mod <- lm(Section_47s ~ date)
strategy_mod <- lm(Strategy_discussions ~ date)

referral_quad <- lm(Num_of_referrals ~ date + date2)
section_quad <- lm(Section_47s ~ date + date2)
strategy_quad <- lm(Strategy_discussions ~ date + date2)

referral_exp <- lm(log(Num_of_referrals) ~ date)
section_exp <- lm(log(Section_47s) ~ date)
strategy_exp <- lm(log(Strategy_discussions) ~ date)

#graph setup
par(mfrow=c(3,3))

#linear models
plot(Num_of_referrals ~ date,pch = 16, col = "#0c00ad", main = "Number of Referrals per year", xlab = "Year", ylab = "Number of Referrals")
referral_linear <- referral_mod$coefficients[2]*date + referral_mod$coefficients[1]
lines(date, referral_linear, lwd = 3)

plot(Section_47s ~ date,pch = 16, col = "#03962f", main = "Section 47s per year", xlab = "Year", ylab = "Section 47s")
section_linear <- section_mod$coefficients[2]*date + section_mod$coefficients[1]
lines(date, section_linear, lwd = 3)

plot(Strategy_discussions ~ date,pch = 16, col = "#c50202", main = "Strategy Discussions per year", xlab = "year", ylab = "Strategy discussions")
strategy_linear <- strategy_mod$coefficients[2]*date + strategy_mod$coefficients[1]
lines(date, strategy_linear, lwd = 3)

#quadratic models
plot(Num_of_referrals ~ date,pch = 16, col = "#0c00ad", main = "Number of Referrals per year", xlab = "Year", ylab = "Number of Referrals")
referral_quadratic <- referral_quad$coefficients[3]*date^2+referral_quad$coefficients[2]*date+referral_quad$coefficients[1]
lines(date, referral_quadratic, lwd = 3)

plot(Section_47s ~ date,pch = 16, col = "#03962f", main = "Section 47s per year", xlab = "Year", ylab = "Section 47s")
section_quadratic <- section_quad$coefficients[3]*date^2+section_quad$coefficients[2]*date+section_quad$coefficients[1]
lines(date, section_quadratic, lwd = 3)

plot(Strategy_discussions ~ date,pch = 16, col = "#c50202", main = "Strategy Discussions per year", xlab = "year", ylab = "Strategy discussions")
strategy_quadratic <- strategy_quad$coefficients[3]*date^2+strategy_quad$coefficients[2]*date+strategy_quad$coefficients[1]
lines(date, strategy_quadratic, lwd = 3)

#exponential models
plot(Num_of_referrals ~ date,pch = 16, col = "#0c00ad", main = "Number of Referrals per year", xlab = "Year", ylab = "Number of Referrals")
referral_exponential <- exp(referral_exp$coefficients[1])*exp(referral_exp$coefficients[2]*date)
lines(date, referral_exponential, lwd = 3)

plot(Section_47s ~ date,pch = 16, col = "#03962f", main = "Section 47s per year", xlab = "Year", ylab = "Section 47s")
section_exponential <- exp(section_exp$coefficients[1])*exp(section_exp$coefficients[2]*date)
lines(date, section_exponential, lwd = 3)

plot(Strategy_discussions ~ date,pch = 16, col = "#c50202", main = "Strategy Discussions per year", xlab = "year", ylab = "Strategy discussions")
strategy_exponential <- exp(strategy_exp$coefficients[1])*exp(strategy_exp$coefficients[2]*date)
lines(date, strategy_exponential, lwd = 3)

#Predicted values
#linear
pred_num_of_referrals_lin <- c()
pred_section_47s_lin <- c()
pred_strategy_discussions_lin <- c()
for(val in pred_date){
  pred_num_of_referrals_lin <- append(pred_num_of_referrals_lin, referral_mod$coefficients[2]*val + referral_mod$coefficients[1])
  pred_section_47s_lin <- append(pred_section_47s_lin, section_mod$coefficients[2]*val + section_mod$coefficients[1])
  pred_strategy_discussions_lin <- append(pred_strategy_discussions_lin, strategy_mod$coefficients[2]*val + strategy_mod$coefficients[1])
}

#quadratic
pred_num_of_referrals_quad <- c()
pred_section_47s_quad <- c()
pred_strategy_discussions_quad <- c()
for(val in pred_date){
  pred_num_of_referrals_quad <- append(pred_num_of_referrals_quad, referral_quad$coefficients[3]*val^2+referral_quad$coefficients[2]*val+referral_quad$coefficients[1])
  pred_section_47s_quad <- append(pred_section_47s_quad, section_quad$coefficients[3]*val^2+section_quad$coefficients[2]*val+section_quad$coefficients[1])
  pred_strategy_discussions_quad <- append(pred_strategy_discussions_quad, strategy_quad$coefficients[3]*val^2+strategy_quad$coefficients[2]*val+strategy_quad$coefficients[1])
}

#exponential
pred_num_of_referrals_exp <- c()
pred_section_47s_exp <- c()
pred_strategy_discussions_exp <- c()
for(val in pred_date){
  pred_num_of_referrals_exp <- append(pred_num_of_referrals_exp, exp(referral_exp$coefficients[1])*exp(referral_exp$coefficients[2]*val))
  pred_section_47s_exp <- append(pred_section_47s_exp, exp(section_exp$coefficients[1])*exp(section_exp$coefficients[2]*val))
  pred_strategy_discussions_exp <- append(pred_strategy_discussions_exp, exp(strategy_exp$coefficients[1])*exp(strategy_exp$coefficients[2]*val))
}