library(tidyverse)
library(fabricatr)
library(boot)

## Add fit and HS rating

myn=2150

admit_data <- fabricate(
  N = 2150,
  income = pmax(0,
                rlnorm(
                  n = N, meanlog = 11.5, sdlog = .78
                )),
  sat = pmin(pmax(
    1200,
    correlate(
      given = income,
      rho = 0.3,
      rnorm,
      mean = 1200,
      sd = 100
    )
  ),
  1600),
  gpa = pmin(pmax(
    2.8, correlate(
      given = sat,
      rho = 0.6,
      rnorm,
      mean = 3.8,
      sd = .3
    )
  ) ,
  4.0),
  visit = correlate(
    given = income,
    rho = .2,
    draw_binomial,
    prob = .4,
    trials = 1
  ),
  legacy = correlate(
    given = income,
    rho = .2,
    draw_binomial,
    prob = .3,
    trials = 1
  ),
  registered = correlate(
    given = income,
    rho = .2,
    draw_binomial,
    prob = .6,
    trials = 1
  ) ,
  fit=correlate(
    given=income,
    rho=.4,
    draw_binomial,
    prob=.5,
    trials=1
  ),
  top_hs=correlate(
    given=income,
    rho=.6,
    draw_binomial,
    prob=.2,
    trials=1
  ),
  sent_scores =  correlate(
    given = income,
    rho = .2,
    draw_binomial,
    prob = .2,
    trials = 1
  ),
  distance = correlate(
    given = income,
    rho = .2,
    rlnorm,
    mean = 4.6,
    sd = 1
  ),
  tuition = 45000,
  need_aid = ifelse(income < 1e5,
                    pmin(45000, (500 + ((income / 1000) - 100
                    ) * (
                      -425
                    ))),
                    0),
  merit_aid = ifelse(sat > 1250,
                     pmin(45000, (5000 + (
                       sat / 100
                     ) * 1500)),
                     0),
  net_price = pmax(0, (tuition - (need_aid + merit_aid))),
  yield = draw_binary(
    latent =
      -7.3+
      visit * .2 +
      registered * .25 +
      legacy * .4 +
      sent_scores * .2 +
      sat * .005+
      sat^2*(-2.2e-6)+
      income* .000035+
      income^2*(-7.75e-11)+
      gpa* 1+
      gpa^2*(-5e-5)+
      log(distance)*-.2+
      net_price* -.00003+
      top_hs*.1+
      fit*.25,
      link="probit")
)

admit_data<-admit_data%>%ungroup()

write_rds(admit_data,file="../data/cleaned/admit_data.rds")

## Checking Characteristics

table(admit_data$yield)

summary(glm(yield~legacy+
              visit+
              registered+
              sent_scores+
            #  sat+
            #  I(sat*sat)+
              #income+
              gpa+
              I(gpa*gpa)+
              log(distance)+
              top_hs+
              fit+
              net_price,data=admit_data,family=binomial(link="probit")))

admit_data%>%
  mutate(sat_ptile=ntile(sat,100))%>%
  group_by(sat_ptile)%>%
  summarize(pr_yield=mean(yield))%>%
  ggplot(aes(x=sat_ptile,y=pr_yield))+
  geom_point()


admit_data%>%
  mutate(gpa_ptile=ntile(gpa,20))%>%
  group_by(gpa_ptile)%>%
  summarize(pr_yield=mean(yield))%>%
  ggplot(aes(x=gpa_ptile,y=pr_yield))+
  geom_point()

admit_data%>%
  ggplot(aes(x=sat,y=merit_aid))+
  geom_point()


admit_data%>%
  ggplot(aes(x=income,y=need_aid))+
  geom_point()

admit_data%>%
  mutate(income_ptile=ntile(income,100))%>%
  group_by(income_ptile)%>%
  summarize(pr_yield=mean(yield))%>%
  ggplot(aes(x=income_ptile,y=pr_yield))+
  geom_point()


admit_data%>%
  mutate(np_ptile=ntile(net_price,10))%>%
  group_by(np_ptile)%>%
  summarize(pr_yield=mean(yield))%>%
  ggplot(aes(x=np_ptile,y=pr_yield))+
  geom_bar(stat="identity")

admit_data%>%
  ggplot(aes(x=income,y=sat))+
  geom_point()+
  scale_x_continuous(trans="log")

admit_data%>%
  ggplot(aes(x=income,y=need_aid))+geom_point()

admit_data%>%
  ggplot(aes(x=income,y=net_price))+geom_point()

admit_data%>%
  ggplot(aes(x=sat,y=merit_aid))+geom_point()




admit_data%>%ggplot(aes(x=income,y=sat))+geom_point()

admit_data%>%ggplot(aes(x=gpa,y=sat))+geom_point()


admit_data%>%
  group_by(legacy)%>%
  summarize(mean(yield))