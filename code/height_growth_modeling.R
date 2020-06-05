library(tidyverse)
library(lme4)

# load data and filter
#data <- baad.data::baad_data()$data %>%  
#  as_tibble() %>% filter(!is.na(age))

data<-read.csv("data/BAAD/baad_with_map.csv",stringsAsFactors = FALSE)

# plot height vs age for trees < 200 years old
data %>% 
  filter(age < 201) %>% 
  group_by(location) %>% 
  filter(max(age) > 10) %>% 
  filter(!is.na(mean_annual_precipitation))%>%
  ungroup()->smallish

# random slope for each site
m <- lmer(h.t~0+(0+age|location),data=smallish)
# random term for species nested within site
m2 <- lmer(h.t~0+(0+age|location/species),data=smallish)

slopes<-coef(m2)$location


ggplot(slopes,aes(x=age))+geom_histogram()+xlab("growth rate (m/yr)")
ggplot(slopes,aes(x=20/age))+geom_histogram()+xlab("time to 20m tall (years)")+scale_x_log10()

data.fitted <- broom::augment(m2) %>% arrange(location, age)

ggplot(data.fitted, aes(age, h.t,colour=species)) + geom_point() + 
  geom_line(aes(age, .fitted,colour=species)) + 
  facet_wrap(~location)+theme(legend.position = "none")

data.fitted$map<-data$mean_annual_precipitation[match(data.fitted$location,data$location)]

data.fitted$growth.rate<-data.fitted$.fitted/data.fitted$age

data.fitted %>%
  group_by(location,species)%>%
  summarize(map=mean(map),growth.rate=mean(growth.rate))->species_df


ggplot(species_df,aes(x=map,y=growth.rate))+geom_point()+geom_smooth(method="lm")+theme_bw()+
  ggtitle("Climate Effects on Growth Rate of Trees") + 
  ylab("growth rate (m/year)") + 
  xlab("mean annual precipitation (mm)")


m3 <- lmer(h.t~0+age:mean_annual_precipitation+(0+age|location/species),data=smallish)
summary(m3)
anova(m2,m3)

m4<-lm(growth.rate~map,data=species_df)
summary(m4)

