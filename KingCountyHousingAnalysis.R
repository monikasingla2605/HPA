
#loading libraries hexbin and repr
library(repr)
library(hexbin)


## A possible solution is here:
## Change the full path to the  file. If in working directory use ''.
read.house = function(file = 'kc_house_data.csv'){
  require(tidyverse)
  house = read_csv(file)
  house = house %>% mutate(bedrooms_fac = as.factor(bedrooms), 
                           bathrooms_fac = as.factor(bathrooms), 
                           floors_fac = as.factor(floors), 
                           waterfront_fac = as.factor(waterfront), 
                           view_fac = as.factor(view), 
                           condition_fac = as.factor(condition), 
                           grade_fac = as.factor(grade), 
                           zipcode_fac = as.factor(zipcode))
  house
}
house = read.house()
glimpse(house)

#data filtering
house <- house %>% 
   select(-date,-lat ,-long, -sqft_living15, -sqft_lot15)

# Glimpse result
glimpse(house)

## remove Nas
house %>% drop_na()
glimpse(house)
#total count is still same

#histogram function 
gg_hist <- function(cols, df){
    p1  = ggplot(df, aes_string(x= cols)) + 
    theme_dark()+
  geom_histogram(aes(y=..density..),      # Density on y-axis
                 color="black", fill="white",
                 bins = 100) +
  geom_density(alpha=.2, fill="#FF7777") +
  ggtitle(paste('Density vs', cols),
    subtitle = paste("Row count", df %>% nrow())) +
    labs (caption = "Data from Kaggle : House sales in King County, USA from May 2014 to May 2015")
    p1 %>% print()
    }


gg_hist('price', house)

house <- house%>%mutate(price_log = log(price))

gg_hist('price_log', house)

cols = c("sqft_living", "sqft_lot", "sqft_above", "sqft_basement", "yr_built")
invisible(lapply(cols, gg_hist, house))

gg_hex = function(cols, df, y1){
p1=    df %>% ggplot(aes_string(x = cols, y = y1))  +
    theme_classic()+
       geom_hex() +
       geom_jitter(alpha = 0.1) +
       geom_smooth(method = "loess", se = FALSE, size =2) + 
       ggtitle(paste(y1, 'vs', cols),
       subtitle = paste("Row count", df %>% nrow())) +
       labs (caption = "Data from Kaggle : House sales in King County, USA from May 2014 to May 2015")
      p1%>%print()
}

cols1 = c("sqft_living", "sqft_lot", "sqft_above", "sqft_basement", "yr_built")
y1 = "price_log"
invisible(lapply(cols, gg_hex, house, y1))

gg_box<- function(cols,df,y1){
p3 = ggplot(df, aes_string(cols, y1, fill = cols)) +
     geom_boxplot()  +
     theme_linedraw()+
    ggtitle(paste(y1, 'vs', cols),
       subtitle = paste("Row count", df %>% nrow())) +
       labs (caption = "Data from Kaggle : House sales in King County, USA from May 2014 to May 2015")
    print(p3)
}




cols = c("bedrooms_fac", "bathrooms_fac", "floors_fac", "zipcode_fac", "waterfront_fac", "view_fac", "condition_fac", "grade_fac")
invisible(lapply(cols, gg_box, house, "price_log"))

house <- house%>%mutate(sqft_log = price_log/sqft_living)

gg_hist('sqft_log', house)

cols1 = c("sqft_living", "sqft_lot", "sqft_above", "sqft_basement", "yr_built")
y1 = "sqft_log"
invisible(lapply(cols1, gg_hex, house, y1))

cols = c("bedrooms_fac", "bathrooms_fac", "floors_fac", "zipcode_fac", "waterfront_fac", "view_fac", "condition_fac", "grade_fac")
y1 = "sqft_log"
invisible(lapply(cols, gg_box, house, y1))

house <- house%>%mutate(sqrt_sqft_living = sqrt(sqft_living),
                           log_sqft_living = log(sqft_living))
cols3 <- c("sqrt_sqft_living", "log_sqft_living")
invisible(lapply(cols3, gg_hist, house))

#Q smoother trendline #plot.feature('sqft_root', house, 'price_log')
gg_hex_lm = function(cols, df, y1){
p1=    df %>% ggplot(aes_string(x = cols, y = y1))  +
    theme_classic()+
       geom_hex() +
       geom_jitter(alpha = 0.1) +
       geom_smooth(method = "lm", se = FALSE, size =2) + 
       ggtitle(paste(y1, 'vs', cols),
       subtitle = paste("Row count", df %>% nrow())) +
       labs (caption = "Data from Kaggle : House sales in King County, USA from May 2014 to May 2015")
      p1%>%print()
}


gg_hex_lm("sqrt_sqft_living", house, "price_log")

waterfront_homes<- house%>%filter(waterfront == "1")
nonwaterfront_homes<- house%>%filter(waterfront == "0")
glimpse(waterfront_homes)

t.test(waterfront_homes$price, nonwaterfront_homes$price, alternative  = "greater")


t.test(waterfront_homes$sqft_log, nonwaterfront_homes$sqft_log, alternative  = "greater")


t.test(waterfront_homes$sqft_living, nonwaterfront_homes$sqft_living, alternative  = "greater")


house1 <- house%>%group_by(zipcode)%>%
summarise(mean_log_price = mean(price_log),
         median_log_price = median(price_log),
         sd_price_log = sd(price_log),
         mean_sqft_log = mean(sqft_log),
         median_sqft_log = median(sqft_log),
         sds_sqft_log = sd(sqft_log),
         mean_price  = mean(price),
         median_price= median(price),
         sds_price = sd(price),
         count = n())%>%
          arrange(mean_price%>%desc())
df_hp_zipcode <-house1%>%head(10)
df_lp_zipcode <- house1%>%tail(10)
df_hp_zipcode
df_lp_zipcode

df_high <- house%>%semi_join(df_hp_zipcode)

df_low <- house%>%semi_join(df_lp_zipcode)
t.test(x = df_high$sqft_log , y =  df_low$sqft_log, alternative = "greater")



t.test(x = df_high$log_sqft_living , y =  df_low$log_sqft_living, alternative = "greater")



#we see that means are different, however both have fairly large standard deviation





house2<- house%>% group_by(zipcode)%>%
summarise(mean_bath = mean(bathrooms),
         median_bath = median(bathrooms),
         mean_bed = mean(bedrooms),
         median_bed = median(bedrooms),
         median_price= median(price),
         count = n())%>%
          arrange(median_price%>%desc())
df_higher_bb <-house2%>%head(10)
df_lower_bb <- house2%>%tail(10)

df_greater <- house %>% semi_join(df_higher_bb)
df_lower <- house %>% semi_join(df_lower_bb)
df_greater%>%head(10)
df_lower %>%tail(10)

t.test(df_greater$bedrooms, df_lower$bedrooms, alternaive = "greater")
t.test(df_greater$bathrooms, df_lower$bathrooms, alternaive = "greater")


normalize = function(x) (x - mean(x))/sd(x)
house_scaled <- house %>% mutate(bedrooms = normalize(bedrooms),
                           bathrooms = normalize(bathrooms),
                            sqft_living = normalize(sqft_living),
                            sqft_lot = normalize(sqft_lot),
                            floors = normalize(floors),
                            waterfront = normalize(waterfront),
                            view = normalize(view),
                            condition = normalize(condition),
                            grade = normalize(grade),
                            sqft_above = normalize(sqft_above),
                            sqft_root = normalize(sqft_root),
                            living_log = normalize(living_log))

 house_scaled.mod = lm(price_log ~ bedrooms + bathrooms + sqft_living +sqft_lot+ floors + waterfront + view + condition + grade + sqft_above
                    + sqft_root + living_log, data = house_scaled)                    
summary (house_scaled.mod)
cat('The coefficient confidence intervals')
confint( house_scaled.mod)


#removed sqft_lot
 house_scaled.mod = lm(price_log ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + condition + grade + sqft_above
                    + sqft_root + living_log, data = house_scaled)  

summary (house_scaled.mod)
cat('The coefficient confidence intervals')
confint( house_scaled.mod)


# removed bathrooms
house_scaled.mod = lm(price_log ~ bedrooms + sqft_living +floors + waterfront + view + condition + grade + sqft_above
                    + sqft_root + living_log, data = house_scaled)                    
summary (house_scaled.mod)
cat('The coefficient confidence intervals')
confint( house_scaled.mod)

house_scaled$score <- predict( house_scaled.mod, data = house_scaled)
head(house, 10)
house_scaled$resids <- house$price_log - house_scaled$score
head(house_scaled, 10)

plot.resids <- function(df){
  require(ggplot2)
  require(gridExtra)
  p1 = ggplot(df, aes(resids, ..density..)) + 
         geom_histogram(bins = 10, alpha = 0.3, color = 'blue') +
         geom_density(size = 1) +
         xlab('Residual value') +
         ggtitle('Histogram and density function \n for residuals')
  p2 = ggplot(df, aes(sample = resids)) + 
         geom_qq() + 
         ggtitle('Quantile-quantile Normal plot \n of residuals')
  grid.arrange(p1, p2, ncol = 2)  
}
options(repr.plot.width=7, repr.plot.height=4) ## set the plot area size
plot.resids(house_scaled)

scatter.resids <- function(df){
  require(ggplot2)
  ggplot(df, aes(score, resids)) + 
    geom_point(size = 2,) +
    geom_smooth(size = 1) + # I cant use method loess for large data set
    ggtitle('Residuals vs fitted values') +
    xlab('Fitted values') + ylab('Residuals')+
    labs(
         caption ="Data from Kaggle:House Scales in king County,USA"
    )
}
options(repr.plot.width=6, repr.plot.height=3) ## set the plot area size
scatter.resids(house_scaled)
