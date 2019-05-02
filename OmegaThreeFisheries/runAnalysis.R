### Do secondary analysis of the nutrient to size ratios  ####
require(ggplot2)
require(mgcv)

df <- read.csv('data/Vaitla et al (2018)_data.csv')
units <- read.csv('data/Vaitla_units.csv')

names(df)
df$winf <- df$a_lw*df$maxlen^df$b_lw*1e-3

ggplot(df, aes(x = maxlen, y= lysine))+geom_point()
ggplot(df, aes(x = maxlen, y= protein))+geom_point()+geom_smooth()


