# Environment
library(ggplot2)
library(ggthemes)

# Read Ant data into a dataframe
ants.df <- read.table('~/Projects/RMIT/IDTC/Intro2R/AntProject/Data/Ants.csv', sep = ',',
                       header = TRUE, check.names = FALSE)
# Summarise data
summary(ants.df)

class(ants.df$Disturbance)
class(ants.df$Hemisphere)
class(ants.df$Continent)
# Data transforms
# ants.df$lnPIE <- log(ants.df$PIE / (1 - ants.df$PIE))
ants.df$`Mean annual temperature` <- with(ants.df, scale(`Mean annual temperature`))
ants.df$`Total annual precipitation` <- with(ants.df, scale(`Mean annual precipitation`))
ants.df$`Temperature range` <- with(ants.df, scale(`Temperature range`))


# Charts
ants.p <- ggplot(aes(x = `Transect length`, y = lnPIE), data = ants.df)
ants.p + geom_point(aes(colour = Disturbance))

ants.p1 <- ggplot(aes(x = `Species richness`), data = ants.df)
ants.p1 + geom_histogram() + facet_grid(`Disturbance` ~ .) + theme_tufte()
ants.p1 + geom_histogram() + facet_grid(`Continent` ~ .) + theme_tufte()
ants.p1 + geom_histogram() + facet_grid(`Hemisphere` ~ .) + theme_tufte()

# Linear Models

# Intercept only
ants.m1 = lm(`Species richness` ~ +1, data = ants.df)
summary(ants.m1)

ants.m2 <- lm(`Species richness` ~ Disturbance + Continent + 
                `Mean annual temperature` + `Temperature range` + 
                `Total annual precipitation` + `Pitfall days` +
                `Transect length` + Disturbance * `Mean annual temperature` +
                Disturbance * `Total annual precipitation` +
                Disturbance * `Temperature range`, data = ants.df)
summary(ants.m2)

m.select <- step(object = ants.m1, scope = list(lower = ants.m1, upper = ants.m2),
                 direction = 'both', steps = 1e5)

m.select
sort(coef(m.select))
par(mfcol = c(2,2))
plot(m.select)

