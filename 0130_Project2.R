library(usmap)
library(ggplot2)
library(dplyr)
library(pheatmap)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)


# MAC
# monthly_index <- read.csv("/Users/yunqingma/Library/CloudStorage/OneDrive-UniversityofSouthCarolina/Research/CHQ_Twitter/dataset/0707_all_data/racism_tweets_county_monthly.csv")

# Windows
monthly_index <- read.csv("D:\\OneDrive - University of South Carolina\\Research\\CHQ_Twitter\\dataset\\racism_tweets_county_monthly.csv")
county_level_2017_2023 <- readxl::read_xlsx("D:\\OneDrive - University of South Carolina\\Research\\CHQ_Twitter\\dataset\\0111_2017-2023_County Health Rankings South Carolina Data.xlsx")
county_level_2017_2023 <- county_level_2017_2023 %>%
    rename(fips = FIPS, year = Year)



df <- monthly_index %>%
    group_by(year, fips) %>%
    summarize(positive_ratio_tweets = mean(positive_ratio_tweets)) %>%
    mutate(fips = as.character(fips)) %>%
    left_join(., county_level_2017_2023, by = c('fips', 'year')) %>%
    filter(year >=2018, year <= 2022) %>%
    mutate_at(c('% Female', '% Rural', '% Black', '% Completed High School',
                '% Non-Hispanic White', '%Uninsured adults', '% Unemployed'), function (x) {10 * x}) %>%
    mutate(`Household Income (Black)` = `Household Income (Black)` / 1000)

colnames(df)


glm_YPLL_crude <- lmer(`YPLL Rate (Black)` ~ log(positive_ratio_tweets) + (1 | fips),
          control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)),
    data = df
)
summary(glm_YPLL_crude)
confint(glm_YPLL_crude)


glm_YPLL_crude <- lmer(`YPLL Rate (Black)` ~ `% Female` + (1 | fips),
                       control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)),
                       data = df
)
summary(glm_YPLL_crude)
confint(glm_YPLL_crude)

glm_YPLL_crude <- lmer(`YPLL Rate (Black)` ~ `% Rural` + (1 | fips),
                       control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)),
                       data = df
)
summary(glm_YPLL_crude)
confint(glm_YPLL_crude)

glm_YPLL_crude <- lmer(`YPLL Rate (Black)` ~ `% Black` + (1 | fips),
                       control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)),
                       data = df
)
summary(glm_YPLL_crude)
confint(glm_YPLL_crude)

glm_YPLL_crude <- lmer(`YPLL Rate (Black)` ~ `% Non-Hispanic White` + (1 | fips),
                       control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)),
                       data = df
)
summary(glm_YPLL_crude)
confint(glm_YPLL_crude)

glm_YPLL_crude <- lmer(`YPLL Rate (Black)` ~ `%Uninsured adults` + (1 | fips),
                       control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)),
                       data = df
)
summary(glm_YPLL_crude)
confint(glm_YPLL_crude)

glm_YPLL_crude <- lmer(`YPLL Rate (Black)` ~ `% Unemployed` + (1 | fips),
                       control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)),
                       data = df
)
summary(glm_YPLL_crude)
confint(glm_YPLL_crude)

glm_YPLL_crude <- lmer(`YPLL Rate (Black)` ~ `% Completed High School` + (1 | fips),
                       control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)),
                       data = df
)
summary(glm_YPLL_crude)
confint(glm_YPLL_crude)

glm_YPLL_crude <- lmer(`YPLL Rate (Black)` ~ `Household Income (Black)` + (1 | fips),
                       control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)),
                       data = df
)
summary(glm_YPLL_crude)
confint(glm_YPLL_crude)


glm_YPLL_demo <- lmer(`YPLL Rate (Black)` ~ log(positive_ratio_tweets) + `% Female` + `% Rural` + `% Black` + `% Non-Hispanic White` + (1 | fips),
                       control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)),
                       data = df
)
summary(glm_YPLL_demo)
confint(glm_YPLL_demo)


glm_YPLL_final <- lmer(`YPLL Rate (Black)` ~ log(positive_ratio_tweets) + `% Female` + `% Rural` + `% Black` + `% Non-Hispanic White`
                       + `%Uninsured adults` + `% Unemployed`
                       + `% Completed High School` + `Household Income (Black)` + (1 | fips),
                     control = lmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)),
                     data = df
)
summary(glm_YPLL_final)
confint(glm_YPLL_final)


