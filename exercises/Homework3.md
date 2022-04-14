    library(tidyverse)
    library(mosaic)
    library(ggplot2)
    library(dplyr)
    library(stringr)
    library(modelr)
    library(rsample)
    library(caret)
    library(parallel)
    library(foreach)
    library(knitr)
    library(nnet)
    library(ROCR)
    library(gbm)
    library(rpart)
    library(randomForest)
    library(ggmap)
    library(scales)

    ## Why can’t I just get data from a few different cities and run the regression of “Crime” on “Police” to understand how more cops in the streets affect crime? (“Crime” refers to some measure of crime rate and “Police” measures the number of cops in a city.)

The relationship between crime and the number of police may not be
causal.There are numerous confounding factors that could impact the
crime rate. Furthermore, the number of cops may actually be a response
to the previous crime rate. It is conceivable that crime precedes
police.

    ## How were the researchers from UPenn able to isolate this effect? Briefly describe their approach and discuss their result in the “Table 2” below, from the researchers' paper.

![Table 2](ex3table2.png)

The researchers were able to isolate the effect by factoring in the
terror alert warning system. On days of “high alert”, DC authorities
were legally obligated to increase the police presence around the city.
They could then observe variation in the crime rate controllig for
significant changes in police presence by proxy.

    ## Why did they have to control for Metro ridership? What was that trying to capture?

There was the consideration that there were secondary effects of an
increase in the terror alert level. Perhaps there would be fewer
potential victims on the streets while there was a high alert. To
control for this, they factored in metro ridership numbers, as such
figures would be highly correlated to the number of potential victims
out in public.

    ## Below I am showing you "Table 4" from the researchers' paper. Just focus on the first column of the table. Can you describe the model being estimated here? What is the conclusion?

![Table 4](ex3table4.png)

This model is similar to the model in table 2, with a few changes. There
is the addtion of a dummy variable which controls for the variation in a
specfic police district(1), compared to the other districts. This could
be important, if District 1 includes high priority areas such as the
National Mall which would require an even greater police presence in
response to an increase in the terror alert level. Another change is
putting ridership in log form, so the partial effect can be interpreted
relative to a percent change in ridership.We are asked to consider
log-transformation of the dependent variable. Log transformation is
useful in reducing the distribution of standard errors, or simplify the
interpretation of the interaction terms. We do not find it particularly
useful for either to log-transform the dependent variable.

# Problem 1

## CART

    DEN <- read_csv("dengue.csv")

    DEN$season = as.factor(DEN$season)
    DEN$city = as.factor(DEN$city)

    x = initial_split(DEN, prop = 0.8)
    DEN_train = training(x)
    DEN_test = testing(x)

    cart_test = rpart(total_cases ~ season + avg_temp_k + precipitation_amt + city + specific_humidity, 
                   data = DEN_train,
                   control = rpart.control(cp=.000002))

    plotcp(cart_test)

![](Homework3_files/figure-markdown_strict/Problem%201a-1.png)

    # cp = .02 seems reasonable
    cttRMSE = modelr::rmse(cart_test,DEN_test)

    rmse_frame_cart=foreach(x=1:10, .combine='rbind')%do%{
    x = initial_split(DEN, prop = 0.8)
    DEN_train = training(x)
    DEN_test = testing(x)

    cart = rpart(total_cases ~ season + avg_temp_k + precipitation_amt + city + specific_humidity, 
                   data = DEN_train,
                   control = rpart.control(cp=.02))
    modelr::rmse(cart,DEN_test)
    } %>% as.data.frame
    validate_RMSE_cart = mean(rmse_frame_cart$V1) 
    ## 39.38
    ## Setting cp to .02 reduced RMSE by ~7%!

# Problem 1

## Random Forest

    omit = na.omit(DEN)

    rmse_frame_forest=foreach(x=1:10, .combine='rbind')%do%{
    x = initial_split(omit, prop = 0.8)
    omit_train = training(x)
    omit_test = testing(x)


    forest = randomForest(total_cases ~ season + avg_temp_k + precipitation_amt + city + specific_humidity, data = omit_train, importance = TRUE)
    modelr::rmse(forest,DEN_test)
    } %>% as.data.frame
    validate_RMSE_forest = mean(rmse_frame_forest$V1) 
    ## 18.53

# Problem 1

## Gradient Boosted Trees

    DEN <- read_csv("dengue.csv")

    DEN$season = as.factor(DEN$season)
    DEN$city = as.factor(DEN$city)


    rmse_frame_boosty=foreach(x=1:10, .combine='rbind')%do%{

    x = initial_split(DEN, prop = 0.8)
    DEN_train = training(x)
    DEN_test = testing(x)  

    boosty = gbm(total_cases ~ season + avg_temp_k + precipitation_amt + city + specific_humidity, 
                   data = DEN_train,
                   interaction.depth=5, n.trees=500, shrinkage=.05)

    modelr::rmse(boosty,DEN_test)
    } %>% as.data.frame

    ## Distribution not specified, assuming gaussian ...
    ## Distribution not specified, assuming gaussian ...
    ## Distribution not specified, assuming gaussian ...
    ## Distribution not specified, assuming gaussian ...
    ## Distribution not specified, assuming gaussian ...
    ## Distribution not specified, assuming gaussian ...
    ## Distribution not specified, assuming gaussian ...
    ## Distribution not specified, assuming gaussian ...
    ## Distribution not specified, assuming gaussian ...
    ## Distribution not specified, assuming gaussian ...

    validate_RMSE_boosty = mean(rmse_frame_boosty$V1) 
    ## 39.67

# Problem 1

## Partial Dependence Plots

    omit_test = as.data.frame(omit_test)

    partialPlot(forest, omit_test, 'specific_humidity')

![](Homework3_files/figure-markdown_strict/Problem%201d-1.png)

    partialPlot(forest, omit_test, 'precipitation_amt')

![](Homework3_files/figure-markdown_strict/Problem%201d-2.png)

    partialPlot(forest, omit_test, 'city')

![](Homework3_files/figure-markdown_strict/Problem%201d-3.png)

# Problem 2 - Green Buildings Predictive Model Building

## Feature Engineering and Baseline Setting

    GB <- read_csv("greenbuildings.csv")

    # We want to collapse Energystar and LEED certifications into a two-level single variable.
    # We define a "green building" as having either certification, or in the extreme, both certifications.
    # This should provide higher fidelity than simply the "Green rating" variable.
    GB = GB %>% mutate(green = (GB$Energystar + GB$LEED))
    GB$green = as.factor(GB$green)

    GB = GB %>% mutate(rpsf = (GB$Rent*GB$leasing_rate)) 

    City_Market_Rent = scale(GB$City_Market_Rent)

    rmse_frame_baseline=foreach(x=1:10, .combine='rbind')%do%{

    x = initial_split(GB, prop = 0.8)
    GB_train = training(x)
    GB_test = testing(x)  

    base =lm(rpsf ~size + empl_gr + stories + age + renovated + green + amenities + cd_total_07 + hd_total07 + City_Market_Rent, data = GB_train)

    modelr::rmse(base,GB_test)
    } %>% as.data.frame
    validate_RMSE_base = mean(rmse_frame_baseline$V1) 
    # Validated RMSE is 1036.5

We are tasked with answering a question for a building developer. Does
green certification lead to increased revenue per square foot per
calendar year? As both building to green standards and the certification
process itself can be costly, it is of high importance to understand
whether or not such investment will pay off. We begin with a dataset
consisting of 7820 observations of 23 variables. We have to accomplish
two things before we begin our analysis:

1.  Conduct some feature engineering
2.  Make a baseline model for comparison

First, we want to define our measure of “green”. There are two binary
indicators for whether or not a building is “Energystar” certified or
“LEED” certified. As there is no significant difference between these
certifications, and some buildings hold both certifications, we will
collapse these two binary variables into one three-level variable, where
“0” means no certifcation, “1” means one certification, and “2” means
both certifications.By constructing the variable in such a way, we can
assess whether or not having additional certifications is worth the
investment.

Second, we need to construct our target variable, “revenue per square
foot per calendar year”, which is the product of “rent per square foot
per calendar year” and “leasing rate”.

Lastly, we assume that the most significant variable,
“city\_market\_rent”, which is the average market rate of rent per
square foot in the city the building is in. As this variable varies
widely, and is likely of high importance, we standardize it for
consistency.

For the baseline model, we run a simple linear regression on the
variables we conject are of most importance. For greater reliability, we
run 10 train test splits, and average the RMSE\_out of each train/test
split. This results in an RSME\_out of 1036.5. Now we will move on to
testing some more advanced modeling techniques against our baseline.

## Stepwise Selection

The first model we will try to test is a stepwise selected model
utilizing the baseline model as a starting point. The stepwise selection
will optimize against AIC, picking the features and interactions for us.
Once the model is optimized against AIC, we run it through a 10 step
cross-validation to get 10 instances of RMSE\_out. We take the mean, and
get a validated RMSE\_out of 1002.35, which is roughly a 3% improvement.
From the summary of the regression, we observe that a single green
certification results in a 400 unit increase in rpsf, at a 99%
confidence interval,ceteris paribus.There is no significant effect when
we consider the second certification. While 3% in RMSE\_out is a
reasonable improvement it is likely we can have a more dramatic
improvement using a random forest.

## Random Forest

    omit = na.omit(GB)

    rmse_frame_forest=foreach(x=1:10, .combine='rbind')%do%{
    x = initial_split(omit, prop = 0.8)
    omit_train = training(x)
    omit_test = testing(x)


    forest = randomForest(rpsf ~size + empl_gr + stories + age + renovated + green + City_Market_Rent, data = omit_train, importance = TRUE)
    modelr::rmse(forest,omit_test)
    } %>% as.data.frame
    validate_RMSE_forest = mean(rmse_frame_forest$V1) 
    ## Validated RMSE of 780.26

When we run the random forest model,even on fewer covariates, we see an
extremely dramatic improvement in our out of sample error. Similarly to
the stepwise model and the baseline model, we run 10 train/test splits,
and average the RMSE\_out. Our validated RMSE\_out for the random forest
is 780.26, which is nearly a 25% improvement. Satisfied with this
result, we will move on to the results.

## Random Forest- Results

    vi = varImpPlot(forest, type=1)

![](Homework3_files/figure-markdown_strict/Problem%202d-1.png)

    omit_test = as.data.frame(omit_test)
    partialPlot(forest, omit_test, 'City_Market_Rent')

![](Homework3_files/figure-markdown_strict/Problem%202d-2.png)

    partialPlot(forest, omit_test, 'age')

![](Homework3_files/figure-markdown_strict/Problem%202d-3.png)

    partialPlot(forest, omit_test, 'size')

![](Homework3_files/figure-markdown_strict/Problem%202d-4.png)

    partialPlot(forest, omit_test, 'green')

![](Homework3_files/figure-markdown_strict/Problem%202d-5.png)

The variable importance plot demonstrates what we assumed from the
beginning. The market rate of rent is by far the most important variable
in the model in terms of MSE. At the bottom, we see that our green
indicator is of very low importance. We can infer that green
certification has a mild effect, and likely accounts for a small amount
of the variable in rpsf.

Next, we make partial dependence plots of the three most important
variables, and our target variable of interest, green. In the first
plot, for city market rent, we observe a near linear relationship. As
one expects, there is a steady positive relationship between rpsf, and
the prevailing rent rates in the city.In the second plot, age, we
observe a steady decline in rpsf as a building age increases. This makes
sense, as older buildings are consistently less attractive to renters.
For the size variable, we observe an interesting relationship,
especially around where the majority of the distribution is. As building
size increases, there is a dramatic rise in rpsf, the rate of which
begins to decrease, which makes sense, when considering diminishing
marginal returns on size at scale.

Lastly, our target variable, green; We observe that the first
certification is related to roughly a 7-10% increase in rpsf. The
addition of the second certification has almost no effect. If we take
the evidence from the random forest model and the stepwise selected
model together, I recommend that as long building to green certification
standards costs less than ~5%, it is a safe investment in general,
however, we could provide a far more reliable recomendation with more
details.

# Problem 3 -California Housing

## Feature Engineering and Graphing

    CH <- read_csv("CAhousing.csv")

    CH = CH %>% mutate(avg_rm = (CH$totalRooms/CH$households))
    CH = CH %>% mutate(avg_bd = (CH$totalBedrooms/CH$households))
    longitude = CH$longitude
    latitude = CH$latitude
    CH$medianHouseValue = scale(CH$medianHouseValue)
    options(scipen=10000)

    map = get_map(location = c(
      left = -124.936287,
      bottom = 31.679835,
      right = -113.447052,
      top = 43.000835))
    mappy= ggmap(map)
    mappy + geom_point(aes(x=longitude,y=latitude,color=medianHouseValue),data=CH,size=.5)+ggtitle("Median House Value")

![](Homework3_files/figure-markdown_strict/Problem%203a-1.png)

We are tasked with creating a model for the median house value in
different geographic regions in California. The first task we have is to
do some feature engineering. Firstly, there is significant variation in
median house value across the census tracts, so we will standardize, so
results are reported in standard deviations from the mean. The data set
is relatively flat, however, there are a few transformations that have
to take place. We are given the total number of bedrooms in each census
tract, and the total number of rooms in each census tract. By dividing
the total number of each by the number of households, we have a
reasonable approximation of the average number of bedrooms and rooms in
each census tract.

For later comparison, we create a choropleth of the median housing value
on the centroid of each census tract.

## Finding a strong model

    baseline = lm(medianHouseValue ~ avg_rm + avg_bd + medianIncome + population,data=CH)
    lm_step = step(baseline,scope=~(.)^2)

    ## Start:  AIC=-14716.91
    ## medianHouseValue ~ avg_rm + avg_bd + medianIncome + population
    ## 
    ##                           Df Sum of Sq   RSS      AIC
    ## + avg_rm:medianIncome      1      52.9 10059 -14823.1
    ## + avg_rm:population        1      32.9 10079 -14782.1
    ## + avg_bd:medianIncome      1       9.9 10102 -14735.1
    ## + avg_rm:avg_bd            1       8.0 10104 -14731.3
    ## + medianIncome:population  1       7.9 10104 -14731.0
    ## + avg_bd:population        1       4.2 10108 -14723.4
    ## <none>                                 10112 -14716.9
    ## - population               1      27.7 10140 -14662.4
    ## - avg_bd                   1     608.5 10720 -13512.9
    ## - avg_rm                   1     738.7 10851 -13263.6
    ## - medianIncome             1    7786.9 17899  -2933.1
    ## 
    ## Step:  AIC=-14823.1
    ## medianHouseValue ~ avg_rm + avg_bd + medianIncome + population + 
    ##     avg_rm:medianIncome
    ## 
    ##                           Df Sum of Sq   RSS    AIC
    ## + avg_rm:population        1     32.05 10027 -14887
    ## + avg_bd:population        1      7.32 10052 -14836
    ## + avg_rm:avg_bd            1      6.87 10052 -14835
    ## + avg_bd:medianIncome      1      5.62 10054 -14833
    ## + medianIncome:population  1      4.84 10054 -14831
    ## <none>                                 10059 -14823
    ## - population               1     28.24 10087 -14767
    ## - avg_rm:medianIncome      1     52.86 10112 -14717
    ## - avg_bd                   1    627.96 10687 -13575
    ## 
    ## Step:  AIC=-14886.96
    ## medianHouseValue ~ avg_rm + avg_bd + medianIncome + population + 
    ##     avg_rm:medianIncome + avg_rm:population
    ## 
    ##                           Df Sum of Sq     RSS    AIC
    ## + medianIncome:population  1     55.47  9971.6 -14999
    ## + avg_bd:population        1     37.06  9990.0 -14961
    ## + avg_bd:medianIncome      1      7.27 10019.8 -14900
    ## + avg_rm:avg_bd            1      1.53 10025.5 -14888
    ## <none>                                 10027.0 -14887
    ## - avg_rm:population        1     32.05 10059.1 -14823
    ## - avg_rm:medianIncome      1     52.02 10079.1 -14782
    ## - avg_bd                   1    405.83 10432.9 -14070
    ## 
    ## Step:  AIC=-14999.45
    ## medianHouseValue ~ avg_rm + avg_bd + medianIncome + population + 
    ##     avg_rm:medianIncome + avg_rm:population + medianIncome:population
    ## 
    ##                           Df Sum of Sq     RSS    AIC
    ## + avg_bd:population        1    117.55  9854.0 -15242
    ## + avg_bd:medianIncome      1      8.25  9963.3 -15014
    ## <none>                                  9971.6 -14999
    ## + avg_rm:avg_bd            1      0.00  9971.6 -14997
    ## - avg_rm:medianIncome      1     39.63 10011.2 -14920
    ## - medianIncome:population  1     55.47 10027.0 -14887
    ## - avg_rm:population        1     82.67 10054.2 -14831
    ## - avg_bd                   1    317.18 10288.8 -14355
    ## 
    ## Step:  AIC=-15242.2
    ## medianHouseValue ~ avg_rm + avg_bd + medianIncome + population + 
    ##     avg_rm:medianIncome + avg_rm:population + medianIncome:population + 
    ##     avg_bd:population
    ## 
    ##                           Df Sum of Sq     RSS    AIC
    ## + avg_rm:avg_bd            1    18.745  9835.3 -15280
    ## + avg_bd:medianIncome      1    13.328  9840.7 -15268
    ## <none>                                  9854.0 -15242
    ## - avg_rm:medianIncome      1    46.183  9900.2 -15148
    ## - avg_bd:population        1   117.546  9971.6 -14999
    ## - medianIncome:population  1   135.950  9990.0 -14961
    ## - avg_rm:population        1   192.612 10046.6 -14845
    ## 
    ## Step:  AIC=-15279.5
    ## medianHouseValue ~ avg_rm + avg_bd + medianIncome + population + 
    ##     avg_rm:medianIncome + avg_rm:population + medianIncome:population + 
    ##     avg_bd:population + avg_rm:avg_bd
    ## 
    ##                           Df Sum of Sq     RSS    AIC
    ## + avg_bd:medianIncome      1     8.650  9826.6 -15296
    ## <none>                                  9835.3 -15280
    ## - avg_rm:avg_bd            1    18.745  9854.0 -15242
    ## - avg_rm:medianIncome      1    46.789  9882.1 -15184
    ## - avg_bd:population        1   136.290  9971.6 -14997
    ## - medianIncome:population  1   138.257  9973.5 -14993
    ## - avg_rm:population        1   191.209 10026.5 -14884
    ## 
    ## Step:  AIC=-15295.66
    ## medianHouseValue ~ avg_rm + avg_bd + medianIncome + population + 
    ##     avg_rm:medianIncome + avg_rm:population + medianIncome:population + 
    ##     avg_bd:population + avg_rm:avg_bd + avg_bd:medianIncome
    ## 
    ##                           Df Sum of Sq     RSS    AIC
    ## <none>                                  9826.6 -15296
    ## - avg_bd:medianIncome      1     8.650  9835.3 -15280
    ## - avg_rm:avg_bd            1    14.068  9840.7 -15268
    ## - avg_rm:medianIncome      1    49.349  9876.0 -15194
    ## - avg_bd:population        1   136.349  9963.0 -15013
    ## - medianIncome:population  1   141.434  9968.1 -15003
    ## - avg_rm:population        1   197.195 10023.8 -14888

    summary(lm_step)

    ## 
    ## Call:
    ## lm(formula = medianHouseValue ~ avg_rm + avg_bd + medianIncome + 
    ##     population + avg_rm:medianIncome + avg_rm:population + medianIncome:population + 
    ##     avg_bd:population + avg_rm:avg_bd + avg_bd:medianIncome, 
    ##     data = CH)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -6.7140 -0.4388 -0.1307  0.2941  4.8939 
    ## 
    ## Coefficients:
    ##                             Estimate   Std. Error t value             Pr(>|t|)
    ## (Intercept)             -1.519057078  0.042137126 -36.050 < 0.0000000000000002
    ## avg_rm                  -0.063576164  0.008904934  -7.139    0.000000000000968
    ## avg_bd                   0.233682289  0.051259607   4.559    0.000005174217145
    ## medianIncome             0.466185010  0.010349926  45.042 < 0.0000000000000002
    ## population              -0.000420175  0.000034661 -12.122 < 0.0000000000000002
    ## avg_rm:medianIncome     -0.015326169  0.001505773 -10.178 < 0.0000000000000002
    ## avg_rm:population       -0.000093355  0.000004588 -20.346 < 0.0000000000000002
    ## medianIncome:population  0.000059416  0.000003448  17.231 < 0.0000000000000002
    ## avg_bd:population        0.000612419  0.000036198  16.919 < 0.0000000000000002
    ## avg_rm:avg_bd            0.001231669  0.000226643   5.434    0.000000055606670
    ## avg_bd:medianIncome      0.042729934  0.010027150   4.261    0.000020402969737
    ##                            
    ## (Intercept)             ***
    ## avg_rm                  ***
    ## avg_bd                  ***
    ## medianIncome            ***
    ## population              ***
    ## avg_rm:medianIncome     ***
    ## avg_rm:population       ***
    ## medianIncome:population ***
    ## avg_bd:population       ***
    ## avg_rm:avg_bd           ***
    ## avg_bd:medianIncome     ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6902 on 20629 degrees of freedom
    ## Multiple R-squared:  0.5239, Adjusted R-squared:  0.5236 
    ## F-statistic:  2270 on 10 and 20629 DF,  p-value: < 0.00000000000000022

    resid = rstandard(lm_step)
    pred = predict(lm_step)

For this problem, we face the constraint of a limited feature set. To
get the most out of this data, it makes sense to lean on linear
regression, and factor in as many interactions as reasonable. Hence, a
stepwise selected model seems prudent. Running stepwise selection on all
possible features, we are left with 5 added interactions, and
statistical significance at the 99% level for all features. After the
model is optimized against AIC, we extract predictions for median house
value, and the residuals for graphing.

## Graphing Results

    mappy + geom_point(aes(x=longitude,y=latitude,color=resid),data=CH,size=.5)+ggtitle("Residuals")

![](Homework3_files/figure-markdown_strict/Problem%203c-1.png)

    mappy + geom_point(aes(x=longitude,y=latitude,color=pred),data=CH,size=.5)+ggtitle("Predicted Median House Value")

![](Homework3_files/figure-markdown_strict/Problem%203c-2.png)

For the graphed results, we see very flat outcomes. Both the predicted
median house value and residual plots show limited variation. In the
future, it would likely be wise to test more refined models, which would
be enabled more greatly by more nuanced data availability.
