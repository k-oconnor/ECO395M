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
relative to a percent change in ridership.

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

    base = Baseline = lm(rpsf ~size + empl_gr + stories + age + renovated + green + amenities + cd_total_07 + hd_total07 + City_Market_Rent, data = GB_train)

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

    lm_step = step(base,scope=~(.)^2)

    ## Start:  AIC=87242.48
    ## rpsf ~ size + empl_gr + stories + age + renovated + green + amenities + 
    ##     cd_total_07 + hd_total07 + City_Market_Rent
    ## 
    ##                                Df  Sum of Sq        RSS   AIC
    ## + size:City_Market_Rent         1  296083853 6.7858e+09 86977
    ## + stories:City_Market_Rent      1  206161569 6.8757e+09 87060
    ## + amenities:City_Market_Rent    1   52087196 7.0298e+09 87198
    ## + size:hd_total07               1   28097860 7.0538e+09 87220
    ## + size:amenities                1   20577588 7.0613e+09 87226
    ## + stories:amenities             1   20519133 7.0613e+09 87226
    ## + stories:hd_total07            1   19610512 7.0622e+09 87227
    ## + size:cd_total_07              1   18437003 7.0634e+09 87228
    ## + amenities:hd_total07          1   18307971 7.0635e+09 87228
    ## + stories:renovated             1   17734430 7.0641e+09 87229
    ## + size:stories                  1   16926439 7.0649e+09 87230
    ## + cd_total_07:hd_total07        1   15340854 7.0665e+09 87231
    ## + size:green                    2   16729957 7.0651e+09 87232
    ## + age:City_Market_Rent          1   13510725 7.0683e+09 87233
    ## + size:renovated                1   12499777 7.0694e+09 87233
    ## + green:amenities               1   11666633 7.0702e+09 87234
    ## + stories:green                 2   13889879 7.0680e+09 87234
    ## + stories:age                   1    9599800 7.0723e+09 87236
    ## + stories:cd_total_07           1    7160242 7.0747e+09 87238
    ## + hd_total07:City_Market_Rent   1    7139499 7.0747e+09 87238
    ## + empl_gr:age                   1    6708832 7.0751e+09 87239
    ## - renovated                     1     226135 7.0821e+09 87241
    ## - empl_gr                       1     416925 7.0823e+09 87241
    ## + empl_gr:amenities             1    4059027 7.0778e+09 87241
    ## + age:renovated                 1    3178316 7.0787e+09 87242
    ## + renovated:cd_total_07         1    2815613 7.0790e+09 87242
    ## + green:hd_total07              2    5075256 7.0768e+09 87242
    ## <none>                                       7.0819e+09 87242
    ## + renovated:amenities           1    2003457 7.0798e+09 87243
    ## + size:age                      1    1525151 7.0803e+09 87243
    ## + age:cd_total_07               1    1437109 7.0804e+09 87243
    ## + cd_total_07:City_Market_Rent  1    1425296 7.0804e+09 87243
    ## + renovated:City_Market_Rent    1    1395562 7.0805e+09 87243
    ## + amenities:cd_total_07         1    1209696 7.0806e+09 87243
    ## + green:City_Market_Rent        2    3441825 7.0784e+09 87243
    ## + age:hd_total07                1    1062147 7.0808e+09 87244
    ## + empl_gr:cd_total_07           1     778080 7.0811e+09 87244
    ## + empl_gr:City_Market_Rent      1     763472 7.0811e+09 87244
    ## + size:empl_gr                  1     529284 7.0813e+09 87244
    ## + empl_gr:renovated             1     498592 7.0814e+09 87244
    ## + empl_gr:green                 2    2712757 7.0791e+09 87244
    ## + renovated:hd_total07          1     108079 7.0817e+09 87244
    ## + empl_gr:hd_total07            1      10652 7.0818e+09 87244
    ## + empl_gr:stories               1       2537 7.0818e+09 87244
    ## + age:amenities                 1        374 7.0819e+09 87244
    ## + age:green                     2    1589560 7.0803e+09 87245
    ## + renovated:green               2    1115305 7.0807e+09 87245
    ## + green:cd_total_07             2     600325 7.0813e+09 87246
    ## - cd_total_07                   1    9571307 7.0914e+09 87249
    ## - stories                       1   10771656 7.0926e+09 87250
    ## - hd_total07                    1   16501243 7.0984e+09 87255
    ## - green                         2   22193389 7.1040e+09 87258
    ## - age                           1   39894854 7.1217e+09 87276
    ## - amenities                     1   49770243 7.1316e+09 87284
    ## - size                          1   68615282 7.1505e+09 87301
    ## - City_Market_Rent              1 6520309173 1.3602e+10 91324
    ## 
    ## Step:  AIC=86977.26
    ## rpsf ~ size + empl_gr + stories + age + renovated + green + amenities + 
    ##     cd_total_07 + hd_total07 + City_Market_Rent + size:City_Market_Rent
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + size:stories                  1  26013787 6759753981 86955
    ## + size:amenities                1  18223021 6767544747 86962
    ## + stories:amenities             1  16734908 6769032860 86964
    ## + amenities:hd_total07          1  10241355 6775526413 86970
    ## + cd_total_07:hd_total07        1   9326819 6776440949 86971
    ## + green:amenities               1   8927766 6776840002 86971
    ## + empl_gr:age                   1   8852702 6776915066 86971
    ## + size:hd_total07               1   8319638 6777448130 86972
    ## + stories:hd_total07            1   7282444 6778485324 86973
    ## + age:cd_total_07               1   4966938 6780800829 86975
    ## + empl_gr:amenities             1   4528697 6781239070 86975
    ## + stories:renovated             1   4025103 6781742665 86976
    ## + size:empl_gr                  1   3882972 6781884796 86976
    ## + size:green                    2   6006120 6779761648 86976
    ## + renovated:cd_total_07         1   3733972 6782033796 86976
    ## + empl_gr:City_Market_Rent      1   3731053 6782036715 86976
    ## - empl_gr                       1    876066 6786643834 86976
    ## + cd_total_07:City_Market_Rent  1   3045418 6782722350 86976
    ## - renovated                     1   1452869 6787220637 86977
    ## + stories:green                 2   4868777 6780898991 86977
    ## + age:City_Market_Rent          1   2685068 6783082700 86977
    ## + empl_gr:cd_total_07           1   2422147 6783345620 86977
    ## <none>                                      6785767768 86977
    ## + age:renovated                 1   2068773 6783698995 86977
    ## + renovated:City_Market_Rent    1   1755793 6784011975 86978
    ## + size:cd_total_07              1   1650433 6784117335 86978
    ## + hd_total07:City_Market_Rent   1   1395273 6784372495 86978
    ## + age:amenities                 1   1354835 6784412933 86978
    ## + stories:age                   1   1303051 6784464717 86978
    ## + stories:City_Market_Rent      1   1121369 6784646398 86978
    ## + green:hd_total07              2   3117737 6782650031 86978
    ## + amenities:City_Market_Rent    1    760076 6785007692 86979
    ## + renovated:amenities           1    717666 6785050102 86979
    ## + empl_gr:renovated             1    650858 6785116910 86979
    ## + renovated:hd_total07          1    548258 6785219510 86979
    ## + empl_gr:green                 2   2593691 6783174077 86979
    ## + empl_gr:hd_total07            1    416056 6785351712 86979
    ## + empl_gr:stories               1    257856 6785509911 86979
    ## + amenities:cd_total_07         1    109236 6785658531 86979
    ## + size:age                      1     44924 6785722844 86979
    ## + age:hd_total07                1     13404 6785754364 86979
    ## + size:renovated                1     10791 6785756977 86979
    ## + stories:cd_total_07           1      1251 6785766517 86979
    ## + age:green                     2   1353256 6784414512 86980
    ## + green:cd_total_07             2   1226809 6784540959 86980
    ## + green:City_Market_Rent        2   1199598 6784568170 86980
    ## + renovated:green               2    624793 6785142974 86981
    ## - hd_total07                    1   7253302 6793021070 86982
    ## - cd_total_07                   1  18778789 6804546557 86993
    ## - stories                       1  20583209 6806350977 86994
    ## - green                         2  24811678 6810579445 86996
    ## - amenities                     1  50999980 6836767747 87022
    ## - age                           1  55035321 6840803088 87026
    ## - size:City_Market_Rent         1 296083853 7081851621 87242
    ## 
    ## Step:  AIC=86955.23
    ## rpsf ~ size + empl_gr + stories + age + renovated + green + amenities + 
    ##     cd_total_07 + hd_total07 + City_Market_Rent + size:City_Market_Rent + 
    ##     size:stories
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + cd_total_07:hd_total07        1   9503721 6750250260 86948
    ## + amenities:hd_total07          1   9477822 6750276159 86948
    ## + green:amenities               1   8878437 6750875543 86949
    ## + size:amenities                1   8361735 6751392246 86949
    ## + empl_gr:age                   1   8258428 6751495552 86950
    ## + stories:amenities             1   7677633 6752076348 86950
    ## + size:hd_total07               1   4723887 6755030093 86953
    ## + age:cd_total_07               1   4508773 6755245208 86953
    ## + empl_gr:amenities             1   4326120 6755427861 86953
    ## + empl_gr:City_Market_Rent      1   3894334 6755859646 86954
    ## + renovated:cd_total_07         1   3653756 6756100224 86954
    ## + stories:hd_total07            1   3395203 6756358778 86954
    ## - renovated                     1    944267 6760698247 86954
    ## - empl_gr                       1   1292358 6761046338 86954
    ## + size:empl_gr                  1   2837279 6756916702 86955
    ## + age:City_Market_Rent          1   2697661 6757056319 86955
    ## + size:green                    2   4801721 6754952260 86955
    ## + cd_total_07:City_Market_Rent  1   2601274 6757152706 86955
    ## + empl_gr:cd_total_07           1   2441948 6757312033 86955
    ## <none>                                      6759753981 86955
    ## + age:renovated                 1   2067314 6757686666 86955
    ## + stories:renovated             1   2052043 6757701938 86955
    ## + size:age                      1   1848283 6757905698 86956
    ## + renovated:City_Market_Rent    1   1674801 6758079180 86956
    ## + size:cd_total_07              1   1674566 6758079415 86956
    ## + hd_total07:City_Market_Rent   1   1650446 6758103534 86956
    ## + stories:green                 2   3770431 6755983550 86956
    ## + age:amenities                 1   1346479 6758407501 86956
    ## + green:hd_total07              2   3083154 6756670827 86956
    ## + amenities:City_Market_Rent    1    839898 6758914082 86956
    ## + renovated:hd_total07          1    773912 6758980068 86957
    ## + empl_gr:renovated             1    650924 6759103056 86957
    ## + renovated:amenities           1    635528 6759118452 86957
    ## + size:renovated                1    522655 6759231326 86957
    ## + empl_gr:hd_total07            1    478611 6759275369 86957
    ## + stories:City_Market_Rent      1    451131 6759302850 86957
    ## + stories:age                   1    155136 6759598844 86957
    ## + empl_gr:green                 2   2289069 6757464911 86957
    ## + empl_gr:stories               1     62028 6759691952 86957
    ## + amenities:cd_total_07         1     54316 6759699665 86957
    ## + age:hd_total07                1      6810 6759747170 86957
    ## + stories:cd_total_07           1       133 6759753848 86957
    ## - hd_total07                    1   4494247 6764248228 86957
    ## + age:green                     2   1384050 6758369931 86958
    ## + green:cd_total_07             2   1242616 6758511365 86958
    ## + green:City_Market_Rent        2    978760 6758775221 86958
    ## + renovated:green               2    650896 6759103084 86959
    ## - green                         2  22348019 6782101999 86972
    ## - cd_total_07                   1  23335995 6783089976 86975
    ## - size:stories                  1  26013787 6785767768 86977
    ## - amenities                     1  35074904 6794828885 86986
    ## - age                           1  51851833 6811605813 87001
    ## - size:City_Market_Rent         1 305171202 7064925183 87230
    ## 
    ## Step:  AIC=86948.42
    ## rpsf ~ size + empl_gr + stories + age + renovated + green + amenities + 
    ##     cd_total_07 + hd_total07 + City_Market_Rent + size:City_Market_Rent + 
    ##     size:stories + cd_total_07:hd_total07
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + amenities:hd_total07          1  10447026 6739803234 86941
    ## + green:amenities               1   9036077 6741214183 86942
    ## + empl_gr:age                   1   8150956 6742099303 86943
    ## + size:amenities                1   7768073 6742482187 86943
    ## + stories:amenities             1   6520221 6743730038 86944
    ## + size:hd_total07               1   4892523 6745357736 86946
    ## + empl_gr:amenities             1   4141607 6746108652 86947
    ## - empl_gr                       1    216086 6750466346 86947
    ## - renovated                     1    763997 6751014256 86947
    ## + age:City_Market_Rent          1   3543415 6746706845 86947
    ## + size:empl_gr                  1   3047322 6747202937 86948
    ## + empl_gr:City_Market_Rent      1   3041787 6747208473 86948
    ## + empl_gr:cd_total_07           1   2690533 6747559726 86948
    ## + renovated:cd_total_07         1   2600735 6747649525 86948
    ## + stories:hd_total07            1   2568065 6747682194 86948
    ## + size:green                    2   4538567 6745711693 86948
    ## <none>                                      6750250260 86948
    ## + age:renovated                 1   1911274 6748338986 86949
    ## + size:age                      1   1904541 6748345719 86949
    ## + age:cd_total_07               1   1787902 6748462358 86949
    ## + stories:renovated             1   1538831 6748711428 86949
    ## + stories:green                 2   3674471 6746575789 86949
    ## + hd_total07:City_Market_Rent   1   1498782 6748751478 86949
    ## + size:cd_total_07              1   1360961 6748889298 86949
    ## + age:amenities                 1   1267026 6748983233 86949
    ## + renovated:City_Market_Rent    1   1098298 6749151962 86949
    ## + green:hd_total07              2   3045087 6747205173 86950
    ## + cd_total_07:City_Market_Rent  1    787548 6749462712 86950
    ## + empl_gr:hd_total07            1    776204 6749474056 86950
    ## + amenities:City_Market_Rent    1    718654 6749531606 86950
    ## + size:renovated                1    682321 6749567939 86950
    ## + empl_gr:renovated             1    598960 6749651299 86950
    ## + renovated:amenities           1    516705 6749733555 86950
    ## + renovated:hd_total07          1    439282 6749810978 86950
    ## + stories:age                   1    434479 6749815781 86950
    ## + stories:City_Market_Rent      1    362247 6749888012 86950
    ## + amenities:cd_total_07         1    320812 6749929448 86950
    ## + empl_gr:green                 2   2329701 6747920559 86950
    ## + empl_gr:stories               1     96894 6750153365 86950
    ## + age:hd_total07                1     41157 6750209103 86950
    ## + stories:cd_total_07           1     19785 6750230475 86950
    ## + age:green                     2   1330583 6748919677 86951
    ## + green:cd_total_07             2   1072725 6749177535 86951
    ## + green:City_Market_Rent        2    855381 6749394878 86952
    ## + renovated:green               2    648279 6749601981 86952
    ## - cd_total_07:hd_total07        1   9503721 6759753981 86955
    ## - green                         2  22523671 6772773931 86965
    ## - size:stories                  1  26190689 6776440949 86971
    ## - amenities                     1  39000442 6789250702 86982
    ## - age                           1  53836635 6804086895 86996
    ## - size:City_Market_Rent         1 299058158 7049308418 87218
    ## 
    ## Step:  AIC=86940.73
    ## rpsf ~ size + empl_gr + stories + age + renovated + green + amenities + 
    ##     cd_total_07 + hd_total07 + City_Market_Rent + size:City_Market_Rent + 
    ##     size:stories + cd_total_07:hd_total07 + amenities:hd_total07
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + green:amenities               1  10997105 6728806129 86933
    ## + empl_gr:age                   1   8868892 6730934341 86934
    ## + size:amenities                1   5514168 6734289066 86938
    ## - empl_gr                       1    124793 6739928026 86939
    ## + stories:amenities             1   4044549 6735758684 86939
    ## + empl_gr:cd_total_07           1   3826246 6735976987 86939
    ## + empl_gr:City_Market_Rent      1   3639648 6736163585 86939
    ## - renovated                     1    948116 6740751350 86940
    ## + renovated:cd_total_07         1   3030077 6736773157 86940
    ## + age:City_Market_Rent          1   2752882 6737050352 86940
    ## + size:green                    2   4890627 6734912606 86940
    ## + size:cd_total_07              1   2651984 6737151249 86940
    ## + empl_gr:amenities             1   2463040 6737340193 86940
    ## + age:renovated                 1   2286038 6737517195 86941
    ## <none>                                      6739803234 86941
    ## + age:cd_total_07               1   1991607 6737811627 86941
    ## + size:empl_gr                  1   1946574 6737856660 86941
    ## + stories:green                 2   3977694 6735825540 86941
    ## + stories:renovated             1   1411013 6738392220 86941
    ## + hd_total07:City_Market_Rent   1   1289610 6738513623 86942
    ## + empl_gr:hd_total07            1   1271636 6738531597 86942
    ## + renovated:City_Market_Rent    1   1180911 6738622323 86942
    ## + size:age                      1   1174098 6738629136 86942
    ## + cd_total_07:City_Market_Rent  1   1148587 6738654647 86942
    ## + size:hd_total07               1   1112045 6738691189 86942
    ## + size:renovated                1    846023 6738957210 86942
    ## + stories:age                   1    770899 6739032334 86942
    ## + renovated:hd_total07          1    770735 6739032498 86942
    ## + empl_gr:renovated             1    741524 6739061709 86942
    ## + age:hd_total07                1    606035 6739197198 86942
    ## + empl_gr:green                 2   2485148 6737318086 86942
    ## + stories:cd_total_07           1    217640 6739585594 86943
    ## + renovated:amenities           1    188006 6739615228 86943
    ## + stories:hd_total07            1    166857 6739636376 86943
    ## + stories:City_Market_Rent      1    139663 6739663570 86943
    ## + amenities:cd_total_07         1    129970 6739673263 86943
    ## + age:amenities                 1     74021 6739729212 86943
    ## + amenities:City_Market_Rent    1     10667 6739792567 86943
    ## + empl_gr:stories               1       154 6739803080 86943
    ## + green:hd_total07              2   2049632 6737753602 86943
    ## + age:green                     2   1279473 6738523761 86944
    ## + green:cd_total_07             2    989039 6738814195 86944
    ## + green:City_Market_Rent        2    652766 6739150468 86944
    ## + renovated:green               2    554987 6739248247 86944
    ## - amenities:hd_total07          1  10447026 6750250260 86948
    ## - cd_total_07:hd_total07        1  10472925 6750276159 86948
    ## - green                         2  20819328 6760622562 86956
    ## - size:stories                  1  25395635 6765198869 86962
    ## - age                           1  56398427 6796201660 86991
    ## - size:City_Market_Rent         1 290329032 7030132266 87203
    ## 
    ## Step:  AIC=86932.51
    ## rpsf ~ size + empl_gr + stories + age + renovated + green + amenities + 
    ##     cd_total_07 + hd_total07 + City_Market_Rent + size:City_Market_Rent + 
    ##     size:stories + cd_total_07:hd_total07 + amenities:hd_total07 + 
    ##     green:amenities
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + empl_gr:age                   1   8414315 6720391813 86927
    ## - empl_gr                       1     89717 6728895846 86931
    ## + size:amenities                1   4094449 6724711679 86931
    ## + stories:amenities             1   3783006 6725023123 86931
    ## + empl_gr:cd_total_07           1   3702384 6725103745 86931
    ## + empl_gr:City_Market_Rent      1   3532860 6725273269 86931
    ## - renovated                     1    939831 6729745960 86931
    ## + renovated:cd_total_07         1   2972268 6725833860 86932
    ## + size:cd_total_07              1   2640618 6726165511 86932
    ## + age:City_Market_Rent          1   2599149 6726206980 86932
    ## + empl_gr:amenities             1   2570773 6726235356 86932
    ## <none>                                      6728806129 86933
    ## + age:cd_total_07               1   2132681 6726673448 86933
    ## + age:renovated                 1   2012557 6726793572 86933
    ## + stories:renovated             1   1754037 6727052092 86933
    ## + size:empl_gr                  1   1597468 6727208660 86933
    ## + size:age                      1   1506153 6727299976 86933
    ## + hd_total07:City_Market_Rent   1   1435728 6727370401 86933
    ## + renovated:City_Market_Rent    1   1287023 6727519106 86933
    ## + cd_total_07:City_Market_Rent  1   1277235 6727528894 86933
    ## + empl_gr:hd_total07            1   1203512 6727602616 86933
    ## + size:hd_total07               1   1122701 6727683428 86933
    ## + age:hd_total07                1    816690 6727989439 86934
    ## + age:amenities                 1    762007 6728044122 86934
    ## + empl_gr:renovated             1    726836 6728079292 86934
    ## + renovated:hd_total07          1    679549 6728126579 86934
    ## + size:renovated                1    663946 6728142183 86934
    ## + renovated:amenities           1    531272 6728274857 86934
    ## + stories:age                   1    487403 6728318726 86934
    ## + stories:hd_total07            1    180189 6728625939 86934
    ## + stories:cd_total_07           1    179459 6728626670 86934
    ## + stories:City_Market_Rent      1    110590 6728695539 86934
    ## + amenities:cd_total_07         1     68903 6728737226 86934
    ## + empl_gr:stories               1      6751 6728799378 86935
    ## + amenities:City_Market_Rent    1       328 6728805800 86935
    ## + empl_gr:green                 2   1878753 6726927376 86935
    ## + size:green                    2   1617812 6727188317 86935
    ## + green:hd_total07              2   1575899 6727230230 86935
    ## + renovated:green               2   1143034 6727663094 86935
    ## + age:green                     2   1052788 6727753341 86936
    ## + stories:green                 2   1031469 6727774659 86936
    ## + green:City_Market_Rent        2    928914 6727877215 86936
    ## + green:cd_total_07             2    758843 6728047286 86936
    ## - cd_total_07:hd_total07        1  10752099 6739558227 86941
    ## - green:amenities               1  10997105 6739803234 86941
    ## - amenities:hd_total07          1  12408054 6741214183 86942
    ## - size:stories                  1  25269514 6754075643 86954
    ## - age                           1  53941618 6782747747 86980
    ## - size:City_Market_Rent         1 286488403 7015294532 87191
    ## 
    ## Step:  AIC=86926.68
    ## rpsf ~ size + empl_gr + stories + age + renovated + green + amenities + 
    ##     cd_total_07 + hd_total07 + City_Market_Rent + size:City_Market_Rent + 
    ##     size:stories + cd_total_07:hd_total07 + amenities:hd_total07 + 
    ##     green:amenities + empl_gr:age
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + size:amenities                1   4061357 6716330456 86925
    ## + stories:amenities             1   3814490 6716577323 86925
    ## - renovated                     1    972404 6721364218 86926
    ## + size:cd_total_07              1   3149895 6717241918 86926
    ## + age:City_Market_Rent          1   3124590 6717267223 86926
    ## + empl_gr:amenities             1   2624075 6717767738 86926
    ## <none>                                      6720391813 86927
    ## + cd_total_07:City_Market_Rent  1   2121869 6718269944 86927
    ## + stories:renovated             1   1696803 6718695010 86927
    ## + age:renovated                 1   1549235 6718842578 86927
    ## + renovated:cd_total_07         1   1516633 6718875180 86927
    ## + hd_total07:City_Market_Rent   1   1462610 6718929203 86927
    ## + size:age                      1   1432666 6718959147 86927
    ## + renovated:City_Market_Rent    1   1221052 6719170762 86928
    ## + age:hd_total07                1   1214179 6719177634 86928
    ## + empl_gr:cd_total_07           1    949153 6719442660 86928
    ## + size:hd_total07               1    891938 6719499876 86928
    ## + size:empl_gr                  1    811465 6719580348 86928
    ## + size:renovated                1    749299 6719642514 86928
    ## + age:cd_total_07               1    638225 6719753589 86928
    ## + age:amenities                 1    628335 6719763478 86928
    ## + renovated:amenities           1    508965 6719882849 86928
    ## + stories:age                   1    468977 6719922837 86928
    ## + renovated:hd_total07          1    367938 6720023875 86928
    ## + empl_gr:City_Market_Rent      1    333623 6720058190 86928
    ## + empl_gr:renovated             1    313006 6720078808 86928
    ## + stories:hd_total07            1    151582 6720240232 86929
    ## + stories:cd_total_07           1    125344 6720266470 86929
    ## + stories:City_Market_Rent      1     85303 6720306510 86929
    ## + amenities:cd_total_07         1     28892 6720362922 86929
    ## + empl_gr:hd_total07            1     22866 6720368948 86929
    ## + empl_gr:stories               1     15232 6720376582 86929
    ## + amenities:City_Market_Rent    1        26 6720391788 86929
    ## + size:green                    2   1608829 6718782984 86929
    ## + green:hd_total07              2   1324951 6719066862 86929
    ## + renovated:green               2   1217768 6719174046 86930
    ## + age:green                     2   1214251 6719177562 86930
    ## + stories:green                 2   1009986 6719381828 86930
    ## + green:City_Market_Rent        2    947650 6719444163 86930
    ## + empl_gr:green                 2    467184 6719924629 86930
    ## + green:cd_total_07             2    302604 6720089209 86930
    ## - empl_gr:age                   1   8414315 6728806129 86933
    ## - green:amenities               1  10542528 6730934341 86934
    ## - cd_total_07:hd_total07        1  10664786 6731056599 86935
    ## - amenities:hd_total07          1  13120617 6733512430 86937
    ## - size:stories                  1  24653277 6745045090 86948
    ## - size:City_Market_Rent         1 288311081 7008702894 87188
    ## 
    ## Step:  AIC=86924.9
    ## rpsf ~ size + empl_gr + stories + age + renovated + green + amenities + 
    ##     cd_total_07 + hd_total07 + City_Market_Rent + size:City_Market_Rent + 
    ##     size:stories + cd_total_07:hd_total07 + amenities:hd_total07 + 
    ##     green:amenities + empl_gr:age + size:amenities
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## - renovated                     1   1027938 6717358394 86924
    ## + age:City_Market_Rent          1   2848737 6713481719 86924
    ## + empl_gr:amenities             1   2553896 6713776560 86925
    ## + size:cd_total_07              1   2471734 6713858722 86925
    ## <none>                                      6716330456 86925
    ## + cd_total_07:City_Market_Rent  1   2063080 6714267376 86925
    ## + renovated:cd_total_07         1   1570982 6714759475 86925
    ## + size:age                      1   1526765 6714803691 86925
    ## + hd_total07:City_Market_Rent   1   1518592 6714811864 86925
    ## + size:hd_total07               1   1434154 6714896302 86926
    ## + stories:renovated             1   1399918 6714930539 86926
    ## + renovated:City_Market_Rent    1   1358467 6714971990 86926
    ## + age:amenities                 1   1321786 6715008671 86926
    ## + age:renovated                 1   1248088 6715082369 86926
    ## + age:hd_total07                1   1131922 6715198535 86926
    ## + empl_gr:cd_total_07           1   1050569 6715279888 86926
    ## + size:renovated                1    850540 6715479916 86926
    ## + size:empl_gr                  1    775562 6715554894 86926
    ## + age:cd_total_07               1    666274 6715664183 86926
    ## + renovated:amenities           1    621699 6715708757 86926
    ## + stories:amenities             1    477909 6715852547 86926
    ## + renovated:hd_total07          1    444239 6715886218 86926
    ## + stories:age                   1    331347 6715999109 86927
    ## + empl_gr:City_Market_Rent      1    327572 6716002884 86927
    ## + stories:hd_total07            1    310039 6716020417 86927
    ## + empl_gr:renovated             1    271715 6716058741 86927
    ## - size:amenities                1   4061357 6720391813 86927
    ## + empl_gr:hd_total07            1     63862 6716266594 86927
    ## + stories:cd_total_07           1     36516 6716293940 86927
    ## + stories:City_Market_Rent      1     34209 6716296247 86927
    ## + amenities:City_Market_Rent    1     16795 6716313661 86927
    ## + empl_gr:stories               1     16088 6716314369 86927
    ## + amenities:cd_total_07         1       774 6716329682 86927
    ## + size:green                    2   1839879 6714490577 86927
    ## + green:hd_total07              2   1432614 6714897843 86928
    ## + age:green                     2   1350383 6714980073 86928
    ## + renovated:green               2   1220833 6715109623 86928
    ## + stories:green                 2   1061381 6715269075 86928
    ## + green:City_Market_Rent        2    917187 6715413269 86928
    ## + empl_gr:green                 2    432030 6715898426 86928
    ## + green:cd_total_07             2    297802 6716032655 86929
    ## - empl_gr:age                   1   8381223 6724711679 86931
    ## - green:amenities               1   9161963 6725492419 86931
    ## - cd_total_07:hd_total07        1  10083027 6726413483 86932
    ## - amenities:hd_total07          1  10710787 6727041243 86933
    ## - size:stories                  1  17181968 6733512424 86939
    ## - size:City_Market_Rent         1 286857161 7003187617 87185
    ## 
    ## Step:  AIC=86923.86
    ## rpsf ~ size + empl_gr + stories + age + green + amenities + cd_total_07 + 
    ##     hd_total07 + City_Market_Rent + size:City_Market_Rent + size:stories + 
    ##     cd_total_07:hd_total07 + amenities:hd_total07 + green:amenities + 
    ##     empl_gr:age + size:amenities
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + age:City_Market_Rent          1   2885965 6714472429 86923
    ## + empl_gr:amenities             1   2486604 6714871790 86924
    ## + size:cd_total_07              1   2425417 6714932977 86924
    ## <none>                                      6717358394 86924
    ## + cd_total_07:City_Market_Rent  1   2037481 6715320913 86924
    ## + hd_total07:City_Market_Rent   1   1571664 6715786731 86924
    ## + size:age                      1   1485097 6715873297 86924
    ## + size:hd_total07               1   1440497 6715917897 86925
    ## + age:hd_total07                1   1316326 6716042068 86925
    ## + age:amenities                 1   1131897 6716226497 86925
    ## + empl_gr:cd_total_07           1   1050596 6716307798 86925
    ## + renovated                     1   1027938 6716330456 86925
    ## + size:empl_gr                  1    808055 6716550339 86925
    ## + age:cd_total_07               1    685656 6716672738 86925
    ## + stories:amenities             1    495676 6716862718 86925
    ## + stories:age                   1    426860 6716931534 86925
    ## + empl_gr:City_Market_Rent      1    332085 6717026309 86926
    ## + stories:hd_total07            1    306046 6717052349 86926
    ## - size:amenities                1   4005823 6721364218 86926
    ## + empl_gr:hd_total07            1     73234 6717285160 86926
    ## + stories:cd_total_07           1     33603 6717324791 86926
    ## + amenities:City_Market_Rent    1     22745 6717335649 86926
    ## + empl_gr:stories               1     19797 6717338597 86926
    ## + stories:City_Market_Rent      1     10303 6717348091 86926
    ## + amenities:cd_total_07         1       264 6717358131 86926
    ## + size:green                    2   1960889 6715397505 86926
    ## + green:hd_total07              2   1459432 6715898963 86927
    ## + age:green                     2   1458383 6715900011 86927
    ## + stories:green                 2   1154271 6716204123 86927
    ## + green:City_Market_Rent        2    997087 6716361307 86927
    ## + empl_gr:green                 2    435431 6716922963 86927
    ## + green:cd_total_07             2    275547 6717082847 86928
    ## - empl_gr:age                   1   8348020 6725706414 86930
    ## - green:amenities               1   9179284 6726537678 86930
    ## - cd_total_07:hd_total07        1  10290475 6727648869 86931
    ## - amenities:hd_total07          1  10531512 6727889906 86932
    ## - size:stories                  1  17633735 6734992129 86938
    ## - size:City_Market_Rent         1 285955368 7003313762 87183
    ## 
    ## Step:  AIC=86923.17
    ## rpsf ~ size + empl_gr + stories + age + green + amenities + cd_total_07 + 
    ##     hd_total07 + City_Market_Rent + size:City_Market_Rent + size:stories + 
    ##     cd_total_07:hd_total07 + amenities:hd_total07 + green:amenities + 
    ##     empl_gr:age + size:amenities + age:City_Market_Rent
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + age:hd_total07                1   3212828 6711259601 86922
    ## + empl_gr:amenities             1   2674499 6711797931 86923
    ## + cd_total_07:City_Market_Rent  1   2446448 6712025981 86923
    ## <none>                                      6714472429 86923
    ## + size:cd_total_07              1   2076740 6712395689 86923
    ## + age:cd_total_07               1   1603279 6712869150 86924
    ## + age:amenities                 1   1419511 6713052918 86924
    ## - age:City_Market_Rent          1   2885965 6717358394 86924
    ## + size:age                      1   1328901 6713143528 86924
    ## + size:hd_total07               1   1269197 6713203232 86924
    ## + empl_gr:cd_total_07           1   1004827 6713467603 86924
    ## + renovated                     1    990710 6713481719 86924
    ## + size:empl_gr                  1    797896 6713674534 86924
    ## + stories:age                   1    689645 6713782784 86925
    ## - size:amenities                1   3731256 6718203685 86925
    ## + stories:amenities             1    555699 6713916730 86925
    ## + hd_total07:City_Market_Rent   1    456346 6714016084 86925
    ## + stories:hd_total07            1    233237 6714239192 86925
    ## + empl_gr:City_Market_Rent      1    165242 6714307188 86925
    ## + stories:City_Market_Rent      1     97101 6714375328 86925
    ## + amenities:City_Market_Rent    1     81280 6714391150 86925
    ## + empl_gr:hd_total07            1     70077 6714402352 86925
    ## + empl_gr:stories               1     18466 6714453963 86925
    ## + amenities:cd_total_07         1     12764 6714459665 86925
    ## + stories:cd_total_07           1     10407 6714462023 86925
    ## + size:green                    2   1978337 6712494093 86925
    ## + age:green                     2   1350010 6713122419 86926
    ## + green:hd_total07              2   1226252 6713246178 86926
    ## + stories:green                 2   1178237 6713294193 86926
    ## + green:City_Market_Rent        2    470827 6714001602 86927
    ## + empl_gr:green                 2    399196 6714073233 86927
    ## + green:cd_total_07             2    356515 6714115914 86927
    ## - empl_gr:age                   1   8851884 6723324313 86929
    ## - green:amenities               1   9061401 6723533831 86930
    ## - amenities:hd_total07          1   9815023 6724287452 86930
    ## - cd_total_07:hd_total07        1  11052918 6725525347 86931
    ## - size:stories                  1  17819046 6732291476 86938
    ## - size:City_Market_Rent         1 275554085 6990026514 87173
    ## 
    ## Step:  AIC=86922.18
    ## rpsf ~ size + empl_gr + stories + age + green + amenities + cd_total_07 + 
    ##     hd_total07 + City_Market_Rent + size:City_Market_Rent + size:stories + 
    ##     cd_total_07:hd_total07 + amenities:hd_total07 + green:amenities + 
    ##     empl_gr:age + size:amenities + age:City_Market_Rent + age:hd_total07
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + age:cd_total_07               1   2780337 6708479265 86922
    ## + empl_gr:amenities             1   2505042 6708754559 86922
    ## + cd_total_07:City_Market_Rent  1   2303965 6708955636 86922
    ## <none>                                      6711259601 86922
    ## + size:hd_total07               1   1944477 6709315124 86922
    ## + size:cd_total_07              1   1878148 6709381453 86922
    ## + stories:age                   1   1300906 6709958695 86923
    ## - age:hd_total07                1   3212828 6714472429 86923
    ## + empl_gr:cd_total_07           1    887359 6710372242 86923
    ## + age:amenities                 1    864232 6710395369 86923
    ## + size:empl_gr                  1    831069 6710428532 86923
    ## + size:age                      1    789579 6710470023 86923
    ## - size:amenities                1   3506731 6714766332 86923
    ## + renovated                     1    687553 6710572048 86924
    ## + stories:amenities             1    600111 6710659490 86924
    ## + hd_total07:City_Market_Rent   1    516854 6710742747 86924
    ## + stories:hd_total07            1    459228 6710800373 86924
    ## + stories:City_Market_Rent      1    181927 6711077674 86924
    ## + amenities:City_Market_Rent    1    139051 6711120550 86924
    ## + empl_gr:City_Market_Rent      1    127934 6711131667 86924
    ## + empl_gr:hd_total07            1     41827 6711217774 86924
    ## + empl_gr:stories               1     19663 6711239939 86924
    ## + amenities:cd_total_07         1      2998 6711256603 86924
    ## + stories:cd_total_07           1       388 6711259213 86924
    ## + size:green                    2   2135995 6709123606 86924
    ## + green:hd_total07              2   2001427 6709258175 86924
    ## - age:City_Market_Rent          1   4782467 6716042068 86925
    ## + stories:green                 2   1284334 6709975267 86925
    ## + age:green                     2   1243949 6710015653 86925
    ## + green:City_Market_Rent        2    476912 6710782689 86926
    ## + green:cd_total_07             2    399232 6710860369 86926
    ## + empl_gr:green                 2    368833 6710890768 86926
    ## - green:amenities               1   9438371 6720697972 86929
    ## - empl_gr:age                   1   9756457 6721016058 86929
    ## - amenities:hd_total07          1  11518180 6722777781 86931
    ## - cd_total_07:hd_total07        1  12460459 6723720060 86932
    ## - size:stories                  1  17930559 6729190160 86937
    ## - size:City_Market_Rent         1 275803647 6987063248 87172
    ## 
    ## Step:  AIC=86921.58
    ## rpsf ~ size + empl_gr + stories + age + green + amenities + cd_total_07 + 
    ##     hd_total07 + City_Market_Rent + size:City_Market_Rent + size:stories + 
    ##     cd_total_07:hd_total07 + amenities:hd_total07 + green:amenities + 
    ##     empl_gr:age + size:amenities + age:City_Market_Rent + age:hd_total07 + 
    ##     age:cd_total_07
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + size:cd_total_07              1   2896754 6705582510 86921
    ## + empl_gr:amenities             1   2574132 6705905133 86921
    ## + cd_total_07:City_Market_Rent  1   2427998 6706051267 86921
    ## <none>                                      6708479265 86922
    ## + stories:age                   1   1807026 6706672238 86922
    ## + size:hd_total07               1   1696262 6706783003 86922
    ## - age:cd_total_07               1   2780337 6711259601 86922
    ## + empl_gr:cd_total_07           1    918395 6707560870 86923
    ## - size:amenities                1   3459631 6711938895 86923
    ## + size:empl_gr                  1    754476 6707724788 86923
    ## + renovated                     1    595061 6707884204 86923
    ## + stories:amenities             1    557331 6707921934 86923
    ## + age:amenities                 1    466591 6708012674 86923
    ## + size:age                      1    400514 6708078750 86923
    ## + stories:City_Market_Rent      1    372853 6708106412 86923
    ## + hd_total07:City_Market_Rent   1    342007 6708137257 86923
    ## + stories:hd_total07            1    309724 6708169541 86923
    ## + amenities:City_Market_Rent    1    230036 6708249228 86923
    ## + size:green                    2   2365998 6706113267 86923
    ## + amenities:cd_total_07         1    107864 6708371401 86923
    ## + empl_gr:City_Market_Rent      1    101495 6708377770 86923
    ## + empl_gr:hd_total07            1     66616 6708412649 86924
    ## + stories:cd_total_07           1     28212 6708451052 86924
    ## + empl_gr:stories               1     18710 6708460554 86924
    ## - age:hd_total07                1   4389886 6712869150 86924
    ## + green:hd_total07              2   1942443 6706536822 86924
    ## + age:green                     2   1515363 6706963902 86924
    ## + stories:green                 2   1441833 6707037432 86924
    ## + green:City_Market_Rent        2    548931 6707930334 86925
    ## + empl_gr:green                 2    345195 6708134070 86925
    ## + green:cd_total_07             2    133271 6708345993 86925
    ## - age:City_Market_Rent          1   6637944 6715117209 86926
    ## - empl_gr:age                   1   7225919 6715705183 86926
    ## - cd_total_07:hd_total07        1   9066227 6717545491 86928
    ## - green:amenities               1   9695294 6718174558 86929
    ## - amenities:hd_total07          1  11914447 6720393712 86931
    ## - size:stories                  1  17672963 6726152228 86936
    ## - size:City_Market_Rent         1 277751945 6986231210 87173
    ## 
    ## Step:  AIC=86920.88
    ## rpsf ~ size + empl_gr + stories + age + green + amenities + cd_total_07 + 
    ##     hd_total07 + City_Market_Rent + size:City_Market_Rent + size:stories + 
    ##     cd_total_07:hd_total07 + amenities:hd_total07 + green:amenities + 
    ##     empl_gr:age + size:amenities + age:City_Market_Rent + age:hd_total07 + 
    ##     age:cd_total_07 + size:cd_total_07
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + stories:cd_total_07           1   5497953 6700084558 86918
    ## + size:hd_total07               1   3683303 6701899207 86919
    ## + empl_gr:amenities             1   3259584 6702322926 86920
    ## + cd_total_07:City_Market_Rent  1   2609460 6702973051 86920
    ## <none>                                      6705582510 86921
    ## + stories:age                   1   1708791 6703873719 86921
    ## + size:empl_gr                  1   1695886 6703886625 86921
    ## - size:amenities                1   2797157 6708379667 86921
    ## - size:cd_total_07              1   2896754 6708479265 86922
    ## + stories:hd_total07            1   1041270 6704541240 86922
    ## + empl_gr:cd_total_07           1    775406 6704807105 86922
    ## + renovated                     1    629830 6704952680 86922
    ## + size:age                      1    606933 6704975577 86922
    ## + stories:amenities             1    511651 6705070859 86922
    ## - age:cd_total_07               1   3798943 6709381453 86922
    ## + amenities:City_Market_Rent    1    412423 6705170087 86922
    ## + age:amenities                 1    407885 6705174626 86923
    ## + empl_gr:stories               1    237737 6705344773 86923
    ## + hd_total07:City_Market_Rent   1    217835 6705364675 86923
    ## + stories:City_Market_Rent      1    143014 6705439496 86923
    ## + amenities:cd_total_07         1     81044 6705501466 86923
    ## + empl_gr:City_Market_Rent      1     40663 6705541847 86923
    ## + empl_gr:hd_total07            1     27370 6705555140 86923
    ## + green:hd_total07              2   2116400 6703466111 86923
    ## - age:hd_total07                1   4374835 6709957345 86923
    ## + size:green                    2   1697409 6703885101 86923
    ## + age:green                     2   1433939 6704148571 86924
    ## + stories:green                 2    989551 6704592959 86924
    ## + empl_gr:green                 2    500336 6705082174 86924
    ## + green:City_Market_Rent        2    465498 6705117012 86924
    ## + green:cd_total_07             2    285528 6705296983 86925
    ## - age:City_Market_Rent          1   6423705 6712006215 86925
    ## - empl_gr:age                   1   7258901 6712841411 86926
    ## - cd_total_07:hd_total07        1   8147898 6713730408 86926
    ## - green:amenities               1   9828504 6715411014 86928
    ## - amenities:hd_total07          1  13521985 6719104496 86931
    ## - size:stories                  1  18025943 6723608453 86936
    ## - size:City_Market_Rent         1 260469131 6966051641 87157
    ## 
    ## Step:  AIC=86917.75
    ## rpsf ~ size + empl_gr + stories + age + green + amenities + cd_total_07 + 
    ##     hd_total07 + City_Market_Rent + size:City_Market_Rent + size:stories + 
    ##     cd_total_07:hd_total07 + amenities:hd_total07 + green:amenities + 
    ##     empl_gr:age + size:amenities + age:City_Market_Rent + age:hd_total07 + 
    ##     age:cd_total_07 + size:cd_total_07 + stories:cd_total_07
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + size:hd_total07               1   4169422 6695915136 86916
    ## + empl_gr:amenities             1   2937519 6697147038 86917
    ## + cd_total_07:City_Market_Rent  1   2847755 6697236803 86917
    ## <none>                                      6700084558 86918
    ## + stories:age                   1   2138943 6697945614 86918
    ## - size:amenities                1   2663314 6702747872 86918
    ## + size:empl_gr                  1   1280017 6698804540 86919
    ## + empl_gr:cd_total_07           1    866428 6699218130 86919
    ## + size:age                      1    834771 6699249787 86919
    ## + renovated                     1    630207 6699454351 86919
    ## + stories:City_Market_Rent      1    602315 6699482242 86919
    ## + amenities:City_Market_Rent    1    487931 6699596627 86919
    ## + stories:hd_total07            1    441045 6699643512 86919
    ## + age:amenities                 1    426432 6699658126 86919
    ## + stories:amenities             1    319277 6699765281 86919
    ## + green:hd_total07              2   2313845 6697770713 86920
    ## + empl_gr:City_Market_Rent      1    167848 6699916709 86920
    ## + amenities:cd_total_07         1    114376 6699970182 86920
    ## + hd_total07:City_Market_Rent   1    105363 6699979195 86920
    ## + empl_gr:hd_total07            1     38908 6700045650 86920
    ## + empl_gr:stories               1      9069 6700075488 86920
    ## - age:cd_total_07               1   4522133 6704606690 86920
    ## + size:green                    2   1583612 6698500945 86920
    ## + age:green                     2   1420203 6698664355 86920
    ## - age:hd_total07                1   5145078 6705229635 86921
    ## + stories:green                 2   1000725 6699083832 86921
    ## - stories:cd_total_07           1   5497953 6705582510 86921
    ## + green:cd_total_07             2    637511 6699447046 86921
    ## + empl_gr:green                 2    593630 6699490928 86921
    ## + green:City_Market_Rent        2    379477 6699705080 86921
    ## - cd_total_07:hd_total07        1   6116727 6706201284 86921
    ## - age:City_Market_Rent          1   6686928 6706771485 86922
    ## - size:cd_total_07              1   8366495 6708451052 86924
    ## - empl_gr:age                   1   8611440 6708695997 86924
    ## - green:amenities               1  10202452 6710287010 86925
    ## - amenities:hd_total07          1  14462422 6714546979 86929
    ## - size:stories                  1  18240054 6718324611 86933
    ## - size:City_Market_Rent         1 261706295 6961790853 87155
    ## 
    ## Step:  AIC=86915.85
    ## rpsf ~ size + empl_gr + stories + age + green + amenities + cd_total_07 + 
    ##     hd_total07 + City_Market_Rent + size:City_Market_Rent + size:stories + 
    ##     cd_total_07:hd_total07 + amenities:hd_total07 + green:amenities + 
    ##     empl_gr:age + size:amenities + age:City_Market_Rent + age:hd_total07 + 
    ##     age:cd_total_07 + size:cd_total_07 + stories:cd_total_07 + 
    ##     size:hd_total07
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + stories:age                   1   3453777 6692461359 86915
    ## + empl_gr:amenities             1   3273920 6692641216 86915
    ## + stories:hd_total07            1   2948452 6692966684 86915
    ## + cd_total_07:City_Market_Rent  1   2658793 6693256342 86915
    ## <none>                                      6695915136 86916
    ## + size:empl_gr                  1   1123532 6694791603 86917
    ## - size:amenities                1   3284958 6699200094 86917
    ## + empl_gr:cd_total_07           1    917639 6694997496 86917
    ## + renovated                     1    597164 6695317972 86917
    ## + stories:amenities             1    517233 6695397903 86917
    ## + stories:City_Market_Rent      1    456988 6695458147 86917
    ## + age:amenities                 1    436727 6695478409 86917
    ## + amenities:City_Market_Rent    1    347552 6695567584 86918
    ## + amenities:cd_total_07         1    248307 6695666829 86918
    ## + hd_total07:City_Market_Rent   1    168390 6695746746 86918
    ## + empl_gr:City_Market_Rent      1    162187 6695752949 86918
    ## + size:age                      1    151497 6695763638 86918
    ## + green:hd_total07              2   2254116 6693661020 86918
    ## - size:hd_total07               1   4169422 6700084558 86918
    ## + empl_gr:hd_total07            1     66271 6695848865 86918
    ## + empl_gr:stories               1      9059 6695906077 86918
    ## + size:green                    2   2115135 6693800001 86918
    ## - age:cd_total_07               1   4560148 6700475284 86918
    ## + age:green                     2   1401120 6694514016 86919
    ## + stories:green                 2   1314627 6694600509 86919
    ## + empl_gr:green                 2    604264 6695310872 86919
    ## + green:cd_total_07             2    581814 6695333322 86919
    ## - cd_total_07:hd_total07        1   5943503 6701858639 86919
    ## - stories:cd_total_07           1   5984072 6701899207 86919
    ## + green:City_Market_Rent        2    390237 6695524899 86919
    ## - age:hd_total07                1   6384259 6702299395 86920
    ## - age:City_Market_Rent          1   6477356 6702392492 86920
    ## - amenities:hd_total07          1   8251030 6704166165 86922
    ## - empl_gr:age                   1   8472805 6704387940 86922
    ## - green:amenities               1  10193296 6706108431 86923
    ## - size:cd_total_07              1  10633173 6706548309 86924
    ## - size:stories                  1  14781677 6710696813 86928
    ## - size:City_Market_Rent         1 239259115 6935174251 87134
    ## 
    ## Step:  AIC=86914.63
    ## rpsf ~ size + empl_gr + stories + age + green + amenities + cd_total_07 + 
    ##     hd_total07 + City_Market_Rent + size:City_Market_Rent + size:stories + 
    ##     cd_total_07:hd_total07 + amenities:hd_total07 + green:amenities + 
    ##     empl_gr:age + size:amenities + age:City_Market_Rent + age:hd_total07 + 
    ##     age:cd_total_07 + size:cd_total_07 + stories:cd_total_07 + 
    ##     size:hd_total07 + stories:age
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + size:age                      1   5960129 6686501230 86911
    ## + empl_gr:amenities             1   3343455 6689117903 86913
    ## + cd_total_07:City_Market_Rent  1   2617464 6689843895 86914
    ## + stories:hd_total07            1   2455715 6690005644 86914
    ## <none>                                      6692461359 86915
    ## + age:amenities                 1   1504193 6690957166 86915
    ## - size:amenities                1   2963127 6695424486 86915
    ## + empl_gr:cd_total_07           1   1116245 6691345114 86916
    ## + size:empl_gr                  1   1051557 6691409802 86916
    ## - stories:age                   1   3453777 6695915136 86916
    ## + amenities:City_Market_Rent    1    393757 6692067602 86916
    ## + renovated                     1    334838 6692126520 86916
    ## + stories:amenities             1    330326 6692131033 86916
    ## + stories:City_Market_Rent      1    300662 6692160697 86916
    ## + amenities:cd_total_07         1    225109 6692236250 86916
    ## + empl_gr:City_Market_Rent      1    184641 6692276718 86916
    ## + hd_total07:City_Market_Rent   1    158041 6692303318 86916
    ## + empl_gr:hd_total07            1    141523 6692319836 86916
    ## + empl_gr:stories               1      3549 6692457810 86917
    ## + green:hd_total07              2   2097563 6690363796 86917
    ## + size:green                    2   1509712 6690951647 86917
    ## + age:green                     2   1267278 6691194081 86917
    ## - age:cd_total_07               1   5477690 6697939049 86918
    ## - size:hd_total07               1   5484255 6697945614 86918
    ## + stories:green                 2    808530 6691652829 86918
    ## + green:cd_total_07             2    646867 6691814491 86918
    ## + empl_gr:green                 2    628066 6691833293 86918
    ## + green:City_Market_Rent        2    340033 6692121326 86918
    ## - cd_total_07:hd_total07        1   6610831 6699072190 86919
    ## - stories:cd_total_07           1   6651140 6699112499 86919
    ## - age:City_Market_Rent          1   8047958 6700509317 86920
    ## - age:hd_total07                1   8216243 6700677602 86920
    ## - empl_gr:age                   1   8424385 6700885744 86920
    ## - amenities:hd_total07          1   8576426 6701037785 86921
    ## - green:amenities               1   9686473 6702147832 86922
    ## - size:cd_total_07              1  11565746 6704027105 86923
    ## - size:stories                  1  12155335 6704616694 86924
    ## - size:City_Market_Rent         1 223811189 6916272548 87118
    ## 
    ## Step:  AIC=86911.05
    ## rpsf ~ size + empl_gr + stories + age + green + amenities + cd_total_07 + 
    ##     hd_total07 + City_Market_Rent + size:City_Market_Rent + size:stories + 
    ##     cd_total_07:hd_total07 + amenities:hd_total07 + green:amenities + 
    ##     empl_gr:age + size:amenities + age:City_Market_Rent + age:hd_total07 + 
    ##     age:cd_total_07 + size:cd_total_07 + stories:cd_total_07 + 
    ##     size:hd_total07 + stories:age + size:age
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + empl_gr:amenities             1   3320402 6683180829 86910
    ## + cd_total_07:City_Market_Rent  1   2617408 6683883822 86911
    ## <none>                                      6686501230 86911
    ## - size:amenities                1   2481525 6688982755 86911
    ## + empl_gr:cd_total_07           1   1112174 6685389056 86912
    ## + size:empl_gr                  1    997081 6685504150 86912
    ## - size:hd_total07               1   3291985 6689793215 86912
    ## + age:amenities                 1    864015 6685637215 86912
    ## + stories:hd_total07            1    763299 6685737931 86912
    ## + stories:City_Market_Rent      1    695274 6685805957 86912
    ## + stories:amenities             1    389556 6686111674 86913
    ## + amenities:City_Market_Rent    1    313117 6686188113 86913
    ## + green:hd_total07              2   2374702 6684126528 86913
    ## + renovated                     1    218307 6686282923 86913
    ## + amenities:cd_total_07         1    215419 6686285812 86913
    ## + empl_gr:City_Market_Rent      1    188016 6686313214 86913
    ## + empl_gr:hd_total07            1    122946 6686378285 86913
    ## + hd_total07:City_Market_Rent   1     98499 6686402732 86913
    ## + empl_gr:stories               1      2692 6686498538 86913
    ## + size:green                    2   1951024 6684550206 86913
    ## - age:cd_total_07               1   4528431 6691029662 86913
    ## + age:green                     2   1685098 6684816132 86913
    ## + stories:green                 2    780020 6685721210 86914
    ## + green:cd_total_07             2    761017 6685740214 86914
    ## + empl_gr:green                 2    627488 6685873742 86914
    ## - size:age                      1   5960129 6692461359 86915
    ## + green:City_Market_Rent        2    308393 6686192838 86915
    ## - age:hd_total07                1   6872233 6693373463 86915
    ## - cd_total_07:hd_total07        1   7702114 6694203345 86916
    ## - age:City_Market_Rent          1   7997550 6694498780 86917
    ## - stories:cd_total_07           1   8367755 6694868986 86917
    ## - empl_gr:age                   1   8733267 6695234498 86917
    ## - amenities:hd_total07          1   8843316 6695344547 86917
    ## - stories:age                   1   9262408 6695763638 86918
    ## - green:amenities               1   9845492 6696346722 86918
    ## - size:cd_total_07              1  13657874 6700159105 86922
    ## - size:stories                  1  16723946 6703225176 86925
    ## - size:City_Market_Rent         1 224273628 6910774858 87115
    ## 
    ## Step:  AIC=86909.94
    ## rpsf ~ size + empl_gr + stories + age + green + amenities + cd_total_07 + 
    ##     hd_total07 + City_Market_Rent + size:City_Market_Rent + size:stories + 
    ##     cd_total_07:hd_total07 + amenities:hd_total07 + green:amenities + 
    ##     empl_gr:age + size:amenities + age:City_Market_Rent + age:hd_total07 + 
    ##     age:cd_total_07 + size:cd_total_07 + stories:cd_total_07 + 
    ##     size:hd_total07 + stories:age + size:age + empl_gr:amenities
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## + cd_total_07:City_Market_Rent  1   2447919 6680732910 86910
    ## <none>                                      6683180829 86910
    ## - size:amenities                1   2374579 6685555407 86910
    ## - empl_gr:amenities             1   3320402 6686501230 86911
    ## + empl_gr:cd_total_07           1    906017 6682274811 86911
    ## + stories:hd_total07            1    710040 6682470789 86911
    ## + age:amenities                 1    708742 6682472087 86911
    ## - size:hd_total07               1   3599239 6686780068 86911
    ## + stories:City_Market_Rent      1    643800 6682537028 86911
    ## + empl_gr:stories               1    460939 6682719890 86912
    ## + green:hd_total07              2   2491437 6680689392 86912
    ## + stories:amenities             1    285095 6682895733 86912
    ## + renovated                     1    259500 6682921328 86912
    ## + amenities:City_Market_Rent    1    208606 6682972223 86912
    ## + empl_gr:City_Market_Rent      1    187954 6682992874 86912
    ## + amenities:cd_total_07         1    104392 6683076437 86912
    ## + hd_total07:City_Market_Rent   1     93530 6683087299 86912
    ## + size:empl_gr                  1     65414 6683115414 86912
    ## + empl_gr:hd_total07            1     62763 6683118066 86912
    ## + size:green                    2   1826871 6681353958 86912
    ## + age:green                     2   1640668 6681540161 86912
    ## - age:cd_total_07               1   4770122 6687950951 86912
    ## + green:cd_total_07             2    773965 6682406864 86913
    ## + stories:green                 2    687420 6682493408 86913
    ## + empl_gr:green                 2    497022 6682683807 86913
    ## - size:age                      1   5937075 6689117903 86913
    ## + green:City_Market_Rent        2    317838 6682862991 86914
    ## - age:hd_total07                1   6676540 6689857368 86914
    ## - amenities:hd_total07          1   7228686 6690409515 86915
    ## - cd_total_07:hd_total07        1   7415153 6690595981 86915
    ## - stories:cd_total_07           1   7977757 6691158585 86915
    ## - age:City_Market_Rent          1   8226365 6691407194 86916
    ## - empl_gr:age                   1   8703430 6691884258 86916
    ## - stories:age                   1   9321435 6692502264 86917
    ## - green:amenities               1   9959768 6693140596 86917
    ## - size:cd_total_07              1  14134012 6697314841 86921
    ## - size:stories                  1  16583603 6699764432 86923
    ## - size:City_Market_Rent         1 223251984 6906432812 87114
    ## 
    ## Step:  AIC=86909.65
    ## rpsf ~ size + empl_gr + stories + age + green + amenities + cd_total_07 + 
    ##     hd_total07 + City_Market_Rent + size:City_Market_Rent + size:stories + 
    ##     cd_total_07:hd_total07 + amenities:hd_total07 + green:amenities + 
    ##     empl_gr:age + size:amenities + age:City_Market_Rent + age:hd_total07 + 
    ##     age:cd_total_07 + size:cd_total_07 + stories:cd_total_07 + 
    ##     size:hd_total07 + stories:age + size:age + empl_gr:amenities + 
    ##     cd_total_07:City_Market_Rent
    ## 
    ##                                Df Sum of Sq        RSS   AIC
    ## <none>                                      6680732910 86910
    ## - size:amenities                1   2286685 6683019595 86910
    ## - cd_total_07:City_Market_Rent  1   2447919 6683180829 86910
    ## - empl_gr:amenities             1   3150912 6683883822 86911
    ## + empl_gr:cd_total_07           1   1103131 6679629779 86911
    ## - size:hd_total07               1   3421577 6684154487 86911
    ## + stories:hd_total07            1    810161 6679922749 86911
    ## + stories:City_Market_Rent      1    716432 6680016478 86911
    ## + age:amenities                 1    697149 6680035761 86911
    ## + empl_gr:stories               1    645662 6680087248 86911
    ## + empl_gr:City_Market_Rent      1    513431 6680219479 86911
    ## + green:hd_total07              2   2497345 6678235565 86911
    ## + renovated                     1    276542 6680456368 86911
    ## + stories:amenities             1    236154 6680496756 86911
    ## + amenities:City_Market_Rent    1    164238 6680568672 86911
    ## + empl_gr:hd_total07            1    109070 6680623840 86912
    ## + size:empl_gr                  1     32097 6680700813 86912
    ## + amenities:cd_total_07         1     20566 6680712344 86912
    ## + hd_total07:City_Market_Rent   1         0 6680732910 86912
    ## + size:green                    2   1742052 6678990858 86912
    ## + age:green                     2   1644093 6679088817 86912
    ## - age:cd_total_07               1   4970704 6685703614 86912
    ## - cd_total_07:hd_total07        1   5158312 6685891222 86912
    ## + green:cd_total_07             2    739541 6679993369 86913
    ## + stories:green                 2    640588 6680092322 86913
    ## + empl_gr:green                 2    477096 6680255814 86913
    ## - size:age                      1   5937605 6686670515 86913
    ## + green:City_Market_Rent        2    230898 6680502011 86913
    ## - age:hd_total07                1   6494452 6687227362 86914
    ## - amenities:hd_total07          1   7784255 6688517165 86915
    ## - stories:cd_total_07           1   8230701 6688963611 86915
    ## - age:City_Market_Rent          1   8798794 6689531704 86916
    ## - stories:age                   1   9273651 6690006561 86916
    ## - empl_gr:age                   1   9611948 6690344858 86917
    ## - green:amenities               1  10116734 6690849644 86917
    ## - size:cd_total_07              1  14524759 6695257669 86921
    ## - size:stories                  1  16351913 6697084823 86923
    ## - size:City_Market_Rent         1 224594853 6905327763 87115

    rmse_frame_lm_step=foreach(x=1:10, .combine='rbind')%do%{

    x = initial_split(GB, prop = 0.8)
    GB_train = training(x)
    GB_test = testing(x)  

    modelr::rmse(lm_step,GB_test)
    } %>% as.data.frame

    validated_result_lm_step = mean(rmse_frame_lm_step$V1)
    ## Validated RMSE of 1002.35

    summary(lm_step)

    ## 
    ## Call:
    ## lm(formula = rpsf ~ size + empl_gr + stories + age + green + 
    ##     amenities + cd_total_07 + hd_total07 + City_Market_Rent + 
    ##     size:City_Market_Rent + size:stories + cd_total_07:hd_total07 + 
    ##     amenities:hd_total07 + green:amenities + empl_gr:age + size:amenities + 
    ##     age:City_Market_Rent + age:hd_total07 + age:cd_total_07 + 
    ##     size:cd_total_07 + stories:cd_total_07 + size:hd_total07 + 
    ##     stories:age + size:age + empl_gr:amenities + cd_total_07:City_Market_Rent, 
    ##     data = GB_train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -5400.2  -424.6    -7.8   377.4 15576.3 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  -8.899e+02  1.636e+02  -5.440 5.52e-08 ***
    ## size                          8.419e-04  3.279e-04   2.568 0.010257 *  
    ## empl_gr                       6.340e+00  3.752e+00   1.690 0.091105 .  
    ## stories                      -8.642e+00  4.883e+00  -1.770 0.076792 .  
    ## age                           3.500e+00  2.117e+00   1.654 0.098278 .  
    ## green1                        4.416e+02  9.050e+01   4.880 1.09e-06 ***
    ## green2                        1.528e+02  7.331e+02   0.208 0.834888    
    ## amenities                     3.662e+02  6.482e+01   5.649 1.68e-08 ***
    ## cd_total_07                   1.041e-01  6.134e-02   1.697 0.089834 .  
    ## hd_total07                    1.116e-01  2.237e-02   4.987 6.31e-07 ***
    ## City_Market_Rent              9.904e+01  4.236e+00  23.383  < 2e-16 ***
    ## size:City_Market_Rent         4.949e-05  3.420e-06  14.471  < 2e-16 ***
    ## size:stories                 -8.943e-06  2.290e-06  -3.905 9.54e-05 ***
    ## cd_total_07:hd_total07       -2.242e-05  1.022e-05  -2.193 0.028340 *  
    ## amenities:hd_total07         -4.111e-02  1.526e-02  -2.694 0.007078 ** 
    ## green1:amenities             -3.265e+02  1.063e+02  -3.071 0.002141 ** 
    ## green2:amenities                     NA         NA      NA       NA    
    ## empl_gr:age                  -2.926e-01  9.774e-02  -2.994 0.002767 ** 
    ## size:amenities               -2.075e-04  1.421e-04  -1.460 0.144297    
    ## age:City_Market_Rent         -1.351e-01  4.717e-02  -2.864 0.004194 ** 
    ## age:hd_total07               -6.333e-04  2.574e-04  -2.461 0.013891 *  
    ## age:cd_total_07              -1.135e-03  5.272e-04  -2.153 0.031372 *  
    ## size:cd_total_07             -3.903e-07  1.061e-07  -3.680 0.000235 ***
    ## stories:cd_total_07           5.874e-03  2.120e-03   2.770 0.005618 ** 
    ## size:hd_total07              -5.104e-08  2.858e-08  -1.786 0.074129 .  
    ## stories:age                   2.047e-01  6.962e-02   2.941 0.003289 ** 
    ## size:age                     -6.806e-06  2.893e-06  -2.353 0.018659 *  
    ## empl_gr:amenities             5.622e+00  3.280e+00   1.714 0.086575 .  
    ## cd_total_07:City_Market_Rent -3.310e-03  2.191e-03  -1.511 0.130900    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1036 on 6229 degrees of freedom
    ##   (58 observations deleted due to missingness)
    ## Multiple R-squared:  0.5687, Adjusted R-squared:  0.5669 
    ## F-statistic: 304.3 on 27 and 6229 DF,  p-value: < 2.2e-16

The first model we will try to test is a stepwise selected model
utilizing the baseline model as a starting point. The stepwise selection
will optimize against RMSE\_in, picking the features and interactions
for us. Once the model is optimized against RMSE\_in, we run it through
a 10 step cross-validation to get 10 instances of RMSE\_out. We take the
mean, and get a validated RMSE\_out of 1002.35, which is roughly a 3%
improvement. From the summary of the regression, we observe that a
single green certification results in a 400 unit increase in rpsf, at a
99% confidence interval,ceteris paribus.There is no significant effect
when we consider the second certification. While 3% in RMSE\_out is a
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
