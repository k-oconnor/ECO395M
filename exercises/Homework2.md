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
    library(gamlr)
    library(glmnet)
    library(nnet)
    library(ROCR)

# Problem 1

## Part A

We are tasked with creating a plot of average boardings at each hour of
the day, for each day of the week, with a line corresponding to each
month in the data set.

    # Read in data
    CAPM <- read_csv("capmetro_UT.csv")

    # Mutate data so we aren't sorting days alphabetically.
    CAPM = mutate(CAPM,
                   day_of_week = factor(day_of_week,
                     levels=c("Mon", "Tue", "Wed","Thu", "Fri", "Sat", "Sun")),
                   month = factor(month,
                     levels=c("Sep", "Oct","Nov")))

    avg_board_ = CAPM %>% group_by(hour_of_day,day_of_week,month) %>%
      summarize(average_boarding_ = mean(boarding))


    ggplot(data = avg_board_) + geom_line(aes(x= hour_of_day,y=average_boarding_, group=month,color=month ),size =1) + labs(y= "Average Boardings", x="Hour of Day", title = "Average Boardings for each Day of Week")+facet_grid(~day_of_week)+theme_linedraw()

![](Homework2_files/figure-markdown_strict/Problem%201a-1.png)

From our plot, we can see that regardless of month, ridership sharply
decreases on the weekend. Peak ridership occurs around 5PM on the
weekdays. An interesting interpretation of this plot could be, that as
the semester goes on, students ride the bus less as the week progresses.
By november, ridership declines steadily during the weekday. This could
be a result of a few things, such as students getting fatigued at the
end of the semester, and skipping class later in the week. This could
also be due to the holidays at the end of november, where many students
leave campus for Thanksgiving on wednesday, decreasing ridership.

## Part B

     avg_board_ = CAPM %>% group_by(timestamp,day_of_week,weekend,temperature) 

    ggplot(data = avg_board_) + geom_point(aes(x= temperature,y=boarding, group=weekend,color=weekend ),size =1) + labs(y= "Boardings", x="Temperature (Farenheit)", title = "Average Boardings for Each Hour of the Day vs. Temperature")+facet_wrap(~hour_of_day)+theme_linedraw()

![](Homework2_files/figure-markdown_strict/Problem%201b-1.png)

An interesting interpretation from this plot could be how there are gaps
in the data around 5-6PM when the temperature is near 70 degrees. We
might assume that at the end of the workday, riders are more likely to
walk to thier destination when the weather is pleasant.

# Problem 2

## Part A:

After running 100 train/test splits for the stepwise selected model, and
running 100/train/test splits for the baseline model, we can conclude
that the stepwise model outperforms the baseline model.The validated
RMSEout for the baseline model is 66,572. The validated RMSEout for the
stepwise model is 63,460, or an improvement of 4.6% in RMSE. We will see
if we can arrive at a stronger result with a gamma lasso.

## Part A: Gamma Lasso

    set.seed(23)
    shx = model.matrix(price ~ .-1, data=SaratogaHouses)
    shy = SaratogaHouses$price
    shcv = cv.gamlr(shx, shy, nfold=10, family="gaussian", verb=FALSE)

    x= plot(shcv, bty="n")

![](Homework2_files/figure-markdown_strict/Problem%202b-1.png)

    sh.min = coef(shcv, select="min")

    # This results in the optimal lambda at the min
    log(shcv$lambda.min)

    ## [1] 6.552628

    # This counts the number of non-zero coefficients
    sum(sh.min!=0)

    ## [1] 14

    mse = min(shcv$cvm)
    rmse = sqrt(mse)
    print(rmse)

    ## [1] 58786.55

    rmse(lm_medium, saratoga_test)

    ## [1] 57590.32

    # 57,590


    # We get all of our named coefficients and print a table
    df = data.frame(name = sh.min@Dimnames[[1]][sh.min@i + 1], coefficient = sh.min@x)
    print(df)

    ##                 name   coefficient
    ## 1          intercept  9.309759e+04
    ## 2            lotSize  6.617322e+03
    ## 3                age -1.248700e+02
    ## 4          landValue  9.063596e-01
    ## 5         livingArea  6.853247e+01
    ## 6           bedrooms -4.922017e+03
    ## 7         fireplaces  3.157955e+02
    ## 8          bathrooms  2.266030e+04
    ## 9              rooms  2.428864e+03
    ## 10    heatinghot air  9.016811e+03
    ## 11           fueloil -4.749373e+02
    ## 12      waterfrontNo -1.149361e+05
    ## 13 newConstructionNo  3.870286e+04
    ## 14      centralAirNo -9.667206e+03

The cross validated gamma lasso at lambda.min proves to improve our
error dramatically.With 14 coefficients at lambda (6.55), RMSEout is
57,590, which is a ~13.5% improvement over baseline. Next, we will apply
K-nearest neighbors, and see which model is the best.

## Part B: KNN

    # We first will standardize our regressors, using the scale function.
    set.seed(23)
    scaled = data.frame(scale(SaratogaHouses[2:10]))
    scaled[10] = SaratogaHouses[1]

    # Before we can move on to more complete cross-validation, we first will tune our
    # k hyper-parameter
    saratoga_split = initial_split(scaled, prop = 0.8)
    saratoga_train = training(saratoga_split)
    saratoga_test = testing(saratoga_split)

    rmse_out=foreach(k=1:150, .combine='rbind')%do%{


    knn_model = knnreg(price ~ lotSize + age + landValue + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms, data = saratoga_train, k =k)
    modelr::rmse(knn_model,saratoga_test)
    } %>% as.data.frame
    rmse_out

    ##                  V1
    ## result.1   78768.22
    ## result.2   69245.18
    ## result.3   63032.02
    ## result.4   62576.19
    ## result.5   61357.26
    ## result.6   59213.39
    ## result.7   58509.33
    ## result.8   57600.01
    ## result.9   57262.07
    ## result.10  57008.77
    ## result.11  57042.97
    ## result.12  56993.25
    ## result.13  57088.43
    ## result.14  57116.42
    ## result.15  57136.00
    ## result.16  57154.16
    ## result.17  57085.44
    ## result.18  56841.13
    ## result.19  56424.59
    ## result.20  56330.94
    ## result.21  56210.67
    ## result.22  56349.08
    ## result.23  56180.42
    ## result.24  56197.25
    ## result.25  56384.60
    ## result.26  56270.70
    ## result.27  56339.84
    ## result.28  56368.07
    ## result.29  56336.23
    ## result.30  56538.51
    ## result.31  56814.33
    ## result.32  57097.23
    ## result.33  56929.29
    ## result.34  56942.87
    ## result.35  57112.42
    ## result.36  57276.37
    ## result.37  57215.04
    ## result.38  57309.91
    ## result.39  57574.00
    ## result.40  57932.70
    ## result.41  57923.46
    ## result.42  58001.35
    ## result.43  58115.18
    ## result.44  58155.98
    ## result.45  58205.89
    ## result.46  58238.75
    ## result.47  58322.28
    ## result.48  58411.81
    ## result.49  58380.13
    ## result.50  58419.88
    ## result.51  58357.35
    ## result.52  58531.44
    ## result.53  58579.47
    ## result.54  58599.91
    ## result.55  58555.35
    ## result.56  58350.65
    ## result.57  58330.83
    ## result.58  58496.67
    ## result.59  58492.63
    ## result.60  58445.33
    ## result.61  58525.44
    ## result.62  58698.06
    ## result.63  58805.11
    ## result.64  58768.99
    ## result.65  58711.95
    ## result.66  58684.57
    ## result.67  58654.83
    ## result.68  58728.32
    ## result.69  58718.33
    ## result.70  58701.85
    ## result.71  58685.05
    ## result.72  58739.40
    ## result.73  58804.34
    ## result.74  58869.09
    ## result.75  58888.12
    ## result.76  59014.68
    ## result.77  59094.78
    ## result.78  59088.84
    ## result.79  59103.21
    ## result.80  59106.87
    ## result.81  59095.71
    ## result.82  59136.30
    ## result.83  59140.51
    ## result.84  59129.20
    ## result.85  59176.47
    ## result.86  59243.06
    ## result.87  59185.20
    ## result.88  59239.00
    ## result.89  59118.34
    ## result.90  59101.59
    ## result.91  59108.33
    ## result.92  59129.20
    ## result.93  59100.37
    ## result.94  59113.67
    ## result.95  59109.15
    ## result.96  59116.06
    ## result.97  59179.82
    ## result.98  59227.64
    ## result.99  59189.20
    ## result.100 59235.54
    ## result.101 59270.67
    ## result.102 59272.15
    ## result.103 59258.14
    ## result.104 59311.44
    ## result.105 59340.72
    ## result.106 59407.07
    ## result.107 59380.70
    ## result.108 59420.86
    ## result.109 59485.70
    ## result.110 59549.68
    ## result.111 59646.20
    ## result.112 59668.01
    ## result.113 59718.24
    ## result.114 59792.23
    ## result.115 59842.89
    ## result.116 59905.50
    ## result.117 59953.83
    ## result.118 59986.58
    ## result.119 60019.20
    ## result.120 60069.22
    ## result.121 60132.82
    ## result.122 60149.40
    ## result.123 60166.09
    ## result.124 60227.05
    ## result.125 60250.49
    ## result.126 60196.71
    ## result.127 60232.98
    ## result.128 60291.66
    ## result.129 60309.66
    ## result.130 60334.16
    ## result.131 60367.57
    ## result.132 60410.92
    ## result.133 60427.89
    ## result.134 60478.02
    ## result.135 60500.28
    ## result.136 60489.41
    ## result.137 60479.22
    ## result.138 60512.65
    ## result.139 60518.34
    ## result.140 60516.39
    ## result.141 60528.88
    ## result.142 60524.86
    ## result.143 60500.11
    ## result.144 60561.31
    ## result.145 60584.65
    ## result.146 60612.72
    ## result.147 60618.67
    ## result.148 60653.20
    ## result.149 60683.43
    ## result.150 60712.42

    row_adjust = c(1:150)
    rownames(rmse_out) = row_adjust
    rmse_out$k <- c(1:150)


    ggplot(rmse_out) + geom_line(aes(x=k,y=V1),size =2) + labs(y= "RMSE", x="K", title = "Root Mean Square Error for Different Values of K") +theme_linedraw() + geom_vline(xintercept=25, linetype="dashed", color = "red")

![](Homework2_files/figure-markdown_strict/Problem%202c-1.png)

    # From this analysis, we can conclude that k=25, is a very reasonable choice.

    # Next, we will validate our KNN regression across 100 train/test splits.
    rmse_frame=foreach(x=1:100, .combine='rbind')%do%{

    x = initial_split(scaled, prop = 0.8)
    saratoga_train = training(x)
    saratoga_test = testing(x)

    knn_model = knnreg(price ~ lotSize + age + landValue + livingArea + pctCollege + bedrooms + fireplaces + bathrooms + rooms, data = saratoga_train, k =25)

    modelr::rmse(knn_model,saratoga_test)
    } %>% as.data.frame

    validated_result_KNN = mean(rmse_frame$V1)
    # 62,609

KNN poses a new challenge for cross validation. We cannot cross-validate
until we find a reasonable value for our hyperparameter (K). Firstly, we
conduct a baseline train/test split, and plot the RMSE across different
values of K for tuning. From this, we can assume that K=25 is a
reasonable choice.

We then apply our KNN model, with K=25, across 100 different train/test
splits, and take an average of the RMSE over all splits. This results in
a validated RMSEout mean of 62,609, which is a ~6% improvement over
baseline. While the KNN model is a strong improvement over baseline, the
gamma lasso model is by far the best for out of sample accuracy.

\#Problem 3 \# Part A For this problem, given a set of data on consumer
defaults at a German bank, we are to find the probability of default at
each credit rating.

    GC <- read_csv("german_credit.csv")

    def_prob = GC %>% group_by(history) %>%
      summarize(prob = mean(Default))


    def_prob %>% ggplot(aes(prob,history)) + geom_col(fill="steelblue") + coord_flip() + labs(y= "Credit Rating", x="Probability of Default", title = "Credit Rating and Default Rates for Selected Data")+theme_linedraw()

![](Homework2_files/figure-markdown_strict/Problem%203a-1.png)

Finding the default probability for each credit rating is trivial. We
can simply sum the number of defaults for each category, and divde by
the total number of consumers in each credit class. Interestingly, the
default rates seem counterfactual. As credit ratings get worse, the
default probability goes down.

# Part B

    model = multinom(Default ~duration + amount + installment + age + history + purpose + foreign, data=GC )

    ## # weights:  13 (12 variable)
    ## initial  value 693.147181 
    ## iter  10 value 554.284880
    ## final  value 534.976886 
    ## converged

    summary(model)

    ## Call:
    ## multinom(formula = Default ~ duration + amount + installment + 
    ##     age + history + purpose + foreign, data = GC)
    ## 
    ## Coefficients:
    ##                            Values    Std. Err.
    ## (Intercept)         -7.075102e-01 1.402291e-02
    ## duration             2.525815e-02 7.933011e-03
    ## amount               9.596539e-05 3.471396e-05
    ## installment          2.216043e-01 6.126687e-02
    ## age                 -2.018434e-02 5.761836e-03
    ## historypoor         -1.107590e+00 9.920503e-02
    ## historyterrible     -1.884678e+00 7.345331e-02
    ## purposeedu           7.247605e-01 6.755908e-03
    ## purposegoods/repair  1.048903e-01 1.010056e-01
    ## purposenewcar        8.544448e-01 7.866423e-02
    ## purposeusedcar      -7.959573e-01 5.714036e-03
    ## foreigngerman       -1.264693e+00 2.458243e-03
    ## 
    ## Residual Deviance: 1069.954 
    ## AIC: 1093.954

When we run a multi-nomial logistic regression on default, the findings
of our bar plot are confirmed. Poor and terrible credit histories have a
negative effect on the probability of default. This is likely due to the
sampling methodology of the bank, where they are oversampling defaults.

\#Problem 4 \# Part A For this problem, we will be conducting analysis
on hotel data. The target variable of interest is “children”, a binary
representation of whether or not parents in a specific booking bring
children with them to a hotel or not. It is of the hotel’s interest to
anticipate how many children may be coming in order to effectively plan
resource utilization, as hotels don’t generally know if parents are
brining children until they arrive. We will try to make a reliable
prediction.

    HD <- read_csv("hotels_dev.csv")
    HD_filter = HD %>% filter(reserved_room_type != "F")


    rmse_frame_baseline_1=foreach(x=1:10, .combine='rbind')%do%{

    x = initial_split(HD_filter, prop = 0.8)
    HD_train = training(x)
    HD_test = testing(x)

    # baseline small model with 4 main effects
    baseline_1 = glm(children ~ market_segment + adults + customer_type + is_repeated_guest, data=HD_train, family = 'binomial')
    modelr::rmse(baseline_1,HD_test)
    } %>% as.data.frame

    validated_result_baseline_1 = mean(rmse_frame_baseline_1$V1)
    # 3.12

    rmse_frame_baseline_2=foreach(x=1:10, .combine='rbind')%do%{

    x = initial_split(HD_filter, prop = 0.8)
    HD_train = training(x)
    HD_test = testing(x)

    # baseline large model with 20 main effects
    baseline_2 = glm(children ~ .-arrival_date, data=HD_train,family='binomial')
    modelr::rmse(baseline_2,HD_test)
    } %>% as.data.frame

    validated_result_baseline_2 = mean(rmse_frame_baseline_2$V1)
    # 4.04

    sensible_model =lm(children ~ adults + stays_in_weekend_nights + stays_in_week_nights + hotel+meal+total_of_special_requests + customer_type, data=HD_filter)

    lm_step = step(sensible_model,scope=~(.)^2)

    ## Start:  AIC=-121204.3
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + adults:hotel                                       1    16.398 2742.0 -121464
    ## + meal:total_of_special_requests                     4    11.394 2747.0 -121378
    ## + meal:customer_type                                12    11.133 2747.3 -121358
    ## + adults:total_of_special_requests                   1     6.608 2751.8 -121307
    ## + adults:customer_type                               3     2.700 2755.7 -121241
    ## + adults:stays_in_weekend_nights                     1     1.862 2756.6 -121232
    ## + adults:stays_in_week_nights                        1     1.849 2756.6 -121232
    ## + stays_in_weekend_nights:stays_in_week_nights       1     1.284 2757.1 -121223
    ## + hotel:meal                                         3     1.333 2757.1 -121219
    ## + adults:meal                                        4     1.395 2757.0 -121218
    ## + stays_in_week_nights:meal                          4     1.193 2757.2 -121215
    ## + stays_in_weekend_nights:meal                       4     1.087 2757.3 -121214
    ## + hotel:total_of_special_requests                    1     0.390 2758.0 -121208
    ## - stays_in_week_nights                               1     0.037 2758.4 -121206
    ## <none>                                                           2758.4 -121204
    ## + stays_in_week_nights:total_of_special_requests     1     0.122 2758.3 -121204
    ## + stays_in_week_nights:customer_type                 3     0.329 2758.1 -121204
    ## + stays_in_weekend_nights:hotel                      1     0.050 2758.4 -121203
    ## + stays_in_week_nights:hotel                         1     0.042 2758.4 -121203
    ## + stays_in_weekend_nights:total_of_special_requests  1     0.012 2758.4 -121202
    ## - stays_in_weekend_nights                            1     0.275 2758.7 -121202
    ## + stays_in_weekend_nights:customer_type              3     0.168 2758.2 -121201
    ## + total_of_special_requests:customer_type            3     0.123 2758.3 -121200
    ## + hotel:customer_type                                3     0.081 2758.3 -121200
    ## - hotel                                              1     1.209 2759.6 -121187
    ## - adults                                             1     1.504 2759.9 -121182
    ## - meal                                               4    19.769 2778.2 -120899
    ## - customer_type                                      3    26.153 2784.6 -120797
    ## - total_of_special_requests                          1    38.618 2797.0 -120597
    ## 
    ## Step:  AIC=-121463.6
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + meal:total_of_special_requests                     4    10.558 2731.5 -121625
    ## + meal:customer_type                                12     9.758 2732.3 -121596
    ## + adults:total_of_special_requests                   1     4.283 2737.7 -121530
    ## + adults:stays_in_week_nights                        1     3.442 2738.6 -121517
    ## + adults:stays_in_weekend_nights                     1     2.658 2739.4 -121504
    ## + adults:customer_type                               3     2.720 2739.3 -121501
    ## + hotel:total_of_special_requests                    1     1.981 2740.0 -121493
    ## + stays_in_weekend_nights:stays_in_week_nights       1     1.064 2740.9 -121479
    ## + hotel:meal                                         3     1.131 2740.9 -121476
    ## + stays_in_week_nights:meal                          4     1.119 2740.9 -121474
    ## + stays_in_weekend_nights:meal                       4     0.959 2741.1 -121471
    ## + stays_in_weekend_nights:hotel                      1     0.535 2741.5 -121470
    ## - stays_in_week_nights                               1     0.000 2742.0 -121466
    ## + adults:meal                                        4     0.611 2741.4 -121465
    ## + stays_in_week_nights:total_of_special_requests     1     0.131 2741.9 -121464
    ## <none>                                                           2742.0 -121464
    ## + stays_in_week_nights:hotel                         1     0.116 2741.9 -121463
    ## + stays_in_week_nights:customer_type                 3     0.307 2741.7 -121463
    ## - stays_in_weekend_nights                            1     0.205 2742.2 -121462
    ## + stays_in_weekend_nights:total_of_special_requests  1     0.013 2742.0 -121462
    ## + hotel:customer_type                                3     0.199 2741.8 -121461
    ## + total_of_special_requests:customer_type            3     0.197 2741.8 -121461
    ## + stays_in_weekend_nights:customer_type              3     0.189 2741.8 -121461
    ## - adults:hotel                                       1    16.398 2758.4 -121204
    ## - meal                                               4    19.079 2761.1 -121168
    ## - customer_type                                      3    27.449 2769.5 -121033
    ## - total_of_special_requests                          1    38.856 2780.9 -120849
    ## 
    ## Step:  AIC=-121624.7
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + meal:customer_type                                12    7.6685 2723.8 -121724
    ## + hotel:total_of_special_requests                    1    5.9785 2725.5 -121719
    ## + adults:total_of_special_requests                   1    5.0114 2726.4 -121703
    ## + adults:stays_in_week_nights                        1    3.5444 2727.9 -121680
    ## + adults:stays_in_weekend_nights                     1    2.7691 2728.7 -121667
    ## + adults:customer_type                               3    2.8955 2728.6 -121665
    ## + stays_in_weekend_nights:stays_in_week_nights       1    1.0845 2730.4 -121640
    ## + hotel:meal                                         3    1.2610 2730.2 -121639
    ## + adults:meal                                        4    1.1127 2730.3 -121635
    ## + stays_in_weekend_nights:hotel                      1    0.6875 2730.8 -121634
    ## + stays_in_weekend_nights:meal                       4    0.9736 2730.5 -121632
    ## + stays_in_week_nights:meal                          4    0.9195 2730.5 -121632
    ## - stays_in_week_nights                               1    0.0000 2731.5 -121627
    ## + stays_in_week_nights:hotel                         1    0.1792 2731.3 -121626
    ## <none>                                                           2731.5 -121625
    ## - stays_in_weekend_nights                            1    0.1443 2731.6 -121624
    ## + hotel:customer_type                                3    0.3474 2731.1 -121624
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.0860 2731.4 -121624
    ## + stays_in_week_nights:customer_type                 3    0.3214 2731.1 -121624
    ## + stays_in_week_nights:total_of_special_requests     1    0.0213 2731.4 -121623
    ## + stays_in_weekend_nights:customer_type              3    0.1905 2731.3 -121622
    ## + total_of_special_requests:customer_type            3    0.0036 2731.4 -121619
    ## - meal:total_of_special_requests                     4   10.5583 2742.0 -121464
    ## - adults:hotel                                       1   15.5626 2747.0 -121378
    ## - customer_type                                      3   25.2157 2756.7 -121228
    ## 
    ## Step:  AIC=-121724
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + hotel:total_of_special_requests                    1    5.7792 2718.0 -121815
    ## + adults:total_of_special_requests                   1    5.0402 2718.8 -121803
    ## + adults:stays_in_week_nights                        1    3.6867 2720.1 -121781
    ## + adults:stays_in_weekend_nights                     1    2.9574 2720.8 -121770
    ## + adults:customer_type                               3    2.2884 2721.5 -121755
    ## + stays_in_weekend_nights:meal                       4    2.1826 2721.6 -121751
    ## + stays_in_week_nights:meal                          4    2.1741 2721.6 -121751
    ## + adults:meal                                        4    1.8798 2721.9 -121746
    ## + stays_in_weekend_nights:stays_in_week_nights       1    0.9027 2722.9 -121737
    ## + stays_in_weekend_nights:hotel                      1    0.7227 2723.1 -121734
    ## + hotel:customer_type                                3    0.9168 2722.9 -121733
    ## + hotel:meal                                         3    0.6319 2723.2 -121728
    ## - stays_in_week_nights                               1    0.0313 2723.8 -121725
    ## + stays_in_week_nights:hotel                         1    0.1691 2723.6 -121725
    ## - stays_in_weekend_nights                            1    0.0843 2723.9 -121725
    ## <none>                                                           2723.8 -121724
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.0313 2723.8 -121722
    ## + stays_in_week_nights:total_of_special_requests     1    0.0000 2723.8 -121722
    ## + stays_in_week_nights:customer_type                 3    0.1361 2723.7 -121720
    ## + stays_in_weekend_nights:customer_type              3    0.0931 2723.7 -121719
    ## + total_of_special_requests:customer_type            3    0.0005 2723.8 -121718
    ## - meal:customer_type                                12    7.6685 2731.5 -121625
    ## - meal:total_of_special_requests                     4    8.4685 2732.3 -121596
    ## - adults:hotel                                       1   14.5393 2738.3 -121493
    ## 
    ## Step:  AIC=-121815.1
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + adults:total_of_special_requests                   1    5.2011 2712.8 -121897
    ## + adults:stays_in_week_nights                        1    3.7692 2714.2 -121874
    ## + adults:stays_in_weekend_nights                     1    2.9752 2715.0 -121861
    ## + adults:customer_type                               3    2.4756 2715.5 -121849
    ## + stays_in_weekend_nights:meal                       4    2.3183 2715.7 -121844
    ## + stays_in_week_nights:meal                          4    2.2850 2715.7 -121844
    ## + adults:meal                                        4    2.2131 2715.8 -121843
    ## + stays_in_weekend_nights:stays_in_week_nights       1    0.8920 2717.1 -121827
    ## + stays_in_weekend_nights:hotel                      1    0.5002 2717.5 -121821
    ## + hotel:meal                                         3    0.6920 2717.3 -121820
    ## - stays_in_week_nights                               1    0.0244 2718.0 -121817
    ## + stays_in_week_nights:total_of_special_requests     1    0.2158 2717.8 -121817
    ## - stays_in_weekend_nights                            1    0.1208 2718.1 -121815
    ## <none>                                                           2718.0 -121815
    ## + hotel:customer_type                                3    0.3438 2717.7 -121815
    ## + stays_in_week_nights:hotel                         1    0.0831 2717.9 -121814
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.0263 2718.0 -121814
    ## + stays_in_week_nights:customer_type                 3    0.0835 2717.9 -121810
    ## + stays_in_weekend_nights:customer_type              3    0.0451 2718.0 -121810
    ## + total_of_special_requests:customer_type            3    0.0055 2718.0 -121809
    ## - hotel:total_of_special_requests                    1    5.7792 2723.8 -121724
    ## - meal:customer_type                                12    7.4691 2725.5 -121719
    ## - meal:total_of_special_requests                     4   11.8690 2729.9 -121632
    ## - adults:hotel                                       1   17.6172 2735.6 -121534
    ## 
    ## Step:  AIC=-121897
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + adults:customer_type                               3    4.4745 2708.3 -121963
    ## + adults:stays_in_week_nights                        1    3.0268 2709.8 -121944
    ## + adults:meal                                        4    2.7527 2710.1 -121934
    ## + adults:stays_in_weekend_nights                     1    2.3465 2710.5 -121933
    ## + stays_in_weekend_nights:meal                       4    2.3832 2710.4 -121928
    ## + stays_in_week_nights:meal                          4    2.3484 2710.5 -121927
    ## + stays_in_weekend_nights:stays_in_week_nights       1    0.9868 2711.8 -121911
    ## + hotel:meal                                         3    0.7175 2712.1 -121903
    ## + stays_in_weekend_nights:hotel                      1    0.4594 2712.3 -121902
    ## + stays_in_week_nights:total_of_special_requests     1    0.4123 2712.4 -121902
    ## - stays_in_week_nights                               1    0.0287 2712.8 -121899
    ## - stays_in_weekend_nights                            1    0.0885 2712.9 -121898
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.1248 2712.7 -121897
    ## <none>                                                           2712.8 -121897
    ## + stays_in_week_nights:hotel                         1    0.0698 2712.7 -121896
    ## + hotel:customer_type                                3    0.2758 2712.5 -121895
    ## + total_of_special_requests:customer_type            3    0.0842 2712.7 -121892
    ## + stays_in_week_nights:customer_type                 3    0.0609 2712.8 -121892
    ## + stays_in_weekend_nights:customer_type              3    0.0286 2712.8 -121891
    ## - adults:total_of_special_requests                   1    5.2011 2718.0 -121815
    ## - hotel:total_of_special_requests                    1    5.9401 2718.8 -121803
    ## - meal:customer_type                                12    7.4995 2720.3 -121800
    ## - meal:total_of_special_requests                     4   12.5883 2725.4 -121702
    ## - adults:hotel                                       1   15.0430 2727.8 -121657
    ## 
    ## Step:  AIC=-121963.4
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests + 
    ##     adults:customer_type
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + adults:stays_in_week_nights                        1    2.5096 2705.8 -122002
    ## + adults:stays_in_weekend_nights                     1    2.3189 2706.0 -121999
    ## + stays_in_weekend_nights:meal                       4    2.3639 2706.0 -121994
    ## + stays_in_week_nights:meal                          4    2.2525 2706.1 -121992
    ## + adults:meal                                        4    1.5966 2706.7 -121981
    ## + stays_in_weekend_nights:stays_in_week_nights       1    0.8753 2707.5 -121976
    ## + stays_in_week_nights:total_of_special_requests     1    0.5398 2707.8 -121970
    ## + stays_in_weekend_nights:hotel                      1    0.4619 2707.9 -121969
    ## + hotel:meal                                         3    0.6797 2707.7 -121968
    ## - stays_in_week_nights                               1    0.0712 2708.4 -121964
    ## - stays_in_weekend_nights                            1    0.0782 2708.4 -121964
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.1517 2708.2 -121964
    ## <none>                                                           2708.3 -121963
    ## + hotel:customer_type                                3    0.3124 2708.0 -121962
    ## + stays_in_week_nights:hotel                         1    0.0421 2708.3 -121962
    ## + stays_in_weekend_nights:customer_type              3    0.0574 2708.3 -121958
    ## + stays_in_week_nights:customer_type                 3    0.0518 2708.3 -121958
    ## + total_of_special_requests:customer_type            3    0.0220 2708.3 -121958
    ## - adults:customer_type                               3    4.4745 2712.8 -121897
    ## - meal:customer_type                                12    6.6440 2715.0 -121880
    ## - hotel:total_of_special_requests                    1    6.2611 2714.6 -121864
    ## - adults:total_of_special_requests                   1    7.1999 2715.5 -121849
    ## - meal:total_of_special_requests                     4   13.2393 2721.6 -121758
    ## - adults:hotel                                       1   14.5525 2722.9 -121730
    ## 
    ## Step:  AIC=-122002
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests + 
    ##     adults:customer_type + adults:stays_in_week_nights
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + stays_in_weekend_nights:meal                       4    2.6621 2703.2 -122037
    ## + stays_in_weekend_nights:stays_in_week_nights       1    2.2415 2703.6 -122036
    ## + stays_in_week_nights:meal                          4    2.5379 2703.3 -122035
    ## + adults:meal                                        4    1.2290 2704.6 -122014
    ## + adults:stays_in_weekend_nights                     1    0.6253 2705.2 -122010
    ## + stays_in_week_nights:total_of_special_requests     1    0.4932 2705.3 -122008
    ## + hotel:meal                                         3    0.6599 2705.2 -122007
    ## + stays_in_weekend_nights:hotel                      1    0.3033 2705.5 -122005
    ## - stays_in_weekend_nights                            1    0.0320 2705.9 -122004
    ## <none>                                                           2705.8 -122002
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.0857 2705.7 -122001
    ## + hotel:customer_type                                3    0.3215 2705.5 -122001
    ## + stays_in_week_nights:hotel                         1    0.0019 2705.8 -122000
    ## + stays_in_weekend_nights:customer_type              3    0.0438 2705.8 -121997
    ## + total_of_special_requests:customer_type            3    0.0361 2705.8 -121997
    ## + stays_in_week_nights:customer_type                 3    0.0323 2705.8 -121997
    ## - adults:stays_in_week_nights                        1    2.5096 2708.3 -121963
    ## - adults:customer_type                               3    3.9574 2709.8 -121944
    ## - meal:customer_type                                12    6.7907 2712.6 -121916
    ## - hotel:total_of_special_requests                    1    6.3056 2712.1 -121902
    ## - adults:total_of_special_requests                   1    6.3219 2712.2 -121902
    ## - meal:total_of_special_requests                     4   13.2270 2719.1 -121796
    ## - adults:hotel                                       1   15.8685 2721.7 -121748
    ## 
    ## Step:  AIC=-122037.2
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests + 
    ##     adults:customer_type + adults:stays_in_week_nights + stays_in_weekend_nights:meal
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + stays_in_weekend_nights:stays_in_week_nights       1    1.5113 2701.7 -122060
    ## + adults:meal                                        4    1.2627 2701.9 -122050
    ## + stays_in_week_nights:total_of_special_requests     1    0.7058 2702.5 -122047
    ## + adults:stays_in_weekend_nights                     1    0.6418 2702.5 -122046
    ## + hotel:meal                                         3    0.4816 2702.7 -122039
    ## + stays_in_week_nights:meal                          4    0.5735 2702.6 -122038
    ## + stays_in_weekend_nights:hotel                      1    0.1620 2703.0 -122038
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.1324 2703.0 -122037
    ## <none>                                                           2703.2 -122037
    ## + hotel:customer_type                                3    0.3496 2702.8 -122037
    ## + stays_in_week_nights:hotel                         1    0.0095 2703.2 -122035
    ## + stays_in_weekend_nights:customer_type              3    0.1243 2703.0 -122033
    ## + total_of_special_requests:customer_type            3    0.0414 2703.1 -122032
    ## + stays_in_week_nights:customer_type                 3    0.0376 2703.1 -122032
    ## - stays_in_weekend_nights:meal                       4    2.6621 2705.8 -122002
    ## - adults:stays_in_week_nights                        1    2.8079 2706.0 -121994
    ## - adults:customer_type                               3    3.9027 2707.1 -121980
    ## - adults:total_of_special_requests                   1    6.3289 2709.5 -121937
    ## - hotel:total_of_special_requests                    1    6.4670 2709.6 -121934
    ## - meal:customer_type                                12    8.1395 2711.3 -121929
    ## - meal:total_of_special_requests                     4   13.3392 2716.5 -121829
    ## - adults:hotel                                       1   15.6605 2718.8 -121786
    ## 
    ## Step:  AIC=-122059.7
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests + 
    ##     adults:customer_type + adults:stays_in_week_nights + stays_in_weekend_nights:meal + 
    ##     stays_in_weekend_nights:stays_in_week_nights
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + stays_in_week_nights:total_of_special_requests     1    1.4624 2700.2 -122081
    ## + adults:stays_in_weekend_nights                     1    0.8225 2700.8 -122071
    ## + adults:meal                                        4    1.1869 2700.5 -122071
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.3884 2701.3 -122064
    ## + hotel:meal                                         3    0.4278 2701.2 -122061
    ## <none>                                                           2701.7 -122060
    ## + stays_in_week_nights:hotel                         1    0.1092 2701.5 -122059
    ## + stays_in_weekend_nights:hotel                      1    0.0456 2701.6 -122058
    ## + hotel:customer_type                                3    0.2807 2701.4 -122058
    ## + stays_in_week_nights:meal                          4    0.3496 2701.3 -122057
    ## + stays_in_weekend_nights:customer_type              3    0.1124 2701.5 -122056
    ## + stays_in_week_nights:customer_type                 3    0.0709 2701.6 -122055
    ## + total_of_special_requests:customer_type            3    0.0496 2701.6 -122054
    ## - stays_in_weekend_nights:stays_in_week_nights       1    1.5113 2703.2 -122037
    ## - stays_in_weekend_nights:meal                       4    1.9319 2703.6 -122036
    ## - adults:customer_type                               3    3.6485 2705.3 -122007
    ## - adults:stays_in_week_nights                        1    3.8731 2705.5 -121999
    ## - adults:total_of_special_requests                   1    6.2102 2707.9 -121961
    ## - meal:customer_type                                12    7.6182 2709.3 -121960
    ## - hotel:total_of_special_requests                    1    6.4319 2708.1 -121957
    ## - meal:total_of_special_requests                     4   13.3288 2715.0 -121852
    ## - adults:hotel                                       1   15.8463 2717.5 -121805
    ## 
    ## Step:  AIC=-122081.4
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests + 
    ##     adults:customer_type + adults:stays_in_week_nights + stays_in_weekend_nights:meal + 
    ##     stays_in_weekend_nights:stays_in_week_nights + stays_in_week_nights:total_of_special_requests
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + adults:meal                                        4    1.1239 2699.1 -122092
    ## + adults:stays_in_weekend_nights                     1    0.7450 2699.4 -122092
    ## + stays_in_week_nights:hotel                         1    0.1864 2700.0 -122082
    ## + hotel:meal                                         3    0.4163 2699.8 -122082
    ## <none>                                                           2700.2 -122081
    ## + hotel:customer_type                                3    0.3188 2699.9 -122081
    ## + stays_in_weekend_nights:hotel                      1    0.0252 2700.2 -122080
    ## + stays_in_week_nights:meal                          4    0.3910 2699.8 -122080
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.0004 2700.2 -122079
    ## + stays_in_weekend_nights:customer_type              3    0.1582 2700.0 -122078
    ## + total_of_special_requests:customer_type            3    0.0714 2700.1 -122077
    ## + stays_in_week_nights:customer_type                 3    0.0654 2700.1 -122076
    ## - stays_in_week_nights:total_of_special_requests     1    1.4624 2701.7 -122060
    ## - stays_in_weekend_nights:meal                       4    2.0376 2702.2 -122056
    ## - stays_in_weekend_nights:stays_in_week_nights       1    2.2679 2702.5 -122047
    ## - adults:customer_type                               3    3.7875 2704.0 -122026
    ## - adults:stays_in_week_nights                        1    4.2090 2704.4 -122015
    ## - meal:customer_type                                12    7.9401 2708.1 -121977
    ## - adults:total_of_special_requests                   1    6.6870 2706.9 -121975
    ## - hotel:total_of_special_requests                    1    7.4334 2707.6 -121963
    ## - meal:total_of_special_requests                     4   12.2976 2712.5 -121890
    ## - adults:hotel                                       1   16.1589 2716.3 -121822
    ## 
    ## Step:  AIC=-122091.7
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests + 
    ##     adults:customer_type + adults:stays_in_week_nights + stays_in_weekend_nights:meal + 
    ##     stays_in_weekend_nights:stays_in_week_nights + stays_in_week_nights:total_of_special_requests + 
    ##     adults:meal
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + adults:stays_in_weekend_nights                     1    0.6595 2698.4 -122100
    ## + hotel:meal                                         3    0.4220 2698.6 -122093
    ## + stays_in_week_nights:hotel                         1    0.1647 2698.9 -122092
    ## <none>                                                           2699.1 -122092
    ## + hotel:customer_type                                3    0.3161 2698.8 -122091
    ## + stays_in_weekend_nights:hotel                      1    0.0354 2699.0 -122090
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.0006 2699.1 -122090
    ## + stays_in_week_nights:meal                          4    0.3533 2698.7 -122089
    ## + stays_in_weekend_nights:customer_type              3    0.1526 2698.9 -122088
    ## + stays_in_week_nights:customer_type                 3    0.0630 2699.0 -122087
    ## + total_of_special_requests:customer_type            3    0.0610 2699.0 -122087
    ## - adults:meal                                        4    1.1239 2700.2 -122081
    ## - stays_in_week_nights:total_of_special_requests     1    1.3994 2700.5 -122071
    ## - stays_in_weekend_nights:meal                       4    2.0913 2701.2 -122066
    ## - stays_in_weekend_nights:stays_in_week_nights       1    2.1595 2701.2 -122059
    ## - adults:customer_type                               3    2.9620 2702.0 -122050
    ## - adults:stays_in_week_nights                        1    3.7211 2702.8 -122033
    ## - adults:total_of_special_requests                   1    6.8222 2705.9 -121983
    ## - meal:customer_type                                12    8.2737 2707.3 -121982
    ## - hotel:total_of_special_requests                    1    7.5758 2706.6 -121971
    ## - meal:total_of_special_requests                     4   12.5953 2711.7 -121896
    ## - adults:hotel                                       1   16.2415 2715.3 -121831
    ## 
    ## Step:  AIC=-122100.4
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests + 
    ##     adults:customer_type + adults:stays_in_week_nights + stays_in_weekend_nights:meal + 
    ##     stays_in_weekend_nights:stays_in_week_nights + stays_in_week_nights:total_of_special_requests + 
    ##     adults:meal + adults:stays_in_weekend_nights
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + stays_in_week_nights:hotel                         1    0.2032 2698.2 -122102
    ## + hotel:meal                                         3    0.4134 2698.0 -122101
    ## <none>                                                           2698.4 -122100
    ## + hotel:customer_type                                3    0.2938 2698.1 -122099
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.0297 2698.4 -122099
    ## + stays_in_weekend_nights:hotel                      1    0.0271 2698.4 -122099
    ## + stays_in_week_nights:meal                          4    0.3648 2698.0 -122098
    ## + stays_in_weekend_nights:customer_type              3    0.1154 2698.3 -122096
    ## + stays_in_week_nights:customer_type                 3    0.0648 2698.3 -122095
    ## + total_of_special_requests:customer_type            3    0.0563 2698.3 -122095
    ## - adults:stays_in_weekend_nights                     1    0.6595 2699.1 -122092
    ## - adults:meal                                        4    1.0384 2699.4 -122092
    ## - stays_in_week_nights:total_of_special_requests     1    1.3317 2699.7 -122081
    ## - adults:stays_in_week_nights                        1    1.6168 2700.0 -122076
    ## - stays_in_weekend_nights:meal                       4    2.0773 2700.5 -122075
    ## - stays_in_weekend_nights:stays_in_week_nights       1    2.3255 2700.7 -122065
    ## - adults:customer_type                               3    3.0993 2701.5 -122056
    ## - adults:total_of_special_requests                   1    6.6611 2705.1 -121994
    ## - meal:customer_type                                12    8.2290 2706.6 -121991
    ## - hotel:total_of_special_requests                    1    7.5222 2705.9 -121980
    ## - meal:total_of_special_requests                     4   12.5955 2711.0 -121904
    ## - adults:hotel                                       1   16.1832 2714.6 -121840
    ## 
    ## Step:  AIC=-122101.7
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests + 
    ##     adults:customer_type + adults:stays_in_week_nights + stays_in_weekend_nights:meal + 
    ##     stays_in_weekend_nights:stays_in_week_nights + stays_in_week_nights:total_of_special_requests + 
    ##     adults:meal + adults:stays_in_weekend_nights + stays_in_week_nights:hotel
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + hotel:meal                                         3    0.3971 2697.8 -122102
    ## + stays_in_weekend_nights:hotel                      1    0.1423 2698.1 -122102
    ## <none>                                                           2698.2 -122102
    ## + stays_in_week_nights:meal                          4    0.4230 2697.8 -122101
    ## - stays_in_week_nights:hotel                         1    0.2032 2698.4 -122100
    ## + hotel:customer_type                                3    0.2772 2697.9 -122100
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.0287 2698.2 -122100
    ## + stays_in_weekend_nights:customer_type              3    0.1013 2698.1 -122097
    ## + stays_in_week_nights:customer_type                 3    0.0589 2698.1 -122097
    ## + total_of_special_requests:customer_type            3    0.0567 2698.2 -122097
    ## - adults:meal                                        4    1.0132 2699.2 -122093
    ## - adults:stays_in_weekend_nights                     1    0.6981 2698.9 -122092
    ## - stays_in_week_nights:total_of_special_requests     1    1.4090 2699.6 -122081
    ## - adults:stays_in_week_nights                        1    1.7225 2699.9 -122076
    ## - stays_in_weekend_nights:meal                       4    2.1338 2700.3 -122075
    ## - stays_in_weekend_nights:stays_in_week_nights       1    2.5120 2700.7 -122063
    ## - adults:customer_type                               3    3.1369 2701.3 -122057
    ## - adults:total_of_special_requests                   1    6.6808 2704.9 -121995
    ## - meal:customer_type                                12    8.2660 2706.5 -121992
    ## - hotel:total_of_special_requests                    1    7.6564 2705.9 -121979
    ## - meal:total_of_special_requests                     4   12.5733 2710.8 -121906
    ## - adults:hotel                                       1   15.5465 2713.8 -121852
    ## 
    ## Step:  AIC=-122102.1
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests + 
    ##     adults:customer_type + adults:stays_in_week_nights + stays_in_weekend_nights:meal + 
    ##     stays_in_weekend_nights:stays_in_week_nights + stays_in_week_nights:total_of_special_requests + 
    ##     adults:meal + adults:stays_in_weekend_nights + stays_in_week_nights:hotel + 
    ##     hotel:meal
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## + stays_in_weekend_nights:hotel                      1    0.1517 2697.7 -122103
    ## <none>                                                           2697.8 -122102
    ## - hotel:meal                                         3    0.3971 2698.2 -122102
    ## - stays_in_week_nights:hotel                         1    0.1869 2698.0 -122101
    ## + stays_in_week_nights:meal                          4    0.4013 2697.4 -122101
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.0264 2697.8 -122101
    ## + hotel:customer_type                                3    0.2697 2697.5 -122101
    ## + stays_in_weekend_nights:customer_type              3    0.0960 2697.7 -122098
    ## + total_of_special_requests:customer_type            3    0.0580 2697.8 -122097
    ## + stays_in_week_nights:customer_type                 3    0.0561 2697.8 -122097
    ## - adults:meal                                        4    1.0202 2698.8 -122094
    ## - adults:stays_in_weekend_nights                     1    0.6881 2698.5 -122093
    ## - stays_in_week_nights:total_of_special_requests     1    1.3927 2699.2 -122082
    ## - stays_in_weekend_nights:meal                       4    1.8734 2699.7 -122080
    ## - adults:stays_in_week_nights                        1    1.7003 2699.5 -122077
    ## - stays_in_weekend_nights:stays_in_week_nights       1    2.4312 2700.2 -122065
    ## - adults:customer_type                               3    3.0894 2700.9 -122058
    ## - meal:customer_type                                12    7.7172 2705.5 -122001
    ## - adults:total_of_special_requests                   1    6.7024 2704.5 -121995
    ## - hotel:total_of_special_requests                    1    7.6623 2705.5 -121980
    ## - meal:total_of_special_requests                     4   12.6852 2710.5 -121905
    ## - adults:hotel                                       1   15.5549 2713.4 -121852
    ## 
    ## Step:  AIC=-122102.6
    ## children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests + 
    ##     adults:customer_type + adults:stays_in_week_nights + stays_in_weekend_nights:meal + 
    ##     stays_in_weekend_nights:stays_in_week_nights + stays_in_week_nights:total_of_special_requests + 
    ##     adults:meal + adults:stays_in_weekend_nights + stays_in_week_nights:hotel + 
    ##     hotel:meal + stays_in_weekend_nights:hotel
    ## 
    ##                                                     Df Sum of Sq    RSS     AIC
    ## <none>                                                           2697.7 -122103
    ## - stays_in_weekend_nights:hotel                      1    0.1517 2697.8 -122102
    ## - hotel:meal                                         3    0.4066 2698.1 -122102
    ## + stays_in_week_nights:meal                          4    0.4285 2697.2 -122102
    ## + stays_in_weekend_nights:total_of_special_requests  1    0.0257 2697.6 -122101
    ## + hotel:customer_type                                3    0.2520 2697.4 -122101
    ## - stays_in_week_nights:hotel                         1    0.3046 2698.0 -122100
    ## + stays_in_weekend_nights:customer_type              3    0.0962 2697.6 -122098
    ## + stays_in_week_nights:customer_type                 3    0.0586 2697.6 -122098
    ## + total_of_special_requests:customer_type            3    0.0567 2697.6 -122098
    ## - adults:meal                                        4    1.0353 2698.7 -122094
    ## - adults:stays_in_weekend_nights                     1    0.6827 2698.3 -122094
    ## - stays_in_week_nights:total_of_special_requests     1    1.3803 2699.0 -122082
    ## - stays_in_weekend_nights:meal                       4    1.8255 2699.5 -122081
    ## - adults:stays_in_week_nights                        1    1.6534 2699.3 -122078
    ## - stays_in_weekend_nights:stays_in_week_nights       1    2.3046 2700.0 -122067
    ## - adults:customer_type                               3    3.1061 2700.8 -122058
    ## - meal:customer_type                                12    7.6755 2705.3 -122002
    ## - adults:total_of_special_requests                   1    6.6993 2704.3 -121996
    ## - hotel:total_of_special_requests                    1    7.5417 2705.2 -121982
    ## - meal:total_of_special_requests                     4   12.6649 2710.3 -121905
    ## - adults:hotel                                       1   15.6972 2713.3 -121850

    getCall(lm_step)

    ## lm(formula = children ~ adults + stays_in_weekend_nights + stays_in_week_nights + 
    ##     hotel + meal + total_of_special_requests + customer_type + 
    ##     adults:hotel + meal:total_of_special_requests + meal:customer_type + 
    ##     hotel:total_of_special_requests + adults:total_of_special_requests + 
    ##     adults:customer_type + adults:stays_in_week_nights + stays_in_weekend_nights:meal + 
    ##     stays_in_weekend_nights:stays_in_week_nights + stays_in_week_nights:total_of_special_requests + 
    ##     adults:meal + adults:stays_in_weekend_nights + stays_in_week_nights:hotel + 
    ##     hotel:meal + stays_in_weekend_nights:hotel, data = HD_filter)

    coef(lm_step)

    ##                                    (Intercept) 
    ##                                   -0.049018896 
    ##                                         adults 
    ##                                    0.011909686 
    ##                        stays_in_weekend_nights 
    ##                                    0.027054793 
    ##                           stays_in_week_nights 
    ##                                    0.012295180 
    ##                              hotelResort_Hotel 
    ##                                   -0.125511584 
    ##                                         mealFB 
    ##                                    0.271710875 
    ##                                         mealHB 
    ##                                    0.083577132 
    ##                                         mealSC 
    ##                                    0.053972422 
    ##                                  mealUndefined 
    ##                                   -0.072451450 
    ##                      total_of_special_requests 
    ##                                    0.100269982 
    ##                             customer_typeGroup 
    ##                                    0.041685710 
    ##                         customer_typeTransient 
    ##                                    0.021199328 
    ##                   customer_typeTransient-Party 
    ##                                    0.050274424 
    ##                       adults:hotelResort_Hotel 
    ##                                    0.086956383 
    ##               mealFB:total_of_special_requests 
    ##                                    0.179700699 
    ##               mealHB:total_of_special_requests 
    ##                                    0.024719950 
    ##               mealSC:total_of_special_requests 
    ##                                   -0.042477287 
    ##        mealUndefined:total_of_special_requests 
    ##                                    0.177707079 
    ##                      mealFB:customer_typeGroup 
    ##                                   -0.384104105 
    ##                      mealHB:customer_typeGroup 
    ##                                   -0.034819379 
    ##                      mealSC:customer_typeGroup 
    ##                                   -0.054310632 
    ##               mealUndefined:customer_typeGroup 
    ##                                    0.162079419 
    ##                  mealFB:customer_typeTransient 
    ##                                   -0.334284308 
    ##                  mealHB:customer_typeTransient 
    ##                                    0.017072578 
    ##                  mealSC:customer_typeTransient 
    ##                                   -0.061944351 
    ##           mealUndefined:customer_typeTransient 
    ##                                    0.262631572 
    ##            mealFB:customer_typeTransient-Party 
    ##                                   -0.335710442 
    ##            mealHB:customer_typeTransient-Party 
    ##                                   -0.063743572 
    ##            mealSC:customer_typeTransient-Party 
    ##                                   -0.023427656 
    ##     mealUndefined:customer_typeTransient-Party 
    ##                                    0.131142490 
    ##    hotelResort_Hotel:total_of_special_requests 
    ##                                   -0.036262059 
    ##               adults:total_of_special_requests 
    ##                                   -0.031542274 
    ##                      adults:customer_typeGroup 
    ##                                   -0.003147267 
    ##                  adults:customer_typeTransient 
    ##                                    0.029149154 
    ##            adults:customer_typeTransient-Party 
    ##                                   -0.010552355 
    ##                    adults:stays_in_week_nights 
    ##                                   -0.007753163 
    ##                 stays_in_weekend_nights:mealFB 
    ##                                    0.055581402 
    ##                 stays_in_weekend_nights:mealHB 
    ##                                   -0.014195734 
    ##                 stays_in_weekend_nights:mealSC 
    ##                                   -0.009275667 
    ##          stays_in_weekend_nights:mealUndefined 
    ##                                   -0.025440463 
    ##   stays_in_weekend_nights:stays_in_week_nights 
    ##                                   -0.001149866 
    ## stays_in_week_nights:total_of_special_requests 
    ##                                    0.003568627 
    ##                                  adults:mealFB 
    ##                                   -0.122460701 
    ##                                  adults:mealHB 
    ##                                   -0.020663337 
    ##                                  adults:mealSC 
    ##                                   -0.009814425 
    ##                           adults:mealUndefined 
    ##                                   -0.045142005 
    ##                 adults:stays_in_weekend_nights 
    ##                                   -0.009492022 
    ##         stays_in_week_nights:hotelResort_Hotel 
    ##                                    0.003622663 
    ##                       hotelResort_Hotel:mealFB 
    ##                                    0.240059580 
    ##                       hotelResort_Hotel:mealHB 
    ##                                    0.001278947 
    ##                       hotelResort_Hotel:mealSC 
    ##                                    0.063220559 
    ##                hotelResort_Hotel:mealUndefined 
    ##                                             NA 
    ##      stays_in_weekend_nights:hotelResort_Hotel 
    ##                                   -0.004989935

    # Stepwise selection returns a large model, with 21 coefficients including interactions.

    rmse_frame_lm_step=foreach(x=1:100, .combine='rbind')%do%{

    x = initial_split(HD_filter, prop = 0.8)
    HD_train = training(x)
    HD_test = testing(x)

    modelr::rmse(lm_step,HD_test)
    } %>% as.data.frame

    validated_result_lm_step = mean(rmse_frame_lm_step$V1)
    # .249

We first set of two simple baseline models. We run two logistic
regressions on the binary outcome “children”. The first of which is very
simple, and the second of which contains all possible variables except
arrival date. The first baseline model seems to outperform the larger
baseline model with mean log(RMSEout)=1.14. We will try to compete with
the results of the smaller model.

The first attempt will be based on a simple and intuitive linear
probability model.(sensible\_model) From this simple model, we then
complete stepwise selection on all interactions, to arrive at
(lm\_step). We then apply our model to 100 train/test splits, resulting
in a mean validated RMSEout of .249, which is a 78% improvement. This
will likely be difficult to beat, but we will also try a cross validated
lasso.

\#Problem 4 \# Part A: Lasso

    # Next, we will try a lasso regression

    HD_filter = HD %>% filter(reserved_room_type != "F")

    hdx = model.matrix(children ~ .-1-arrival_date, data=HD_filter)
    hdy = HD_filter$children
    cv.hgglm = cv.glmnet(hdx,hdy,family="binomial",folds=20,standardize = TRUE)
    cv.hgglm$lambda.min

    ## [1] 0.0001020243

    # .0001119715
    plot(cv.hgglm, bty = "n")

![](Homework2_files/figure-markdown_strict/Problem%204a.2-1.png)

    hd_min = coef(cv.hgglm, select="min")
    # This results in the optimal lambda at the min

    # This counts the number of non-zero coefficients
    sum(hd_min!=0)

    ## [1] 30

    mse = min(cv.hgglm$cvm)
    rmse = sqrt(mse)
    print(rmse)

    ## [1] 0.6036578

    # .603

    # The cross validated gamma lasso outperforms both baselines

    # We get all of our named coefficients and print a table
    df = data.frame(name = hd_min@Dimnames[[1]][hd_min@i + 1], coefficient = hd_min@x)
    print(df)

    ##                              name   coefficient
    ## 1                     (Intercept) -4.387000e+00
    ## 2                 hotelCity_Hotel  1.029466e-01
    ## 3               hotelResort_Hotel -4.066873e-12
    ## 4         stays_in_weekend_nights  8.888424e-03
    ## 5                          adults -2.704109e-01
    ## 6                          mealFB  1.745838e-01
    ## 7                          mealSC -7.794573e-01
    ## 8         market_segmentCorporate -5.823826e-01
    ## 9            market_segmentDirect  1.073419e-02
    ## 10           market_segmentGroups -7.358491e-01
    ## 11     distribution_channelDirect  2.606363e-01
    ## 12              is_repeated_guest -8.201321e-01
    ## 13 previous_bookings_not_canceled -9.802960e-03
    ## 14            reserved_room_typeB  1.847140e+00
    ## 15            reserved_room_typeC  2.620580e+00
    ## 16            reserved_room_typeD -6.063024e-01
    ## 17            reserved_room_typeG  2.498842e+00
    ## 18            reserved_room_typeH  3.011752e+00
    ## 19            assigned_room_typeB  5.788980e-02
    ## 20            assigned_room_typeC  8.938309e-01
    ## 21            assigned_room_typeD  4.998320e-01
    ## 22            assigned_room_typeE  9.279075e-02
    ## 23            assigned_room_typeG  3.642426e-01
    ## 24            assigned_room_typeH  5.869260e-01
    ## 25            assigned_room_typeI  5.749945e-02
    ## 26                booking_changes  2.460855e-01
    ## 27         customer_typeTransient  5.318017e-01
    ## 28   customer_typeTransient-Party -4.388338e-02
    ## 29             average_daily_rate  9.284238e-03
    ## 30      total_of_special_requests  4.586819e-01

The lasso regression outperforms both baseline models
considerably(validated mean RMSEout =.603), however, the stepwise
selected model considerably outperforms the lasso.So, for further
analysis, we will utilize the stepwise selected model. (lm\_step)

## Part B

    HV <- read_csv("hotels_val.csv")
    hvy = HV$children


    pred =predict(lm_step,newdata=HV)
    confusion_table = data.frame(fold_id=integer(),TPR=integer(),FPR=integer())

    level = seq(.1,.8,by=.05)


    confusion_level=foreach(x=level)%do%{
    yhat_test = ifelse(pred > x,1,0)
    confusion_out = table(y=hvy, yhat = yhat_test)
    TPR = (confusion_out[2,2]/(confusion_out[2,1]+confusion_out[2,2]))
    FPR = (confusion_out[1,2]/(confusion_out[1,1]+confusion_out[1,2]))
    confusion_table[nrow(confusion_table)+1,] = c(x,TPR,FPR)
    }


    confusion_table %>% ggplot(aes(FPR,TPR)) + geom_line(fill="steelblue") + labs(y= "True Positive Rate", x="False Positive Rate", title = "ROC Curve for Stepwise Model")+theme_linedraw() + geom_abline(slope=1,intercept = 0)

![](Homework2_files/figure-markdown_strict/Problem%204b-1.png) \#\# Part
C

    folds = createFolds(HV$children,k=20, list = TRUE,returnTrain = FALSE )


    # sapply(lapply(Fold_set,unique),length)
    # # The folds introduce limitations to our analysis. 
    # # The levels of some variables in our folds are less than two.
    # # We can only use folds where this error does not arise
    # feasible_sets = c(4,8,11,12,13,15,17,18,19,20)
    # 
    # accuracy=foreach(x=feasible_sets, .combine='rbind')%do%{
    # Fold_set = HV[ folds[[x]], ]
    # hvx = model.matrix(children ~ .-1-arrival_date-deposit_type, data=Fold_set)
    # hvy = Fold_set$children
    # sum_pred =sum(predict.glmnet(hgglm,newx =hvx,s=c(.0001119715), type="response"))
    # sum_actual = sum(Fold_set$children)
    # perf = c(x,sum_pred,sum_actual)
    # } %>% as.data.frame

    # The other issue, is that the folds also do not represent every combination of
    # possibilities, giving us the result The number of variables in newx must be 48".
    # I don't see any way past this, so we will do this exercise on a more simple model.

    table=foreach(x=1:20, .combine='rbind')%do%{
    Fold_set = HV[ folds[[x]], ]
    sum_pred =round(sum(predict(lm_step,newdata = Fold_set, type="response")))
    sum_actual = sum(Fold_set$children)
    perf = c(x,sum_pred,sum_actual)
    } %>% as.data.frame
    table = table %>% mutate(Detection_Rate = V2/V3)
    names(table)[1] = 'Fold'
    names(table)[2] = 'Predicted Number of Children'
    names(table)[3] = 'Actual Number of Children'
    mean(table$Detection_Rate)

    ## [1] 0.945556

    # 1.056
    # The model predicts ~6% more children on average across 20 folds.

    table

    ##           Fold Predicted Number of Children Actual Number of Children
    ## result.1     1                           19                        22
    ## result.2     2                           17                        17
    ## result.3     3                           19                        24
    ## result.4     4                           18                        20
    ## result.5     5                           20                        27
    ## result.6     6                           16                        19
    ## result.7     7                           18                        20
    ## result.8     8                           18                        18
    ## result.9     9                           18                        19
    ## result.10   10                           17                        22
    ## result.11   11                           17                        22
    ## result.12   12                           18                        25
    ## result.13   13                           18                        13
    ## result.14   14                           20                        20
    ## result.15   15                           19                        25
    ## result.16   16                           18                        17
    ## result.17   17                           19                        12
    ## result.18   18                           17                        14
    ## result.19   19                           18                        24
    ## result.20   20                           20                        22
    ##           Detection_Rate
    ## result.1       0.8636364
    ## result.2       1.0000000
    ## result.3       0.7916667
    ## result.4       0.9000000
    ## result.5       0.7407407
    ## result.6       0.8421053
    ## result.7       0.9000000
    ## result.8       1.0000000
    ## result.9       0.9473684
    ## result.10      0.7727273
    ## result.11      0.7727273
    ## result.12      0.7200000
    ## result.13      1.3846154
    ## result.14      1.0000000
    ## result.15      0.7600000
    ## result.16      1.0588235
    ## result.17      1.5833333
    ## result.18      1.2142857
    ## result.19      0.7500000
    ## result.20      0.9090909

The model predicts on average, 6% more children expected than actual
across the 20 folds.
