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

# Problem 1

    ABIA <- read_csv("ABIA.csv")

    ABIA_Adjust = ABIA%>% select(UniqueCarrier,ArrDelay,Origin)

    ABIA_Adjust = na.omit(ABIA_Adjust)

    mean_table =ABIA_Adjust %>%
    dplyr::filter(str_detect(Origin,"AUS"))%>%
      group_by(UniqueCarrier) %>%
      summarize(mean_delay = mean(ArrDelay))

    mean_table %>% ggplot(aes(fct_reorder(UniqueCarrier,mean_delay),mean_delay)) + geom_col(fill="steelblue") + coord_flip() + labs(y= "Average Time Late on Arrival(min)", x="Airline", title = "Departing Austin: Which Airline is Most Late on Average")+theme_linedraw()

![](Homework1_files/figure-markdown_strict/Problem%201a-1.png) Imagine
you are the most sensitive person to having your time wasted in Austin.
Every flight you take feels like a torturous lottery. You may wonder
which airlines are more likely to be on time. As we see in the above
naive plot, some airlines appear truly later than others. Particularly,
Delta and PSA Airlines. Is it fair to make this judgement? Perhaps there
are other factors influencing the distribution. For example, maybe some
airlines are far more likely to travel longer distances, and thereby
lose more time on the longer flights due to factors such as prevailing
winds on a particular long route.

    ABIA_Adjust = ABIA%>% select(UniqueCarrier,ArrDelay,Origin,Dest,Month)

    ABIA_Adjust = na.omit(ABIA_Adjust)

    mean_table =ABIA_Adjust %>%
    dplyr::filter(str_detect(Origin,"AUS"))%>%
      group_by(UniqueCarrier,Month) %>%
      summarize(mean_delay = mean(ArrDelay))

    mean_table %>% ggplot(aes(x=UniqueCarrier, y=mean_delay, group=Month)) + geom_boxplot()+ylim (1,40)

![](Homework1_files/figure-markdown_strict/Problem%201b-1.png) A
possibly more honest way to look at the data would be to plot the
distributions of average late arrivals over the months. From this view,
we can see that even with relatively low averages, some airlines have
far more variability in their arrival times. From the previous graph,
one might consider United Airlines (UA) as a reliable choice. However,
United has some of the most extreme variation in late arrival times.
This may be a better view of consistency.

# Problem 2

## Part A:

    billboard <- read_csv("billboard.csv")

    billboard_ct = billboard %>%
    mutate(count = weeks_on_chart)

    billboard_c <- billboard %>% group_by(performer,song) %>%
    mutate(count = max(weeks_on_chart)) %>%
    arrange(desc(count))

    billboard_cd <- billboard_c %>%
    select(performer, song, count)

    billboard_ce <- unique(billboard_cd)

    top_ten <- billboard_ce[1:10,]
    print(top_ten)

    ## # A tibble: 10 x 3
    ## # Groups:   performer, song [10]
    ##    performer                              song                             count
    ##    <chr>                                  <chr>                            <dbl>
    ##  1 Imagine Dragons                        Radioactive                         87
    ##  2 AWOLNATION                             Sail                                79
    ##  3 The Weeknd                             Blinding Lights                     76
    ##  4 Jason Mraz                             I'm Yours                           76
    ##  5 LeAnn Rimes                            How Do I Live                       69
    ##  6 OneRepublic                            Counting Stars                      68
    ##  7 LMFAO Featuring Lauren Bennett & Goon~ Party Rock Anthem                   68
    ##  8 Adele                                  Rolling In The Deep                 65
    ##  9 Jewel                                  Foolish Games/You Were Meant Fo~    65
    ## 10 Carrie Underwood                       Before He Cheats                    64

We are tasked with making a table of the 10 most popular songs measured
by the total number of weeks the song appeared in the billboard “Top
100” list. We observe that some newer songs from the early 2010s and
2000s are some of the most popular of all time. Imagine Dragons’ smash
hit “Radioactive”, with almost two years of being on the top 100 is a
clear standout winner.

## Part B:

    billboard_u <- billboard %>%
    select(year,song)

    billboard_u = unique(billboard_u)

    spy <- billboard_u %>% count(year)


    spy_adjust = spy %>% slice(-c(1,64))
    year = spy_adjust$year
    number_of_songs = spy_adjust$n

    ggplot(data = spy_adjust) + geom_line(aes(x= year,y=number_of_songs), color = "steelblue",size =2) + labs(y= "Number of Unique Songs", x="Year", title = "Musical Diversity on the Billboard Top 100") +theme_linedraw()

![](Homework1_files/figure-markdown_strict/Problem%202b-1.png) In this
figure, we are measuring musical diversity defined as the number of
unique songs to appear on the Billboard “Top 100” list over the course
of a given year. We omitted 1958 and 2021 from this figure, as the data
for these two years is incomplete. We observe that musical diversity
steadily declined from the 60’s onward into the 2000s. In 2004, musical
diversity began to rebound to where music is almost as diverse in the
2020s as in the measures all time high in the mid 1960s. \#\# Part C:

    twh = billboard_ce %>%
    mutate(ten_week_hit = ifelse(count >= 10, yes=1, no =0))

    ten_week_hit = twh$ten_week_hit
    performer = twh$performer

    twhl = twh[!(twh$ten_week_hit=="0"),] %>%
    group_by(performer)%>%
    count(ten_week_hit)%>%
    arrange(desc(n))
    final_list = twhl[1:19,] %>% select(performer,n)

    performer = final_list$performer
    number_of_ten_week_hits = final_list$n


    final_list %>% ggplot(aes(fct_reorder(performer,number_of_ten_week_hits),number_of_ten_week_hits)) + geom_col(fill="steelblue") + coord_flip() + labs(y= "Number of Ten Week Hits", x="Performing Artist", title = "Artists With the Most Ten Week Hits")+theme_linedraw()

![](Homework1_files/figure-markdown_strict/Problem%202c-1.png) In this
figure, we are dialing in on major hits that were on the Billboard “Top
100” list for at least 10 weeks, which we will refer to as “Ten Week
Hits”, and the artists that performed them. Some artists have been
astonishingly successful at making many chart topping hits. We observe
19 artists with at least 30 “Ten Week Hits”. Elton John is the most
successful of all time, with a mind boggling 53 “Ten Week Hits”. \#
Problem 3 \#\# Part A:

    olympics_top20 <- read_csv("olympics_top20.csv")

    q95 <- olympics_top20 %>%
    filter(str_detect(sex,"F")) %>%
    group_by(event) %>%
    summarize(Height_at_95th_Quantile = quantile(height, prob =(.95)))
    print(q95)

    ## # A tibble: 132 x 2
    ##    event                                       Height_at_95th_Quantile
    ##    <chr>                                                         <dbl>
    ##  1 Athletics Women's 1,500 metres                                 172 
    ##  2 Athletics Women's 10 kilometres Walk                           170 
    ##  3 Athletics Women's 10,000 metres                                168.
    ##  4 Athletics Women's 100 metres                                   180.
    ##  5 Athletics Women's 100 metres Hurdles                           176 
    ##  6 Athletics Women's 20 kilometres Walk                           173 
    ##  7 Athletics Women's 200 metres                                   180 
    ##  8 Athletics Women's 3,000 metres                                 170 
    ##  9 Athletics Women's 3,000 metres Steeplechase                    177.
    ## 10 Athletics Women's 4 x 100 metres Relay                         176 
    ## # ... with 122 more rows

How does height vary across all of the women’s olympic events? In the
above table, we show what height at the 95th quantile is for each of the
132 women’s olympic events in centimeters. Unsurprisingly, the tallest
women at the 95th quantile are Women’s Basketball players, with a 95th
quantile height of 197.55 cm. \#\# Part B:

    wsd <- olympics_top20 %>%
    filter(str_detect(sex,"F")) %>%
    group_by(event) %>%
    summarize(Standard_Deviation_Height = sd(height))%>%
    arrange(desc(Standard_Deviation_Height))
    print(wsd[1,])

    ## # A tibble: 1 x 2
    ##   event                      Standard_Deviation_Height
    ##   <chr>                                          <dbl>
    ## 1 Rowing Women's Coxed Fours                      10.9

In which women’s Olympic event is the variation in height the greatest?
We would expect that it would be an event where height doesn’t provide
much of a meaningful advantage, and what we would expect is evidently
true. In “Rowing Women’s Coxed Fours”, the standard deviation of height
is 10.865 cm.

## Part C:

    swm <- olympics_top20 %>%
    filter(str_detect(sport, "Swimming")) %>%
    group_by(year, sex) %>%
    summarize(age = mean(age))

    swm%>%
    ggplot() + geom_line(aes(x=year, y=age, group =sex, color = sex),size =2) + labs(y= "Athlete Age", x="Year", title = "Average Age of Olympic Swimmers") +theme_linedraw()

![](Homework1_files/figure-markdown_strict/Problem%203c-1.png) How has
the average age of Olympic swimmers changed over the years? For men, we
initially observe that swimmers were extremely young, at 18 years old.
The average age rose sharply into the 1920s before sharply declining to
19 years of age, and stabilizing around 20 years old until the late
1970s. Since the 1970s, age has been steadily increasing to the mid 20s.
Woman swimmers were allowed to participate in the games starting in the
1920s. Women swimmers on average have been roughly two years younger
than their male counterparts.The overall trend however has been similar.
In the mid to late 1970s, the average age of women swimmers began to
steadily increase from younger than 18 to 22 years of age into the
2000s.

# Problem 4

## Part A:

    sclass <- read_csv("sclass.csv")

    Three_Fifty = sclass %>%
      filter(trim == 350)
    set.seed(23)
    split = initial_split(Three_Fifty, prop=.8)
    Three_Fifty_train = training(split)
    Three_Fifty_test = testing(split)

    rmse_out=foreach(k=1:150, .combine='rbind')%do%{


    knn_model = knnreg(price ~mileage, data = Three_Fifty_train, k =k)
    modelr::rmse(knn_model,Three_Fifty_test)
    } %>% as.data.frame
    rmse_out

    ##                   V1
    ## result.1   11248.475
    ## result.2   10521.059
    ## result.3   10084.269
    ## result.4   10469.269
    ## result.5    9730.314
    ## result.6    9366.014
    ## result.7    9669.107
    ## result.8    9491.901
    ## result.9    9651.920
    ## result.10   9472.043
    ## result.11   9530.677
    ## result.12   9458.163
    ## result.13   9484.328
    ## result.14   9404.228
    ## result.15   9430.819
    ## result.16   9463.194
    ## result.17   9358.610
    ## result.18   9359.546
    ## result.19   9337.879
    ## result.20   9348.756
    ## result.21   9417.405
    ## result.22   9406.585
    ## result.23   9466.916
    ## result.24   9449.021
    ## result.25   9448.850
    ## result.26   9452.717
    ## result.27   9526.494
    ## result.28   9538.980
    ## result.29   9515.987
    ## result.30   9584.791
    ## result.31   9534.029
    ## result.32   9549.112
    ## result.33   9601.802
    ## result.34   9592.699
    ## result.35   9538.931
    ## result.36   9526.428
    ## result.37   9536.409
    ## result.38   9528.122
    ## result.39   9534.442
    ## result.40   9597.143
    ## result.41   9643.177
    ## result.42   9659.437
    ## result.43   9651.259
    ## result.44   9640.411
    ## result.45   9686.571
    ## result.46   9727.815
    ## result.47   9744.577
    ## result.48   9771.018
    ## result.49   9804.704
    ## result.50   9827.704
    ## result.51   9844.196
    ## result.52   9873.396
    ## result.53   9912.761
    ## result.54   9905.119
    ## result.55   9924.707
    ## result.56   9941.673
    ## result.57   9998.543
    ## result.58  10002.307
    ## result.59  10001.889
    ## result.60  10010.796
    ## result.61  10043.973
    ## result.62  10034.047
    ## result.63  10040.733
    ## result.64  10094.907
    ## result.65  10059.542
    ## result.66  10077.851
    ## result.67  10083.528
    ## result.68  10085.898
    ## result.69  10101.292
    ## result.70  10085.907
    ## result.71  10087.805
    ## result.72  10098.989
    ## result.73  10103.037
    ## result.74  10113.953
    ## result.75  10175.764
    ## result.76  10194.914
    ## result.77  10260.632
    ## result.78  10271.786
    ## result.79  10297.313
    ## result.80  10315.327
    ## result.81  10336.406
    ## result.82  10308.634
    ## result.83  10359.393
    ## result.84  10407.615
    ## result.85  10451.302
    ## result.86  10492.604
    ## result.87  10536.109
    ## result.88  10578.049
    ## result.89  10602.988
    ## result.90  10615.962
    ## result.91  10643.478
    ## result.92  10704.869
    ## result.93  10782.803
    ## result.94  10795.807
    ## result.95  10837.123
    ## result.96  10896.111
    ## result.97  10910.571
    ## result.98  10963.824
    ## result.99  10994.824
    ## result.100 11021.075
    ## result.101 11090.735
    ## result.102 11168.192
    ## result.103 11220.330
    ## result.104 11271.866
    ## result.105 11294.457
    ## result.106 11353.909
    ## result.107 11415.488
    ## result.108 11466.446
    ## result.109 11518.119
    ## result.110 11544.557
    ## result.111 11623.834
    ## result.112 11681.035
    ## result.113 11710.594
    ## result.114 11762.088
    ## result.115 11785.481
    ## result.116 11842.890
    ## result.117 11907.972
    ## result.118 11963.026
    ## result.119 11996.505
    ## result.120 12090.220
    ## result.121 12149.763
    ## result.122 12224.158
    ## result.123 12279.102
    ## result.124 12322.758
    ## result.125 12388.629
    ## result.126 12440.647
    ## result.127 12515.706
    ## result.128 12551.954
    ## result.129 12601.260
    ## result.130 12640.558
    ## result.131 12688.151
    ## result.132 12749.314
    ## result.133 12799.868
    ## result.134 12836.122
    ## result.135 12893.288
    ## result.136 12968.020
    ## result.137 13026.226
    ## result.138 13074.712
    ## result.139 13122.303
    ## result.140 13211.156
    ## result.141 13245.171
    ## result.142 13306.248
    ## result.143 13392.985
    ## result.144 13437.749
    ## result.145 13492.254
    ## result.146 13505.903
    ## result.147 13535.437
    ## result.148 13597.077
    ## result.149 13658.865
    ## result.150 13727.673

    row_adjust = c(1:150)
    rownames(rmse_out) = row_adjust
    rmse_out$k <- c(1:150) 

    ggplot(rmse_out) + geom_line(aes(x=k,y=V1),size =2) + labs(y= "RMSE", x="K", title = "Root Mean Square Error for Different Values of K") +theme_linedraw() + geom_vline(xintercept=67, linetype="dashed", color = "red")

![](Homework1_files/figure-markdown_strict/Problem%204a-1.png)

    knn_model1 = knnreg(price ~mileage, data = Three_Fifty_train, k =67)
    Three_Fifty_test= Three_Fifty_test %>%
      mutate(TF_pred = predict(knn_model1,Three_Fifty_test))

    Three_Fifty_test %>%
      ggplot() + geom_point(aes(x=mileage, y=price)) + geom_line(aes(x=mileage, y=TF_pred),color = "darkred") + theme_linedraw() +labs(y= "Price", x="Mileage", title = "Predicted Price of Secondhand Mercedes S350")+ scale_x_continuous(labels = scales:: comma)

![](Homework1_files/figure-markdown_strict/Problem%204b-1.png)

    Sixty_five_AMG = sclass %>%
      filter(str_detect(trim,"65 AMG"))
    set.seed(123)
    split = initial_split(Sixty_five_AMG, prop=.8)
    Sixty_five_AMG_train = training(split)
    Sixty_five_AMG_test = testing(split)

    rmse_out=foreach(k=1:150, .combine='rbind')%do%{



    knn_model = knnreg(price ~mileage, data = Sixty_five_AMG_train, k =k)
    modelr::rmse(knn_model,Sixty_five_AMG_test)
    } %>% as.data.frame
    rmse_out

    ##                  V1
    ## result.1   26802.15
    ## result.2   20018.64
    ## result.3   18826.97
    ## result.4   17248.99
    ## result.5   15608.69
    ## result.6   14616.56
    ## result.7   13885.63
    ## result.8   13896.08
    ## result.9   13578.82
    ## result.10  13254.39
    ## result.11  13253.50
    ## result.12  13326.21
    ## result.13  13308.53
    ## result.14  13117.08
    ## result.15  12622.84
    ## result.16  12405.18
    ## result.17  12278.54
    ## result.18  12266.44
    ## result.19  12623.32
    ## result.20  12488.40
    ## result.21  12761.20
    ## result.22  12738.03
    ## result.23  13005.01
    ## result.24  12877.48
    ## result.25  12809.72
    ## result.26  12708.40
    ## result.27  12779.26
    ## result.28  12664.18
    ## result.29  12644.55
    ## result.30  12645.26
    ## result.31  12492.52
    ## result.32  12515.85
    ## result.33  12466.86
    ## result.34  12471.64
    ## result.35  12429.52
    ## result.36  12451.02
    ## result.37  12420.29
    ## result.38  12490.51
    ## result.39  12560.01
    ## result.40  12618.17
    ## result.41  12600.46
    ## result.42  12612.56
    ## result.43  12592.51
    ## result.44  12615.15
    ## result.45  12703.19
    ## result.46  12762.58
    ## result.47  12796.86
    ## result.48  12874.96
    ## result.49  13038.74
    ## result.50  13084.32
    ## result.51  13164.35
    ## result.52  13195.63
    ## result.53  13269.71
    ## result.54  13379.55
    ## result.55  13434.02
    ## result.56  13469.22
    ## result.57  13498.01
    ## result.58  13633.59
    ## result.59  13672.19
    ## result.60  13721.67
    ## result.61  13822.71
    ## result.62  13818.88
    ## result.63  13906.66
    ## result.64  13926.83
    ## result.65  14104.82
    ## result.66  14176.92
    ## result.67  14229.23
    ## result.68  14370.69
    ## result.69  14432.11
    ## result.70  14553.53
    ## result.71  14741.43
    ## result.72  14944.65
    ## result.73  15268.70
    ## result.74  15447.56
    ## result.75  15626.51
    ## result.76  15868.86
    ## result.77  16213.23
    ## result.78  16666.65
    ## result.79  17158.09
    ## result.80  17481.70
    ## result.81  18201.02
    ## result.82  18830.33
    ## result.83  19415.03
    ## result.84  19899.31
    ## result.85  20363.59
    ## result.86  20594.68
    ## result.87  21251.34
    ## result.88  21737.83
    ## result.89  22264.78
    ## result.90  22823.28
    ## result.91  23338.69
    ## result.92  24386.19
    ## result.93  24952.65
    ## result.94  25658.11
    ## result.95  26295.62
    ## result.96  27104.98
    ## result.97  27631.07
    ## result.98  27937.47
    ## result.99  28447.47
    ## result.100 29061.67
    ## result.101 29820.33
    ## result.102 30157.95
    ## result.103 30748.00
    ## result.104 31358.97
    ## result.105 32103.68
    ## result.106 32605.44
    ## result.107 33307.25
    ## result.108 33628.03
    ## result.109 34342.47
    ## result.110 34962.06
    ## result.111 35269.97
    ## result.112 36034.94
    ## result.113 36350.37
    ## result.114 36920.46
    ## result.115 37552.48
    ## result.116 38152.57
    ## result.117 38310.05
    ## result.118 39011.51
    ## result.119 39475.76
    ## result.120 40094.82
    ## result.121 40767.40
    ## result.122 41393.88
    ## result.123 41983.84
    ## result.124 42687.06
    ## result.125 43148.59
    ## result.126 43711.16
    ## result.127 44348.90
    ## result.128 44837.93
    ## result.129 45447.62
    ## result.130 45985.95
    ## result.131 46540.53
    ## result.132 46955.38
    ## result.133 47684.73
    ## result.134 48191.05
    ## result.135 48686.28
    ## result.136 48962.85
    ## result.137 49393.82
    ## result.138 49942.98
    ## result.139 50274.91
    ## result.140 50761.18
    ## result.141 51162.29
    ## result.142 51662.45
    ## result.143 52210.01
    ## result.144 52819.00
    ## result.145 53312.15
    ## result.146 53780.87
    ## result.147 54243.63
    ## result.148 54848.13
    ## result.149 55291.94
    ## result.150 55868.71

    row_adjust = c(1:150)
    rownames(rmse_out) = row_adjust
    rmse_out$k <- c(1:150) 

    ggplot(rmse_out) + geom_line(aes(x=k,y=V1),size =2) + labs(y= "RMSE", x="K", title = "Root Mean Square Error for Different Values of K") +theme_linedraw() + geom_vline(xintercept=15, linetype="dashed", color = "red")

![](Homework1_files/figure-markdown_strict/Problem%204c-1.png)

    knn_model2 = knnreg(price ~mileage,data = Sixty_five_AMG_train, k =15)
    data = Sixty_five_AMG_test= Sixty_five_AMG_test %>%
      mutate(TF_pred = predict(knn_model2,Sixty_five_AMG_test))

    Sixty_five_AMG_test %>%
      ggplot() + geom_point(aes(x=mileage, y=price)) + geom_line(aes(x=mileage, y=TF_pred),color = "darkred") + theme_linedraw() +labs(y= "Price", x="Mileage", title = "Predicted Price of Secondhand Mercedes 65 AMG") + scale_x_continuous(labels = scales:: comma)

![](Homework1_files/figure-markdown_strict/Problem%204d-1.png) When
choosing an optimal value for “K” in KNN models, we have to consider the
bias-variance trade off. As K decreases, the variance of the fit tends
to increase,and bias decreases. As shown in the RMSE vs. K plots, the
optimal value of K varies between the different trim levels of the
Mercedes S-Class. For the S350 trim level, we select an optimal K value
of 67. For the 65 AMG trim, we select an optimal K of 15.

What is the reason behind the different values of K between the trim
levels? I believe that the answer lies in the sizes of the data sets for
each trim level. There are 416 observations for the S350 trim, and 292
for the S65 AMG trim. As we are selecting for the lowest RMSE, we would
expect the data set with more observations to favor lower variance over
bias, as with the higher amount of observations, bias is likely lower to
begin with. For the smaller data set, we would expect the converse to be
true. We will favor lower bias estimates over higher variance estimates.
