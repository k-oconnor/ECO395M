    library(tidyverse)
    library(dplyr)
    library(mosaic)
    library(ggplot2)
    library(foreach)
    library(xtable)
    library(pdp)
    library(kableExtra)
    library(randomForest)
    library(rsample)
    library(arules)
    library(arulesViz)

# Problem 1: PCA and Clustering

## General Analysis and Data Loading

    vin = read.csv("wine.csv")

    #Encoding 
    vin$color = ifelse(vin$color == "red", 1, 0)
    kable(cor(vin, method = "pearson"))

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
fixed.acidity
</th>
<th style="text-align:right;">
volatile.acidity
</th>
<th style="text-align:right;">
citric.acid
</th>
<th style="text-align:right;">
residual.sugar
</th>
<th style="text-align:right;">
chlorides
</th>
<th style="text-align:right;">
free.sulfur.dioxide
</th>
<th style="text-align:right;">
total.sulfur.dioxide
</th>
<th style="text-align:right;">
density
</th>
<th style="text-align:right;">
pH
</th>
<th style="text-align:right;">
sulphates
</th>
<th style="text-align:right;">
alcohol
</th>
<th style="text-align:right;">
quality
</th>
<th style="text-align:right;">
color
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
fixed.acidity
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.2190083
</td>
<td style="text-align:right;">
0.3244357
</td>
<td style="text-align:right;">
-0.1119813
</td>
<td style="text-align:right;">
0.2981948
</td>
<td style="text-align:right;">
-0.2827354
</td>
<td style="text-align:right;">
-0.3290539
</td>
<td style="text-align:right;">
0.4589100
</td>
<td style="text-align:right;">
-0.2527005
</td>
<td style="text-align:right;">
0.2995677
</td>
<td style="text-align:right;">
-0.0954515
</td>
<td style="text-align:right;">
-0.0767432
</td>
<td style="text-align:right;">
0.4867398
</td>
</tr>
<tr>
<td style="text-align:left;">
volatile.acidity
</td>
<td style="text-align:right;">
0.2190083
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.3779813
</td>
<td style="text-align:right;">
-0.1960112
</td>
<td style="text-align:right;">
0.3771243
</td>
<td style="text-align:right;">
-0.3525573
</td>
<td style="text-align:right;">
-0.4144762
</td>
<td style="text-align:right;">
0.2712956
</td>
<td style="text-align:right;">
0.2614544
</td>
<td style="text-align:right;">
0.2259837
</td>
<td style="text-align:right;">
-0.0376404
</td>
<td style="text-align:right;">
-0.2656995
</td>
<td style="text-align:right;">
0.6530356
</td>
</tr>
<tr>
<td style="text-align:left;">
citric.acid
</td>
<td style="text-align:right;">
0.3244357
</td>
<td style="text-align:right;">
-0.3779813
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.1424512
</td>
<td style="text-align:right;">
0.0389980
</td>
<td style="text-align:right;">
0.1331258
</td>
<td style="text-align:right;">
0.1952420
</td>
<td style="text-align:right;">
0.0961539
</td>
<td style="text-align:right;">
-0.3298082
</td>
<td style="text-align:right;">
0.0561973
</td>
<td style="text-align:right;">
-0.0104935
</td>
<td style="text-align:right;">
0.0855317
</td>
<td style="text-align:right;">
-0.1873965
</td>
</tr>
<tr>
<td style="text-align:left;">
residual.sugar
</td>
<td style="text-align:right;">
-0.1119813
</td>
<td style="text-align:right;">
-0.1960112
</td>
<td style="text-align:right;">
0.1424512
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.1289405
</td>
<td style="text-align:right;">
0.4028706
</td>
<td style="text-align:right;">
0.4954816
</td>
<td style="text-align:right;">
0.5525170
</td>
<td style="text-align:right;">
-0.2673198
</td>
<td style="text-align:right;">
-0.1859274
</td>
<td style="text-align:right;">
-0.3594148
</td>
<td style="text-align:right;">
-0.0369805
</td>
<td style="text-align:right;">
-0.3488210
</td>
</tr>
<tr>
<td style="text-align:left;">
chlorides
</td>
<td style="text-align:right;">
0.2981948
</td>
<td style="text-align:right;">
0.3771243
</td>
<td style="text-align:right;">
0.0389980
</td>
<td style="text-align:right;">
-0.1289405
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.1950448
</td>
<td style="text-align:right;">
-0.2796304
</td>
<td style="text-align:right;">
0.3626147
</td>
<td style="text-align:right;">
0.0447080
</td>
<td style="text-align:right;">
0.3955933
</td>
<td style="text-align:right;">
-0.2569156
</td>
<td style="text-align:right;">
-0.2006655
</td>
<td style="text-align:right;">
0.5126782
</td>
</tr>
<tr>
<td style="text-align:left;">
free.sulfur.dioxide
</td>
<td style="text-align:right;">
-0.2827354
</td>
<td style="text-align:right;">
-0.3525573
</td>
<td style="text-align:right;">
0.1331258
</td>
<td style="text-align:right;">
0.4028706
</td>
<td style="text-align:right;">
-0.1950448
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.7209341
</td>
<td style="text-align:right;">
0.0257168
</td>
<td style="text-align:right;">
-0.1458539
</td>
<td style="text-align:right;">
-0.1884572
</td>
<td style="text-align:right;">
-0.1798384
</td>
<td style="text-align:right;">
0.0554631
</td>
<td style="text-align:right;">
-0.4716437
</td>
</tr>
<tr>
<td style="text-align:left;">
total.sulfur.dioxide
</td>
<td style="text-align:right;">
-0.3290539
</td>
<td style="text-align:right;">
-0.4144762
</td>
<td style="text-align:right;">
0.1952420
</td>
<td style="text-align:right;">
0.4954816
</td>
<td style="text-align:right;">
-0.2796304
</td>
<td style="text-align:right;">
0.7209341
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.0323945
</td>
<td style="text-align:right;">
-0.2384131
</td>
<td style="text-align:right;">
-0.2757268
</td>
<td style="text-align:right;">
-0.2657396
</td>
<td style="text-align:right;">
-0.0413855
</td>
<td style="text-align:right;">
-0.7003572
</td>
</tr>
<tr>
<td style="text-align:left;">
density
</td>
<td style="text-align:right;">
0.4589100
</td>
<td style="text-align:right;">
0.2712956
</td>
<td style="text-align:right;">
0.0961539
</td>
<td style="text-align:right;">
0.5525170
</td>
<td style="text-align:right;">
0.3626147
</td>
<td style="text-align:right;">
0.0257168
</td>
<td style="text-align:right;">
0.0323945
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.0116861
</td>
<td style="text-align:right;">
0.2594785
</td>
<td style="text-align:right;">
-0.6867454
</td>
<td style="text-align:right;">
-0.3058579
</td>
<td style="text-align:right;">
0.3906453
</td>
</tr>
<tr>
<td style="text-align:left;">
pH
</td>
<td style="text-align:right;">
-0.2527005
</td>
<td style="text-align:right;">
0.2614544
</td>
<td style="text-align:right;">
-0.3298082
</td>
<td style="text-align:right;">
-0.2673198
</td>
<td style="text-align:right;">
0.0447080
</td>
<td style="text-align:right;">
-0.1458539
</td>
<td style="text-align:right;">
-0.2384131
</td>
<td style="text-align:right;">
0.0116861
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.1921234
</td>
<td style="text-align:right;">
0.1212485
</td>
<td style="text-align:right;">
0.0195057
</td>
<td style="text-align:right;">
0.3291287
</td>
</tr>
<tr>
<td style="text-align:left;">
sulphates
</td>
<td style="text-align:right;">
0.2995677
</td>
<td style="text-align:right;">
0.2259837
</td>
<td style="text-align:right;">
0.0561973
</td>
<td style="text-align:right;">
-0.1859274
</td>
<td style="text-align:right;">
0.3955933
</td>
<td style="text-align:right;">
-0.1884572
</td>
<td style="text-align:right;">
-0.2757268
</td>
<td style="text-align:right;">
0.2594785
</td>
<td style="text-align:right;">
0.1921234
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.0030292
</td>
<td style="text-align:right;">
0.0384854
</td>
<td style="text-align:right;">
0.4872180
</td>
</tr>
<tr>
<td style="text-align:left;">
alcohol
</td>
<td style="text-align:right;">
-0.0954515
</td>
<td style="text-align:right;">
-0.0376404
</td>
<td style="text-align:right;">
-0.0104935
</td>
<td style="text-align:right;">
-0.3594148
</td>
<td style="text-align:right;">
-0.2569156
</td>
<td style="text-align:right;">
-0.1798384
</td>
<td style="text-align:right;">
-0.2657396
</td>
<td style="text-align:right;">
-0.6867454
</td>
<td style="text-align:right;">
0.1212485
</td>
<td style="text-align:right;">
-0.0030292
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.4443185
</td>
<td style="text-align:right;">
-0.0329696
</td>
</tr>
<tr>
<td style="text-align:left;">
quality
</td>
<td style="text-align:right;">
-0.0767432
</td>
<td style="text-align:right;">
-0.2656995
</td>
<td style="text-align:right;">
0.0855317
</td>
<td style="text-align:right;">
-0.0369805
</td>
<td style="text-align:right;">
-0.2006655
</td>
<td style="text-align:right;">
0.0554631
</td>
<td style="text-align:right;">
-0.0413855
</td>
<td style="text-align:right;">
-0.3058579
</td>
<td style="text-align:right;">
0.0195057
</td>
<td style="text-align:right;">
0.0384854
</td>
<td style="text-align:right;">
0.4443185
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
-0.1193233
</td>
</tr>
<tr>
<td style="text-align:left;">
color
</td>
<td style="text-align:right;">
0.4867398
</td>
<td style="text-align:right;">
0.6530356
</td>
<td style="text-align:right;">
-0.1873965
</td>
<td style="text-align:right;">
-0.3488210
</td>
<td style="text-align:right;">
0.5126782
</td>
<td style="text-align:right;">
-0.4716437
</td>
<td style="text-align:right;">
-0.7003572
</td>
<td style="text-align:right;">
0.3906453
</td>
<td style="text-align:right;">
0.3291287
</td>
<td style="text-align:right;">
0.4872180
</td>
<td style="text-align:right;">
-0.0329696
</td>
<td style="text-align:right;">
-0.1193233
</td>
<td style="text-align:right;">
1.0000000
</td>
</tr>
</tbody>
</table>

From the correlation table, we can see which factors have strong
relationships between color and quality. Color is strongly correlated
with volatile acidity, and strongly negatively correlated with total
sulfur dioxide.

For PCA, We believe we can heavily reduce the number of features and
still keep most of the variation, as a few features seem to be
responsible for much of the variation.

## PCA

    ## Importance of first k=3 (out of 11) components:
    ##                            PC1      PC2     PC3
    ## Standard deviation     58.0698 11.98513 4.13082
    ## Proportion of Variance  0.9538  0.04063 0.00483
    ## Cumulative Proportion   0.9538  0.99439 0.99921

As we see in the above figure, our 3 principle components preserve over
99% of the variation. Next, we will see how each each factor is
represented by our principle components.

<table>
<thead>
<tr>
<th style="text-align:left;">
Factor
</th>
<th style="text-align:right;">
PC1
</th>
<th style="text-align:right;">
PC2
</th>
<th style="text-align:right;">
PC3
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
fixed.acidity
</td>
<td style="text-align:right;">
-0.0074080
</td>
<td style="text-align:right;">
-0.0053656
</td>
<td style="text-align:right;">
0.0237980
</td>
</tr>
<tr>
<td style="text-align:left;">
volatile.acidity
</td>
<td style="text-align:right;">
-0.0011843
</td>
<td style="text-align:right;">
-0.0007845
</td>
<td style="text-align:right;">
0.0008841
</td>
</tr>
<tr>
<td style="text-align:left;">
citric.acid
</td>
<td style="text-align:right;">
0.0004869
</td>
<td style="text-align:right;">
-0.0002479
</td>
<td style="text-align:right;">
0.0019287
</td>
</tr>
<tr>
<td style="text-align:left;">
residual.sugar
</td>
<td style="text-align:right;">
0.0410197
</td>
<td style="text-align:right;">
0.0186364
</td>
<td style="text-align:right;">
0.9952741
</td>
</tr>
<tr>
<td style="text-align:left;">
chlorides
</td>
<td style="text-align:right;">
-0.0001682
</td>
<td style="text-align:right;">
0.0000673
</td>
<td style="text-align:right;">
0.0001730
</td>
</tr>
<tr>
<td style="text-align:left;">
free.sulfur.dioxide
</td>
<td style="text-align:right;">
0.2304818
</td>
<td style="text-align:right;">
0.9726583
</td>
<td style="text-align:right;">
-0.0272149
</td>
</tr>
<tr>
<td style="text-align:left;">
total.sulfur.dioxide
</td>
<td style="text-align:right;">
0.9721668
</td>
<td style="text-align:right;">
-0.2314097
</td>
<td style="text-align:right;">
-0.0358290
</td>
</tr>
<tr>
<td style="text-align:left;">
density
</td>
<td style="text-align:right;">
0.0000018
</td>
<td style="text-align:right;">
0.0000013
</td>
<td style="text-align:right;">
0.0004604
</td>
</tr>
<tr>
<td style="text-align:left;">
pH
</td>
<td style="text-align:right;">
-0.0006555
</td>
<td style="text-align:right;">
0.0006480
</td>
<td style="text-align:right;">
-0.0069116
</td>
</tr>
<tr>
<td style="text-align:left;">
sulphates
</td>
<td style="text-align:right;">
-0.0007043
</td>
<td style="text-align:right;">
0.0003464
</td>
<td style="text-align:right;">
-0.0019353
</td>
</tr>
<tr>
<td style="text-align:left;">
alcohol
</td>
<td style="text-align:right;">
-0.0054517
</td>
<td style="text-align:right;">
0.0028502
</td>
<td style="text-align:right;">
-0.0823558
</td>
</tr>
</tbody>
</table>

Evidently, PC1 represents sulfur dioxide in the wine in question. PC2 is
accounting mostly for free sulfur dioxide. PC3 accounts for residual
sugar. Next, we will see if color and quality naturally appear based on
our principle components.

## PCA Visualization

![](Homework4_files/figure-markdown_strict/Problem%201d-1.png)![](Homework4_files/figure-markdown_strict/Problem%201d-2.png)![](Homework4_files/figure-markdown_strict/Problem%201d-3.png)![](Homework4_files/figure-markdown_strict/Problem%201d-4.png)

We see the strongest distinction in color when comparing PC1 and PC2,
which makes sense intuitively, as those two components preserve the
greatest variation. When we plot our main principle components against
quality, we see no discernable clustering.When we try to graph the
principle components against quality, we see absolutely no discernible
trend.

## Hierarchical Clustering: Quality

![](Homework4_files/figure-markdown_strict/Problem%201e-1.png)

To see if quality or color naturally becomes evident from hierarchical
clustering, we have picked to graph clusters of factors which are
strongly related to quality.It would appear that there are distinct
categories that appear, and sensibly, as sulfur dioxide increases, one
would expect the taste to get worse. Seeing clusters that appear along
this scale is promising. Next, we will try to modify our clustering
approach to see if we can get reliable clusters for color.

## Hierarchical Clustering: Color

![](Homework4_files/figure-markdown_strict/Problem%201f-1.png)

When selecting clusters factors for color, and setting k=4, it appears
that two distinct clusters appear, which could very well be color. As
total sulfur dioxide is strongly negatively correlated with color,
seeing the clustering happening on this axis is very promising.

It seems that both can handle color, but neither particularly
demonstrate variation in quality. However, it would appear that PCA will
perform better for predicting color.To that end, to solidify this
assertion, we will run some supervised learning on our principle
components.

    ## [1] 95.77

With a TPR of almost 95%, our principle components perform well in
predicting.

# Problem 2: Market Segmentation

## Loading Data and PCA

    ## Importance of first k=6 (out of 36) components:
    ##                           PC1    PC2    PC3    PC4     PC5     PC6
    ## Standard deviation     5.2035 4.3366 3.8224 3.7751 3.60916 3.21543
    ## Proportion of Variance 0.1924 0.1336 0.1038 0.1013 0.09255 0.07346
    ## Cumulative Proportion  0.1924 0.3260 0.4298 0.5311 0.62363 0.69709

## PCA Visualization

<table>
<thead>
<tr>
<th style="text-align:left;">
Factor
</th>
<th style="text-align:right;">
PC1
</th>
<th style="text-align:right;">
PC2
</th>
<th style="text-align:right;">
PC3
</th>
<th style="text-align:right;">
PC4
</th>
<th style="text-align:right;">
PC5
</th>
<th style="text-align:right;">
PC6
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
health\_nutrition
</td>
<td style="text-align:right;">
0.8084217
</td>
<td style="text-align:right;">
-0.2731887
</td>
<td style="text-align:right;">
0.0357547
</td>
<td style="text-align:right;">
0.0749270
</td>
<td style="text-align:right;">
-0.2239792
</td>
<td style="text-align:right;">
-0.0444602
</td>
</tr>
<tr>
<td style="text-align:left;">
personal\_fitness
</td>
<td style="text-align:right;">
0.3873704
</td>
<td style="text-align:right;">
-0.1057094
</td>
<td style="text-align:right;">
0.0201947
</td>
<td style="text-align:right;">
0.0367269
</td>
<td style="text-align:right;">
-0.1061482
</td>
<td style="text-align:right;">
0.0014327
</td>
</tr>
<tr>
<td style="text-align:left;">
cooking
</td>
<td style="text-align:right;">
0.3404963
</td>
<td style="text-align:right;">
0.2701636
</td>
<td style="text-align:right;">
-0.1201330
</td>
<td style="text-align:right;">
-0.2504953
</td>
<td style="text-align:right;">
0.6488193
</td>
<td style="text-align:right;">
-0.1207419
</td>
</tr>
<tr>
<td style="text-align:left;">
outdoors
</td>
<td style="text-align:right;">
0.1446150
</td>
<td style="text-align:right;">
-0.0362823
</td>
<td style="text-align:right;">
0.0382914
</td>
<td style="text-align:right;">
0.0163605
</td>
<td style="text-align:right;">
-0.0230079
</td>
<td style="text-align:right;">
0.0056388
</td>
</tr>
<tr>
<td style="text-align:left;">
photo\_sharing
</td>
<td style="text-align:right;">
0.1319231
</td>
<td style="text-align:right;">
0.4784651
</td>
<td style="text-align:right;">
-0.1649944
</td>
<td style="text-align:right;">
0.0405932
</td>
<td style="text-align:right;">
0.0253015
</td>
<td style="text-align:right;">
-0.0335667
</td>
</tr>
<tr>
<td style="text-align:left;">
fashion
</td>
<td style="text-align:right;">
0.0968248
</td>
<td style="text-align:right;">
0.1739817
</td>
<td style="text-align:right;">
-0.0598368
</td>
<td style="text-align:right;">
-0.1179886
</td>
<td style="text-align:right;">
0.3021205
</td>
<td style="text-align:right;">
-0.0268016
</td>
</tr>
<tr>
<td style="text-align:left;">
food
</td>
<td style="text-align:right;">
0.0952024
</td>
<td style="text-align:right;">
0.0109789
</td>
<td style="text-align:right;">
0.1226658
</td>
<td style="text-align:right;">
-0.0120525
</td>
<td style="text-align:right;">
0.0121891
</td>
<td style="text-align:right;">
0.3760726
</td>
</tr>
<tr>
<td style="text-align:left;">
chatter
</td>
<td style="text-align:right;">
0.0837792
</td>
<td style="text-align:right;">
0.6174357
</td>
<td style="text-align:right;">
-0.2098548
</td>
<td style="text-align:right;">
0.2822340
</td>
<td style="text-align:right;">
-0.4304128
</td>
<td style="text-align:right;">
0.0268010
</td>
</tr>
<tr>
<td style="text-align:left;">
beauty
</td>
<td style="text-align:right;">
0.0600013
</td>
<td style="text-align:right;">
0.1107477
</td>
<td style="text-align:right;">
-0.0334058
</td>
<td style="text-align:right;">
-0.0736145
</td>
<td style="text-align:right;">
0.2136049
</td>
<td style="text-align:right;">
0.0283324
</td>
</tr>
<tr>
<td style="text-align:left;">
shopping
</td>
<td style="text-align:right;">
0.0524583
</td>
<td style="text-align:right;">
0.2497367
</td>
<td style="text-align:right;">
-0.0833536
</td>
<td style="text-align:right;">
0.0923377
</td>
<td style="text-align:right;">
-0.1157340
</td>
<td style="text-align:right;">
0.0098964
</td>
</tr>
<tr>
<td style="text-align:left;">
dating
</td>
<td style="text-align:right;">
0.0397760
</td>
<td style="text-align:right;">
0.0635112
</td>
<td style="text-align:right;">
0.0321203
</td>
<td style="text-align:right;">
0.0293135
</td>
<td style="text-align:right;">
-0.0301508
</td>
<td style="text-align:right;">
0.0142423
</td>
</tr>
<tr>
<td style="text-align:left;">
eco
</td>
<td style="text-align:right;">
0.0391899
</td>
<td style="text-align:right;">
0.0244800
</td>
<td style="text-align:right;">
0.0102188
</td>
<td style="text-align:right;">
0.0101501
</td>
<td style="text-align:right;">
-0.0158616
</td>
<td style="text-align:right;">
0.0196514
</td>
</tr>
<tr>
<td style="text-align:left;">
parenting
</td>
<td style="text-align:right;">
0.0292934
</td>
<td style="text-align:right;">
0.0402374
</td>
<td style="text-align:right;">
0.0868603
</td>
<td style="text-align:right;">
-0.0047901
</td>
<td style="text-align:right;">
0.0430655
</td>
<td style="text-align:right;">
0.3486896
</td>
</tr>
<tr>
<td style="text-align:left;">
religion
</td>
<td style="text-align:right;">
0.0278591
</td>
<td style="text-align:right;">
0.0374217
</td>
<td style="text-align:right;">
0.1194893
</td>
<td style="text-align:right;">
-0.0176992
</td>
<td style="text-align:right;">
0.0637558
</td>
<td style="text-align:right;">
0.4819924
</td>
</tr>
<tr>
<td style="text-align:left;">
uncategorized
</td>
<td style="text-align:right;">
0.0252406
</td>
<td style="text-align:right;">
0.0263781
</td>
<td style="text-align:right;">
0.0002664
</td>
<td style="text-align:right;">
-0.0193753
</td>
<td style="text-align:right;">
0.0128544
</td>
<td style="text-align:right;">
-0.0042478
</td>
</tr>
<tr>
<td style="text-align:left;">
music
</td>
<td style="text-align:right;">
0.0247824
</td>
<td style="text-align:right;">
0.0449086
</td>
<td style="text-align:right;">
0.0069128
</td>
<td style="text-align:right;">
-0.0333090
</td>
<td style="text-align:right;">
0.0127111
</td>
<td style="text-align:right;">
0.0104636
</td>
</tr>
<tr>
<td style="text-align:left;">
school
</td>
<td style="text-align:right;">
0.0217604
</td>
<td style="text-align:right;">
0.0555640
</td>
<td style="text-align:right;">
0.0387065
</td>
<td style="text-align:right;">
0.0049080
</td>
<td style="text-align:right;">
0.0303429
</td>
<td style="text-align:right;">
0.2199870
</td>
</tr>
<tr>
<td style="text-align:left;">
sports\_fandom
</td>
<td style="text-align:right;">
0.0214999
</td>
<td style="text-align:right;">
0.0598114
</td>
<td style="text-align:right;">
0.1540162
</td>
<td style="text-align:right;">
-0.0021114
</td>
<td style="text-align:right;">
0.0528511
</td>
<td style="text-align:right;">
0.5532707
</td>
</tr>
<tr>
<td style="text-align:left;">
crafts
</td>
<td style="text-align:right;">
0.0213890
</td>
<td style="text-align:right;">
0.0295497
</td>
<td style="text-align:right;">
0.0239538
</td>
<td style="text-align:right;">
0.0000525
</td>
<td style="text-align:right;">
-0.0012024
</td>
<td style="text-align:right;">
0.0594159
</td>
</tr>
<tr>
<td style="text-align:left;">
family
</td>
<td style="text-align:right;">
0.0199947
</td>
<td style="text-align:right;">
0.0433798
</td>
<td style="text-align:right;">
0.0488461
</td>
<td style="text-align:right;">
-0.0131611
</td>
<td style="text-align:right;">
0.0084281
</td>
<td style="text-align:right;">
0.1784207
</td>
</tr>
<tr>
<td style="text-align:left;">
sports\_playing
</td>
<td style="text-align:right;">
0.0194224
</td>
<td style="text-align:right;">
0.0478324
</td>
<td style="text-align:right;">
0.0532964
</td>
<td style="text-align:right;">
-0.1171326
</td>
<td style="text-align:right;">
-0.0421860
</td>
<td style="text-align:right;">
0.0009805
</td>
</tr>
<tr>
<td style="text-align:left;">
art
</td>
<td style="text-align:right;">
0.0193229
</td>
<td style="text-align:right;">
0.0248381
</td>
<td style="text-align:right;">
0.0464870
</td>
<td style="text-align:right;">
-0.0453269
</td>
<td style="text-align:right;">
0.0036605
</td>
<td style="text-align:right;">
0.0178441
</td>
</tr>
<tr>
<td style="text-align:left;">
news
</td>
<td style="text-align:right;">
0.0182144
</td>
<td style="text-align:right;">
0.0636201
</td>
<td style="text-align:right;">
0.3251126
</td>
<td style="text-align:right;">
0.1339427
</td>
<td style="text-align:right;">
0.0655970
</td>
<td style="text-align:right;">
-0.0258962
</td>
</tr>
<tr>
<td style="text-align:left;">
current\_events
</td>
<td style="text-align:right;">
0.0167408
</td>
<td style="text-align:right;">
0.0563347
</td>
<td style="text-align:right;">
0.0115753
</td>
<td style="text-align:right;">
0.0180803
</td>
<td style="text-align:right;">
-0.0111751
</td>
<td style="text-align:right;">
0.0220828
</td>
</tr>
<tr>
<td style="text-align:left;">
computers
</td>
<td style="text-align:right;">
0.0159914
</td>
<td style="text-align:right;">
0.0615433
</td>
<td style="text-align:right;">
0.1623149
</td>
<td style="text-align:right;">
0.0651711
</td>
<td style="text-align:right;">
0.0336142
</td>
<td style="text-align:right;">
-0.0451957
</td>
</tr>
<tr>
<td style="text-align:left;">
online\_gaming
</td>
<td style="text-align:right;">
0.0158193
</td>
<td style="text-align:right;">
0.1210238
</td>
<td style="text-align:right;">
0.2096421
</td>
<td style="text-align:right;">
-0.5521841
</td>
<td style="text-align:right;">
-0.2565729
</td>
<td style="text-align:right;">
-0.0634797
</td>
</tr>
<tr>
<td style="text-align:left;">
home\_and\_garden
</td>
<td style="text-align:right;">
0.0152976
</td>
<td style="text-align:right;">
0.0189057
</td>
<td style="text-align:right;">
0.0147998
</td>
<td style="text-align:right;">
-0.0024461
</td>
<td style="text-align:right;">
0.0005337
</td>
<td style="text-align:right;">
0.0171384
</td>
</tr>
<tr>
<td style="text-align:left;">
business
</td>
<td style="text-align:right;">
0.0132191
</td>
<td style="text-align:right;">
0.0370572
</td>
<td style="text-align:right;">
0.0192119
</td>
<td style="text-align:right;">
0.0103925
</td>
<td style="text-align:right;">
0.0010125
</td>
<td style="text-align:right;">
0.0053111
</td>
</tr>
<tr>
<td style="text-align:left;">
politics
</td>
<td style="text-align:right;">
0.0078588
</td>
<td style="text-align:right;">
0.1707149
</td>
<td style="text-align:right;">
0.6454786
</td>
<td style="text-align:right;">
0.2945516
</td>
<td style="text-align:right;">
0.1016005
</td>
<td style="text-align:right;">
-0.2257825
</td>
</tr>
<tr>
<td style="text-align:left;">
travel
</td>
<td style="text-align:right;">
0.0076946
</td>
<td style="text-align:right;">
0.1026362
</td>
<td style="text-align:right;">
0.4025784
</td>
<td style="text-align:right;">
0.1435321
</td>
<td style="text-align:right;">
0.0745887
</td>
<td style="text-align:right;">
-0.1655085
</td>
</tr>
<tr>
<td style="text-align:left;">
automotive
</td>
<td style="text-align:right;">
0.0063316
</td>
<td style="text-align:right;">
0.0713500
</td>
<td style="text-align:right;">
0.1026727
</td>
<td style="text-align:right;">
0.0430721
</td>
<td style="text-align:right;">
0.0021679
</td>
<td style="text-align:right;">
0.0452230
</td>
</tr>
<tr>
<td style="text-align:left;">
tv\_film
</td>
<td style="text-align:right;">
0.0058427
</td>
<td style="text-align:right;">
0.0327393
</td>
<td style="text-align:right;">
0.0603145
</td>
<td style="text-align:right;">
-0.0539555
</td>
<td style="text-align:right;">
-0.0245127
</td>
<td style="text-align:right;">
0.0139846
</td>
</tr>
<tr>
<td style="text-align:left;">
college\_uni
</td>
<td style="text-align:right;">
0.0058102
</td>
<td style="text-align:right;">
0.1672739
</td>
<td style="text-align:right;">
0.2416773
</td>
<td style="text-align:right;">
-0.6018851
</td>
<td style="text-align:right;">
-0.2825785
</td>
<td style="text-align:right;">
-0.0706580
</td>
</tr>
<tr>
<td style="text-align:left;">
small\_business
</td>
<td style="text-align:right;">
0.0055705
</td>
<td style="text-align:right;">
0.0283978
</td>
<td style="text-align:right;">
0.0159788
</td>
<td style="text-align:right;">
-0.0039587
</td>
<td style="text-align:right;">
0.0001296
</td>
<td style="text-align:right;">
0.0039114
</td>
</tr>
<tr>
<td style="text-align:left;">
spam
</td>
<td style="text-align:right;">
0.0000411
</td>
<td style="text-align:right;">
0.0000104
</td>
<td style="text-align:right;">
0.0004008
</td>
<td style="text-align:right;">
0.0000048
</td>
<td style="text-align:right;">
-0.0001868
</td>
<td style="text-align:right;">
0.0002208
</td>
</tr>
<tr>
<td style="text-align:left;">
adult
</td>
<td style="text-align:right;">
-0.0004332
</td>
<td style="text-align:right;">
0.0024888
</td>
<td style="text-align:right;">
-0.0020797
</td>
<td style="text-align:right;">
-0.0003000
</td>
<td style="text-align:right;">
-0.0032464
</td>
<td style="text-align:right;">
0.0232445
</td>
</tr>
</tbody>
</table>

The results of the principle component reduction offer some very clear
and insightful segmentation. Each principle component clearly represents
the different interests and lifestyles of consumers.

PC1 - The health fanatic: PC1 captures the consumer who is extremely
interested in health, nutrition, and fitness. These individuals love to
cook, and spend time outdoors; living a vibrant and active lifestyle.
Marketing to this segment should be centered around positioning
NutrientH20 as a superior product for re-hydrating and recovering from a
workout.

PC2 - The young female socialite: PC2 captures the consumer who loves to
share photos, travel, and talk with friends. Judging by their interests
in beauty and college/university, they more likely to be a young female.
With high interest in travel, they are more likely to be middle/upper
middle class.Successful marketing in this category will likely involve
partnership with travel and beauty influencers on social media,
positioning NutrientH20 as a lifestyle brand.

PC3 - The young male netizen: PC3 captures the consumer who is extremely
interested in politics and travel. With high interests in online gaming,
computers, and automotive, they are more likely to be males. With higher
interest in college/university, they are likely in their early to late
20s. With high interest in travel, they are more likely to be
middle/upper middle class.While they seem to be highly politically
motivated, the safer marketing strategy will likely be to focus on
gaming. Partnership with popular online games to award free content if
they purchase NutrientH20 would likely be a safe way to reach this
demographic.

PC3 - Unclear: This principle component doesn’t offer much insight. It
captures some of the variation in chatter, and not much else.

PC4 - The single working woman: PC4 captures the consumer who is uses
social media more generally, for talking with friends and family.
Sharing some photos, and mostly for talking. They are mildly politically
motivated. Judging by their interest in beauty, interest in travel, and
lack of interest in parenting they are most likely working professional
women. Marketing successfully in this segment is less straightforward.
They are most interesting in cooking, so we can surmise that flavor is
extremely important to them. It would likely be best to position
NutrientH20 as a great alternative to flavorless and inferior hydration
options for the hard working independent woman.

PC5 - The family oriented sports fan: PC5 captures a very interesting
demographic. This individual is extremely interested in family, sports
fandom, and religion. They are also highly interested in food, and
hence, likely prioritize flavor. Successful marketing in this area could
be done in a few ways. A potentially strong way to market to this
segment could be to have an endorsement deal with a prominent athelete.
Seeing thier sports idols drinking NutrientH20 could have a powerful
suggestive effect. Alternatively, NutrientH20 could be positioned as the
drink of choice that parents and kids alike can enjoy in thier busy
lives.

# Problem 3: Association Rules

## Data Reading and Rule Extraction

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##        0.05    0.1    1 none FALSE            TRUE       5    0.01      1
    ##  maxlen target  ext
    ##       4  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 98 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [88 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [541 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

![](Homework4_files/figure-markdown_strict/Problem%203a-1.png)

From the scatter plot of rules, we see there are numerous association
rules (541). We have chosen to do our analysis at fairly conservative
values for support and confidence (.01) and (.05). We have chosen these
levels as such, as there will likely be a lot of noise in the data, and
want more strict associations.

## Rule Visualization: By Lift

    ##      lhs                        rhs                     support    confidence
    ## [1]  {}                      => {canned beer}           0.07768175 0.07768175
    ## [2]  {}                      => {coffee}                0.05805796 0.05805796
    ## [3]  {}                      => {beef}                  0.05246568 0.05246568
    ## [4]  {}                      => {curd}                  0.05327911 0.05327911
    ## [5]  {}                      => {napkins}               0.05236401 0.05236401
    ## [6]  {}                      => {pork}                  0.05765125 0.05765125
    ## [7]  {}                      => {frankfurter}           0.05897306 0.05897306
    ## [8]  {}                      => {bottled beer}          0.08052872 0.08052872
    ## [9]  {}                      => {brown bread}           0.06487036 0.06487036
    ## [10] {}                      => {margarine}             0.05856634 0.05856634
    ## [11] {}                      => {butter}                0.05541434 0.05541434
    ## [12] {}                      => {newspapers}            0.07981698 0.07981698
    ## [13] {}                      => {domestic eggs}         0.06344687 0.06344687
    ## [14] {}                      => {fruit/vegetable juice} 0.07229283 0.07229283
    ## [15] {}                      => {whipped/sour cream}    0.07168277 0.07168277
    ## [16] {}                      => {pip fruit}             0.07564820 0.07564820
    ## [17] {}                      => {pastry}                0.08896797 0.08896797
    ## [18] {}                      => {citrus fruit}          0.08276563 0.08276563
    ## [19] {}                      => {shopping bags}         0.09852567 0.09852567
    ## [20] {}                      => {sausage}               0.09395018 0.09395018
    ## [21] {}                      => {bottled water}         0.11052364 0.11052364
    ## [22] {}                      => {tropical fruit}        0.10493137 0.10493137
    ## [23] {}                      => {root vegetables}       0.10899847 0.10899847
    ## [24] {}                      => {soda}                  0.17437722 0.17437722
    ## [25] {}                      => {yogurt}                0.13950178 0.13950178
    ## [26] {}                      => {rolls/buns}            0.18393493 0.18393493
    ## [27] {}                      => {other vegetables}      0.19349263 0.19349263
    ## [28] {}                      => {whole milk}            0.25551601 0.25551601
    ## [29] {curd}                  => {whole milk}            0.02613116 0.49045802
    ## [30] {whole milk}            => {curd}                  0.02613116 0.10226821
    ## [31] {brown bread}           => {whole milk}            0.02521607 0.38871473
    ## [32] {whole milk}            => {brown bread}           0.02521607 0.09868683
    ## [33] {butter}                => {whole milk}            0.02755465 0.49724771
    ## [34] {whole milk}            => {butter}                0.02755465 0.10783924
    ## [35] {newspapers}            => {whole milk}            0.02735130 0.34267516
    ## [36] {whole milk}            => {newspapers}            0.02735130 0.10704337
    ## [37] {domestic eggs}         => {whole milk}            0.02999492 0.47275641
    ## [38] {whole milk}            => {domestic eggs}         0.02999492 0.11738957
    ## [39] {fruit/vegetable juice} => {whole milk}            0.02663955 0.36849508
    ## [40] {whole milk}            => {fruit/vegetable juice} 0.02663955 0.10425786
    ## [41] {whipped/sour cream}    => {other vegetables}      0.02887646 0.40283688
    ## [42] {other vegetables}      => {whipped/sour cream}    0.02887646 0.14923805
    ## [43] {whipped/sour cream}    => {whole milk}            0.03223183 0.44964539
    ## [44] {whole milk}            => {whipped/sour cream}    0.03223183 0.12614405
    ## [45] {pip fruit}             => {other vegetables}      0.02613116 0.34543011
    ## [46] {other vegetables}      => {pip fruit}             0.02613116 0.13504992
    ## [47] {pip fruit}             => {whole milk}            0.03009659 0.39784946
    ## [48] {whole milk}            => {pip fruit}             0.03009659 0.11778750
    ## [49] {pastry}                => {whole milk}            0.03324860 0.37371429
    ## [50] {whole milk}            => {pastry}                0.03324860 0.13012336
    ## [51] {citrus fruit}          => {other vegetables}      0.02887646 0.34889435
    ## [52] {other vegetables}      => {citrus fruit}          0.02887646 0.14923805
    ## [53] {citrus fruit}          => {whole milk}            0.03050330 0.36855037
    ## [54] {whole milk}            => {citrus fruit}          0.03050330 0.11937923
    ## [55] {sausage}               => {rolls/buns}            0.03060498 0.32575758
    ## [56] {rolls/buns}            => {sausage}               0.03060498 0.16639027
    ## [57] {sausage}               => {other vegetables}      0.02694459 0.28679654
    ## [58] {other vegetables}      => {sausage}               0.02694459 0.13925381
    ## [59] {sausage}               => {whole milk}            0.02989324 0.31818182
    ## [60] {whole milk}            => {sausage}               0.02989324 0.11699164
    ## [61] {bottled water}         => {soda}                  0.02897814 0.26218951
    ## [62] {soda}                  => {bottled water}         0.02897814 0.16618076
    ## [63] {bottled water}         => {whole milk}            0.03436706 0.31094756
    ## [64] {whole milk}            => {bottled water}         0.03436706 0.13450060
    ## [65] {tropical fruit}        => {yogurt}                0.02928317 0.27906977
    ## [66] {yogurt}                => {tropical fruit}        0.02928317 0.20991254
    ## [67] {tropical fruit}        => {other vegetables}      0.03589222 0.34205426
    ## [68] {other vegetables}      => {tropical fruit}        0.03589222 0.18549658
    ## [69] {tropical fruit}        => {whole milk}            0.04229792 0.40310078
    ## [70] {whole milk}            => {tropical fruit}        0.04229792 0.16553920
    ## [71] {root vegetables}       => {yogurt}                0.02582613 0.23694030
    ## [72] {yogurt}                => {root vegetables}       0.02582613 0.18513120
    ## [73] {root vegetables}       => {other vegetables}      0.04738180 0.43470149
    ## [74] {other vegetables}      => {root vegetables}       0.04738180 0.24487651
    ## [75] {root vegetables}       => {whole milk}            0.04890696 0.44869403
    ## [76] {whole milk}            => {root vegetables}       0.04890696 0.19140470
    ## [77] {soda}                  => {yogurt}                0.02735130 0.15685131
    ## [78] {yogurt}                => {soda}                  0.02735130 0.19606414
    ## [79] {soda}                  => {rolls/buns}            0.03833249 0.21982507
    ## [80] {rolls/buns}            => {soda}                  0.03833249 0.20840243
    ## [81] {soda}                  => {other vegetables}      0.03274021 0.18775510
    ## [82] {other vegetables}      => {soda}                  0.03274021 0.16920652
    ## [83] {soda}                  => {whole milk}            0.04006101 0.22973761
    ## [84] {whole milk}            => {soda}                  0.04006101 0.15678472
    ## [85] {yogurt}                => {rolls/buns}            0.03436706 0.24635569
    ## [86] {rolls/buns}            => {yogurt}                0.03436706 0.18684356
    ## [87] {yogurt}                => {other vegetables}      0.04341637 0.31122449
    ## [88] {other vegetables}      => {yogurt}                0.04341637 0.22438255
    ## [89] {yogurt}                => {whole milk}            0.05602440 0.40160350
    ## [90] {whole milk}            => {yogurt}                0.05602440 0.21925985
    ## [91] {rolls/buns}            => {other vegetables}      0.04260295 0.23161968
    ## [92] {other vegetables}      => {rolls/buns}            0.04260295 0.22017867
    ## [93] {rolls/buns}            => {whole milk}            0.05663447 0.30790492
    ## [94] {whole milk}            => {rolls/buns}            0.05663447 0.22164743
    ## [95] {other vegetables}      => {whole milk}            0.07483477 0.38675775
    ## [96] {whole milk}            => {other vegetables}      0.07483477 0.29287704
    ##      coverage   lift      count
    ## [1]  1.00000000 1.0000000  764 
    ## [2]  1.00000000 1.0000000  571 
    ## [3]  1.00000000 1.0000000  516 
    ## [4]  1.00000000 1.0000000  524 
    ## [5]  1.00000000 1.0000000  515 
    ## [6]  1.00000000 1.0000000  567 
    ## [7]  1.00000000 1.0000000  580 
    ## [8]  1.00000000 1.0000000  792 
    ## [9]  1.00000000 1.0000000  638 
    ## [10] 1.00000000 1.0000000  576 
    ## [11] 1.00000000 1.0000000  545 
    ## [12] 1.00000000 1.0000000  785 
    ## [13] 1.00000000 1.0000000  624 
    ## [14] 1.00000000 1.0000000  711 
    ## [15] 1.00000000 1.0000000  705 
    ## [16] 1.00000000 1.0000000  744 
    ## [17] 1.00000000 1.0000000  875 
    ## [18] 1.00000000 1.0000000  814 
    ## [19] 1.00000000 1.0000000  969 
    ## [20] 1.00000000 1.0000000  924 
    ## [21] 1.00000000 1.0000000 1087 
    ## [22] 1.00000000 1.0000000 1032 
    ## [23] 1.00000000 1.0000000 1072 
    ## [24] 1.00000000 1.0000000 1715 
    ## [25] 1.00000000 1.0000000 1372 
    ## [26] 1.00000000 1.0000000 1809 
    ## [27] 1.00000000 1.0000000 1903 
    ## [28] 1.00000000 1.0000000 2513 
    ## [29] 0.05327911 1.9194805  257 
    ## [30] 0.25551601 1.9194805  257 
    ## [31] 0.06487036 1.5212930  248 
    ## [32] 0.25551601 1.5212930  248 
    ## [33] 0.05541434 1.9460530  271 
    ## [34] 0.25551601 1.9460530  271 
    ## [35] 0.07981698 1.3411103  269 
    ## [36] 0.25551601 1.3411103  269 
    ## [37] 0.06344687 1.8502027  295 
    ## [38] 0.25551601 1.8502027  295 
    ## [39] 0.07229283 1.4421604  262 
    ## [40] 0.25551601 1.4421604  262 
    ## [41] 0.07168277 2.0819237  284 
    ## [42] 0.19349263 2.0819237  284 
    ## [43] 0.07168277 1.7597542  317 
    ## [44] 0.25551601 1.7597542  317 
    ## [45] 0.07564820 1.7852365  257 
    ## [46] 0.19349263 1.7852365  257 
    ## [47] 0.07564820 1.5570432  296 
    ## [48] 0.25551601 1.5570432  296 
    ## [49] 0.08896797 1.4625865  327 
    ## [50] 0.25551601 1.4625865  327 
    ## [51] 0.08276563 1.8031403  284 
    ## [52] 0.19349263 1.8031403  284 
    ## [53] 0.08276563 1.4423768  300 
    ## [54] 0.25551601 1.4423768  300 
    ## [55] 0.09395018 1.7710480  301 
    ## [56] 0.18393493 1.7710480  301 
    ## [57] 0.09395018 1.4822091  265 
    ## [58] 0.19349263 1.4822091  265 
    ## [59] 0.09395018 1.2452520  294 
    ## [60] 0.25551601 1.2452520  294 
    ## [61] 0.11052364 1.5035766  285 
    ## [62] 0.17437722 1.5035766  285 
    ## [63] 0.11052364 1.2169396  338 
    ## [64] 0.25551601 1.2169396  338 
    ## [65] 0.10493137 2.0004746  288 
    ## [66] 0.13950178 2.0004746  288 
    ## [67] 0.10493137 1.7677896  353 
    ## [68] 0.19349263 1.7677896  353 
    ## [69] 0.10493137 1.5775950  416 
    ## [70] 0.25551601 1.5775950  416 
    ## [71] 0.10899847 1.6984751  254 
    ## [72] 0.13950178 1.6984751  254 
    ## [73] 0.10899847 2.2466049  466 
    ## [74] 0.19349263 2.2466049  466 
    ## [75] 0.10899847 1.7560310  481 
    ## [76] 0.25551601 1.7560310  481 
    ## [77] 0.17437722 1.1243678  269 
    ## [78] 0.13950178 1.1243678  269 
    ## [79] 0.17437722 1.1951242  377 
    ## [80] 0.18393493 1.1951242  377 
    ## [81] 0.17437722 0.9703476  322 
    ## [82] 0.19349263 0.9703476  322 
    ## [83] 0.17437722 0.8991124  394 
    ## [84] 0.25551601 0.8991124  394 
    ## [85] 0.13950178 1.3393633  338 
    ## [86] 0.18393493 1.3393633  338 
    ## [87] 0.13950178 1.6084566  427 
    ## [88] 0.19349263 1.6084566  427 
    ## [89] 0.13950178 1.5717351  551 
    ## [90] 0.25551601 1.5717351  551 
    ## [91] 0.18393493 1.1970465  419 
    ## [92] 0.19349263 1.1970465  419 
    ## [93] 0.18393493 1.2050318  557 
    ## [94] 0.25551601 1.2050318  557 
    ## [95] 0.19349263 1.5136341  736 
    ## [96] 0.25551601 1.5136341  736

<table>
<caption>
Top Twenty Rules by Lift
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
lhs
</th>
<th style="text-align:left;">
Var.2
</th>
<th style="text-align:left;">
rhs
</th>
<th style="text-align:right;">
support
</th>
<th style="text-align:right;">
confidence
</th>
<th style="text-align:right;">
coverage
</th>
<th style="text-align:right;">
lift
</th>
<th style="text-align:right;">
count
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\[73\]
</td>
<td style="text-align:left;">
{root vegetables}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:right;">
0.0473818
</td>
<td style="text-align:right;">
0.4347015
</td>
<td style="text-align:right;">
0.1089985
</td>
<td style="text-align:right;">
2.246605
</td>
<td style="text-align:right;">
466
</td>
</tr>
<tr>
<td style="text-align:left;">
\[74\]
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{root vegetables}
</td>
<td style="text-align:right;">
0.0473818
</td>
<td style="text-align:right;">
0.2448765
</td>
<td style="text-align:right;">
0.1934926
</td>
<td style="text-align:right;">
2.246605
</td>
<td style="text-align:right;">
466
</td>
</tr>
<tr>
<td style="text-align:left;">
\[41\]
</td>
<td style="text-align:left;">
{whipped/sour cream}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:right;">
0.0288765
</td>
<td style="text-align:right;">
0.4028369
</td>
<td style="text-align:right;">
0.0716828
</td>
<td style="text-align:right;">
2.081924
</td>
<td style="text-align:right;">
284
</td>
</tr>
<tr>
<td style="text-align:left;">
\[42\]
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whipped/sour cream}
</td>
<td style="text-align:right;">
0.0288765
</td>
<td style="text-align:right;">
0.1492380
</td>
<td style="text-align:right;">
0.1934926
</td>
<td style="text-align:right;">
2.081924
</td>
<td style="text-align:right;">
284
</td>
</tr>
<tr>
<td style="text-align:left;">
\[65\]
</td>
<td style="text-align:left;">
{tropical fruit}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{yogurt}
</td>
<td style="text-align:right;">
0.0292832
</td>
<td style="text-align:right;">
0.2790698
</td>
<td style="text-align:right;">
0.1049314
</td>
<td style="text-align:right;">
2.000475
</td>
<td style="text-align:right;">
288
</td>
</tr>
<tr>
<td style="text-align:left;">
\[66\]
</td>
<td style="text-align:left;">
{yogurt}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{tropical fruit}
</td>
<td style="text-align:right;">
0.0292832
</td>
<td style="text-align:right;">
0.2099125
</td>
<td style="text-align:right;">
0.1395018
</td>
<td style="text-align:right;">
2.000475
</td>
<td style="text-align:right;">
288
</td>
</tr>
<tr>
<td style="text-align:left;">
\[33\]
</td>
<td style="text-align:left;">
{butter}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0275547
</td>
<td style="text-align:right;">
0.4972477
</td>
<td style="text-align:right;">
0.0554143
</td>
<td style="text-align:right;">
1.946053
</td>
<td style="text-align:right;">
271
</td>
</tr>
<tr>
<td style="text-align:left;">
\[34\]
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{butter}
</td>
<td style="text-align:right;">
0.0275547
</td>
<td style="text-align:right;">
0.1078392
</td>
<td style="text-align:right;">
0.2555160
</td>
<td style="text-align:right;">
1.946053
</td>
<td style="text-align:right;">
271
</td>
</tr>
<tr>
<td style="text-align:left;">
\[29\]
</td>
<td style="text-align:left;">
{curd}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0261312
</td>
<td style="text-align:right;">
0.4904580
</td>
<td style="text-align:right;">
0.0532791
</td>
<td style="text-align:right;">
1.919480
</td>
<td style="text-align:right;">
257
</td>
</tr>
<tr>
<td style="text-align:left;">
\[30\]
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{curd}
</td>
<td style="text-align:right;">
0.0261312
</td>
<td style="text-align:right;">
0.1022682
</td>
<td style="text-align:right;">
0.2555160
</td>
<td style="text-align:right;">
1.919480
</td>
<td style="text-align:right;">
257
</td>
</tr>
<tr>
<td style="text-align:left;">
\[37\]
</td>
<td style="text-align:left;">
{domestic eggs}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0299949
</td>
<td style="text-align:right;">
0.4727564
</td>
<td style="text-align:right;">
0.0634469
</td>
<td style="text-align:right;">
1.850203
</td>
<td style="text-align:right;">
295
</td>
</tr>
<tr>
<td style="text-align:left;">
\[38\]
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{domestic eggs}
</td>
<td style="text-align:right;">
0.0299949
</td>
<td style="text-align:right;">
0.1173896
</td>
<td style="text-align:right;">
0.2555160
</td>
<td style="text-align:right;">
1.850203
</td>
<td style="text-align:right;">
295
</td>
</tr>
<tr>
<td style="text-align:left;">
\[51\]
</td>
<td style="text-align:left;">
{citrus fruit}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:right;">
0.0288765
</td>
<td style="text-align:right;">
0.3488943
</td>
<td style="text-align:right;">
0.0827656
</td>
<td style="text-align:right;">
1.803140
</td>
<td style="text-align:right;">
284
</td>
</tr>
<tr>
<td style="text-align:left;">
\[52\]
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{citrus fruit}
</td>
<td style="text-align:right;">
0.0288765
</td>
<td style="text-align:right;">
0.1492380
</td>
<td style="text-align:right;">
0.1934926
</td>
<td style="text-align:right;">
1.803140
</td>
<td style="text-align:right;">
284
</td>
</tr>
<tr>
<td style="text-align:left;">
\[45\]
</td>
<td style="text-align:left;">
{pip fruit}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:right;">
0.0261312
</td>
<td style="text-align:right;">
0.3454301
</td>
<td style="text-align:right;">
0.0756482
</td>
<td style="text-align:right;">
1.785236
</td>
<td style="text-align:right;">
257
</td>
</tr>
<tr>
<td style="text-align:left;">
\[46\]
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{pip fruit}
</td>
<td style="text-align:right;">
0.0261312
</td>
<td style="text-align:right;">
0.1350499
</td>
<td style="text-align:right;">
0.1934926
</td>
<td style="text-align:right;">
1.785236
</td>
<td style="text-align:right;">
257
</td>
</tr>
<tr>
<td style="text-align:left;">
\[55\]
</td>
<td style="text-align:left;">
{sausage}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{rolls/buns}
</td>
<td style="text-align:right;">
0.0306050
</td>
<td style="text-align:right;">
0.3257576
</td>
<td style="text-align:right;">
0.0939502
</td>
<td style="text-align:right;">
1.771048
</td>
<td style="text-align:right;">
301
</td>
</tr>
<tr>
<td style="text-align:left;">
\[56\]
</td>
<td style="text-align:left;">
{rolls/buns}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{sausage}
</td>
<td style="text-align:right;">
0.0306050
</td>
<td style="text-align:right;">
0.1663903
</td>
<td style="text-align:right;">
0.1839349
</td>
<td style="text-align:right;">
1.771048
</td>
<td style="text-align:right;">
301
</td>
</tr>
<tr>
<td style="text-align:left;">
\[67\]
</td>
<td style="text-align:left;">
{tropical fruit}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:right;">
0.0358922
</td>
<td style="text-align:right;">
0.3420543
</td>
<td style="text-align:right;">
0.1049314
</td>
<td style="text-align:right;">
1.767790
</td>
<td style="text-align:right;">
353
</td>
</tr>
<tr>
<td style="text-align:left;">
\[68\]
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{tropical fruit}
</td>
<td style="text-align:right;">
0.0358922
</td>
<td style="text-align:right;">
0.1854966
</td>
<td style="text-align:right;">
0.1934926
</td>
<td style="text-align:right;">
1.767790
</td>
<td style="text-align:right;">
353
</td>
</tr>
</tbody>
</table>

## Rule Visualization: By Confidence

<table>
<caption>
Top Twenty Rules by Confidence
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
lhs
</th>
<th style="text-align:left;">
Var.2
</th>
<th style="text-align:left;">
rhs
</th>
<th style="text-align:right;">
support
</th>
<th style="text-align:right;">
confidence
</th>
<th style="text-align:right;">
coverage
</th>
<th style="text-align:right;">
lift
</th>
<th style="text-align:right;">
count
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\[33\]
</td>
<td style="text-align:left;">
{butter}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0275547
</td>
<td style="text-align:right;">
0.4972477
</td>
<td style="text-align:right;">
0.0554143
</td>
<td style="text-align:right;">
1.946053
</td>
<td style="text-align:right;">
271
</td>
</tr>
<tr>
<td style="text-align:left;">
\[29\]
</td>
<td style="text-align:left;">
{curd}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0261312
</td>
<td style="text-align:right;">
0.4904580
</td>
<td style="text-align:right;">
0.0532791
</td>
<td style="text-align:right;">
1.919480
</td>
<td style="text-align:right;">
257
</td>
</tr>
<tr>
<td style="text-align:left;">
\[37\]
</td>
<td style="text-align:left;">
{domestic eggs}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0299949
</td>
<td style="text-align:right;">
0.4727564
</td>
<td style="text-align:right;">
0.0634469
</td>
<td style="text-align:right;">
1.850203
</td>
<td style="text-align:right;">
295
</td>
</tr>
<tr>
<td style="text-align:left;">
\[43\]
</td>
<td style="text-align:left;">
{whipped/sour cream}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0322318
</td>
<td style="text-align:right;">
0.4496454
</td>
<td style="text-align:right;">
0.0716828
</td>
<td style="text-align:right;">
1.759754
</td>
<td style="text-align:right;">
317
</td>
</tr>
<tr>
<td style="text-align:left;">
\[75\]
</td>
<td style="text-align:left;">
{root vegetables}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0489070
</td>
<td style="text-align:right;">
0.4486940
</td>
<td style="text-align:right;">
0.1089985
</td>
<td style="text-align:right;">
1.756031
</td>
<td style="text-align:right;">
481
</td>
</tr>
<tr>
<td style="text-align:left;">
\[73\]
</td>
<td style="text-align:left;">
{root vegetables}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:right;">
0.0473818
</td>
<td style="text-align:right;">
0.4347015
</td>
<td style="text-align:right;">
0.1089985
</td>
<td style="text-align:right;">
2.246605
</td>
<td style="text-align:right;">
466
</td>
</tr>
<tr>
<td style="text-align:left;">
\[69\]
</td>
<td style="text-align:left;">
{tropical fruit}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0422979
</td>
<td style="text-align:right;">
0.4031008
</td>
<td style="text-align:right;">
0.1049314
</td>
<td style="text-align:right;">
1.577595
</td>
<td style="text-align:right;">
416
</td>
</tr>
<tr>
<td style="text-align:left;">
\[41\]
</td>
<td style="text-align:left;">
{whipped/sour cream}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:right;">
0.0288765
</td>
<td style="text-align:right;">
0.4028369
</td>
<td style="text-align:right;">
0.0716828
</td>
<td style="text-align:right;">
2.081924
</td>
<td style="text-align:right;">
284
</td>
</tr>
<tr>
<td style="text-align:left;">
\[89\]
</td>
<td style="text-align:left;">
{yogurt}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0560244
</td>
<td style="text-align:right;">
0.4016035
</td>
<td style="text-align:right;">
0.1395018
</td>
<td style="text-align:right;">
1.571735
</td>
<td style="text-align:right;">
551
</td>
</tr>
<tr>
<td style="text-align:left;">
\[47\]
</td>
<td style="text-align:left;">
{pip fruit}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0300966
</td>
<td style="text-align:right;">
0.3978495
</td>
<td style="text-align:right;">
0.0756482
</td>
<td style="text-align:right;">
1.557043
</td>
<td style="text-align:right;">
296
</td>
</tr>
<tr>
<td style="text-align:left;">
\[31\]
</td>
<td style="text-align:left;">
{brown bread}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0252161
</td>
<td style="text-align:right;">
0.3887147
</td>
<td style="text-align:right;">
0.0648704
</td>
<td style="text-align:right;">
1.521293
</td>
<td style="text-align:right;">
248
</td>
</tr>
<tr>
<td style="text-align:left;">
\[95\]
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0748348
</td>
<td style="text-align:right;">
0.3867578
</td>
<td style="text-align:right;">
0.1934926
</td>
<td style="text-align:right;">
1.513634
</td>
<td style="text-align:right;">
736
</td>
</tr>
<tr>
<td style="text-align:left;">
\[49\]
</td>
<td style="text-align:left;">
{pastry}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0332486
</td>
<td style="text-align:right;">
0.3737143
</td>
<td style="text-align:right;">
0.0889680
</td>
<td style="text-align:right;">
1.462587
</td>
<td style="text-align:right;">
327
</td>
</tr>
<tr>
<td style="text-align:left;">
\[53\]
</td>
<td style="text-align:left;">
{citrus fruit}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0305033
</td>
<td style="text-align:right;">
0.3685504
</td>
<td style="text-align:right;">
0.0827656
</td>
<td style="text-align:right;">
1.442377
</td>
<td style="text-align:right;">
300
</td>
</tr>
<tr>
<td style="text-align:left;">
\[39\]
</td>
<td style="text-align:left;">
{fruit/vegetable juice}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0266396
</td>
<td style="text-align:right;">
0.3684951
</td>
<td style="text-align:right;">
0.0722928
</td>
<td style="text-align:right;">
1.442160
</td>
<td style="text-align:right;">
262
</td>
</tr>
<tr>
<td style="text-align:left;">
\[51\]
</td>
<td style="text-align:left;">
{citrus fruit}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:right;">
0.0288765
</td>
<td style="text-align:right;">
0.3488943
</td>
<td style="text-align:right;">
0.0827656
</td>
<td style="text-align:right;">
1.803140
</td>
<td style="text-align:right;">
284
</td>
</tr>
<tr>
<td style="text-align:left;">
\[45\]
</td>
<td style="text-align:left;">
{pip fruit}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:right;">
0.0261312
</td>
<td style="text-align:right;">
0.3454301
</td>
<td style="text-align:right;">
0.0756482
</td>
<td style="text-align:right;">
1.785236
</td>
<td style="text-align:right;">
257
</td>
</tr>
<tr>
<td style="text-align:left;">
\[35\]
</td>
<td style="text-align:left;">
{newspapers}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{whole milk}
</td>
<td style="text-align:right;">
0.0273513
</td>
<td style="text-align:right;">
0.3426752
</td>
<td style="text-align:right;">
0.0798170
</td>
<td style="text-align:right;">
1.341110
</td>
<td style="text-align:right;">
269
</td>
</tr>
<tr>
<td style="text-align:left;">
\[67\]
</td>
<td style="text-align:left;">
{tropical fruit}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{other vegetables}
</td>
<td style="text-align:right;">
0.0358922
</td>
<td style="text-align:right;">
0.3420543
</td>
<td style="text-align:right;">
0.1049314
</td>
<td style="text-align:right;">
1.767790
</td>
<td style="text-align:right;">
353
</td>
</tr>
<tr>
<td style="text-align:left;">
\[55\]
</td>
<td style="text-align:left;">
{sausage}
</td>
<td style="text-align:left;">
=&gt;
</td>
<td style="text-align:left;">
{rolls/buns}
</td>
<td style="text-align:right;">
0.0306050
</td>
<td style="text-align:right;">
0.3257576
</td>
<td style="text-align:right;">
0.0939502
</td>
<td style="text-align:right;">
1.771048
</td>
<td style="text-align:right;">
301
</td>
</tr>
</tbody>
</table>

![Table 3](groceries.graphml)

From the rules, we some very intuitive associations. Individuals tend to
purchase dairy items together. When purchasing sausage, people tend to
purchase buns alongside, as those items are extremely commonly consumed
together. Yogurt is commonly purchased alongside fruit to put in the
yogurt, so seeing that association is very sensible. When looking at the
rules with the highest confidence scores, we see the top rules almost
exclusively are associations between the most commonly purchased staple
items (milk, eggs, and bread).These associations are very reasonable.
