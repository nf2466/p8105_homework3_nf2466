Homework 2
================
Nancy Fang (nf2466)
2020-10-09

### Problem 1

``` r
data("instacart")
```

This dataset contains 1384617 rows and 15 columns.

Observations are the level of items in orders by user. There are user /
order variables – user ID, order ID, order day, and order hour. There
are also item variables – name, aisle, department, and some numeric
codes.

How many aisles, and which are most items from?

``` r
instacart %>% 
    count(aisle) %>% 
    arrange(desc(n))
```

    ## # A tibble: 134 x 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # ... with 124 more rows

Let’s make a plot

``` r
instacart %>% 
    count(aisle) %>% 
    filter(n > 10000) %>% 
    mutate(
        aisle = factor(aisle),
        aisle = fct_reorder(aisle, n)
    ) %>% 
    ggplot(aes(x = aisle, y = n)) + 
    geom_point() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

<img src="p8105_homework3_nf2466_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

Let’s make a table\!\!

``` r
instacart %>% 
    filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>% 
    group_by(aisle) %>% 
    count(product_name) %>% 
    mutate(rank = min_rank(desc(n))) %>% 
    filter(rank < 4) %>% 
    arrange(aisle, rank) %>% 
    knitr::kable()
```

| aisle                      | product\_name                                 |    n | rank |
| :------------------------- | :-------------------------------------------- | ---: | ---: |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |

Apples vs ice cream..

``` r
instacart %>% 
    filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>% 
    group_by(product_name, order_dow) %>% 
    summarize(mean_hour = mean(order_hour_of_day)) %>% 
    pivot_wider(
        names_from = order_dow,
        values_from = mean_hour
    )
```

    ## `summarise()` regrouping output by 'product_name' (override with `.groups` argument)

    ## # A tibble: 2 x 8
    ## # Groups:   product_name [2]
    ##   product_name       `0`   `1`   `2`   `3`   `4`   `5`   `6`
    ##   <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Coffee Ice Cream  13.8  14.3  15.4  15.3  15.2  12.3  13.8
    ## 2 Pink Lady Apples  13.4  11.4  11.7  14.2  11.6  12.8  11.9

### Problem 2

Load the dataset and tidy

``` r
accel_df=
  read_csv("./data/accel_data.csv") %>%
  janitor::clean_names()%>%
  pivot_longer(activity_1:activity_1440, names_to = "activity_minute", names_prefix = "activity_", values_to = "activity_count")%>%
  mutate(activity_minute = as.numeric(activity_minute),
         day_id=as.factor(day_id),
         day=as.factor(day),
  day = factor(day, levels = str_c(c("Mon", "Tues", "Wednes", "Thurs", "Fri", "Satur", "Sun"),"day"))
  )%>%
  arrange(week, day)%>%
  mutate(
  day_type=recode(day, "Saturday" = "weekend", "Sunday" = "weekend", 
                  "Monday" = "weekday", "Tuesday" = "weekday", "Wednesday" = "weekday", 
                  "Thursday" = "weekday","Friday" = "weekday")
  )
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   day = col_character()
    ## )

    ## See spec(...) for full column specifications.

Part b: Now, aggregate total day activity count.

``` r
accel_df%>%
  group_by(week,day)%>%
  summarize(count_total=sum(activity_count))%>%
  pivot_wider(
    names_from= day,
    values_from = count_total
  )%>%
  knitr::kable()
```

    ## `summarise()` regrouping output by 'week' (override with `.groups` argument)

| week |    Monday |  Tuesday | Wednesday | Thursday |   Friday | Saturday | Sunday |
| ---: | --------: | -------: | --------: | -------: | -------: | -------: | -----: |
|    1 |  78828.07 | 307094.2 |    340115 | 355923.6 | 480542.6 |   376254 | 631105 |
|    2 | 295431.00 | 423245.0 |    440962 | 474048.0 | 568839.0 |   607175 | 422018 |
|    3 | 685910.00 | 381507.0 |    468869 | 371230.0 | 467420.0 |   382928 | 467052 |
|    4 | 409450.00 | 319568.0 |    434460 | 340291.0 | 154049.0 |     1440 | 260617 |
|    5 | 389080.00 | 367824.0 |    445366 | 549658.0 | 620860.0 |     1440 | 138421 |

There doesn’t appear to be a strong pattern in terms of the total
activity count over the 5 weeks. Overall, the activity total appears
highest on Mondays and lowest on Saturdays.

Part c

``` r
 accel_df_p =
  accel_df %>%
  ggplot(aes(x = activity_minute, y = activity_count, color = day)) + 
   geom_smooth(se=FALSE) +
  labs(
    title = "Daily activity by day",
    x = "Minutes in a day (24 hours)",
    y = "Activity count"
  )

ggsave("accel_df_p.pdf", accel_df_p, width = 8, height = 5)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

``` r
accel_df_p
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

<img src="p8105_homework3_nf2466_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" />

It appears that there is an expected pattern of low activity during
sleeping hours and more activity during the middle of the day (between
minute 500-1000). We also see that on average, there is more activity in
the morning/afternoon on Sunday and more activity in the evening time on
Fridays.

-----

\#\#\#Problem 3

Download the NOAA dataset from P8105

``` r
library(p8105.datasets)
data("ny_noaa")
```

In this dataset, there are 2595176 rows and 7 columns. There are 747
distinct locations in New York state. The variables also include the
date of data collection, between 1981-01-01 and 2010-12-31,
precipitation, snowfall, snow depth, max temp and min temp in Celsius.
There are 145838 missing values for precipitation, 381221 missing values
for snowfall, 591786 missing values for snow depth, 1134358 missing
values for max temp and 1134420 missing values for min temp.

Data cleaning: Do some data cleaning. Create separate variables for
year, month, and day. Ensure observations for temperature,
precipitation, and snowfall are given in reasonable units. For snowfall,
what are the most commonly observed values? Why?

``` r
ny_noaa%>%
  janitor::clean_names()%>%
  separate(date, into = c('year','month','day')) %>%
  mutate(month = month.abb[as.factor(month)]) %>%
  mutate_at(vars(year, day, tmax, tmin), as.numeric)
```

    ## # A tibble: 2,595,176 x 9
    ##    id           year month   day  prcp  snow  snwd  tmax  tmin
    ##    <chr>       <dbl> <chr> <dbl> <int> <int> <int> <dbl> <dbl>
    ##  1 US1NYAB0001  2007 Nov       1    NA    NA    NA    NA    NA
    ##  2 US1NYAB0001  2007 Nov       2    NA    NA    NA    NA    NA
    ##  3 US1NYAB0001  2007 Nov       3    NA    NA    NA    NA    NA
    ##  4 US1NYAB0001  2007 Nov       4    NA    NA    NA    NA    NA
    ##  5 US1NYAB0001  2007 Nov       5    NA    NA    NA    NA    NA
    ##  6 US1NYAB0001  2007 Nov       6    NA    NA    NA    NA    NA
    ##  7 US1NYAB0001  2007 Nov       7    NA    NA    NA    NA    NA
    ##  8 US1NYAB0001  2007 Nov       8    NA    NA    NA    NA    NA
    ##  9 US1NYAB0001  2007 Nov       9    NA    NA    NA    NA    NA
    ## 10 US1NYAB0001  2007 Nov      10    NA    NA    NA    NA    NA
    ## # ... with 2,595,166 more rows
