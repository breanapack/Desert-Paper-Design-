---
title: "Desert Paper Design"
author: "Breana Pack"
date: "April 02, 2023"
execute:
  keep-md: true
  df-print: paged
  warning: false
format:
  html:
    code-fold: true
    code-line-numbers: true

---


::: {.cell}

```{.r .cell-code}
library(pander)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(mosaic)
library(ggridges)

library(readr)
desert_paper_sales <- read_csv("desert_paper_sales.csv")
#View(desert_paper_sales)
```
:::


The data below has been gathered from Desert Paper Design, a business that does custom stickers for companies and individuals. This analysis is going over the data collected from sales since the company started in May of 2022. One thing to make note of this data does not include the money this company has earned from design work, only the money that has been earned from selling and creating stickers. 

*This data was collected on March 23*

## How is the company doing monthly?


::: {.cell}

```{.r .cell-code}
monthly <- desert_paper_sales%>%
  mutate(month = m)%>%
  group_by(month, year)%>%
  summarize(total_price = sum(total_price),
            total_cost= sum(cost),
            total_revenue = sum(revenue),
            avg_revenue = mean(revenue),
            average_per_hour = mean(per_hour),
            total_hours_worked = sum(time_hours),
            num_sales = n())


monthly%>%
  arrange(total_revenue)%>%
  pander()
```

::: {.cell-output-display}
---------------------------------------------------------------------------
   month     year   total_price   total_cost   total_revenue   avg_revenue 
----------- ------ ------------- ------------ --------------- -------------
  January    2023       115         11.37          103.6          103.6    

  August     2022      170.5         4.4           166.1          33.22    

   March     2023       214          6.4           207.6          51.9     

    May      2022      284.8        33.97          250.8          27.86    

  October    2022       315         10.51          304.5          43.5     

 December    2022       430          55.5          374.5          187.2    

   July      2022       400         10.07          389.9          43.33    

 November    2022      452.5         21.9          430.6          107.7    

   June      2022       668           28            640           49.23    

  Febuary    2023       736          62.3          673.7          168.4    

 September   2022      718.2        32.17          686.1          31.19    
---------------------------------------------------------------------------

Table: Table continues below

 
---------------------------------------------------
 average_per_hour   total_hours_worked   num_sales 
------------------ -------------------- -----------
      51.82                 2                1     

      33.22                 5                5     

      34.65                 5                4     

      27.86                 9                9     

      24.34                 10               7     

      58.92                 7                2     

      25.59                12.5              9     

      37.62                 9                4     

       23.1                 18              13     

      54.86                 11               4     

      17.56                 27              22     
---------------------------------------------------
:::
:::

In this first graph we are looking at how the business performed each month in total revenue. This is taking out the cost of shipping, paper, and other various expenses. From this graph we can see that September was the highest month, and January was the lowest month. However February is the second highest month wich is the second to last month that this data was collected (March being the last month).




::: {.cell}

```{.r .cell-code}
ggplot(monthly, aes(y= reorder(month, (total_revenue)), x = total_revenue))+
  geom_col(fill='lightblue')+
  theme_bw()+
  geom_text(aes(x = total_revenue, y = month,
                label=paste0('$', round(total_revenue, 2)),
            nudge_x = 55))+
  guides(fill=FALSE)+
  labs(title= 'Total Revune Per Month', x='Total Reveune', y='Month')
```

::: {.cell-output-display}
![](final_project_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::
:::

::: {.cell}

```{.r .cell-code}
current <- monthly%>%
  filter(year == 2023)

current%>%
  arrange(total_revenue)%>%
  pander()
```

::: {.cell-output-display}
-------------------------------------------------------------------------
  month    year   total_price   total_cost   total_revenue   avg_revenue 
--------- ------ ------------- ------------ --------------- -------------
 January   2023       115         11.37          103.6          103.6    

  March    2023       214          6.4           207.6          51.9     

 Febuary   2023       736          62.3          673.7          168.4    
-------------------------------------------------------------------------

Table: Table continues below

 
---------------------------------------------------
 average_per_hour   total_hours_worked   num_sales 
------------------ -------------------- -----------
      51.82                 2                1     

      34.65                 5                4     

      54.86                 11               4     
---------------------------------------------------
:::
:::


Bellow we are looking at only the three months from 2023 that have data collected so far. *This data was collected on March 23*
For the year of 2023 the best month so far has been Febuary.

::: {.cell}

```{.r .cell-code}
ggplot(current, aes(y= reorder(month, (total_revenue)), x = total_revenue))+
  geom_col(fill='lightblue')+
  theme_bw()+
  geom_text(aes(x = total_revenue, y = month,
                label=paste0('$', round(total_revenue, 2)),
            nudge_x = 55))+
  guides(fill=FALSE)+
  labs(title= 'Total Revune Per Month', x='Total Reveune', y='Month')
```

::: {.cell-output-display}
![](final_project_files/figure-html/unnamed-chunk-5-1.png){width=672}
:::
:::


## How does the time it takes to complete the project effect revenue per hour?

The client wanted to know that if she is making more or less per hour the bigger the project that she receives. For this we took the revenue she earned and divided it by how many hours it took her to complete to get how much she earned per hour during the project. From the graph below we can see that she makes less per hour when its a small project. However the larger the project the more she is making per hour (thus the more she is making overall). She is making the most when the project takes her anywhere between 3-4 hours.


::: {.cell}

```{.r .cell-code}
ggplot(desert_paper_sales, aes(y = as.factor(time_hours), x = per_hour, fill = as.factor(time_hours)))+
  geom_density_ridges() +
    theme_ridges()+
  labs(title='How Much Made Per Hour by How Many Hours Workds', x='Ammount Made Per Hour',y='Ammount of hours it took', fill = 'Hours')
```

::: {.cell-output-display}
![](final_project_files/figure-html/unnamed-chunk-6-1.png){width=672}
:::
:::


## Are markets worth it to go to?

The client often attends markets throughout the year to sell individual stickers, meet new business, and to market her brand. However she was wondering if it was worth the amount of time and effort that was being put into the market. She stated that each market takes roughly 1 hour of prep time at home to get stickers ready, 30 minutes of set up at the event, each event is between 2-3 hours, and 30 minutes of take down. For a total of 4-5 hours of work.

A little more about each market

1- At a country dancing event a week after she launched the business

2- This was at an event at Love Olive Co

3- This was an event that she put together and hosted at a park in St. Anthony

4- This was a market at La Jolla apartments 

5- This was a winter Christmas market in Idaho Falls, Idaho



::: {.cell}

```{.r .cell-code}
markets <- desert_paper_sales%>%
  filter(business_name  == 'none')%>%
  filter(where != 'instagram')%>%
  group_by(where)%>%
  summarise(total_price = sum(total_price),
            total_cost= sum(cost),
            total_revenue = sum(revenue),
            avg_revenue = mean(revenue),
            num_sales = n())

markets%>%
  arrange(total_revenue)%>%
  pander()
```

::: {.cell-output-display}
------------------------------------------------------------------------------
  where    total_price   total_cost   total_revenue   avg_revenue   num_sales 
--------- ------------- ------------ --------------- ------------- -----------
 market5       15           0.15          14.85          4.95           3     

 market1      24.5          0.27          24.23          4.038          6     

 market2       39           0.39          38.61          4.826          8     

 market3       47           0.48          46.52          15.51          3     

 market4       60           0.69          59.31          5.392         11     
------------------------------------------------------------------------------
:::
:::


This first graph we are just looking at the total revenue per market, this is not including the amount of time that has been put into the market or the profit that has been made from new clients at each market.



::: {.cell}

```{.r .cell-code}
ggplot(markets, aes(y = where, x = total_revenue))+
  geom_col(fill= 'lightblue') +
    theme_ridges()+
  labs(title='Total Revenue By Market', x='Total Revenue',y='Market', fill = 'Hours')+
  guides(fill=FALSE)+
  theme_bw()+
  geom_text(aes(x = total_revenue, y = where,
                label=paste0('$', round(total_revenue, 2)),
            nudge_x = 55))
```

::: {.cell-output-display}
![](final_project_files/figure-html/unnamed-chunk-8-1.png){width=672}
:::
:::


In this next graph we are looking at how much she made per hour at these markets. For consistency we estimated she spent 4.5 hours working for each market. From the graph we can see that her first and fith market were not worth it for her business as she made less than $7.50 which is the current minnamum wage in her town (Rexburg Idaho). However her best performin markets were her third and fourth. Moving forward it would be crutial to think about the target audience that will be attending each market.


::: {.cell}

```{.r .cell-code}
markets_hourly <- markets %>%
  mutate(hourly = (total_revenue / 4.5))

ggplot(markets_hourly, aes(y = where, x = hourly))+
  geom_col(fill= 'lightblue') +
    theme_ridges()+
  labs(title='Total Revenue Per Hour', x='Total Revenue',y='Market', fill = 'Hours')+
  guides(fill=FALSE)+
  theme_bw()+
  geom_text(aes(x = hourly, y = where,
                label=paste0('$', round(hourly, 2)),
            nudge_x = 55))
```

::: {.cell-output-display}
![](final_project_files/figure-html/unnamed-chunk-9-1.png){width=672}
:::
:::




## Who are the top clients?
The client was interested to see who her top clients were over time so she could see where she needs to be putting in effort. This will also help her to know what type of business bring in the most profit for her, so then she can reach out to businesses like them to increase her clientele.



::: {.cell}

```{.r .cell-code}
fizz_bizz <- desert_paper_sales%>%
  filter(business_name =='Fizz Bizz')

companies <- desert_paper_sales%>%
  group_by(business_name)%>%
  summarise(total_price = sum(total_price),
            total_cost= sum(cost),
            total_revenue = sum(revenue),
            avg_revenue = mean(revenue),
            num_sales = n())
top_5_companies <- companies %>%
  top_n(5, total_price) %>%
  arrange(desc(total_price))

top_5_companies%>%
  arrange(desc(total_price))%>%
  pander()
```

::: {.cell-output-display}
-------------------------------------------------------------------
     business_name        total_price   total_cost   total_revenue 
------------------------ ------------- ------------ ---------------
       Fizz Bizz             1266         100.4          1166      

          Reki                461          24.2          436.8     

 Cookies By Kara Bakery       384          9.73          374.3     

         Dawne               312.2         14.7          297.6     

     Smiling Lemons          272.5         13.1          259.4     
-------------------------------------------------------------------

Table: Table continues below

 
-------------------------
 avg_revenue   num_sales 
------------- -----------
    233.1          5     

    218.4          2     

    46.78          8     

    297.6          1     

    259.4          1     
-------------------------
:::
:::


In this graph we are looking at those top 5 companies. We can see that her first company on the list is Fizz Bizz with a total of $1266 that she has earned over 6 orders. 


A little more about these companies 

Fizz Bizz- A local drink shop, she does their monthly sticker that they hand out to customers

Reki- This is a company based on the east coast that does Reki wellness. They have had two separate orders from her.

Cookies By Kara- This is a local cookie business, has placed 8 different orders.

Dawne- This is a wedding planning company based out of Arizona

Smiling Lemons- This is a shae butter and chapstick company based out of California


::: {.cell}

```{.r .cell-code}
ggplot(top_5_companies, 
       aes(y=reorder(business_name,(total_price)),
                     x=total_price))+
  geom_col(fill = 'lightblue')+
  theme_bw()+
  guides(fill=FALSE)+
  geom_text(aes(x = total_price, y = business_name,
                label=paste0('$', round(total_price, 2)),
            nudge_x = -20))+
  labs(title= 'Top 5 Companies', y='Business Name', x='Total Ammount Made')
```

::: {.cell-output-display}
![](final_project_files/figure-html/unnamed-chunk-11-1.png){width=672}
:::
:::



## Who has reordered the most?

Finally we are looking at which of the businesses have had the most orders.

In this graph the top three companies are companies that sell food (drinks, cookies, charcutorie boards). These are the types of companies that she should be reaching out to if she is looking for business that are going to be repeatable and continue to come back to her. This graph is also only showing companies who have had more than one order.


::: {.cell}

```{.r .cell-code}
companies1 <- desert_paper_sales%>%
  group_by(business_name)%>%
  summarise(total_price = sum(total_price),
            total_cost= sum(cost),
            total_revenue = sum(revenue),
            avg_revenue = mean(revenue),
            num_sales = n())%>%
  filter(num_sales > 1)%>%
  filter(business_name != 'none')


ggplot(companies1, 
       aes(y=reorder(business_name,(num_sales)),
                     x=num_sales))+
  geom_col(fill = 'lightblue')+
  theme_bw()+
  guides(fill=FALSE)+
  geom_text(aes(x = num_sales, y = business_name,
                label=(round(num_sales, 2)),
            nudge_x = -20))+
  labs(title= 'Which Companies Have the Most Orders', y='Business Name', x='Total Orders')
```

::: {.cell-output-display}
![](final_project_files/figure-html/unnamed-chunk-12-1.png){width=672}
:::
:::
