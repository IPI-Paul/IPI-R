# IPI R
 Learning and Developing R Programming
After watching a lot of YouTube videos I turned to the 1st book in 3 I will study. I was most impressed with the speed and smooth handling of delimited file uploads to both SQL Server andd MySql by RODBC. SQL Server flatfile import tasks were often prone to failures and MySql took some 30 minutes to import an incomplete data set as opposed to RODBC taking 30 to 50 seconds at most on a full data set. As usual, I have taken my studies a little bit further and altered the examples given. A good example of this is the Blackjack example https://ipi-international.shinyapps.io/IPI_R-Blackjack/

Unfortunately my course work for the 2nd book studied does not offer the same level of diversion as my previous studies. To gain a real benefit from this course you would be better to get the full course and videos. On reaching the chapter on debug and traceback calls I was able to satisfy my wish to utilise the eval and expression function in R. I am sure more advanced users may know of how to take the expressions presented in the traceback call stack and re-run them using the same original structure. My attempt gets close.

Also inspired by my learning from Roger Peng's book and videos, I attempted a permutation management function in R to compare against a previous attempt in Python. Using the sample() function did not give the right result when supplying a 720 itteration Vectorized function of 'Monday' indicating that my use of sample did not give a unique set within the specified count. Instead the result was achieved using sample and an increased vectorised itteration.
