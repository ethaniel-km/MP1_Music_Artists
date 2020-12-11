#----------------------------------------------------------------------------------------------------------------------------------------------
#   Spotify Analytics - Are Male artist really more prominant than female artist today (in Canada)?
#   Web Scraping | Analytics 
#   Source -  https://developer.spotify.com/
#             https://www.r-bloggers.com/2016/09/my-new-r-package/
#             https://open.spotify.com/playlist/37i9dQZEVXbKj23U1GF4IR?si=I7ScjZhlS9CmfyK4QlIHdw
#   By: Ethan Mah
#   BTMA 431 - Mini Project
#   November 17, 2020
#----------------------------------------------------------------------------------------------------------------------------------------------
#  Libraries used for this project:

# Package to install and access Spotify API.    Reference - https://www.r-bloggers.com/2016/09/my-new-r-package/
# I used the above link to get a better understanding of the functions of the Rspotify package and how to properly install it.
install.packages("Rspotify")
library(Rspotify)
install.packages("XML")
library(XML)
library(httr)
library(dplyr)

#----------------------------------------------------------------------------------------------------------------------------------------------
#   Accessing Spotify's public API:     

#   In order to access many of the functions of the Spotify API, you need an authenticated token. 
#     This requires the user to by signing up through Spotify's Developers page: https://developer.spotify.com/
#     The ID's below are the codes developed for my own personal account/project.
#     These ID's will be required every time we want to access Spotify's data, therefore we will save this information to the global environment.

#   If you are rerunning this code on a new computer, Spotify will ask you for authorization, in which you can accept.

# keys format <- spotifyOAuth("app_id","client_id","client_secret")
# app_id = Spotify Analytics
# client_id = aae9693ee49e40ffac07111ad70f1a1c
# client_secret = e4be99cc38734a2c9038f61f4352af98
# http://localhost:1410/ in Redirect URIs

keys <- spotifyOAuth("Spotify Analytics", "aae9693ee49e40ffac07111ad70f1a1c", "e4be99cc38734a2c9038f61f4352af98")
#----------------------------------------------------------------------------------------------------------------------------------------------
#   Data Collection / Cleaning Data:

#   In order to answer my question, I need to retrieve Spotify's Top 50 list for Canada: https://open.spotify.com/playlist/37i9dQZEVXbKj23U1GF4IR?si=ETViLMwPRsC7s_mzb3PJ8A   
#     Instead of copying every single entry of this playlist to a new spreadsheet, we can use the Spotify API to retrieve a full dataset
#     of that exact playlist.

Canada.Top_Nov15 <- getPlaylistSongs("spotify","37i9dQZEVXbKj23U1GF4IR",token = keys)

View(Canada.Top_Nov15)

#   Select only the columns we want to use for the analysis portion
Canada.Top_Nov15 <- getPlaylistSongs("spotify","37i9dQZEVXbKj23U1GF4IR",token = keys) %>% 
  select(tracks, artist)
View(Canada.Top_Nov15)

#   One field that we did not get that was illustrated in Spotify's Canada Top 50 playlist is the daily plays column so we need to use 
#     another way to extract that.

# Scrape data from website:
url <- 'https://spotifycharts.com/regional/ca/daily/latest'

# get the html file for the above url
get_obj <- GET(url)

# Parse the html file into an object that R can read
SC.parse <- htmlParse(get_obj)

# I want to extract the main table concerning the Top 50 most played songs in Canada
page_tables <- readHTMLTable(SC.parse,stringsAsfactors = FALSE)

# Since there is only 1 table, i want to save the first entry to 'daily.plays'
Daily.Plays <- page_tables[[1]]

# Obviously, this method doesn't give all the information we want, for example artist name. 
#   Therefore, this kind of shows it was important to use alternative methods to extract data.
View(Daily.Plays)

# In order to bind the data we just got to the one we obtained through spotify's API, we must match the number of instances between the two datasets.
#   Therefore the only data we want from the data we got is the first 50 entries of the 5th column (daily plays). 
Daily.Plays <- Daily.Plays[,5]
Daily.Plays <- head(Daily.Plays,50)

# Now, I want to add the daily.plays field to the Top50 list
Canada.Top_Nov15 <- cbind(Canada.Top_Nov15,Daily.Plays)


# Spotify updates their top 50 list daily, as the position of each artist on the list changes based on their daily streams.
#   For example, this list may completely change in terms of order in a weeks time. If that were to happen, any addition fields that I would add manually may
#   be incorrect and may disrupt the analyst portion when data is re-run.

#   So, in order to keep the data consistent, I will be saving the dataframe we have now as a csv file and use it for the rest of the 
#   Mini project when adding the additional fields. I will include the created csv file with the package sent to marking.

#write.csv(Canada.Top_Nov15, 'Canada50_Nov15.csv')


#*** 
# IMPORTANT: In order to keep data consistent from when I did it, please use the csv file that I have provided in my submission if you
#                 are going to rerun the code for marking.
#                 I created this csv file on November 15 and I'm not sure by the time of grading if the list will change. If it were
#                 to change, then the additional fields that I am adding may be incorrect to the data collected in the first part.

# load data and remove the unnecessary field
Canada.Top_Nov15 <- read.csv("Canada50_Nov15.csv", stringsAsFactors = FALSE)

View(Canada.Top_Nov15)

Canada.Top_Nov15 <- Canada.Top_Nov15[,-1]

# In order to answer the question, I need to manually add a gender and genre field to the data.
gender <- c('F','F','M','M','M','M','M','M','F','M',
            'M','M','M','M','F','F','M','M','F','M',
            'M','M','F','M','F','M','M','M','M','M',
            'M','M','M','M','M','M','M','M','M','M',
            'M','Mix','M','M','M','M','M','M','M','F')

genre <- c('Pop','Pop','Hip Hop','Hip Hop','Hip Hop','Pop','Hip Hop','Pop','Hip Hop','Hip Hop',
           'Reggaeton','Hip Hop','Hip Hop','Hip Hop','Alternative','Pop','Hip Hop','Pop','Pop','Pop',
           'Hip Hop','Hip Hop', 'Pop','Pop','Pop','Pop','Pop','Hip Hop','Pop','Pop',
           'Hip Hop','Hip Hop','Hip Hop','Country','Blue-eyed soul','Hip Hop','Hip Hop','Hip Hop','Hip Hop','Hip Hop',
           'Pop','Folk Rock','Country','Hip Hop','EDM','Hip Hop','Hip Hop','Hip Hop','Hip Hop','Pop')

# Add this field to the master dataframe
Canada.Top_Nov15 <- cbind(Canada.Top_Nov15,gender,genre)

# We also want to make sure that daily plays is classified as numbers.
str(Canada.Top_Nov15)

# In order to do this, we need to take out the commas that between the values
Canada.Top_Nov15$Daily.Plays <- as.numeric(gsub(',','',Canada.Top_Nov15$Daily.Plays))

# Completed table
View(Canada.Top_Nov15)

#----------------------------------------------------------------------------------------------------------------------------------------------
#   Data Analytics

# So I want to investigate 2 questions:

#   1) Since there are more males in the music industry than females, does that mean males artist receives on avg more plays per day compared to female artist?     
#         If that was the case, wouldn't that mean males would be more dominant in the top 50 list?
#         In other words, are females less represented / known because there are more males artist in the industry?
#         Reference:https://www.cbc.ca/music/women-are-still-missing-in-the-music-industry-updated-2020-study-reveals-1.5436415#:~:text=In%20the%20January%202020%20version,of%20producers%20across%20500%20songs.

#   2) Is Hip Hop really the most popular genre (in terms of Canada)?    
#         Reference:https://hypebeast.com/2020/7/hip-hop-most-popular-genre-2020-streaming-sales-data-info


# To answer the Q1, we want to perform a hypothesis test using the t-test:
#   Null: The avg daily play between Males and Females are equal.
#   Alt: The avg daily plays of Male artist is greater than female artist.
CT_female <- Canada.Top_Nov15 %>% filter(gender == 'F')
CT_male <- Canada.Top_Nov15 %>% filter(gender == 'M')

test.1 <- t.test(CT_male$Daily.Plays, CT_female$Daily.Plays, alternative = 'greater')
test.1$p.value

# Even though females make up less than 20% of the Top 50 list in Canada (9 females / 40 male / 1 group), based on the t.test, we fail to reject the null
#   This illustrates that females receives on average similar plays on their music to their male counterpart. 


# Now to Q2, we will again perform a hypothesis test using the t-test:
#   Null: Hip Hop music and other genres of music have on average the same number of daily plays.
#   Alt: Hip Hop music has on average more daily plays than all the other genres.
CT_HipHop <- Canada.Top_Nov15 %>% filter(genre == 'Hip Hop')
CT_other <- Canada.Top_Nov15 %>% filter(genre != 'Hip Hop')

test.2 <- t.test(CT_HipHop$Daily.Plays, CT_other$Daily.Plays, alternative = 'greater')
test.2$p.value

# Since the p-value is greater than 0.05, we fail to reject the null hypothesis and we can say that Hip Hop music does not receive more daily
#   plays on average to their music compared to other genres.

#----------------------------------------------------------------------------------------------------------------------------------------------
