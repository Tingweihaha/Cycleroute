Project done for NUS module, Descriptive Analytics with R, in Spring 2022
Done by: Han Qian Qian, Kueh Choon Hwa, Tan Ting Wei, Ammar Bin Hussein Bagharib
Supervised by: Professor Liu Qizhang

This shinyapp uses the `geocode()` function in the R package `ggmap`, as well as other functions that require the Google Places API. In order for this to work on your computer, you will need to create an account on Google Cloud Platform and generate an API key to Google's Geocoding API.

To begin, you will need to open the file keyring.R and assign your key to the variable 'googleKey'. After which, run keyring.R to create the variable 'googlekey' which stores your API key (as seen below):

```{r eval = FALSE}
googleKey <- "your-API-key"
register_google(key = googleKey) #register for ggmap
```

After doing the steps above, open and run the file 'cycleroute_ui_server.R' to generate the app.

Note:
The multiple .csv files present within the folder 'Data Files' were derived through scraping google maps using the googleway package, through an extensive scraping code done in the file "Grp_Proj_Scrape_Code.Rmd". Due to certain limitations present within googleway scraping requests (each scrape only provides 60 rows of data from google maps), we have created an artificial map titled 'Sg_points.csv' - from which a visual representatino can be seen in 'Sg_points.png'. 'Sg_points' consists of 55 arbituary points within Singapore, spaced at approximately 3.3 km apart from one another. 

Through the 'Sg_points.csv' dataset, we've managed to scrape for our various datasets:
- Shopping Malls
- Cafes
- Convenience Stores
- Tourist Attractions
- Parks
- Gas Stations
- Meal Takeaways







