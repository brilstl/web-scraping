## packages ----
library(rvest)
library(RSelenium)
library(tidyverse)

## open server via docker ----

remDr <- remoteDriver(
        remoteServerAddr = "192.168.99.100",
        port = 4445L,
        browserName = "chrome"
)

remDr$open()

## direct to website ----

remDr$navigate("https://www.meesterbaan.nl/onderwijs/vacatures.aspx?id_sector=1&id_regio=2&id_functie=-1")

## Check how many pages to scrape ----

page <- read_html(remDr$getPageSource()[[1]]) %>%
        html_nodes("#ctl00_plhControl_lblMessage") %>% 
        html_children() %>% 
        html_text() %>% 
        as.numeric() %>%
        tibble() %>% 
        mutate(. = ceiling(./10) * 10) %/%
        10

## create tibble with page id's to scroll through ----

scroller <- tibble(x = c(1:page$.)) %>%
        transmute(
                arrow =
                        case_when(x %in% c(seq(6, page$., 5)) ~ ">",
                                  TRUE ~ NA_character_),
                arrow = coalesce(arrow, as.character(x))
        )

## create function which can scroll through javascript post content ----

scrol_ler <- function(scroll){
        
        Sys.sleep(3)
        
        webElems <- remDr$findElements(using = "css", ".PageNumbers")
        
        resHeaders <- webElems %>% 
                map_chr(
                        ~ unlist(.x$getElementText())
                        )

        ##PUT a if statement here saying if scroll is not in resheader..
        ## message 'page is non existent' else 'moving on'
        
                
        webElem <- webElems[[which(resHeaders == scroll)]]
        
        webElem$clickElement()
        
        Sys.sleep(3)
        
                        tibble(url = xml2::read_html(remDr$getPageSource()[[1]]) %>%
                                       rvest::html_nodes(".vacancyText") %>%
                                       html_nodes('h2') %>% 
                                       html_nodes('a') %>% 
                                       html_attr("href")) %>% 
                                mutate(id = str_extract(url,'(?<!\\d)[0-9]{6}(?!\\d)')) 
        
        
}

## scrape hrefs from first page ----

hrefs <- tibble(url = xml2::read_html(remDr$getPageSource()[[1]]) %>%
                        rvest::html_nodes(".vacancyText") %>%
                        html_nodes('h2') %>% 
                        html_nodes('a') %>% 
                        html_attr("href")) %>% 
        mutate(id = str_extract(url,'(?<!\\d)[0-9]{6}(?!\\d)'))


## map through the other pages and scrape the href from those pages ----


hrefs2 <- scroller %>% 
        mutate(hrefs = map(scroll, possibly(scrol_ler, NA)))

## Bind the two tibbles together ----

hrefs_all <- hrefs2 %>% 
        unnest() %>% 
        select(-scroll) %>% 
        bind_rows(hrefs)

rm(hrefs, hrefs2, page, scroller)

        

remDr$screenshot(display = TRUE) #This will take a screenshot and display it in the RStudio viewer



remDr$close()
remDr$closeServer()

as.character(save[1,1])

go_school <- function(url) {

remDr$navigate(glue::glue("{url}"))
        
        Sys.sleep(
                sample(3:6, 1)
        )

school_name = read_html(remDr$getPageSource()[[1]]) %>% 
        html_nodes('#ctl00_plhControl_lblSchool') %>% 
        html_text()

street_name = read_html(remDr$getPageSource()[[1]]) %>% 
        html_nodes('#ctl00_plhControl_lblStraatnaam') %>% 
        html_text()

postal = read_html(remDr$getPageSource()[[1]]) %>% 
        html_nodes('#ctl00_plhControl_lblPostalcode') %>% 
        html_text()

city = read_html(remDr$getPageSource()[[1]]) %>% 
        html_nodes('#ctl00_plhControl_lblPlaats') %>% 
        html_text()

edu_type = read_html(remDr$getPageSource()[[1]]) %>% 
        html_nodes('#ctl00_plhControl_lblTypeOnderwijs') %>% 
        html_text()

entry_date = read_html(remDr$getPageSource()[[1]]) %>% 
        html_nodes('#ctl00_plhControl_lblPlaatsing2') %>% 
        html_text()

closing_date = read_html(remDr$getPageSource()[[1]]) %>% 
        html_nodes('#ctl00_plhControl_lblSluitingsDatum') %>% 
        html_text()

starting_date <- read_html(remDr$getPageSource()[[1]]) %>% 
        html_nodes('#ctl00_plhControl_lblMetIngang') %>% 
        html_text()

contract <- read_html(remDr$getPageSource()[[1]]) %>% 
        html_nodes('#ctl00_plhControl_lblDienstverband') %>% 
        html_text()

description <- read_html(remDr$getPageSource()[[1]]) %>% 
        html_nodes("#ctl00_plhControl_lblOmschrijving") %>% 
        html_text()

tibble(school_name = if(length(school_name) == 0) NA_character_ else school_name,
       street_name = if(length(street_name) == 0) NA_character_ else street_name,
       postal = if(length(postal) == 0) NA_character_ else postal,
       city = if(length(city) == 0) NA_character_ else city,
       edu_type = if(length(edu_type) == 0) NA_character_ else edu_type,
       entry_date = if(length(entry_date) == 0) NA_character_ else entry_date,
       closing_date = if(length(closing_date) == 0) NA_character_ else closing_date,
       starting_date = if(length(starting_date) == 0) NA_character_ else starting_date,
       contract = if(length(contract) == 0) NA_character_ else contract,
       description = if(length(description) == 0) NA_character_ else description)

}

vacancies <- hrefs_all %>% 
        mutate(info = map(url, go_school))

webElems <- remDr$findElements(using = "css", ".PageNumbers")

resHeaders <- unlist(lapply(webElems, function(x) {x$getElementText()}))

webElem <- webElems[[which(resHeaders == ">")]]

webElem$clickElement()

go_school(gogo$url[1,1])

gogo$url[7]
