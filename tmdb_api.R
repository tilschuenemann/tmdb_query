# TODO check automatically if libs are installed

library("httr")
library("jsonlite")
library("dplyr")
library("tidyr")
library("stringr")
library("tcltk")

# TODO write and maybe cache in config
api_key <-""
setwd(tclvalue(tkchooseDirectory()))

# check and read existing cache -------------------------------------------

existing_cache <- "movie_id_matching.csv"

if(file.exists(existing_cache)==T){
  existing_cache <- read.csv2(existing_cache)
} else {
  print("no existing cache found in this working directory")
  print("building a new one")
}

# directories to title and year df ----------------------------------------

# get directories
movie_list <- as.data.frame(list.dirs(full.names = F,recursive = F)) 
names(movie_list) <- "cache_movie"

# get paths 
dir_list <- as.data.frame(list.dirs(recursive = F))
names(dir_list)<-"cache_dirname"

# my movies go like this: moviename (year) (subtitles)
movie_list <- movie_list %>%
  mutate(w1 = word(cache_movie,-1), # extracting word by word, 
         w2 = word(cache_movie,-2), # there is probably a regex for this
         toBeRemoved = str_length(paste0(w1,w2))+2, 
         cache_title = substr(cache_movie,0, str_length(cache_movie)-toBeRemoved), # get title
         cache_year = as.numeric(gsub("\\(|\\)","",w2)),
         cache_audsub = gsub("\\(|\\)","",w1)) %>%
  select(-w1,-w2,-toBeRemoved) %>%
  mutate(cache_urltitle = gsub("[ ]", "+", cache_title)) # prepare for url

# add directory list as col
movie_list <- cbind(movie_list,dir_list)

remove(dir_list)

# querying tmdb api -------------------------------------------------------

tmdb_list <- NULL
movies_not_found <- NULL
j <- nrow(movie_list)

for (i in 1:j) {

  # query with title and year of movie
  title = movie_list$cache_urltitle[i]
  year = movie_list$cache_year[i]
  url1 = paste0("https://api.themoviedb.org/3/search/movie?api_key=",api_key,"&query=",title,"&year=",year)
  url2 = paste0("https://api.themoviedb.org/3/search/movie?api_key=",api_key,"&query=",title)
  
  unpack_query <- function(url_arg){
    
    res = GET(url_arg)
    
    # convert from raw characters to json to dataframe
    single_call <- res$content %>%
      rawToChar() %>%
      fromJSON()
    
    single_call<-unlist(single_call[2],recursive = F, use.names = T) %>%
      do.call(cbind,.) %>%
      as.data.frame()
    
    # TODO clean names, artefact from unlist above
    names(single_call)<-str_replace(names(single_call), pattern = "results.", replacement = "tmdb_")
    
    return(single_call)
  } 
  
  single_call <- unpack_query(url1)
  
  # check without year if there are no results
  if(nrow(single_call)==0){
    single_call <- unpack_query(url2)
  }
  
  # get case for pushing result to final list
  n <- nrow(single_call)
 
  if(n==0 | is.null(n)){
    print(paste0("No results found for: ",movie_list$cache_title[i],", year: ",year, ", id: ",i))
    
    movies_not_found <- rbind(movie_list[i,c("cache_dirname", "cache_title")],movies_not_found)
    next
    
  } else if(n>= 1){
    
    # get most popular if there are more results
    if(n > 1){
      single_call <- single_call %>%
        slice(which.max(tmdb_popularity))
    }
    
    # add references for comparison later
    single_call$cache_title = unlist(movie_list$cache_title[i])
    single_call$cache_dirname = movie_list$cache_dirname[i]
    
    # TODO unnest everything except genre ids
    single_call <-  unnest(single_call,cols = c(names(single_call)))
    
    tmdb_list <- rbind(single_call, tmdb_list)
  } #end if
  
}

remove(n, i, j, url1, url2, title, year)

# write new cache to disk
new_cache <- merge(tmdb_list, movies_not_found, by = c("cache_dirname", "cache_title"),all = T)
write.csv2(new_cache, "tmdb_db.csv", row.names = F, )
