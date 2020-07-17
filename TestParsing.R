#' Get Data for Individual Rounds
#'
#' @param df The rawData dataframe.
#' @param roundNum The round number you wish to get data for.
#'
#'
#' @return A dataframe that has the data for that round
#'
isolateRound <- function(df, roundNum) {

  roundStr <- paste0("ROUND ", roundNum, "$")

  roundData <- df %>% filter(str_detect(.$data, roundStr))

  return(roundData)
}


rawDataRating <- readr::read_delim(file = 'IAPS_B.iaps_stream_rating.01.nosessionid.2020-07-16T10_27_19', delim = "\t",
                          col_names = c("time", "data"))

dataRating <- rawDataRating %>% select(data)

rawDataChoice <- readr::read_delim(file = 'IAPS_B.iaps_choice.01.nosessionid.2020-07-16T11_18_33', delim = "\t",
                                   col_names = c("time", "data"))

dataChoice <- rawDataChoice %>% select(data)


numRounds <- 242
vec <- seq(1,242)

sortedRounds <- map_df(vec, ~isolateRound(df = data, roundNum = .x), .id = "round")


startingTime <- data %>% filter(str_detect(.$Data, "starting game"))
endTime <- data %>% filter(str_detect(.$Data, "FINAL RATING SELECTION"))

roundIndicators <- data %>% filter(str_detect(.$data, "ROUND 1")) %>% separate(col = data, into = c("message", "round"), sep = ",")



r1 <- dataRating %>% isolateRound(1)
r1 %>% filter(str_detect(.$data, "Question")) %>% mutate(question = case_when(str_detect(.$data, "NEGATIVE") ~ "negative",
                                                                                 TRUE ~ "positive"))











question <- dataRating %>%
  filter(str_detect(.$data, "Question")) %>%
  transmute(question = if_else(condition = str_detect(.$data, "NEGATIVE"),
                            true = "negative",
                            false = "positive"))



picID <- dataRating %>%
  filter(str_detect(.$data, "Image")) %>%
  filter(!str_detect(.$data, "ROUND 0")) %>%
  separate(col = data,
           into = c("show", "image", "hyphen", "id", "round", "roundNumber"),
           extra = "drop",
           sep = " ",
           remove = FALSE) %>%
  select("id") %>%
  transmute(id = str_remove(.$id, ".jpg,"))


submitRating <- dataRating %>%
filter(str_detect(.$data, "SUBMIT")) %>%
filter(!str_detect(.$data, "ROUND 0")) %>%
  separate(col = data,
           into = c("submit", "rating", "wordValue", "value"),
           extra = "drop",
           sep = " ",
           remove = FALSE) %>%
  select(value) %>%
  mutate(value = str_remove(.$value, ","))


ratings <- bind_cols(picID, question, submitRating) %>%
  mutate(round = row_number())







