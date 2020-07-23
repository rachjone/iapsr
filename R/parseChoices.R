
#Create a phase environment
phase <- base::new.env(parent = base::emptyenv())

#' Read Choice Data
#'
#' @param filePath The path to the ratings data file. Should be in single quotes.
#'
#' @return The raw data downloaded from the task. It is a data frame with two columns, time and data, and 1875 observations.
#' @export
#'
readChoices <- function(filePath) {

  ratingData <- readr::read_delim(file = filePath,
                                  delim = "\t",
                                  col_names = c("time", "data"))
  return(ratingData)
}



#' Get Task Phases
#'
#' This function takes the data column from the output of
#' \code{\link{readChoices}} and separates the three phases of the task, saving
#' them to the environment \code{phrase}, which is initialized behind the
#' scenes. This function \strong{must} be run in order to use the other
#' functions that parse the choice data. If it is not, the phases will need to
#' be isolated by hand and assigned to the phase environment (or local/global
#' variables). See the \emph{Details} section for more how to access the three
#' phases.
#'
#' The three phases of the task may be accessed by calling the variables:
#' \itemize{ \item \code{phase$one} \item \code{phase$two} \item
#' \code{phase$three} }
#'
#' @param data The data output from \code{\link{readChoices}}
#'
#' @export
#'

getPhases <- function(data) {

  data <- data %>%
    dplyr::select(data) %>%
    dplyr::mutate(row = dplyr::row_number())

  start1 <- data %>%
    dplyr::filter(stringr::str_detect(.$data, "Phase 1 Group")) %>%
    dplyr::select(row) %>%
    dplyr::pull(row)

  start2 <- data %>%
    dplyr::filter(stringr::str_detect(.$data, "Phase 2 Group")) %>%
    dplyr::select(row) %>%
    dplyr::pull(row)

  start3 <- data %>%
    dplyr::filter(stringr::str_detect(.$data, "Phase 3 Group")) %>%
    dplyr::select(row) %>%
    dplyr::pull(row)

  end3 <- data %>%
    dplyr::filter(stringr::str_detect(.$data, "CLEAR TRANSITION SCREEN")) %>%
    utils::tail(1) %>%
    dplyr::select(row) %>%
    dplyr::pull(row)

  phase$one <- data %>%
    dplyr::filter(dplyr::between(row, start1, start2 - 1)) %>%
    dplyr::mutate(phase = rep(1))

  phase$two <- data %>%
    dplyr::filter(dplyr::between(row, start2, start3 - 1)) %>%
    dplyr::mutate(phase = rep(2))

  phase$three <- data %>%
    dplyr::filter(dplyr::between(row, start3, end3 - 1)) %>%
    dplyr::mutate(phase = rep(3))

}

#' Get Icons for Each Round
#'
#' This gets the icons for each round in each phase.
#'
#' @param phaseData One of the three phase objects defined by calling \code{\link{getPhases}}.
#'
#' @param pivot Logical: pivoting the data makes it tidier, but makes it harder
#'   to join with the other information. For this reason, it is being set to
#'   FALSE by default.
#'
#' @return Returns a dataframe with four columns:
#' \itemize{
#' \item \emph{icon1:} The icon presented in the first order position.
#' \item \emph{icon2:} The icon presented in the second order position.
#' \item \emph{phase:} The phase these icons were displayed in (constant for each \code{phase} object).
#' \item \emph{round:} The round number these icons were displayed in, specific to the phase.
#' }
#' @export
#'
#' @examples
#' \dontrun{getIcons(phase$one)}

getIcons <- function(phaseData, pivot = FALSE) {

  icons <- phaseData %>%
    dplyr::filter(stringr::str_detect(data, "SHOW: OPTION SELECTION")) %>%
    tidyr::separate(col = data,
             into = c("show", "option", "selection", "icon", "order", "icon_group"),
             extra = "drop",
             sep = " ",
             remove = TRUE) %>%
    dplyr::select(icon_group, phase) %>%
    dplyr::mutate(icon_group = stringr::str_remove_all(icon_group, "\"")) %>%
    dplyr::mutate(icon_group = stringr::str_remove_all(icon_group, "\\{")) %>%
    dplyr::mutate(icon_group = stringr::str_remove_all(icon_group, "\\}")) %>%
    tidyr::separate(col = icon_group, into = c("order1", "icon1", "icon2"),
             extra = "warn",
             remove = FALSE,
             sep = ":") %>%
    tidyr::separate(col = icon1, into = c("icon1", "order2"),
             extra = "warn",
             remove = FALSE,
             sep = ",") %>%
    dplyr::mutate(icon2 = stringr::str_remove_all(icon2, ",")) %>%
    dplyr::select(icon1, icon2, phase) %>%
    dplyr::mutate(round = dplyr::row_number())

  if(pivot) {
    icons <- icons %>%
      tidyr::pivot_longer(cols = c(icon1, icon2),
                   names_to = "image_order",
                   values_to = "icon") %>%
      dplyr::mutate(image_order = stringr::str_remove(.$image_order, "icon"),
             image_order = base::as.numeric(image_order))

  }

  return(icons)

}

#' Get Choices for Each Round
#'
#' This function gets the choices for each round for each phase.
#'
#' @param phaseData One of the three phase objects defined by calling \code{\link{getPhases}}.
#'
#' @return Returns a dataframe with five columns:
#' \itemize{
#' \item \emph{option:} The option the subject chose.
#' This should correspond to the icon order as presented in the task.
#' \item \emph{choice:} The icon the subject chose.
#' \item \emph{group:} The (payment weighting) group the icon corresponds to.
#' \item \emph{phase:} The phase the choices were made in (constant for each \code{phase} object).
#' \item \emph{round:} The round number these choices were made in, specific to the phase.
#' }
#' @export
#'
#' @examples
#' \dontrun{getChoices(phase$two)}

getChoices <- function(phaseData) {

  choices <- phaseData %>%
    dplyr::filter(stringr::str_detect(.$data, "HIGHLIGHT CHOICE")) %>%
    dplyr::mutate(data = stringr::str_remove_all(.$data, "HIGHLIGHT CHOICE: ")) %>%
    tidyr::separate(col = data,
             into = c("wordOption", "option", "wordChoice", "choice", "wordGroup", "group", "roundWord", "round"),
             extra = "warn",
             remove = FALSE,
             sep = " ") %>%
    dplyr::select(option, choice, group, round, phase) %>%
    dplyr::mutate(option = forcats::as_factor(stringr::str_remove_all(option, ",")),
           choice = forcats::as_factor(stringr::str_remove_all(choice, ",")),
           group = forcats::as_factor(stringr::str_remove_all(group, ",")),
           round = dplyr::row_number())

  return(choices)
}

#' Get Images Shown for Each Round
#'
#' This function gets the images shown for each round in each phase.
#'
#' @param phaseData One of the three phase objects defined by calling \code{\link{getPhases}}.
#'
#' #' @return Returns a dataframe with three columns:
#' \itemize{
#' \item \emph{image:} The image the subject was shown after making a choice.
#' \item \emph{phase:} The phase the choices were made in (constant for each \code{phase} object).
#' \item \emph{round:} The round number these choices were made in, specific to the phase.
#' }
#' @export
#'
#' @examples
#'
#' \dontrun{getImageShown(phase$three)}

getImageShown <- function(phaseData){

  output <- phaseData %>%
    dplyr::filter(stringr::str_detect(.$data, "SHOW: image")) %>%
    dplyr::mutate(data = stringr::str_remove_all(.$data, "SHOW: image - ")) %>%
    tidyr::separate(col = data,
             into = c("image", "round"),
             extra = "warn",
             remove = FALSE,
             sep = " , ") %>%
    dplyr::select(image, phase) %>%
    dplyr::mutate(image = stringr::str_remove(image, ".jpg"),
           round = dplyr::row_number())

  return(output)

}

#' Process Choice Data
#'
#' This function takes the data column from the output of
#' \code{\link{readChoices}} and runs four functions, binding the results in a
#' dataframe for easy manipulation: \itemize{ \item \code{\link{getPhases}}
#' which separates the three phases of the task, saving them to the environment
#' \code{phrase}, which is initialized behind the scenes. \item
#' \code{\link{getIcons}} which gets the icon options shown to the subject.
#' \item \code{\link{getChoices}} which gets the choice the subject makes. \item
#' \code{\link{getImageShown}} which gets the images the subject is shown after
#' choosing an icon.}
#'
#'
#' @param data The data output from \code{\link{readChoices}}
#'
#' @return A dataframe 150 rows and 8 columns: \itemize{ \item \emph{phase:} The
#'   phase the choices were made in. \item \emph{round:} The round number these
#'   choices were made in, specific to the phase. \item \emph{icon1:} The icon
#'   presented in the first order position. \item \emph{icon2:} The icon
#'   presented in the second order position. \item \emph{option:} The option the
#'   subject chose. This should correspond to the icon order as presented in the
#'   task. \item \emph{choice:} The icon the subject chose. \item \emph{group:}
#'   The (payment weighting) group the icon corresponds to. \item \emph{image:}
#'   The image the subject was shown after making a choice. }
#'
#' @export

processChoiceData <- function(data){

  getPhases(data)
  phases <- list(phase$one, phase$two, phase$three)

  icons <- purrr::map_df(phases, ~getIcons(.x))
  choices <- purrr::map_df(phases, ~getChoices(.x))
  images <- purrr::map_df(phases, ~getImageShown(.x))


  combined <- dplyr::full_join(icons, choices, by = c("phase", "round")) %>%
    dplyr::full_join(images, by = c("phase", "round")) %>%
    dplyr::select(phase, round, dplyr::everything())

  return(combined)
}


#' Get One Phase's Group Reward and Probability Info
#'
#' @param phaseData One of the three phase objects defined by calling
#'   \code{\link{getPhases}}.
#'
#' @return A dataframe with four columns. It has the reward and probability of
#'   receiving it as a function of each image's group per phase. This can be
#'   compared to the output of \code{\link{getChoices}} or
#'   \code{\link{processChoiceData}}. This function is called by
#'   \code{\link{getGroupInfo}} which combines the information for all three of
#'   the phases.

getGroupRewardProbs <- function(phaseData) {

  #subset the phaseData into just the relevant data column to test for which phase it is.
  testPhase <- phaseData %>%
    dplyr::slice(1) %>%
    dplyr::select(-c(row, phase))

  if (stringr::str_detect(testPhase, "Phase 1 Group")) {
    output <- phaseData %>%
      dplyr::slice(1) %>%
      dplyr::mutate(data = stringr::str_remove_all(.$data, "Phase 1 Group \\{Reward, Probability\\}: "))

  }else if (stringr::str_detect(testPhase, "Phase 2 Group")) {
    output <- phaseData %>%
      dplyr::slice(1) %>%
      dplyr::mutate(data = stringr::str_remove_all(.$data, "Phase 2 Group \\{Reward, Probability\\}: "))

  }else if (stringr::str_detect(testPhase, "Phase 3 Group")) {
    output <- phaseData %>%
      dplyr::slice(1) %>%
      dplyr::mutate(data = stringr::str_remove_all(.$data, "Phase 3 Group \\{Reward, Probability\\}: "))
  }

  output <- output %>%
    tidyr::separate(col = data,
                    into = c("1", "2", "3", "4", "5", "6"),
                    extra = "warn",
                    remove = TRUE,
                    sep = "\\},") %>%
    dplyr::select(-row) %>%
    dplyr::mutate(`1` = stringr::str_remove(`1`, "Group 1 \\{\\$"),
                  `2` = stringr::str_remove(`2`, "Group 2 \\{\\$"),
                  `3` = stringr::str_remove(`3`, "Group 3 \\{\\$"),
                  `4` = stringr::str_remove(`4`, "Group 4 \\{\\$"),
                  `5` = stringr::str_remove(`5`, "Group 5 \\{\\$"),
                  `6` = stringr::str_remove(`6`, "Group 6 \\{\\$")) %>%
    tidyr::pivot_longer(cols = -phase,
                        names_to = "group",
                        values_to = "info") %>%
    dplyr::mutate(info = stringr::str_trim(info),
                  info = stringr::str_remove_all(info, ","),
                  info = stringr::str_remove_all(info, "\\}"),
                  info = stringr::str_remove_all(info, "%")) %>%
    tidyr::separate(col = info,
                    into = c("reward", "probability"),
                    extra = "warn",
                    remove = TRUE,
                    sep = " ") %>%
    dplyr::mutate(group = forcats::as_factor(group),
                  probability = base::as.numeric(probability),
                  probability = probability/100) %>%
    tidyr::drop_na()

  return(output)
}

#' Get Group Reward and Probability Info for All Phases
#'
#' @param data The data output from \code{\link{readChoices}}
#'
#' @return A dataframe with 15 rows and four columns. It has the reward and
#'   probability of receiving it as a function of each image's group per phase.
#'   This can be compared to the output of \code{\link{getChoices}} or
#'   \code{\link{processChoiceData}}.
#' @export

getGroupInfo <- function(data) {

  getPhases(data)
  phases <- list(phase$one, phase$two, phase$three)

  output <- purrr::map_df(phases, ~getGroupRewardProbs(.x))

  return(output)

}
