
#' Get crops list available for a quality laboratory.
#'
#' @param qualityLab name of the quality laboratory. Default: 'ICARDA-MAR'
#'
#' @examples
#' \dontrun{
#'   # Get crops available in ICARDA quality lab
#'   crops <- getCrops(qualityLab = 'ICARDA-MAR')
#' }
#'
#' @return a list of available crops


getCrops <- function(qualityLab = 'ICARDA-MAR') {

  handle <- httr::handle("https://grs.icarda.org/nirquality/api/crop/getCrops.php")

  body <- list(
    lab = qualityLab
  )

  response <- httr::POST(handle = handle, body = body)
  result <- httr::content(response, type = "text/csv")

  return(result)

}

#'
#'

.expand_list <- function(lst){
  res <- utils::stack(lst)
  as.list(stats::setNames(res$values, res$ind))
}

#' Get trials available for specified quality laboratories (ICARDA-MAR, ICARDA-LBN or CIMMYT) and year(s)
#'
#' @param qualityLab name of the quality laboratory. Default: 'ICARDA-MAR'
#' @param year
#'
#' @examples
#' \dontrun{
#'   # Get trials available in the ICARDA quality lab for 2018 and 2019
#'   trials <- getTrials(qualityLab = 'ICARDA-MAR', year = c(2018,2019))
#' }
#'
#' @return a dataframe of trials grouped by quality lab and year
#'

getTrials <- function(qualityLab = 'ICARDA-MAR', year){

  url <- "https://grs.icarda.org/nirquality/api/trial/getTrials.php"
  params <- list(
    "lab[]" = qualityLab,
    "year[]" = year
  )
  body <- .expand_list(params)

  response <- httr::POST(url = url, body = body, httr::verbose())

  result <- httr::content(response, type = "text/csv")

  return(result)
}


#' Get NIR data for a quality laboratory (ICARDA-MAR, ICARDA-LBN or CIMMYT)
#'
#' @param qualityLab name of the quality laboratory. Default: 'ICARDA-MAR'
#' @param crop a crop name or a vector of crop names.
#' @param gid
#' @param year
#' @param nir_model name of the NIR model used to get NIR data. Allowed values: c('Antharis II','FOSS DS2500')
#' @param country
#' @param location
#' @param trial
#'
#' @examples
#' \dontrun{
#'   # Get ICARDA NIR data for bread wheat and barley
#'   nir.data <- getNIRData(qualityLab = 'ICARDA-MAR', crop = c('bread wheat','barley'))
#' }
#'
#' @return a dataframe of NIR data.
#'
#'

getNIRData <- function(qualityLab = 'ICARDA-MAR', crop = NULL, gid = NULL,
                       year = NULL, nir_model = NULL, country = NULL, location = NULL,
                       trial = NULL) {

    url <- "https://grs.icarda.org/nirquality/api/nir/getNIR.php"
    params <- list(
      lab = qualityLab,
      "crop[]" = crop,
      "gid[]" = gid,
      "year[]" = year,
      "model[]" = nir_model,
      "country[]" = country,
      "location[]" = location,
      "trial[]" = trial
    )
    body <- .expand_list(params)

    response <- httr::POST(url = url, body = body, httr::verbose())
    result <- httr::content(response, type = "text/csv")

    return(result)
}

#' Get available traits data
#'
#' @param qualityLab name of the quality laboratory. Default: 'ICARDA-MAR'
#' @param crop a crop name or a vector of crop names.
#' @param gid
#' @param year
#' @param nir_model
#' @param country
#' @param location
#' @param trial
#'
#' @examples
#' \dontrun{
#'   # Get available ICARDA traits data for bread wheat and barley
#'   traits.data <- getTraitsData(qualityLab = 'ICARDA-MAR', crop = c('bread wheat','barley'))
#' }
#'
#' @return a dataframe of traits data.
#'
#'

getTraitsData <- function(qualityLab = "ICARDA-MAR", crop = NULL, gid = NULL, year = NULL, nir_model = NULL, country = NULL, location = NULL, trial = NULL){

  url <- "https://grs.icarda.org/nirquality/api/trait/getTraitsData.php"
  params <- list(
    "lab[]" = qualityLab,
    "crop[]" = crop,
    "gid[]" = gid,
    "year[]" = year,
    "model[]" = nir_model,
    "country[]" = country,
    "location[]" = location,
    "trial[]" = trial
  )
  body <- .expand_list(params)

  response <- httr::POST(url = url, body = body, httr::verbose())
  result <- httr::content(response, as = 'raw', type = "text/csv")
  max_guess <- round(length(result)/17) + 1
  result.parsed <- readr::read_csv(result, guess_max = max_guess)

  return(result.parsed)
}


