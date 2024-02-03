#' create Daymet summaries
#' @param x a daymetr object
#' @param var character string giving desired weather variable
#' @param days numeric vector of desired days, or a function of a daymetr object
#'   that returns a vector of days
#' @param years numeric vector of desired years
#' @param fun summary function taking vector input and returning scalar output
#' @return a tibble giving the summary function of `var` over the 
#'           requested `days`, for each requested year in `years`, for each 
#'           site in `x`
#' @export
summarize_daymet <- function(x, var, days, years = 1980:2022, fun = mean){
  
  if(!("data" %in% names(x))){
    assertthat::assert_that(x[1] == "Error in download_daymet(site = site, lat = lat, lon = lon, start = start,  : \n  Your requested data is outside DAYMET spatial coverage.\n\n            Check the requested coordinates.\n")
    return(
      tibble::tibble(year := years, {{var}} := NA)
    )
  }
  
  if(is.function(days)){
    days <- days(x)
  }
  
  assertthat::assert_that(all(days > 0 & days < 366 & days == floor(days)))
  
  annual <- x$data |>
    dplyr::filter(yday %in% days) |>
    dplyr::group_by(year) |>
    dplyr::summarize(out = fun(!! rlang::sym(var))) |>
    dplyr::pull(out)
  
  tibble::tibble(year := years, {{var}} := annual)
}

