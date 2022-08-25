# ---------------------------------------------------------------------------- #
#' samvatsara
#'
#' @description Shaka Samvatsar for a given Julian day number and maasa number.
#' @param jd Julian day number
#' @param maasa_num Maasa number
#'
#' @return Number associated with the Shaka Samvatsar
#'
#' @examples
#' samvatsara(2459778,2)
samvatsara <- function(jd,maasa_num){
  kali = elapsed_year(jd,maasa_num)[1]
  if(kali >= 4009){
    kali = (kali - 14) %% 60
  }
  samvat = (kali + 27 + as.integer((kali * 211 - 108)/18000)) %% 60
  return (samvat)
}
# ---------------------------------------------------------------------------- #