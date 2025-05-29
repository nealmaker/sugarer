#' Sap collection costs
#'
#' @param taps_per_acre number of maple taps per acre
#' @param wage wage for woods work (USD/hr)
#' @param life_years number of years to amortize tubing, etc.
#' @param real_interest interest rate after inflation (for capital recovery)
#'
#' @returns annual per acre cost (USD) of sap collection, exclusive of land
#'   rent, syrup production, marketing, etc.
#' @references After Huyler 2000 (USFS Res Pap NE-712); UVM 'Business of Sap'
#'   benchmarking slides (2020); Childs 2019 'Estimating costs for installing a
#'   maple tubing system'; and Cornell "Beginer's Notebook" & tap density
#'   guidance (2019)
#' @export
sap_collection_costs <- function(taps_per_acre,
                                 wage            = 25,     # $/hr for woods work
                                 life_years       = 10,     # years to amortise tubing etc.
                                 real_interest    = 0.05) { # after-inflation

  ## 2.1 Capital-recovery component  ───────────────────────────────
  # Cap-ex per tap declines linearly with density (Childs worksheet).
  # 40 $/tap at very open (30 tpa) → 16 $/tap at 120 tpa.
  capex_per_tap <- pmax(16, 40 - 0.25 * taps_per_acre)

  # Capital-recovery factor (flat payment that repays principal + interest)
  crf  <- real_interest * (1 + real_interest)^life_years /
    ((1 + real_interest)^life_years - 1)

  annual_capital <- capex_per_tap * crf

  ## 2.2 Operating components  ─────────────────────────────────────
  labor  <- 0.10 * wage                    # UVM benchmark
  energy <- 0.30                           # pump electricity, supplies

  ## 2.3 Total per-tap & per-acre  ─────────────────────────────────
  total_per_tap  <- annual_capital + labor + energy
  total_per_acre <- total_per_tap * taps_per_acre

  return(total_per_acre)
}
