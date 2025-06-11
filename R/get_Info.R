#' @title Get Stock Basic Information
#' @description
#' Retrieve stock basic information through Tushare API, including code, name, listing date, industry etc.
#' @param ts_code Stock code (e.g. "000001.SZ" or "600519.SH"), default empty means get all stocks
#' @param market Market type, optional "all"(no filter),"main"(Main Board),"gem"(GEM),"star"(STAR Market),"cdr"(CDR),"bse"(BSE), default "all" means no filter
#' @param list_status Listing status, "L"(Listed), "D"(Delisted), "P"(Suspended), default "L"
#' @param exchange Exchange, "SSE"(Shanghai), "SZSE"(Shenzhen), "BSE"(Beijing)
#' @param is_hs Connect program, "N"(No), "H"(SH Connect), "S"(SZ Connect)
#' @param fields Fields to return, default common fields
#' @return Dataframe containing stock info with fields:
#' \describe{
#'   \item{ts_code}{TS stock code}
#'   \item{symbol}{Stock symbol}
#'   \item{name}{Stock name}
#'   \item{area}{Region}
#'   \item{industry}{Industry}
#'   \item{market}{Market type}
#'   \item{list_date}{Listing date}
#'   \item{is_hs}{Connect program flag}
#'   \item{fullname}{Company full name}
#'   \item{enname}{English name}
#'   \item{cnspell}{Pinyin abbreviation}
#'   \item{exchange}{Exchange}
#'   \item{curr_type}{Currency}
#'   \item{list_status}{Listing status}
#'   \item{delist_date}{Delisting date}
#'   \item{act_name}{Actual controller}
#'   \item{act_ent_type}{Controller type}
#' }
#' @importFrom Tushare pro_api
#' @importFrom tibble as_tibble
#' @importFrom utils str
#' @examples
#' \dontrun{
#' # Basic usage: Get all listed stocks
#' stock_list <- get_Info()
#' head(stock_list)
#' # Test ts_code parameter
#' stock_600519 <- get_Info(ts_code = "600519.SH")
#' head(stock_600519)
#' stock_000001 <- get_Info(ts_code = "000001.SZ")
#' head(stock_000001)
#' # Test market parameter combinations
#' main_board <- get_Info(market = "main")
#' head(main_board)
#' gem_board <- get_Info(market = "gem")
#' head(gem_board)
#' star_market <- get_Info(market = "star")
#' head(star_market)
#' # Test the list_status parameter
#' listed_stocks <- get_Info(list_status = "L")
#' head(listed_stocks)
#' delisted_stocks <- get_Info(list_status = "D")
#' head(delisted_stocks)
#' suspended_stocks <- get_Info(list_status = "P")
#' head(suspended_stocks)
#' # Test the exchange parameter
#' sse_stocks <- get_Info(exchange = "SSE")
#' head(sse_stocks)
#' szse_stocks <- get_Info(exchange = "SZSE")
#' head(szse_stocks)
#' bse_stocks <- get_Info(exchange = "BSE")
#' head(bse_stocks)
#' # Test the is_hs parameter
#' hs_stocks <- get_Info(is_hs = "H")
#' head(hs_stocks)
#' sz_hs_stocks <- get_Info(is_hs = "S")
#' head(sz_hs_stocks)
#' non_hs_stocks <- get_Info(is_hs = "N")
#' head(non_hs_stocks)
#' # Test the fields parameter
#' minimal_fields <- get_Info(fields = c("ts_code", "name"))
#' head(minimal_fields)
#' full_fields <- get_Info(fields = c(
#'   "ts_code", "name", "industry",
#'   "list_date", "market", "is_hs"
#' ))
#' head(full_fields)
#' # Test parameter combinations
#' sse_hs_stocks <- get_Info(exchange = "SSE", is_hs = "H")
#' head(sse_hs_stocks)
#' gem_active_stocks <- get_Info(market = "gem", list_status = "L")
#' head(gem_active_stocks)
#' }
#' @author DengYishuo
#' @export
get_Info <- function(token = my_token,
                     ts_code = "",
                     market = "",
                     list_status = "L",
                     exchange = "",
                     is_hs = "",
                     fields = c(
                       "ts_code", "symbol", "name", "area",
                       "industry", "market", "list_date"
                     )) {
  # Parameter validation
  if (nchar(ts_code) > 0 && !grepl("\\.(SH|SZ)$", ts_code)) {
    stop("Invalid stock code format, must include .SH or .SZ suffix")
  }

  # Market type mapping
  market_map <- c(
    "main" = "\\u4e3b\\u677f", # 主板
    "gem" = "\\u521b\\u4e1a\\u677f", # 创业板
    "star" = "\\u79d1\\u521b\\u677f", # 科创板
    "cdr" = "CDR", # CDR
    "bse" = "\\u5317\\u4ea4\\u6240" # 北交所
  )

  # Convert market parameter
  if (market == "") {
    market <- NULL
  } else if (!is.null(market)) {
    if (!market %in% names(market_map)) {
      stop("market must be one of: 'main', 'gem', 'star', 'cdr' or 'bse'")
    }
    market <- as.character(market_map[market])
  }

  if (is_hs == "") {
    is_hs <- NULL
  } else {
    is_hs <- toupper(is_hs)
    if (!is_hs %in% c("N", "H", "S")) {
      stop("is_hs must be one of: 'N', 'H', 'S'")
    }
  }

  params <- list(
    ts_code = ts_code,
    market = market,
    list_status = list_status,
    exchange = exchange,
    is_hs = is_hs
  )

  # Remove NULL params
  params <- params[!sapply(params, is.null)]

  # Handle fields
  if (!is.null(fields)) {
    params$fields <- paste(unique(c("ts_code", fields)), collapse = ",")
  }

  # Printing parameters for debugging

  message("Constructed API parameters:")
  print(params)

  api <- Tushare::pro_api(token)

  # Constructing API Call Strings
  param_str <- paste(names(params),
    sapply(params, function(x) if (is.character(x)) paste0("'", x, "'") else x),
    sep = "=", collapse = ", "
  )

  api_call_str <- paste0("api(api_name = 'stock_basic', ", param_str, ")")

  # Constructing API Call Strings
  message("Executed API Call:")
  message(api_call_str)

  tryCatch(
    {
      # Execute API call
      res <- eval(parse(text = api_call_str))

      # Filter results
      if (nchar(ts_code) > 0 && !is.null(res)) {
        res <- res[res$ts_code == ts_code, ]
      }

      # Handle empty values
      res[is.na(res) | res == ""] <- NA
      res <- tibble::as_tibble(res)

      # print the structure of API response
      message("API Response Structure:")
      str(res)

      return(res)
    },
    error = function(e) {
      message("API call failed: ", e$message)
      return(NULL)
    }
  )
}
