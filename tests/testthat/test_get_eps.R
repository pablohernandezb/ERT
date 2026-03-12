# ==========================================================================
# Test get_eps() function for ESOT
# ==========================================================================
context("Produce data frame with the data for ESOT")

col_names <- c("country_name", "country_id", "country_text_id",
	"year", "codingstart", "codingend",
	"v2clstown_osp", "gapstart1", "gapstart2", "gapstart3",
	"gapend1", "gapend2", "gapend3")

new_cols <- c("country_id", "country_text_id", "country_name",
	"year", "v2clstown_osp", "econ_type",
	"econ_start_year", "econ_end_year",
	"econ_id", "econ_reg_type", "econ_trans", "econ_regch_event",
	"econ_regch_censored",
	"priv_ep", "priv_ep_id", "priv_ep_start_year",
	"priv_ep_end_year", "priv_pre_ep_year", "priv_ep_termination",
	"priv_ep_prch", "priv_ep_ptr", "priv_ep_subdep",
	"priv_ep_outcome", "priv_ep_outcome_agg", "priv_ep_censored",
	"stat_ep", "stat_ep_id",
	"stat_ep_start_year", "stat_ep_end_year", "stat_pre_ep_year",
	"stat_ep_termination", "stat_ep_prch", "stat_ep_pbr", "stat_ep_subreg",
	"stat_ep_outcome", "stat_ep_outcome_agg", "stat_ep_censored")

df <- matrix(1:117, ncol = 13, dimnames = list(NULL, col_names))

test_that("Wrong input format", {
	expect_error(get_eps(data = NULL))
	expect_error(get_eps(data = c(1:10)))
	expect_error(get_eps(data = df))
	})

df <- as.data.frame(df)
df$year <- 1898:{1898 + 8}

test_that("Variables from script are in the dataset", {
	expect_error(get_eps(data = df[, -4]))
	expect_error(get_eps(data = df[, -5]))
	expect_error(get_eps(data = df[, -7]))
	})

df$v2clstown_osp[4:5] <- NA_real_
data <- vdemdata::vdem

test_that("Data dimensions", {
	expect_equal(nrow(get_eps()), nrow(data[data$year >= 1900,]))
	expect_equal(ncol(get_eps()), length(new_cols))
	expect_equal(nrow(get_eps(data = df)), nrow(df[df$year >= 1900,]))
	})

test_that("Check column type output", {
	expect_equal(class(get_eps()$v2clstown_osp), class(vdemdata::vdem$v2clstown_osp))
	})

test_that("Missingness in new variables", {
	expect_equal(sum(is.na(get_eps(data = df)$v2clstown_osp)), sum(is.na(df$v2clstown_osp[df$year >= 1900])))
	})

test_that("Equal values in input and output", {
	expect_equal(get_eps(data = df)$v2clstown_osp, df[df$year >= 1900,"v2clstown_osp"])
	})

rm(data, col_names, new_cols, df)
