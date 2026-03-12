# A sequence starts when there is a positive change of a certain
# threshold (start_incl). It ends according to one of the following
# conditions:
#  - Single unit decrease past a threshold (year_turn)
#  - Cumulative decrease over a period where there is no change over
#    the starting threshold (start_incl)
#  - No further changes over the starting threshold (start_incl) for a
#    fixed period of time (tolerance)
#  - Missing value
#  - econ_type reverts to 0 (planned economy)
###

test_that("Invalid function arguments", {
    expect_error(find_seqs_priv(c(1, 2), 1))
    expect_error(find_seqs_priv(1, 1, start_incl = -1))
    expect_error(find_seqs_priv(1, 1, year_turn = 1))
    expect_error(find_seqs_priv(1, 1, cum_turn = 1))
    expect_error(find_seqs_priv(1, 1, tolerance = 0))
})

test_that("Basic increasing seq", {
    soi <- 1:10
    econ <- 1:10

    # Basic single increasing sequence with default arguments
    out <- find_seqs_priv(soi, econ)
    expect_equal(out, rep(1, 10))

    # Higher threshold for initial increase means that there should be
    # no privatization sequence
    out <- find_seqs_priv(soi, econ, start_incl = 2)
    expect_equal(out, rep(NA_real_, 10))
})

test_that("Multiple sequences", {
    soi <- c(1:2, 1, 5)
    econ <- rep(1, 4)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, c(1, 1, 2, 2))

    out <- find_seqs_priv(soi, econ, start_inc = 2)
    expect_equal(out, c(NA, NA, 1, 1))

    out <- find_seqs_priv(soi, econ, year_turn = -2)
    expect_equal(out, c(1, 1, 2, 2))

    out <- find_seqs_priv(soi, econ, cum_turn = -2)
    expect_equal(out, c(1, 1, 2, 2))

    out <- find_seqs_priv(soi, econ, year_turn = -2, cum_turn = -2)
    expect_equal(out, c(1, 1, 1, 1))

    soi <- c(1:2, 1, 0, 0)
    econ <- rep(1, 5)

    out <- find_seqs_priv(soi, econ, year_turn = -1, cum_turn = -1)
    expect_equal(out, c(1, 1, 1, NA, NA))
})

test_that("Stasis", {
    soi <- c(1, rep(2, 4))
    econ <- rep(1, 5)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, rep(1, 5))

    out <- find_seqs_priv(soi, econ, tolerance = 1)
    expect_equal(out, c(1, 1, NA, NA, NA))

    out <- find_seqs_priv(soi, econ, tolerance = 2)
    expect_equal(out, c(1, 1, 1, NA, NA))

    out <- find_seqs_priv(soi, econ, tolerance = 3)
    expect_equal(out, rep(1, 5))

    soi <- c(2, 1, 2, 1, 1, 1)
    econ <- rep(1, 6)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, c(NA, 1, 1, NA, NA, NA))

    out <- find_seqs_priv(soi, econ, year_turn = -2, cum_turn = -2, tolerance = 2)
    expect_equal(out, c(NA, 1, 1, 1, NA, NA))
})

test_that("Handles NA", {
    soi <- c(NA, 1:2)
    econ <- c(NA, 1, 1)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, c(NA, 1, 1))

    soi <- c(1:2, NA)
    econ <- c(1, 1, NA)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, c(1, 1, NA))

    soi <- c(1, NA, 2, 3, NA)
    econ <- c(1, NA, 1, 1, NA)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, c(NA, NA, 1, 1, NA))

    soi <- c(NA, 1, 2, NA, 1, 0)
    econ <- c(NA, 1, 1, NA, 1, 1)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, c(NA, 1, 1, NA, NA, NA))

    out <- find_seqs_priv(soi, econ, year_turn = -2, cum_turn = -2)
    expect_equal(out, c(NA, 1, 1, NA, NA, NA))
})

test_that("Decrease in econ_type", {
    soi <- 1:2
    econ <- c(1, 0)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, c(1, 1))

    soi <- 1:3
    econ <- c(1, 0, 0)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, c(1, 2, 2))

    soi <- c(1:2, 2, 2, 2)
    econ <- c(1, 1, 1, 0, 1)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, c(1, 1, 1, NA, NA))

    soi <- 1:3
    econ <- c(2, 1, 1)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, c(1, 1, 1))

    soi <- 1:2
    econ <- c(0, 0)

    out <- find_seqs_priv(soi, econ)
    expect_equal(out, c(1, 1))
})
