# A sequence starts when there is a negative change of a certain
# threshold (start_incl). It ends according to one of the following
# conditions:
#  - Single unit increase past the threshold (year_turn)
#  - Cumulative increase over a period where there is no change over
#    the starting threshold (start_incl)
#  - No further changes below the starting threshold (start_incl) for
#    a fixed period of time (tolerance)
#  - Missing value
#  - econ_type reverts to 3 (market economy)
###

test_that("Invalid function arguments", {
    expect_error(find_seqs_stat(c(1, 2), 1))
    expect_error(find_seqs_stat(1, 1, start_incl = 1))
    expect_error(find_seqs_stat(1, 1, year_turn = -1))
    expect_error(find_seqs_stat(1, 1, cum_turn = -1))
    expect_error(find_seqs_stat(1, 1, tolerance = 0))
})

test_that("Basic decreasing seq", {
    soi <- 10:1
    econ <- rep(1, 10)

    out <- find_seqs_stat(soi, econ)
    expect_equal(out, rep(1, 10))

    # Increase the start_incl threshold
    out <- find_seqs_stat(soi, econ, start_incl = -2)
    expect_equal(out, rep(NA_real_, 10))
})

test_that("Multiple sequences", {
    soi <- c(2:1, 3, 1)
    econ <- rep(1, 4)

    out <- find_seqs_stat(soi, econ)
    expect_equal(out, c(1, 1, 2, 2))

    out <- find_seqs_stat(soi, econ, start_incl = -2)
    expect_equal(out, c(NA, NA, 1, 1))

    out <- find_seqs_stat(soi, econ, cum_turn = 2, year_turn = 2)
    expect_equal(out, c(1, 1, 1, 1))

    soi <- c(2:1, 2, 3, 3)
    econ <- rep(1, 5)

    out <- find_seqs_stat(soi, econ, year_turn = 1, cum_turn = 1)
    expect_equal(out, c(1, 1, 1, NA, NA))
})

test_that("Stasis", {
    soi <- c(2, rep(1, 4))
    econ <- rep(1, 5)

    out <- find_seqs_stat(soi, econ)
    expect_equal(out, rep(1, 5))

    out <- find_seqs_stat(soi, econ, tolerance = 1)
    expect_equal(out, c(1, 1, NA, NA, NA))

    out <- find_seqs_stat(soi, econ, tolerance = 2)
    expect_equal(out, c(1, 1, 1, NA, NA))

    out <- find_seqs_stat(soi, econ, tolerance = 3)
    expect_equal(out, c(1, 1, 1, 1, 1))

    soi <- c(1, 2, 1, 2, 1, 1, 1)
    econ <- c(rep(1, 7))

    out <- find_seqs_stat(soi, econ)
    expect_equal(out, c(NA, 1, 1, 2, 2, 2, 2))

    out <- find_seqs_stat(soi, econ, year_turn = 2, cum_turn = 2, tolerance = 2)
    expect_equal(out, c(NA, 1, 1, 1, 1, 1, 1))
})

test_that("Handles NA", {
    soi <- c(NA, 2, 1, NA)
    econ <- c(NA, 1, 1, NA)

    out <- find_seqs_stat(soi, econ)
    expect_equal(out, c(NA, 1, 1, NA))

    soi <- c(NA, 2, 1, NA, 0, NA, 3, 1)
    econ <- c(NA, 1, 1, NA, 1, NA, 1, 1)

    out <- find_seqs_stat(soi, econ)
    expect_equal(out, c(NA, 1, 1, NA, NA, NA, 2, 2))
})

test_that("Increase in econ_type", {
    soi <- c(3, 2, 1)
    econ <- c(2, 1, 1)

    out <- find_seqs_stat(soi, econ)
    expect_equal(out, c(1, 1, 1))

    soi <- c(3, 2, 1, 1, 1)
    econ <- c(1, 3, 1, 1, 1)

    out <- find_seqs_stat(soi, econ)
    expect_equal(out, c(1, 2, 2, 2, 2))

    soi <- c(3, 2, 1, 1, 1, 1)
    econ <- c(1, 1, 1, 1, 3, 1)

    out <- find_seqs_stat(soi, econ)
    expect_equal(out, c(1, 1, 1, 1, NA, NA))
})
