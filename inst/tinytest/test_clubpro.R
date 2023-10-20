set.seed(22)

test_dat <- data.frame(condition = rep(c(1, 2), each = 9),
                       gender = c(1,2,2,1,2,1,1,1,2,1,1,1,2,2,2,2,2,1),
                       items = c(7,8,5,7,8,7,9,6,7,5,6,4,5,7,6,4,5,5),
                       items2 = c(6,7,5,6,8,5,7,5,7,3,4,3,5,5,3,3,3,2))

test_dat$condition <- factor(test_dat$condition)
test_dat$gender <- factor(test_dat$gender)

m1 <- club(items ~ condition, test_dat, nreps = 1000L)

# crossed observations
m2 <- club(items ~ condition * gender, test_dat, nreps = 1000L)

expect_equal(n_correct(m1), 15)
expect_equal(n_incorrect(m1), 3)
expect_equal(n_ambiguous(m1), 0)
expect_equal(round(pcc(m1), 2), 83.33)
expect_true(cval(m1) < 0.2)

expect_identical(clubpro:::to_indicator_matrix(c(1,2,3)), matrix(c(1,0,0,0,1,0,0,0,1), nrow=3, byrow=TRUE))
expect_equal(round(clubpro:::c_pcc(test_dat$items, 
             clubpro:::to_indicator_matrix(test_dat$condition), 
             imprecision=0, normalise_cols=TRUE), 2), 83.33)

expect_equal(round(pcc(m2), 2), 55.56)
