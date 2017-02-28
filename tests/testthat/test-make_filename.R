expect_that(make_filename(2016), is_identical_to(make_filename("2016")))
expect_that(make_filename("a"), gives_warning())
expect_that(make_filename(2022), is_identical_to("accident_2022.csv.bz2"))
