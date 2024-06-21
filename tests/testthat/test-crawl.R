test_that("crawl function works for a specific URL", {
  url <- "https://www.fr.adp.com/rhinfo/articles/2022/11/la-disparition-de-certains-metiers-est-elle-a-craindre.aspx"

  # Run the crawl function
  result <- crawl(url)

  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the result has the expected columns
  expected_columns <- c("title", "date", "text", "excerpt", "hostname")
  expect_true(all(expected_columns %in% colnames(result)))

  # Check that the title is not empty
  expect_true(nchar(result$title) > 0)

  # Check that the text is not empty
  expect_true(nchar(result$text) > 0)

  # Print the result for manual inspection (optional)
  print(result)
})
