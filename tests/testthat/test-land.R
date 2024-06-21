test_that("db_setup creates tables correctly", {
  db_name <- tempfile(fileext = ".db")
  db_setup(db_name)

  con <- dbConnect(SQLite(), dbname = db_name)
  tables <- dbListTables(con)

  expected_tables <- c("Land", "Domain", "Expression", "ExpressionLink", "Word", "LandDictionary", "Media", "Tag", "TaggedContent")
  expect_true(all(expected_tables %in% tables))

  dbDisconnect(con)
})

test_that("create_land works as expected", {
  db_name <- tempfile(fileext = ".db")
  db_setup(db_name)

  create_land("test_land", "A test land", db_name = db_name)

  con <- dbConnect(SQLite(), dbname = db_name)
  lands <- dbGetQuery(con, "SELECT * FROM Land WHERE name = 'test_land'")

  expect_equal(nrow(lands), 1)
  expect_equal(lands$name, "test_land")
  expect_equal(lands$description, "A test land")

  dbDisconnect(con)
})

test_that("addurl adds URLs correctly", {
  db_name <- tempfile(fileext = ".db")
  db_setup(db_name)

  create_land("test_land", "A test land", db_name = db_name)

  result <- addurl("test_land", urls = "http://example.com, http://example.org", db_name = db_name)

  expect_equal(result, 1)

  con <- dbConnect(SQLite(), dbname = db_name)
  urls <- dbGetQuery(con, "SELECT * FROM Expression WHERE land_id = (SELECT id FROM Land WHERE name = 'test_land')")

  expect_equal(nrow(urls), 2)
  expect_true(all(urls$url %in% c("http://example.com", "http://example.org")))

  dbDisconnect(con)
})

test_that("deleteland deletes land correctly", {
  db_name <- tempfile(fileext = ".db")
  db_setup(db_name)

  create_land("test_land", "A test land", db_name = db_name)
  addurl("test_land", urls = "http://example.com", db_name = db_name)

  result <- deleteland("test_land", db_name = db_name)

  expect_equal(result, 1)

  con <- dbConnect(SQLite(), dbname = db_name)
  lands <- dbGetQuery(con, "SELECT * FROM Land WHERE name = 'test_land'")

  expect_equal(nrow(lands), 0)

  dbDisconnect(con)
})

test_that("list_domain lists domains correctly", {
  db_name <- tempfile(fileext = ".db")
  db_setup(db_name)

  create_land("test_land", "A test land", db_name = db_name)
  addurl("test_land", urls = "http://example.com", db_name = db_name)

  con <- dbConnect(SQLite(), dbname = db_name)
  dbExecute(con, "INSERT INTO Domain (name) VALUES ('example.com')")
  dbExecute(con, "UPDATE Expression SET domain_id = (SELECT id FROM Domain WHERE name = 'example.com') WHERE url = 'http://example.com'")
  dbDisconnect(con)

  domains <- list_domain("test_land", db_name = db_name)

  expect_equal(length(domains), 1)
  expect_equal(domains[1], "example.com")
})
