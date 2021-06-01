context("test-utils.R")

test_that(".delim_str_to_json returns correct JSON string", {
  expect_equal(.delim_str_to_json("foo, bar"),
               jsonlite::toJSON(c("foo", "bar")))
  expect_equal(.delim_str_to_json("foo,bar"),
               jsonlite::toJSON(c("foo", "bar")))
  expect_equal(.delim_str_to_json("foo"),
               jsonlite::toJSON(c("foo")))
  expect_equal(.delim_str_to_json(""), "")
})
