## Please do three things to ensure this template is correctly modified:
##
## 1. Rename this file based on the content you are testing using `test-functionName.R` format
##    so that your can directly call `moduleCoverage` to calculate module coverage information.
##    `functionName` is a function's name in your module (e.g., `", name, "Event1`).
##
## 2. Copy this file to the tests folder (i.e., `tests/testthat/`).
##
## 3. Modify the test description based on the content you are testing.
##
test_that("test Event1 and Event2.", {
  module <- list("bird_modelPredict")
  path <- list(
    modulePath = "/home/sdas/bird_NRV/modules",
    outputPath = file.path(tempdir(), "outputs")
  )
  parameters <- list(
    #.progress = list(type = graphical, interval = 1),
    .globals = list(verbose = FALSE),
    bird_modelPredict = list(.saveInitialTime = NA)
  )
  times <- list(start = 0, end = 1)

  # If your test function contains `time(sim)`, you can test the function at a
  # particular simulation time by defining the start time above.
  object1 <- object1 # please specify
  object2 <- object2 # please specify
  objects <- list(object1 = object1, object2 = object2)

  mySim <- simInit(times = times,
                   params = parameters,
                   modules = module,
                   objects = objects,
                   paths = path)

  # You may need to set the random seed if your module or its functions use the
  # random number generator.
  set.seed(1234)

  # You have two strategies to test your module:
  # 1. Test the overall simulation results for the given objects, using the
  #    sample code below:

  output <- spades(mySim, debug = FALSE)

  # is output a simList?
  expect_is(output, "simList")

  # does output have your module in it
  expect_true(any(unlist(modules(output)) %in% c(unlist(module))))

  # did it run to the end?
  expect_true(time(output) == 1)

  # 2. Test the functions inside of the module using the sample code below:
  #    To allow the `moduleCoverage` function to calculate unit test coverage
  #    level, it needs access to all functions directly.
  #    Use this approach when using any function within the simList object
  #    (i.e., one version as a direct call, and one with `simList` object prepended).

  simOutput <- Event1(mySim)

  expectedOutputEvent1Test1 <-  "this is test for event 1."  # please define your expection of your output
  expect_is(class(simOutput$event1Test1), "character")
  expect_equal(simOutput$event1Test1, expectedOutputEvent1Test1) # or other expect function in testthat package.
  expect_equal(simOutput$event1Test2, as.numeric(999)) # or other expect function in testthat package.

  simOutput <- Event2(mySim)

  expectedOutputEvent2Test1 <-  "this is test for event 2."  # please define your expection of your output
  expect_is(class(simOutput$event2Test1), character)
  expect_equal(simOutput$event2Test1, expectedOutputEvent2Test1) # or other expect function in testthat package.
  expect_equal(simOutput$event2Test2, as.numeric(777)) # or other expect function in testthat package.
})
