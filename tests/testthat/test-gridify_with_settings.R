# Tests for gridify_with_settings UI and server module.
# The Shiny module is listed in Suggests, so all tests are skipped when shiny
# (or ggplot2) is unavailable.  The server tests use shiny::testServer() which
# exercises server logic without launching a full Shiny process.

skip_if_not_installed("shiny")
skip_if_not_installed("ggplot2")

# ---------------------------------------------------------------------------
# Helper: build a minimal gridifyClass for use inside reactives
# ---------------------------------------------------------------------------
make_gridify_obj <- function() {
  gridify(
    object = ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
      ggplot2::geom_point(),
    layout = simple_layout()
  ) |>
    set_cell("title",  "Test title") |>
    set_cell("footer", "Test footer")
}

# ===========================================================================
# UI tests
# ===========================================================================

test_that("gridify_with_settings_ui returns a shiny.tag (sidebarLayout)", {
  ui <- gridify_with_settings_ui("test_ui")
  expect_s3_class(ui, "shiny.tag")
})

test_that("gridify_with_settings_ui errors on non-string id", {
  expect_error(
    gridify_with_settings_ui(123),
    "'id' must be a single character string"
  )
})

test_that("gridify_with_settings_ui errors on vector id", {
  expect_error(
    gridify_with_settings_ui(c("a", "b")),
    "'id' must be a single character string"
  )
})

test_that("gridify_with_settings_ui includes a uiOutput for plot_ui", {
  ui  <- gridify_with_settings_ui("check_plot")
  html <- as.character(ui)
  # The dynamic plot container is a uiOutput namespaced as "check_plot-plot_ui"
  expect_true(grepl("check_plot-plot_ui", html, fixed = TRUE))
})

test_that("gridify_with_settings_ui includes download buttons", {
  ui  <- gridify_with_settings_ui("check_dl")
  html <- as.character(ui)
  expect_true(grepl("dl_png", html, fixed = TRUE))
  expect_true(grepl("dl_pdf", html, fixed = TRUE))
})

test_that("gridify_with_settings_ui includes height and width sliders", {
  ui  <- gridify_with_settings_ui("check_sliders")
  html <- as.character(ui)
  expect_true(grepl("check_sliders-height", html, fixed = TRUE))
  expect_true(grepl("check_sliders-width",  html, fixed = TRUE))
})

# ===========================================================================
# Server-level input validation (before moduleServer)
# ===========================================================================

test_that("gridify_with_settings_srv errors on non-string id", {
  expect_error(
    gridify_with_settings_srv(123, shiny::reactive(make_gridify_obj())),
    "'id' must be a single character string"
  )
})

test_that("gridify_with_settings_srv errors when gridify_r is not callable", {
  expect_error(
    gridify_with_settings_srv("id", "not_a_function"),
    "'gridify_r' must be a shiny::reactive\\(\\) or a plain function"
  )
})

test_that("gridify_with_settings_srv errors on wrong height length", {
  expect_error(
    gridify_with_settings_srv("id",
                              shiny::reactive(make_gridify_obj()),
                              height = c(400, 200)),
    "must be a numeric vector of length 3"
  )
})

test_that("gridify_with_settings_srv errors when height value outside min/max", {
  expect_error(
    gridify_with_settings_srv("id",
                              shiny::reactive(make_gridify_obj()),
                              height = c(100, 200, 2000)),
    "must be between min"
  )
})

test_that("gridify_with_settings_srv errors on wrong width length", {
  expect_error(
    gridify_with_settings_srv("id",
                              shiny::reactive(make_gridify_obj()),
                              width = c(400)),
    "must be a numeric vector of length 3"
  )
})

test_that("gridify_with_settings_srv errors when width value outside min/max", {
  expect_error(
    gridify_with_settings_srv("id",
                              shiny::reactive(make_gridify_obj()),
                              width = c(100, 200, 2000)),
    "must be between min"
  )
})

# ===========================================================================
# Server logic via shiny::testServer()
# ===========================================================================

test_that("gridify_with_settings_srv returns NULL invisibly", {
  gridify_r <- shiny::reactive(make_gridify_obj())

  shiny::testServer(
    app  = gridify_with_settings_srv,
    args = list(gridify_r = gridify_r),
    expr = {
      result <- session$getReturned()
      expect_null(result)
    }
  )
})

test_that("gridify_with_settings_srv p_height reflects slider input", {
  gridify_r <- shiny::reactive(make_gridify_obj())

  shiny::testServer(
    app  = gridify_with_settings_srv,
    args = list(gridify_r = gridify_r, height = c(400L, 200L, 1200L)),
    expr = {
      session$setInputs(height = 400L, width = 800L)
      expect_equal(as.integer(input$height), 400L)
    }
  )
})

test_that("gridify_with_settings_srv p_width reflects slider input", {
  gridify_r <- shiny::reactive(make_gridify_obj())

  shiny::testServer(
    app  = gridify_with_settings_srv,
    args = list(gridify_r = gridify_r),
    expr = {
      session$setInputs(height = 600L, width = 900L)
      expect_equal(as.integer(input$width), 900L)
    }
  )
})

test_that("gridify_with_settings_srv errors when reactive returns non-gridifyClass", {
  # get_obj() is a local reactive accessible directly inside testServer expr.
  bad_r <- shiny::reactive("I am not a gridifyClass")

  shiny::testServer(
    app  = gridify_with_settings_srv,
    args = list(gridify_r = bad_r),
    expr = {
      session$setInputs(height = 600L, width = 800L)
      expect_error(
        get_obj(),
        regexp = "must return a .gridifyClass. object",
        perl   = TRUE
      )
    }
  )
})
