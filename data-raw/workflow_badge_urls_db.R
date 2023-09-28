extract_gha_workflows <- function(username,
                                  repository,
                                  folder_path = ".github/workflows",
                                  token = Sys.getenv("GITHUB_PAT")) {
  api_url <- paste0("https://api.github.com/repos/", username, "/", repository, "/contents/", folder_path)
  headers <- httr::add_headers(Authorization = paste("token", token))
  response <- httr::GET(api_url, headers = headers)

  if (httr::http_status(response)$category != "Success") {
    stop("Failed to fetch workflow folder contents. Check your credentials and URL.")
  }

  content <- httr::content(response, as = "text")
  folder_contents <- jsonlite::fromJSON(content)
  file_names <- folder_contents$name
  workflow_names <- tools::file_path_sans_ext(file_names)

  workflow_names
}

# workflows which are run on every commit on the `main`-branch
extract_main_workflows <- function(workflows) {
  workflows_not_run_on_main <- c(
    "lint-changed-files",
    "pre-commit",
    "revdepcheck",
    "test-coverage-examples",
    "update-to-latest-easystats"
  )

  setdiff(workflows, workflows_not_run_on_main)
}

generate_workflow_status_badge_urls <- function(username, repository) {
  workflows <- extract_gha_workflows(username, repository)
  main_workflows <- extract_main_workflows(workflows)

  generate_url <- function(workflow) {
    paste0(
      "https://github.com/",
      username,
      "/",
      repository,
      "/workflows/",
      workflow,
      "/badge.svg"
    )
  }

  unname(vapply(main_workflows, generate_url, FUN.VALUE = character(1L)))
}

easystats_packages <- easystats:::.packages_on_cran()
url_list <- purrr::map(easystats_packages, ~ generate_workflow_status_badge_urls("easystats", .x))

url_df <- purrr::set_names(url_list, easystats_packages) |> as.data.frame()

saveRDS(url_df, "badge_url_data.rds")