#' @importFrom withr with_
#' @importFrom rlang is_missing
#' @importFrom rlang new_environment
#' @importFrom rlang global_env
#' @importFrom rlang abort
#' @importFrom rlang env_poke
#' @importFrom purrr walk
#' @importFrom purrr iwalk
.with_exec_environment = function(expr, packages, data, objects, env) {
  code = rlang::enexpr(expr)
  .with_exec_evironment_helper(
    new = packages,
    code = eval(code, envir = env),
    data = data,
    objects = objects,
    env = env,
    pos = 2,
    lib.loc = NULL,
    logical.return = FALSE,
    warn.conflicts = FALSE,
    quietly = TRUE,
    verbose = getOption("verbose")
  )
}

#' @importFrom rlang env_clone
#' @importFrom rlang env_coalesce
#' @importFrom withr with_
.with_exec_evironment_helper = with_(
  set = function(
    packages,
    data,
    objects,
    env, 
    pos = 2,
    lib.loc = NULL,
    logical.return = FALSE,
    warn.conflicts = FALSE,
    quietly = TRUE,
    verbose = getOption("verbose")
  ) {
    makeFigures::try_or_error(
      {   
        old_global_env = env_clone(global_env())
        rm(list = ls(envir = global_env(), all.names = TRUE), envir = global_env())
        walk(
          packages,
          .attach_package,
          pos = pos,
          lib.loc = lib.loc,
          logical.return = logical.return,
          warn.conflicts = warn.conflicts,
          quietly = quietly,
          verbose = verbose
        )
        set_shims(env)
        walk(data, load, envir = env)
        iwalk(objects, \(p, nm) env_poke(env, nm, p))
      },
      "Error while setting up execution environnement!"
    )
    return(list(packages = packages, global_env = old_global_env))
  },
  reset = function(old) {

    purrr::walk(rev(old$packages), .detach_package)
    rm(list = ls(envir = global_env(), all.names = TRUE), envir = global_env())
    env_coalesce(global_env(), old$global_env)
  }
)

.attach_package = function(
  package,
  pos = 2,
  lib.loc = NULL,
  logical.return = FALSE,
  warn.conflicts = FALSE,
  quietly = TRUE,
  verbose = getOption("verbose")
) {
  if (!(package %in% .packages())) {
    suppressWarnings(suppressPackageStartupMessages((get("library"))(
      package,
      pos = pos,
      lib.loc = lib.loc,
      character.only = TRUE,
      logical.return = logical.return,
      warn.conflicts = warn.conflicts,
      quietly = quietly,
      verbose = verbose
    )))
  }
}

.detach_package = function(package) {
  detach(paste0("package:", package), character.only = TRUE, force = TRUE)
}
