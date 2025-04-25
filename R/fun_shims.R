#' @export
#' 
set_shims = function(env){
  if(is_missing(env)) {
    env = caller_env()
  }
  .set_shim("source", env)
  .set_shim("library", env)
  .set_shim("require", env)
}


.set_shim = function(name, env) {
  name_str = as_name(ensym(name))
  env
  env_poke(env, name_str, .shim_factory(name_str))

}


.shim_factory = function(name) {
  function(...) {
    abort(glue("The function {.fn {name}} should not be used in this context!"))
  }
}


