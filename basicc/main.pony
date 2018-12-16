actor Main
  new create(env: Env) =>
    try
      TestSyntaxCoordinator(env, env.args(1)?)
    else
      env.out.print("Missing file parameter")
      env.exitcode(1)
    end
