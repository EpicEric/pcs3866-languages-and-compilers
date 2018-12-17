use "cli"

actor Main
  new create(env: Env) =>
    let cs =
      try
        CommandSpec.leaf("basicc", "A Dartmouth BASIC compiler", [
          OptionSpec.string("pass", "Which pass to run the compiler for. " +
            "Must be one of: < token | syntax | all >" where short' = 'p',
            default' = "all")
        ], [
          ArgSpec.string("file", "File to read from")
        ])? .> add_help(where descr' = "Prints help")?
      else
        env.exitcode(-1)
        return
      end

    let cmd =
      match CommandParser(cs).parse(env.args, env.vars)
      | let c: Command => c
      | let ch: CommandHelp =>
        ch.print_help(env.out)
        env.exitcode(0)
        return
      | let se: SyntaxError =>
        env.out.print(se.string())
        env.exitcode(1)
        return
      end
    
    match cmd.option("pass").string()
    | "token" =>
      TestTokenCoordinator(env, cmd.arg("file").string())
    | "syntax" =>
      TestSyntaxCoordinator(env, cmd.arg("file").string())
    | "all" =>
      TestSyntaxCoordinator(env, cmd.arg("file").string()) //TODO
    else
      env.out.print(cs.help_string())
      env.exitcode(1)
    end
