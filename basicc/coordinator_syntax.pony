actor TestSyntaxCoordinator
  let env: Env

  new create(env': Env, file: String) =>
    env = env'
    let parser = SyntaxParserPass(
      this, {(token: SyntaxEvent)(coordinator: TestSyntaxCoordinator = this) =>
        coordinator(consume token)} val)
    let categorizer = TokenCategorizerPass(
      this, {(token: TokenEvent) =>
        parser(consume token)} val)
    let filter = CharacterFilterPass(
      this, {(char: CharacterEvent iso) =>
        categorizer(consume char)} val)
    let reader = FileReaderPass(
      this, {(line: FileEvent) =>
        filter(consume line)} val)
    try reader(file, env.root as AmbientAuth) end

  be apply(token: SyntaxEvent) =>
    match consume token
    | SyntaxEOF =>
      env.out.print("EOF")
    | let label: SyntaxLabel val =>
      env.out.print("Label: " + label.label.string())
    | let attribution: SyntaxAttribution val =>
      env.out.print("Attribution: " + attribution.variable.name + " = //TODO")
    | let print: SyntaxPrint val =>
      let print_list = Array[String]
      for i in print.list.values() do
        print_list.push(match i
          | let s: String => "\"" + s + "\", "
          | let e: SyntaxExpression val => "//TODO" + ", "
          end)
      end
      let print_string: String = String.join(print_list.values())
      env.out.print("Print: " + print_string.trim(0, print_string.size() - 2))
    | let goto: SyntaxGoto val =>
      env.out.print("Go to: " + goto.label.string())
    | let ifbody: SyntaxIf val =>
      let comparator: String =
        match ifbody.comparator
        | SyntaxEqualTo => "eq"
        | SyntaxDifferent => "ne"
        | SyntaxGreaterThan => "gt"
        | SyntaxLesserThan => "lt"
        | SyntaxGreaterThanOrEqualTo => "ge"
        | SyntaxLesserThanOrEqualTo => "le"
        end
      env.out.print("If: //TODO " + comparator + " //TODO -> "
        + ifbody.label.string())
    | let gosub: SyntaxSubroutine val =>
      env.out.print("Subroutine: " + gosub.subroutine.string())
    | SyntaxReturn =>
      env.out.print("Return")
    | let remark: SyntaxRemark val =>
      env.out.print("Remark: " + remark.remark)
    // else // TODO
    end

  be pass_error(pass: Pass, err: String = "Unknown error") =>
    let pass_name = match pass
    | let p': FileReaderPass =>
      "File reader"
    | let p': CharacterFilterPass =>
      "Character filter"
    | let p': TokenCategorizerPass =>
      "Token categorizer"
    | let p': SyntaxParserPass =>
      "Syntax parser"
    // else
    //   "Unknown pass"
    end
    let error_string: String iso = recover String(
      pass_name.size() + 8 + err.size()) end
    error_string .> append(pass_name) .> append (" error: ") .> append(err)
    env.out.print(consume error_string)
    env.exitcode(2)
