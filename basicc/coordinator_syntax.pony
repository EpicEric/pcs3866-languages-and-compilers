use "format"

actor CoordinatorSyntax
  let env: Env
  var token_count: USize = 0
  let token_list: Array[String] = Array[String]

  new create(env': Env, file: String) =>
    env = env'
    let parser = SyntaxParserPass(
      this, {(token: SyntaxEvent)(coordinator: CoordinatorSyntax = this) =>
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
      token_count = token_count + 1
      token_list.push(_format_token("End", ""))
      env.out.print("Syntax pass: Read " + token_count.string()
        + " syntactic structure(s).")
      for token' in token_list.values() do
        env.out.print(token')
      end
    | let label: SyntaxCompilerLabel val =>
      token_count = token_count + 1
      token_list.push(_format_token("Compiler label", label.label))
    | let label: SyntaxLabel val =>
      token_count = token_count + 1
      token_list.push(_format_token("Label", label.label.string()))
    | let attribution: SyntaxAttribution val =>
      token_count = token_count + 1
      let index: String =
        match attribution.variable.index
        | None => ""
        | let index': Array[SyntaxExpression] val =>
          let s: String iso = recover String end
          s.append("(")
          for i in index'.values() do
            s .> append(_exp_to_string(i)) .> append(", ")
          end
          try s .> pop()? .> pop()? .> append(")") end
          consume s
        end
      token_list.push(_format_token(
        "Attribution",
        attribution.variable.name + index + " = "
          + _exp_to_string(attribution.expression)))
    | let print: SyntaxPrint val =>
      token_count = token_count + 1
      let print_list = Array[String]
      for i in print.list.values() do
        print_list.push(match i
          | let s: String => "\"" + s.clone() .> replace("\n", "\\n") + "\", "
          | let e: SyntaxExpression val => _exp_to_string(e) + ", "
          end)
      end
      let print_string: String = String.join(print_list.values())
      token_list.push(_format_token(
        "Print",
        print_string.trim(0, print_string.size() - 2)))
    | let goto: SyntaxGoto val =>
      token_count = token_count + 1
      token_list.push(_format_token("Goto", goto.label.string()))
    | let goto: SyntaxCompilerGoto val =>
      token_count = token_count + 1
      token_list.push(_format_token("Compiler goto", goto.label))
    | let ifbody: SyntaxIf val =>
      token_count = token_count + 1
      let comparator: String =
        match ifbody.comparator
        | SyntaxEqualTo => "=="
        | SyntaxDifferent => "!="
        | SyntaxGreaterThan => ">"
        | SyntaxLesserThan => "<"
        | SyntaxGreaterThanOrEqualTo => ">="
        | SyntaxLesserThanOrEqualTo => "<="
        end
      token_list.push(_format_token(
        "If",
        _exp_to_string(ifbody.left_expression) + " " + comparator + " "
          + _exp_to_string(ifbody.right_expression) + " THEN "
          + ifbody.label.string()))
    | let ifbody: SyntaxCompilerIf val =>
      token_count = token_count + 1
      let comparator: String =
        match ifbody.comparator
        | SyntaxEqualTo => "=="
        | SyntaxDifferent => "!="
        | SyntaxGreaterThan => ">"
        | SyntaxLesserThan => "<"
        | SyntaxGreaterThanOrEqualTo => ">="
        | SyntaxLesserThanOrEqualTo => "<="
        end
      token_list.push(_format_token(
        "Compiler if", 
        _exp_to_string(ifbody.left_expression) + " " + comparator + " "
          + _exp_to_string(ifbody.right_expression) + " THEN "
          + ifbody.label))
    | let dim: SyntaxDim val =>
      token_count = token_count + 1
      let dim_string: String iso = recover String end
      for d in dim.dimensions.values() do
        dim_string .> append(d.string()) .> append(", ")
      end
      let dim_string': String = consume dim_string
      token_list.push(_format_token(
        "Dim",
        dim.variable + "(" + dim_string'.trim(0, dim_string'.size() - 2) + ")"))
    | let def: SyntaxUserDefinedFunctionDeclaration val =>
      token_count = token_count + 1
      token_list.push(_format_token(
        "Definition",
        def.name + "(" + def.variable + ") = "
          + _exp_to_string(def.expression)))
    | let gosub: SyntaxSubroutine val =>
      token_count = token_count + 1
      token_list.push(_format_token("Subroutine", gosub.subroutine.string()))
    | SyntaxReturn =>
      token_count = token_count + 1
      token_list.push(_format_token("Return", ""))
    | let remark: SyntaxRemark val =>
      token_count = token_count + 1
      token_list.push(_format_token("Remark", remark.remark))
    end

  fun _exp_to_string(exp: SyntaxExpression val): String =>
    match exp
    | let e: SyntaxExpressionNumber val =>
      e.value.string()
    | let e: SyntaxExpressionVariable val =>
      match e.index
      | None => e.name
      | let index: Array[SyntaxExpression] val =>
        let string: String iso = recover String end
        string .> append("(")
        for i in index.values() do
          string .> append(_exp_to_string(i)) .> append(", ")
        end
        try string .> pop()? .> pop()? .> append(")") end
        consume string
      end
    | let e: SyntaxExpressionUnary val =>
      let operator =
        match e.operator
        | SyntaxNegation => "Neg"
        | SyntaxSine => "Sin"
        | SyntaxCosine => "Cos"
        | SyntaxTangent => "Tan"
        | SyntaxArctangent => "Arctan"
        | SyntaxExponential => "Exp"
        | SyntaxAbsolute => "Abs"
        | SyntaxLogarithm => "Log"
        | SyntaxSquareRoot => "SqRt"
        | SyntaxInteger => "Int"
        | SyntaxRandom => "Rand"
        | let def: SyntaxUserDefinedFunctionCall val => def.name
        end
      _exp_to_string(e.operand) + " " + operator
    | let e: SyntaxExpressionBinary val =>
      let operator =
        match e.operator
        | SyntaxAdd => "+"
        | SyntaxSubtract => "-"
        | SyntaxMultiply => "*"
        | SyntaxDivide => "/"
        | SyntaxPower => "^"
        end
      _exp_to_string(e.left_operand) + " " + _exp_to_string(e.right_operand)
        + " " + operator
    end

  fun _format_token(name: String, data: String): String =>
    "#"
      + Format(token_count.string() where width = 4)
      + " "
      + Format(name where width = 14, align = AlignRight)
      + if data isnt "" then
          ": " + data
        else "" end

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
    else
      "Unknown pass"
    end
    let error_string: String iso = recover String(
      pass_name.size() + 8 + err.size()) end
    error_string .> append(pass_name) .> append (" error: ") .> append(err)
    env.out.print(consume error_string)
    env.exitcode(2)
