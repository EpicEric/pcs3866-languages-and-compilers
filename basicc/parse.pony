primitive AutomatonProgram
  fun string(): String => "Program"
primitive AutomatonStatement
  fun string(): String => "Statement"
primitive AutomatonAssign
  fun string(): String => "Assign"
primitive AutomatonVar
  fun string(): String => "Var"
primitive AutomatonExp
  fun string(): String => "Exp"
primitive AutomatonEb
  fun string(): String => "Eb"
primitive AutomatonRead
  fun string(): String => "Read"
primitive AutomatonData
  fun string(): String => "Data"
primitive AutomatonPrint
  fun string(): String => "Print"
primitive AutomatonPitem
  fun string(): String => "Pitem"
primitive AutomatonGoto
  fun string(): String => "Goto"
primitive AutomatonIf
  fun string(): String => "If"
primitive AutomatonFor
  fun string(): String => "For"
primitive AutomatonNext
  fun string(): String => "Next"
primitive AutomatonDim
  fun string(): String => "Dim"
primitive AutomatonDef
  fun string(): String => "Def"
primitive AutomatonGosub
  fun string(): String => "Gosub"
primitive AutomatonReturn
  fun string(): String => "Return"
primitive AutomatonRemark
  fun string(): String => "Remark"
type AutomatonMachine is
  ( AutomatonProgram
  | AutomatonStatement
  | AutomatonAssign
  | AutomatonVar
  | AutomatonExp
  | AutomatonEb
  | AutomatonRead
  | AutomatonData
  | AutomatonPrint
  | AutomatonPitem
  | AutomatonGoto
  | AutomatonIf
  | AutomatonFor
  | AutomatonNext
  | AutomatonDim
  | AutomatonDef
  | AutomatonGosub
  | AutomatonReturn
  | AutomatonRemark )

primitive MatchStrings
  fun apply(first: String, second: String): Bool =>
    if first.size() != second.size() then return false end
    var i: USize = 0
    try
      while i < first.size() do
        let first_char: U8 =
          if (first(i)? >= 0x61) and (first(i)? <= 0x7A) then
            first(i)? - 0x20
          else first(i)? end
        let second_char: U8 =
          if (second(i)? >= 0x61) and (second(i)? <= 0x7A) then
            second(i)? - 0x20
          else second(i)? end
        if (first_char != second_char) then return false end
        i = i + 1
      end
    else return false end
    true

  fun prefix(original: String, prefix': String): Bool =>
    if original.size() < prefix'.size() then return false end
    var i: USize = 0
    try
      while i < prefix'.size() do
        let first_char: U8 =
          if (original(i)? >= 0x61) and (original(i)? <= 0x7A) then
            original(i)? - 0x20
          else original(i)? end
        let second_char: U8 =
          if (prefix'(i)? >= 0x61) and (prefix'(i)? <= 0x7A) then
            prefix'(i)? - 0x20
          else prefix'(i)? end
        if (first_char != second_char) then return false end
        i = i + 1
      end
    else return false end
    true

  fun suffix(original: String, suffix': String): Bool =>
    if original.size() < suffix'.size() then return false end
    var i: USize = original.size() - suffix'.size()
    try
      while i < original.size() do
        let first_char: U8 =
          if (original(i)? >= 0x61) and (original(i)? <= 0x7A) then
            original(i)? - 0x20
          else original(i)? end
        let second_char: U8 =
          if (suffix'(i)? >= 0x61) and (suffix'(i)? <= 0x7A) then
            suffix'(i)? - 0x20
          else suffix'(i)? end
        if (first_char != second_char) then return false end
        i = i + 1
      end
    else return false end
    true

primitive CapitalizeString
  fun apply(string: String): String =>
    let string': String iso = recover String(string.size()) end
    for char in string.values() do
      string'.push(
        if (char >= 0x61) and (char <= 0x7A) then char - 0x20 else char end)
    end
    consume string'

class ParserStructuredAutomaton
  let pass: SyntaxParserPass ref
  let automaton: Array[(AutomatonMachine, USize)] = automaton.create()
  var last_label: U32 = -1

  // Assign
  var assign_var: SyntaxExpressionVariable iso =
    recover SyntaxExpressionVariable("") end

  // Var
  var var_name: Array[String] = var_name.create()
  var var_index: Array[Array[SyntaxExpression] iso] = var_index.create()

  // Exp
  // None represents noop at the end of Eb
  let exp_unop: Array[(SyntaxUnaryOperator | None)] = exp_unop.create()
  // None represents open-brace
  let exp_binop: Array[(SyntaxBinaryOperator | None)] = exp_binop.create()
  let exp_list: Array[SyntaxExpression iso] = exp_list.create()

  // Data
  var data_sign: String = "+"
  var data_number: F32 = 0

  // Print
  var print_list: Array[(String | SyntaxExpression)] iso =
    recover print_list.create() end

  // If
  var if_comparator: SyntaxComparator = SyntaxEqualTo

  // For
  var for_variable: String = ""
  var for_max: SyntaxExpression iso = recover SyntaxExpressionNumber(1) end
  var for_step: SyntaxExpression iso = recover SyntaxExpressionNumber(1) end

  // Dim
  var dim_variable: String = ""
  var dim_array: Array[U32] = dim_array.create()

  // Def
  var def_name: String = ""
  var def_variable: String = ""

  // Remark
  var remark_list: Array[String] = remark_list.create()

  new create(pass': SyntaxParserPass ref) =>
    pass = pass'
    automaton.push((AutomatonProgram, 0))

  fun ref apply(token: TokenEvent val) ? =>
    let state = try
      automaton.pop()?
    else
      _pass_error("Structured automaton stack is empty")?
      error
    end
    match token
    | let token': TokenEventWord val =>
      match state

      // Program
      | (AutomatonProgram, 0) =>
        _expect_token_category(token', TokenNumber)?
        let label: U32 = try
          token'.data.u32()?
        else
          _pass_error(
            "Label '"
              + token'.data
              + "' is not an integer",
            token'.line, token'.column)?
          error
        end
        if (last_label != -1) and (label <= last_label) then
          _pass_error(
            "Label '"
              + label.string()
              + "' must be bigger than previous label '"
              + last_label.string()
              + "'",
            token'.line, token'.column)?
        end
        last_label = label
        pass.callback(recover SyntaxLabel(label) end)
        automaton.push((AutomatonProgram, 1))
      | (AutomatonProgram, 1) =>
        automaton.push((AutomatonProgram, 2))
        _expect_token_category(token', TokenIdentifier)?
        match true
        | MatchStrings(token'.data, "LET") =>
          automaton.push((AutomatonAssign, 1))
        | MatchStrings(token'.data, "READ") =>
          automaton.push((AutomatonRead, 1))
        | MatchStrings(token'.data, "DATA") =>
          automaton.push((AutomatonData, 1))
        | MatchStrings(token'.data, "PRINT") =>
          automaton.push((AutomatonPrint, 1))
        | MatchStrings(token'.data, "GO") =>
          automaton.push((AutomatonGoto, 1))
        | MatchStrings(token'.data, "GOTO") =>
          automaton.push((AutomatonGoto, 2))
        | MatchStrings(token'.data, "IF") =>
          automaton.push((AutomatonIf, 1))
        | MatchStrings(token'.data, "FOR") =>
          automaton.push((AutomatonFor, 1))
        | MatchStrings(token'.data, "NEXT") =>
          automaton.push((AutomatonNext, 1))
        | MatchStrings(token'.data, "DIM") =>
          automaton.push((AutomatonDim, 1))
        | MatchStrings(token'.data, "DEF") =>
          automaton.push((AutomatonDef, 1))
        | MatchStrings(token'.data, "GOSUB") =>
          automaton.push((AutomatonGosub, 1))
        | MatchStrings(token'.data, "RETURN") =>
          automaton.push((AutomatonReturn, 1))
        | MatchStrings(token'.data, "REM") =>
          automaton.push((AutomatonRemark, 1))
        else
          _invalid_token(state._1, state._2, token')?
        end
      | (AutomatonProgram, 2) =>
        _expect_token_category(token', TokenNumber)?
        let label: U32 = try
          token'.data.u32()?
        else
          _pass_error(
            "Label '"
              + token'.data
              + "' is not an integer",
            token'.line, token'.column)?
          error
        end
        if (last_label != -1) and (label <= last_label) then
          _pass_error(
            "Label '"
              + label.string()
              + "' must be bigger than previous label '"
              + last_label.string()
              + "'",
            token'.line, token'.column)?
        end
        last_label = label
        pass.callback(recover SyntaxLabel(label) end)
        automaton.push((AutomatonProgram, 3))
      | (AutomatonProgram, 3) =>
        automaton.push((AutomatonProgram, 2))
        _expect_token_category(token', TokenIdentifier)?
        match true
        | MatchStrings(token'.data, "LET") =>
          automaton.push((AutomatonAssign, 1))
        | MatchStrings(token'.data, "READ") =>
          automaton.push((AutomatonRead, 1))
        | MatchStrings(token'.data, "DATA") =>
          automaton.push((AutomatonData, 1))
        | MatchStrings(token'.data, "PRINT") =>
          automaton.push((AutomatonPrint, 1))
        | MatchStrings(token'.data, "GO") =>
          automaton.push((AutomatonGoto, 1))
        | MatchStrings(token'.data, "GOTO") =>
          automaton.push((AutomatonGoto, 2))
        | MatchStrings(token'.data, "IF") =>
          automaton.push((AutomatonIf, 1))
        | MatchStrings(token'.data, "FOR") =>
          automaton.push((AutomatonFor, 1))
        | MatchStrings(token'.data, "NEXT") =>
          automaton.push((AutomatonNext, 1))
        | MatchStrings(token'.data, "DIM") =>
          automaton.push((AutomatonDim, 1))
        | MatchStrings(token'.data, "DEF") =>
          automaton.push((AutomatonDef, 1))
        | MatchStrings(token'.data, "GOSUB") =>
          automaton.push((AutomatonGosub, 1))
        | MatchStrings(token'.data, "RETURN") =>
          automaton.push((AutomatonReturn, 1))
        | MatchStrings(token'.data, "REM") =>
          automaton.push((AutomatonRemark, 1))
        | MatchStrings(token'.data, "END") =>
          automaton.pop()?
          automaton.push((AutomatonProgram, 4))
        else
          _invalid_token(state._1, state._2, token')?
        end

      // Assign
      | (AutomatonAssign, 1) =>
        automaton.push((AutomatonAssign, 2))
        automaton.push((AutomatonVar, 0))
        this.apply(token')?
      | (AutomatonAssign, 2) =>
        _expect_token_category(token', TokenSpecial)?
        if not MatchStrings(token'.data, "=") then
          _invalid_token(state._1, state._2, token')?
        end
        assign_var = (exp_list.pop()?) as SyntaxExpressionVariable iso^
        automaton.push((AutomatonAssign, 4))
        automaton.push((AutomatonExp, 0))
        exp_unop.push(None)
      | (AutomatonAssign, 4) =>
        let leftside: SyntaxExpressionVariable iso =
          (assign_var = recover assign_var.create("") end)
        let rightside: SyntaxExpression iso = exp_list.pop()?
        pass.callback(recover SyntaxAttribution(
          consume leftside, consume rightside) end)
        this.apply(token')?

      // Var
      | (AutomatonVar, 0) =>
        _expect_token_category(token', TokenIdentifier)?
        var_name.push(CapitalizeString(token'.data))
        match token'.data.size()
        | 1 =>
          if not(_is_letter(token'.data(0)?)) then
            _invalid_token(state._1, state._2, token')?
          else
            automaton.push((AutomatonVar, 1))
          end
        | 2 =>
          if
            not(_is_letter(token'.data(0)?)) or not(_is_digit(token'.data(1)?))
          then
            _invalid_token(state._1, state._2, token')?
          else
            var_index.push(recover Array[SyntaxExpression] end)
            automaton.push((AutomatonVar, 4))
          end
        else _invalid_token(state._1, state._2, token')? end
      | (AutomatonVar, 1) =>
        if MatchStrings(token'.data, "(") then
          _expect_token_category(token', TokenSpecial)?
          var_index.push(recover Array[SyntaxExpression] end)
          automaton.push((AutomatonVar, 3))
          automaton.push((AutomatonExp, 0))
          exp_unop.push(None)
        else
          let name: String = var_name.pop()?
          exp_list.push(recover SyntaxExpressionVariable(
            CapitalizeString(name),
            None) end)
          this.apply(token')?
        end
      | (AutomatonVar, 3) =>
        let index: Array[SyntaxExpression] iso = var_index.pop()?
        index.push(exp_list.pop()?)
        var_index.push(consume index)
        _expect_token_category(token', TokenIdentifier)?
        match true
        | MatchStrings(token'.data, ",") =>
          automaton.push((AutomatonVar, 3))
          automaton.push((AutomatonExp, 0))
          exp_unop.push(None)
        | MatchStrings(token'.data, ")") =>
          automaton.push((AutomatonVar, 4))
        else _invalid_token(state._1, state._2, token')? end
      | (AutomatonVar, 4) =>
        let name: String = var_name.pop()?
        let index: Array[SyntaxExpression] iso = var_index.pop()?
        if index.size() > 0 then
          exp_list.push(recover SyntaxExpressionVariable(
            CapitalizeString(name),
            consume index) end)
        else
          exp_list.push(recover SyntaxExpressionVariable(
            CapitalizeString(name),
            None) end)
        end
        this.apply(token')?

      // Exp
      | (AutomatonExp, 0) =>
        match true
        | MatchStrings(token'.data, "+") =>
          _expect_token_category(token', TokenSpecial)?
          automaton.push((AutomatonExp, 0))
        | MatchStrings(token'.data, "-") =>
          _expect_token_category(token', TokenSpecial)?
          automaton.push((AutomatonExp, 0))
          if exp_unop.pop()? is None then
            exp_unop.push(SyntaxNegation)
          else
            exp_unop.push(None)
          end
        else
          automaton.push((AutomatonExp, 1))
          automaton.push((AutomatonEb, 0))
          this.apply(token')?
        end
      | (AutomatonExp, 1) =>
        try
          let exp: SyntaxExpression iso = exp_list.pop()?
          if exp_unop.pop()? is SyntaxNegation then
            exp_list.push(
              recover SyntaxExpressionUnary(consume exp, SyntaxNegation) end)
          else
            exp_list.push(consume exp)
          end
        else
          _pass_error(
            "Missing unary operator on expression", token'.line, token'.column)?
        end
        match true
        | (MatchStrings(token'.data, "+") or MatchStrings(token'.data, "-")) =>
          _expect_token_category(token', TokenSpecial)?
          while
            (exp_binop.size() > 0)
              and (exp_binop(exp_binop.size() - 1)? isnt None)
          do
            let binop: SyntaxBinaryOperator =
              exp_binop.pop()? as SyntaxBinaryOperator
            try
              let right_exp: SyntaxExpression iso = exp_list.pop()?
              let left_exp: SyntaxExpression iso = exp_list.pop()?
              exp_list.push(recover SyntaxExpressionBinary(
                consume left_exp, consume right_exp, binop) end)
            else
              _pass_error(
                "Insufficient operands in expression",
                token'.line,
                token'.column)?
            end
          end
          exp_binop.push(
            if MatchStrings(token'.data, "+") then
              SyntaxAdd
            else
              SyntaxSubtract
            end)
          automaton.push((AutomatonExp, 1))
          automaton.push((AutomatonEb, 0))
          exp_unop.push(None)
        | (MatchStrings(token'.data, "*") or MatchStrings(token'.data, "/")) =>
          _expect_token_category(token', TokenSpecial)?
          while
            (exp_binop.size() > 0)
              and (exp_binop(exp_binop.size() - 1)? isnt None)
              and (exp_binop(exp_binop.size() - 1)? isnt SyntaxAdd)
              and (exp_binop(exp_binop.size() - 1)? isnt SyntaxSubtract)
          do
            let binop: SyntaxBinaryOperator =
              exp_binop.pop()? as SyntaxBinaryOperator
            try
              let right_exp: SyntaxExpression iso = exp_list.pop()?
              let left_exp: SyntaxExpression iso = exp_list.pop()?
              exp_list.push(recover SyntaxExpressionBinary(
                consume left_exp, consume right_exp, binop) end)
            else
              _pass_error(
                "Insufficient operands in expression",
                token'.line,
                token'.column)?
            end
          end
          exp_binop.push(
            if MatchStrings(token'.data, "*") then
              SyntaxMultiply
            else
              SyntaxDivide
            end)
          automaton.push((AutomatonExp, 1))
          automaton.push((AutomatonEb, 0))
          exp_unop.push(None)
        | MatchStrings(token'.data, "^") =>
          _expect_token_category(token', TokenSpecial)?
          while
            (exp_binop.size() > 0)
              and (exp_binop(exp_binop.size() - 1)? is SyntaxPower)
          do
            let binop: SyntaxBinaryOperator =
              exp_binop.pop()? as SyntaxBinaryOperator
            try
              let right_exp: SyntaxExpression iso = exp_list.pop()?
              let left_exp: SyntaxExpression iso = exp_list.pop()?
              exp_list.push(recover SyntaxExpressionBinary(
                consume left_exp, consume right_exp, binop) end)
            else
              _pass_error(
                "Insufficient operands in expression",
                token'.line,
                token'.column)?
            end
          end
          exp_binop.push(SyntaxPower)
          automaton.push((AutomatonExp, 1))
          automaton.push((AutomatonEb, 0))
          exp_unop.push(None)
        else
          while
            (exp_binop.size() > 0)
              and (exp_binop(exp_binop.size() - 1)? isnt None)
          do
            let binop: SyntaxBinaryOperator =
              exp_binop.pop()? as SyntaxBinaryOperator
            try
              let right_exp: SyntaxExpression iso = exp_list.pop()?
              let left_exp: SyntaxExpression iso = exp_list.pop()?
              exp_list.push(recover SyntaxExpressionBinary(
                consume left_exp, consume right_exp, binop) end)
            else
              _pass_error(
                "Insufficient operands in expression",
                token'.line,
                token'.column)?
            end
          end
          this.apply(token')?
        end

      // Eb
      | (AutomatonEb, 0) =>
        match true
        | MatchStrings(token'.data, "(") =>
          _expect_token_category(token', TokenSpecial)?
          automaton.push((AutomatonEb, 2))
          automaton.push((AutomatonExp, 0))
          exp_unop.push(None) // Eb operation
          exp_unop.push(None) // Exp sign
          exp_binop.push(None) // Open braces
        | MatchStrings.prefix(token'.data, "FN") =>
          _expect_token_category(token', TokenIdentifier)?
          automaton.push((AutomatonEb, 5))
          if (token'.data.size() != 3) or not(_is_letter(token'.data(2)?)) then
            _pass_error(
              "Invalid function name '" + token'.data + "'",
              token'.line,
              token'.column)?
          end
          exp_unop.push(recover SyntaxUserDefinedFunctionCall(
            CapitalizeString(token'.data)) end)
        | MatchStrings(token'.data, "SIN") =>
          _expect_token_category(token', TokenIdentifier)?
          automaton.push((AutomatonEb, 5))
          exp_unop.push(SyntaxSine)
        | MatchStrings(token'.data, "COS") =>
          _expect_token_category(token', TokenIdentifier)?
          automaton.push((AutomatonEb, 5))
          exp_unop.push(SyntaxCosine)
        | MatchStrings(token'.data, "TAN") =>
          _expect_token_category(token', TokenIdentifier)?
          automaton.push((AutomatonEb, 5))
          exp_unop.push(SyntaxTangent)
        | MatchStrings(token'.data, "ATN") =>
          _expect_token_category(token', TokenIdentifier)?
          automaton.push((AutomatonEb, 5))
          exp_unop.push(SyntaxArctangent)
        | MatchStrings(token'.data, "EXP") =>
          _expect_token_category(token', TokenIdentifier)?
          automaton.push((AutomatonEb, 5))
          exp_unop.push(SyntaxExponential)
        | MatchStrings(token'.data, "ABS") =>
          _expect_token_category(token', TokenIdentifier)?
          automaton.push((AutomatonEb, 5))
          exp_unop.push(SyntaxAbsolute)
        | MatchStrings(token'.data, "LOG") =>
          _expect_token_category(token', TokenIdentifier)?
          automaton.push((AutomatonEb, 5))
          exp_unop.push(SyntaxLogarithm)
        | MatchStrings(token'.data, "SQR") =>
          _expect_token_category(token', TokenIdentifier)?
          automaton.push((AutomatonEb, 5))
          exp_unop.push(SyntaxSquareRoot)
        | MatchStrings(token'.data, "INT") =>
          _expect_token_category(token', TokenIdentifier)?
          automaton.push((AutomatonEb, 5))
          exp_unop.push(SyntaxInteger)
        | MatchStrings(token'.data, "RND") =>
          _expect_token_category(token', TokenIdentifier)?
          automaton.push((AutomatonEb, 5))
          exp_unop.push(SyntaxRandom)
        else
          automaton.push((AutomatonEb, 3))
          if token'.category is TokenNumber then
            let float: F32 = _parse_float(token')?
            exp_list.push(recover SyntaxExpressionNumber(float) end)
          else
            automaton.push((AutomatonVar, 0))
            this.apply(token')?
          end
        end
      | (AutomatonEb, 2) =>
        _expect_token_category(token', TokenSpecial)?
        if not MatchStrings(token'.data, ")") then
          _pass_error(
            "Unbalanced parentheses; expected ')'",
            token'.line,
            token'.column)?
        end
        automaton.push((AutomatonEb, 3))
        var exp = exp_list.pop()?
        let unop: (None | SyntaxUnaryOperator) =
          try
            exp_unop.pop()?
          else
            _pass_error(
              "Missing operator in expression",
              token'.line,
              token'.column)?
            error
          end
        match consume unop
        | None => None
        | let unop': SyntaxUserDefinedFunctionCall iso =>
          exp = recover SyntaxExpressionUnary(consume exp, consume unop') end
        | let unop': SyntaxUnaryOperatorPrimitive =>
          exp = recover SyntaxExpressionUnary(consume exp, unop') end
        end
        // Remove open braces, if applicable
        if
          (exp_binop.size() > 0)
            and (exp_binop(exp_binop.size() - 1)? is None)
        then exp_binop.pop()? end
        // Apply stacked operators
        while
          (exp_binop.size() > 0)
            and (exp_binop(exp_binop.size() - 1)? isnt None)
        do
          let binop: SyntaxBinaryOperator =
            exp_binop.pop()? as SyntaxBinaryOperator
          exp =
            try
              let left_exp: SyntaxExpression iso = exp_list.pop()?
              recover SyntaxExpressionBinary(
                consume left_exp, consume exp, binop) end
            else
              _pass_error(
                "Insufficient operands in closed braces expression",
                token'.line,
                token'.column)?
              error
            end
        end
        exp_list.push(consume exp)
      | (AutomatonEb, 3) =>
        this.apply(token')?
      | (AutomatonEb, 5) =>
        _expect_token_category(token', TokenSpecial)?
        if not MatchStrings(token'.data, "(") then
          _invalid_token(state._1, state._2, token')?
        end
        automaton.push((AutomatonEb, 2))
        automaton.push((AutomatonExp, 0))
        exp_unop.push(None)

      // Read
      | (AutomatonRead, 1) =>
        automaton.push((AutomatonRead, 2))
        automaton.push((AutomatonVar, 0))
        this.apply(token')?
      | (AutomatonRead, 2) =>
        let exp: SyntaxExpression iso = exp_list.pop()?
        match (consume exp)
        | let variable: SyntaxExpressionVariable iso =>
          pass.syntax_read(consume variable)?
        else
          _pass_error(
            "Expression is not a variable",
            token'.line,
            token'.column)?
        end
        if MatchStrings(token'.data, ",") then
          _expect_token_category(token', TokenSpecial)?
          automaton.push((AutomatonRead, 2))
          automaton.push((AutomatonVar, 0))
        else this.apply(token')? end

      // Data
      | (AutomatonData, 1) =>
        match true
        | MatchStrings(token'.data, "+") =>
          automaton.push((AutomatonData, 2))
        | MatchStrings(token'.data, "-") =>
          data_sign = token'.data
          automaton.push((AutomatonData, 2))
        else
          _expect_token_category(token', TokenNumber)?
          data_number = _parse_float(token')?
          automaton.push((AutomatonData, 3))
        end
      | (AutomatonData, 2) =>
        _expect_token_category(token', TokenNumber)?
        data_number = _parse_float(token')?
        automaton.push((AutomatonData, 3))
      | (AutomatonData, 3) =>
        pass.syntax_data(
          if MatchStrings(data_sign, "-") then
            -data_number
          else data_number end)?
        data_sign = "+"
        if MatchStrings(token'.data, ",") then
          _expect_token_category(token', TokenSpecial)?
          automaton.push((AutomatonData, 1))
        else this.apply(token')? end

      // Print
      | (AutomatonPrint, 1) =>
        match token'.category
        | TokenNumber =>
          if print_list.size() == 0 then print_list.push("\n") end
          let final_print_list = (print_list = recover print_list.create() end)
          pass.callback(recover SyntaxPrint(consume final_print_list) end)
          this.apply(token')?
        else
          if print_list.size() > 0 then print_list.push(" ") end
          automaton.push((AutomatonPrint, 2))
          automaton.push((AutomatonPitem, 0))
          this.apply(token')?
        end
      | (AutomatonPrint, 2) =>
        match token'.category
        | TokenNumber =>
          print_list.push("\n")
          let final_print_list = (print_list = recover print_list.create() end)
          pass.callback(recover SyntaxPrint(consume final_print_list) end)
          this.apply(token')?
        else
          _expect_token_category(token', TokenSpecial)?
          if not MatchStrings(token'.data, ",") then
            _invalid_token(state._1, state._2, token')?
          end
          automaton.push((AutomatonPrint, 1))
        end

      // Pitem
      | (AutomatonPitem, 0) =>
        match token'.category
        | TokenString =>
          if not(MatchStrings.prefix(token'.data, "\""))
            // or not(MatchStrings.suffix(token'.data, "\""))
          then
            _invalid_token(state._1, state._2, token')?
          end
          if token'.data.size() > 2 then
            print_list.push(token'.data.trim(1, token'.data.size() - 1))
          end
          automaton.push((AutomatonPitem, 1))
        else
          automaton.push((AutomatonPitem, 2))
          automaton.push((AutomatonExp, 0))
          exp_unop.push(None)
          this.apply(token')?
        end
      | (AutomatonPitem, 1) =>
        match token'.category
        | TokenNumber =>
          this.apply(token')?
        else
          if not(MatchStrings(token'.data, ",")) then
            automaton.push((AutomatonPitem, 2))
            automaton.push((AutomatonExp, 0))
            exp_unop.push(None)
          end
          this.apply(token')?
        end
      | (AutomatonPitem, 2) =>
        print_list.push(exp_list.pop()?)
        this.apply(token')?

      // Goto
      | (AutomatonGoto, 1) =>
        _expect_token_category(token', TokenIdentifier)?
        if not MatchStrings(token'.data, "TO") then
          _invalid_token(state._1, state._2, token')?
        end
        automaton.push((AutomatonGoto, 2))
      | (AutomatonGoto, 2) =>
        _expect_token_category(token', TokenNumber)?
        let label: U32 = try
          token'.data.u32()?
        else
          _pass_error(
            "GOTO label '"
              + token'.data
              + "' is not an integer",
            token'.line, token'.column)?
          error
        end
        pass.callback(recover SyntaxGoto(label) end)

      // If
      | (AutomatonIf, 1) =>
        automaton.push((AutomatonIf, 2))
        automaton.push((AutomatonExp, 0))
        exp_unop.push(None)
        this.apply(token')?
      | (AutomatonIf, 2) =>
        _expect_token_category(token', TokenSpecial)?
        match true
        | MatchStrings(token'.data, ">=") =>
          if_comparator = SyntaxGreaterThanOrEqualTo
        | MatchStrings(token'.data, ">") =>
          if_comparator = SyntaxGreaterThan
        | MatchStrings(token'.data, "<>") =>
          if_comparator = SyntaxDifferent
        | MatchStrings(token'.data, "<") =>
          if_comparator = SyntaxLesserThan
        | MatchStrings(token'.data, "<=") =>
          if_comparator = SyntaxLesserThanOrEqualTo
        | MatchStrings(token'.data, "=") =>
          if_comparator = SyntaxEqualTo
        else
          _invalid_token(state._1, state._2, token')?
        end
        automaton.push((AutomatonIf, 4))
        automaton.push((AutomatonExp, 0))
        exp_unop.push(None)
      | (AutomatonIf, 4) =>
        _expect_token_category(token', TokenIdentifier)?
        if not MatchStrings(token'.data, "THEN") then
          _invalid_token(state._1, state._2, token')?
        end
        automaton.push((AutomatonIf, 5))
      | (AutomatonIf, 5) =>
        _expect_token_category(token', TokenNumber)?
        let label: U32 = try
          token'.data.u32()?
        else
          _pass_error(
            "IF label '"
              + token'.data
              + "' is not an integer",
            token'.line, token'.column)?
          error
        end
        let second_exp: SyntaxExpression iso = exp_list.pop()?
        let first_exp: SyntaxExpression iso = exp_list.pop()?
        pass.callback(recover SyntaxIf(
          consume first_exp, consume second_exp, if_comparator, label) end)

      // For
      | (AutomatonFor, 1) =>
        automaton.push((AutomatonFor, 2))
        _expect_token_category(token', TokenIdentifier)?
        for_variable = token'.data
        // Variable should be letter [ digit ]
        if
          (for_variable.size() > 2)
            or (not(_is_letter(for_variable(0)?)))
            or ((for_variable.size() == 2)
              and not(_is_digit(for_variable(1)?)))
        then
          _invalid_token(state._1, state._2, token')?
        end
        if pass.for_map.contains(for_variable) then
          _pass_error(
            "Cannot use variable '"
              + for_variable
              + "' in nested loops",
            token'.line, token'.column)?
        end
      | (AutomatonFor, 2) =>
        _expect_token_category(token', TokenSpecial)?
        if not MatchStrings(token'.data, "=") then
          _invalid_token(state._1, state._2, token')?
        end
        automaton.push((AutomatonFor, 5))
        automaton.push((AutomatonExp, 0))
        exp_unop.push(None)
      | (AutomatonFor, 5) =>
        automaton.push((AutomatonFor, 7))
        automaton.push((AutomatonExp, 0))
        exp_unop.push(None)
        _expect_token_category(token', TokenIdentifier)?
        if not MatchStrings(token'.data, "TO") then
          _invalid_token(state._1, state._2, token')?
        end
        let exp: SyntaxExpression iso = try exp_list.pop()? else error end
        pass.callback(recover SyntaxAttribution(
          recover SyntaxExpressionVariable(CapitalizeString(for_variable)) end,
          consume exp) end)
      | (AutomatonFor, 7) =>
        let exp: SyntaxExpression iso = try exp_list.pop()? else error end
        for_max = consume exp
        if MatchStrings(token'.data, "STEP") then
          automaton.push((AutomatonFor, 9))
          automaton.push((AutomatonExp, 0))
          exp_unop.push(None)
          _expect_token_category(token', TokenIdentifier)?
        else
          _expect_token_category(token', TokenNumber)?
          let label: U32 = try
            token'.data.u32()?
          else
            _pass_error(
              "Label '"
                + token'.data
                + "' is not an integer",
              token'.line, token'.column)?
            error
          end
          pass.syntax_for(
            for_variable,
            label,
            for_max = recover SyntaxExpressionNumber(1) end,
            for_step = recover SyntaxExpressionNumber(1) end)?
          this.apply(token')?
        end
      | (AutomatonFor, 9) =>
        let exp: SyntaxExpression iso = try exp_list.pop()? else error end
        for_step = consume exp
        _expect_token_category(token', TokenNumber)?
        let label: U32 = try
          token'.data.u32()?
        else
          _pass_error(
            "Label '"
              + token'.data
              + "' is not an integer",
            token'.line, token'.column)?
          error
        end
        pass.syntax_for(
          for_variable,
          label,
          for_max = recover SyntaxExpressionNumber(1) end,
          for_step = recover SyntaxExpressionNumber(1) end)?
        this.apply(token')?

      // Next
      | (AutomatonNext, 1) =>
        _expect_token_category(token', TokenIdentifier)?
        for_variable = token'.data
        // Variable should be letter [ digit ]
        if
          (for_variable.size() > 2)
            or (not(_is_letter(for_variable(0)?)))
            or ((for_variable.size() == 2)
              and not(_is_digit(for_variable(1)?)))
        then
          _invalid_token(state._1, state._2, token')?
        end
        if not(pass.for_map.contains(for_variable)) then
          _pass_error(
            "Variable '" + for_variable + "' cannot be looped without FOR",
            token'.line, token'.column)?
        end
        pass.syntax_next(for_variable)?

      // Dim
      | (AutomatonDim, 1) =>
        _expect_token_category(token', TokenIdentifier)?
        if (token'.data.size() != 1) or not(_is_letter(token'.data(0)?)) then
          _invalid_token(state._1, state._2, token')?
        end
        dim_variable = token'.data
        dim_array = dim_array.create()
        automaton.push((AutomatonDim, 2))
      | (AutomatonDim, 2) =>
        _expect_token_category(token', TokenSpecial)?
        if not(MatchStrings(token'.data, "(")) then
          _invalid_token(state._1, state._2, token')?
        end
        automaton.push((AutomatonDim, 3))
      | (AutomatonDim, 3) =>
        _expect_token_category(token', TokenNumber)?
        let dimension: U32 = try
          token'.data.u32()?
        else
          _pass_error(
            "DIM dimension '"
              + token'.data
              + "' is not an integer",
            token'.line, token'.column)?
          error
        end
        dim_array.push(dimension)
        automaton.push((AutomatonDim, 4))
      | (AutomatonDim, 4) =>
        _expect_token_category(token', TokenSpecial)?
        match true
        | MatchStrings(token'.data, ",") =>
          automaton.push((AutomatonDim, 3))
        | MatchStrings(token'.data, ")") =>
          let dim_syntax_array: Array[U32] iso =
            recover dim_syntax_array.create() end
          for d in dim_array.values() do
            dim_syntax_array.push(d)
          end
          pass.callback(
            recover SyntaxDim(
              CapitalizeString(dim_variable),
              consume dim_syntax_array) end)
          try
            pass.syntax_dim(CapitalizeString(dim_variable), dim_array)?
          else
            _pass_error(
              "DIM variable '" + CapitalizeString(dim_variable)
                + "' already defined",
              token'.line,
              token'.column)?
          end
          automaton.push((AutomatonDim, 5))
        else _invalid_token(state._1, state._2, token')? end
      | (AutomatonDim, 5) =>
        if MatchStrings(token'.data, ",") then
          _expect_token_category(token', TokenSpecial)?
          automaton.push((AutomatonDim, 1))
        else
          this.apply(token')?
        end

      // Def
      | (AutomatonDef, 1) =>
        _expect_token_category(token', TokenIdentifier)?
        if
          not(MatchStrings.prefix(token'.data, "FN"))
            or (token'.data.size() != 3)
            or not(_is_letter(token'.data(2)?))
        then  _invalid_token(state._1, state._2, token')? end
        def_name = token'.data
        automaton.push((AutomatonDef, 3))
      | (AutomatonDef, 3) =>
        _expect_token_category(token', TokenSpecial)?
        if not(MatchStrings(token'.data, "(")) then
          _invalid_token(state._1, state._2, token')?
        end
        automaton.push((AutomatonDef, 4))
      | (AutomatonDef, 4) =>
        _expect_token_category(token', TokenIdentifier)?
        def_variable = token'.data
        // Variable should be letter [ digit ]
        if
          (def_variable.size() > 2)
            or (not(_is_letter(def_variable(0)?)))
            or ((def_variable.size() == 2)
              and not(_is_digit(def_variable(1)?)))
        then
          _invalid_token(state._1, state._2, token')?
        end
        automaton.push((AutomatonDef, 6))
      | (AutomatonDef, 6) =>
        _expect_token_category(token', TokenSpecial)?
        if not(MatchStrings(token'.data, ")")) then
          _invalid_token(state._1, state._2, token')?
        end
        automaton.push((AutomatonDef, 7))
      | (AutomatonDef, 7) =>
        _expect_token_category(token', TokenSpecial)?
        if not(MatchStrings(token'.data, "=")) then
          _invalid_token(state._1, state._2, token')?
        end
        automaton.push((AutomatonDef, 9))
        automaton.push((AutomatonExp, 0))
        exp_unop.push(None)
      | (AutomatonDef, 9) =>
        let exp: SyntaxExpression iso = exp_list.pop()?
        pass.callback(recover SyntaxUserDefinedFunctionDeclaration(
          CapitalizeString(def_name),
          CapitalizeString(def_variable),
          consume exp) end)
        this.apply(token')?

      // Gosub
      | (AutomatonGosub, 1) =>
        _expect_token_category(token', TokenNumber)?
        let label: U32 = try
          token'.data.u32()?
        else
          _pass_error(
            "GOSUB label '"
              + token'.data
              + "' is not an integer",
            token'.line, token'.column)?
          error
        end
        pass.callback(recover SyntaxSubroutine(label) end)

      // Return
      | (AutomatonReturn, 1) =>
        pass.callback(SyntaxReturn)
        this.apply(token')?

      // Remark
      | (AutomatonRemark, 1) =>
        match token'.category
        | TokenNumber =>
          // Remover espaço separador se houver
          try remark_list.pop()? end
          let remark_string: String iso = String.join(
            (remark_list = remark_list.create()).values())
          pass.callback(recover SyntaxRemark(consume remark_string) end)
          // Exit to program machine
          this.apply(token')?
        else
          remark_list.push(token'.data)
          remark_list.push(" ")
          automaton.push((AutomatonRemark, 1))
        end

      // Unknown automaton + state
      else
        _pass_error(
          "Automaton '"
            + state._1.string()
            + "' reached invalid state "
            + state._2.string(),
          token'.line, token'.column)?
      end

    // EOF
    | TokenEOF =>
      if
        (state._1 is AutomatonProgram)
          and (state._2 == 4)
          and (automaton.size() == 0)
      then
        pass.callback(SyntaxEOF)
        pass.finished = true
      else
        _pass_error(
          "Unexpected EOF in automaton '"
            + state._1.string()
            + "' state "
            + state._2.string())?
      end
    end

  fun ref _pass_error(message: String, line: USize = -1, column: USize = -1) ? =>
    if (line != -1) and (column != -1) then
      pass.coordinator.pass_error(pass,
        "Line "
          + line.string()
          + " column "
          + column.string()
          + ": "
          + message)
    else
      pass.coordinator.pass_error(pass, message)
    end
    pass.unknown_error = false
    error

  fun ref _expect_token_category(
    token: TokenEventWord val,
    category: TokenCategory
  ) ? =>
    if token.category isnt category then
      _pass_error(
        "Invalid token category '"
          + token.category.string()
          + "' for token '"
          + token.data
          + "'; expected category '"
          + category.string()
          + "'",
        token.line, token.column)?
    end

  fun ref _invalid_token(
    machine: AutomatonMachine,
    state: USize,
    token: TokenEventWord val
  ) ? =>
    _pass_error(
      "Invalid token '"
        + token.data
        + "' in automaton '"
        + machine.string()
        + "' state "
        + state.string(),
      token.line, token.column)?

  fun _is_letter(character: U8): Bool =>
    try
      CharacterClassifier(character)? is CharacterTypeLetter
    else false end

  fun _is_digit(character: U8): Bool =>
    try
      CharacterClassifier(character)? is CharacterTypeDigit
    else false end

  fun ref _parse_float(token: TokenEventWord val): F32 ? =>
    let float = token.data.f32()
    if
      (float == 0)
        and (not(MatchStrings(token.data, "0")))
        and (not(MatchStrings(token.data, ".0")))
        and (not(MatchStrings(token.data, "0.0")))
    then _pass_error(
      "Could not parse '" + token.data + "' as float. If using a zero value, "
        + "change '" + token.data + "' to '0' in your code.",
      token.line,
      token.column)? end
    float
