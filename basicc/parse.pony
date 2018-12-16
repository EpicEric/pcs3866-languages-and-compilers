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

class ParserStructuredAutomaton
  let pass: SyntaxParserPass ref
  let automaton: Array[(AutomatonMachine, USize)] = automaton.create()
  var last_label: U32 = -1

  // Var
  var var_name: String = ""
  var var_index: Array[SyntaxExpression] iso = recover var_index.create() end

  // Exp
  let exp_list: Array[SyntaxExpression iso] = exp_list.create()

  // Data
  var data_sign: String = "+"
  var data_number: F32 = 0

  // For
  var for_variable: String = ""
  var for_max: SyntaxExpression iso = recover SyntaxExpressionNumber(1) end
  var for_step: SyntaxExpression iso = recover SyntaxExpressionNumber(1) end

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
          automaton.push((AutomatonProgram, 4))
        else
          _invalid_token(state._1, state._2, token')?
        end

      // Var
      | (AutomatonVar, 0) =>
        _expect_token_category(token', TokenIdentifier)?
        var_name = token'.data
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
            automaton.push((AutomatonVar, 4))
          end
        else _invalid_token(state._1, state._2, token')? end
      | (AutomatonVar, 1) =>
        if MatchStrings(token'.data, "(") then
          _expect_token_category(token', TokenSpecial)?
          automaton.push((AutomatonVar, 3))
          automaton.push((AutomatonExp, 0))
        else
          let name: String = (var_name = "")
          let index: Array[SyntaxExpression] iso =
            (var_index = recover var_index.create() end)
          exp_list.push(recover SyntaxExpressionVariable(
            name,
            consume index) end)
          this.apply(token')?
        end
      | (AutomatonVar, 3) =>
        var_index.push(exp_list.pop()?)
        _expect_token_category(token', TokenIdentifier)?
        match true
        | MatchStrings(token'.data, ",") =>
          automaton.push((AutomatonVar, 3))
          automaton.push((AutomatonExp, 0))
        | MatchStrings(token'.data, ")") =>
          automaton.push((AutomatonVar, 4))
        else _invalid_token(state._1, state._2, token')? end
      | (AutomatonVar, 4) =>
        let name: String = (var_name = "")
        let index: Array[SyntaxExpression] iso =
          (var_index = recover var_index.create() end)
        exp_list.push(recover SyntaxExpressionVariable(
          name,
          consume index) end)
        this.apply(token')?

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
          _pass_error("Expression is not variable", token'.line, token'.column)?
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
          if MatchStrings(data_sign, "+") then
            -data_number
          else data_number end)?
        data_sign = "+"
        if MatchStrings(token'.data, ",") then
          _expect_token_category(token', TokenSpecial)?
          automaton.push((AutomatonData, 1))
        else this.apply(token')? end

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
              and not(_is_letter(for_variable(1)?)))
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
        automaton.push((AutomatonFor, 4))
        _expect_token_category(token', TokenSpecial)?
        if not MatchStrings(token'.data, "=") then
          _invalid_token(state._1, state._2, token')?
        end
      | (AutomatonFor, 4) =>
        automaton.push((AutomatonFor, 5))
        automaton.push((AutomatonExp, 0))
        this.apply(token')?
      | (AutomatonFor, 5) =>
        automaton.push((AutomatonFor, 7))
        automaton.push((AutomatonExp, 0))
        _expect_token_category(token', TokenIdentifier)?
        if not MatchStrings(token'.data, "TO") then
          _invalid_token(state._1, state._2, token')?
        end
        let exp: SyntaxExpression iso = try exp_list.pop()? else error end
        pass.callback(recover SyntaxAttribution(
          recover SyntaxExpressionVariable(for_variable) end,
          consume exp) end)
      | (AutomatonFor, 7) =>
        let exp: SyntaxExpression iso = try exp_list.pop()? else error end
        for_max = consume exp
        if MatchStrings(token'.data, "STEP") then
          automaton.push((AutomatonFor, 9))
          automaton.push((AutomatonExp, 0))
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

  fun _pass_error(message: String, line: USize = -1, column: USize = -1) ? =>
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
    error

  fun _expect_token_category(
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
      error
    end

  fun _invalid_token(
    machine: AutomatonMachine,
    state: USize,
    token: TokenEventWord val
  ) ? =>
    _pass_error(
      "Invalid token '"
        + token.data
        + "'' in automaton '"
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

  fun _parse_float(token: TokenEventWord val): F32 ? =>
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
