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

  // Exp
  let exp_list: Array[SyntaxExpression iso] = exp_list.create()

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
          automaton.push((AutomatonRead, 2))
          automaton.push((AutomatonVar, 0))
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

      // Read
      | (AutomatonRead, 2) =>
        if MatchStrings(token'.data, ",") then
          _expect_token_category(token', TokenSpecial)?
          automaton.push((AutomatonRead, 2))
          automaton.push((AutomatonVar, 0))
        else this.apply(token')? end

      // For
      | (AutomatonFor, 1) =>
        automaton.push((AutomatonFor, 2))
        _expect_token_category(token', TokenIdentifier)?
        for_variable = token'.data
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
