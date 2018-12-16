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

  new create(pass': SyntaxParserPass ref) =>
    pass = pass'
    automaton.push((AutomatonProgram, 0))

  fun ref apply(token: TokenEvent) ? =>
    let state = try
      automaton.pop()?
    else
      _pass_error("Structured automaton stack is empty")?
      error
    end
    match consume token
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
          + ":"
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
