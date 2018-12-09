primitive TokenIdentifier
primitive TokenNumber
primitive TokenSpecial
type TokenCategory is
  ( TokenIdentifier
  | TokenNumber
  | TokenSpecial )

class TokenEventWord
  let data: String iso
  let line: USize
  let column: USize
  let category: TokenCategory

  new create(
    data': String iso,
    line': USize,
    column': USize,
    category': TokenCategory)
  =>
    data = consume data'
    line = line'
    column = column'
    category = category'

primitive TokenEOF

type TokenEvent is ( TokenEventWord iso | TokenEOF )

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
      end
    else return false end
    true

actor TokenCategorizerPass
  let coordinator: Coordinator
  let callback: {(TokenEvent)} val
  var pass_error: Bool = false
  var finished: Bool = false
  var data: Array[U8] iso = recover Array[U8] end
  var line: USize = 0
  var column: USize = 0
  var category: (TokenCategory | None) = None

  new create(
    coordinator': Coordinator,
    callback': {(TokenEvent)} val)
  =>
    coordinator = coordinator'
    callback = callback'

  be apply(character: CharacterEvent iso) =>
    if pass_error then return end
    if finished then
      coordinator.pass_error(this, "Cannot tokenize characters after EOF")
      pass_error = true
      return
    end
    let char_type: CharacterType = character.char_type
    let value: U8 = character.character
    match char_type
    | CharacterTypeLetter =>
      match category
      | None => None
      | TokenIdentifier => None
      | TokenNumber =>
        if (value != 'E') and (value != 'e') then commit_token() end
      else commit_token() end
      data.push(value)
      if category is None then
        line = character.line
        column = character.column
        category = TokenIdentifier
      end
    | CharacterTypeDigit =>
      match category
      | None => None
      | TokenIdentifier => None
      | TokenNumber => None
      | TokenSpecial =>
        try
          if
            (data.size() == 1) and ((data(0)? == '+') or (data(0)? == '-'))
          then
            category = TokenNumber
          else commit_token() end
        end
      end
      data.push(value)
      if category is None then
        line = character.line
        column = character.column
        category = TokenNumber
      end
    | CharacterTypeSpecial =>
      // Treat special tokens ["<="; ">="; "<>"]
      match category
      | None => None
      | TokenSpecial =>
        try
          if (data.size() != 1) or not(
            ((value == '=') and ((data(0)? == '<') or (data(0)? == '>'))) or
            ((value == '>') and (data(0)? == '<')))
          then
            commit_token()
          end
        end
      | TokenNumber =>
        if (value != '+') and (value != '-') then commit_token() end
      else commit_token() end
      data.push(value)
      if category is None then
        line = character.line
        column = character.column
        category = TokenSpecial
      end
    | CharacterTypeEOF =>
      commit_token()
      callback(TokenEOF)
      finished = true
    else commit_token() end

  fun ref commit_token() =>
    match category
    | let category': TokenCategory =>
      let data' = (data = recover Array[U8] end)
      callback(recover TokenEventWord(
        String.from_iso_array(consume data'), line, column, category') end)
      category = None
    end
