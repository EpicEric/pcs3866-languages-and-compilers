primitive TokenIdentifier
primitive TokenNumber
primitive TokenSpecial
type TokenCategory is 
  ( TokenIdentifier
  | TokenNumber
  | TokenSpecial )

class TokenEventWord
  let data: Array[U8] iso
  let line: USize
  let column: USize
  let category: TokenCategory

  new create(
    data': Array[U8] iso,
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

actor TokenCategorizerPass
  let coordinator: Coordinator
  let callback: {(TokenEvent)} val
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
    let char_type: CharacterType = character.char_type
    let value: U8 = character.character
    match char_type
    | CharacterTypeLetter =>
      match category
      | None => None
      | TokenIdentifier => None
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
      else commit_token() end
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
    else commit_token() end

  fun ref commit_token() =>
    match category
    | let category': TokenCategory =>
      let data' = (data = recover Array[U8] end)
      callback(recover TokenEventWord(
        consume data', line, column, category') end)
      category = None
    end
