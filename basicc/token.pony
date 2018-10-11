primitive TokenIdentifier
primitive TokenNumber
primitive TokenSpecial
primitive TokenEOF
type TokenCategory is 
  ( TokenIdentifier
  | TokenNumber
  | TokenSpecial
  | TokenEOF )

class TokenEvent
  let data: Array[U8] iso
  let category: TokenCategory

  new create(data': Array[U8] iso, category': TokenCategory) =>
    data = consume data'
    category = category'

actor TokenCategorizerPass
  let coordinator: Coordinator
  let callback: {(TokenEvent iso)} val
  var data: Array[U8] iso = recover Array[U8] end
  var category: (TokenCategory | None) = None

  new create(
    coordinator': Coordinator,
    callback': {(TokenEvent iso)} val)
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
      if category is None then category = TokenIdentifier end
    | CharacterTypeDigit =>
      match category
      | None => None
      | TokenIdentifier => None
      | TokenNumber => None
      else commit_token() end
      data.push(value)
      if category is None then category = TokenNumber end
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
      if category is None then category = TokenSpecial end
    | CharacterTypeEOF =>
      commit_token()
      category = TokenEOF
      commit_token()
    else commit_token() end

  fun ref commit_token() =>
    match category
    | let category': TokenCategory =>
      let data' = data = recover Array[U8] end
      callback(
        recover TokenEvent(consume data', category') end)
      category = None
    end
