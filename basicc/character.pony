primitive CharacterTypeLetter
primitive CharacterTypeDigit
primitive CharacterTypeSpecial
primitive CharacterTypeDelimiter
primitive CharacterTypeControl
primitive CharacterTypeEOF
type CharacterType is 
  ( CharacterTypeLetter
  | CharacterTypeDigit
  | CharacterTypeSpecial
  | CharacterTypeDelimiter
  | CharacterTypeControl
  | CharacterTypeEOF )

type CharacterTypeUseful is
  ( CharacterTypeLetter
  | CharacterTypeDigit
  | CharacterTypeSpecial )
type CharacterTypeDisposable is CharacterTypeDelimiter

class CharacterEvent
  let character: U8
  let char_type: CharacterType

  new create(character': U8, char_type': CharacterType) =>
    character = character'
    char_type = char_type'

actor CharacterFilterPass
  let coordinator: Coordinator
  let callback: {(CharacterEvent iso)} val
  var line_number: USize = 0

  new create(
    coordinator': Coordinator,
    callback': {(CharacterEvent iso)} val)
  =>
    coordinator = coordinator'
    callback = callback'

  be apply(line: FileEvent) =>
    try
      match (consume line)
      | let l: FileEventLine iso =>
        line_number = l.number
        let array: Array[U8] iso = (consume l).string.iso_array()
        for c in (consume array).values() do
          callback(this.classify(c)?)
        end
        callback(recover CharacterEvent(0xA, CharacterTypeControl) end)
      | FileEventEOF =>
        callback(recover CharacterEvent(0x0, CharacterTypeEOF) end)
      end
    else
      coordinator.pass_error(
        this, "Invalid character on line " + line_number.string())
    end

  fun tag classify(character: U8): CharacterEvent iso^ ? =>
    if (character >= 0x61) and (character <= 0x7A) then
      recover CharacterEvent(character - 0x20, CharacterTypeLetter) end
    elseif (character >= 0x41) and (character <= 0x5A) then
      recover CharacterEvent(character, CharacterTypeLetter) end
    elseif (character >= 0x30) and (character <= 0x39) then
      recover CharacterEvent(character, CharacterTypeDigit) end
    elseif (character >= 0x21) and (character <= 0x7E) then
      recover CharacterEvent(character, CharacterTypeSpecial) end
    elseif
      (character == 0x9) or (character == 0xA) or (character == 0xD)
      or (character == 0x20)
    then
      recover CharacterEvent(character, CharacterTypeDelimiter) end
    else
      error
    end
