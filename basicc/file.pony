use "files"

primitive FileEventEOF

class FileEventLine
  let number: USize
  let string: String iso

  new create(number': USize, string': String iso) =>
    number = number'
    string = consume string'

type FileEvent is (FileEventEOF | FileEventLine iso)

actor FileReaderPass
  let coordinator: Coordinator
  let callback: {(FileEvent)} val

  new create(coordinator': Coordinator, callback': {(FileEvent)} val) =>
    coordinator = coordinator'
    callback = callback'

  be apply(filename: String, auth: AmbientAuth) =>
    let caps = recover val FileCaps .> set(FileRead) .> set(FileStat) end
    try
      let path: FilePath = FilePath(auth, filename, caps)?
      let file: File = OpenFile(path) as File
      var line_number: USize = 1
      for line in FileLines(file) do
        callback(recover FileEventLine(
          (line_number = line_number + 1),
          consume line) 
        end)
      end
      callback(FileEventEOF)
    else
      coordinator.pass_error(
        this,
        "Couldn't open file '" + filename + "'.")
    end
