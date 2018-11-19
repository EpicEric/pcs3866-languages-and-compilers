
actor SyntaxParserPass
  let coordinator: Coordinator
  let callback: {(TokenEvent)} val //FIXME
  var pass_error: Bool = false

  new create(
    coordinator': Coordinator,
    callback': {(TokenEvent)} val) //FIXME
  =>
    coordinator = coordinator'
    callback = callback'

  be apply(character: TokenEvent) =>
    if pass_error then return end
    coordinator.pass_error(this, "SyntaxParserPass is unimplemented")
    pass_error = true
