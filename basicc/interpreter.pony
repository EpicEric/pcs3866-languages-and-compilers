use "buffered"
use "collections"
use "random"
use "time"

actor SemanticInterpreterPass
  let coordinator: Coordinator
  let stream: OutStream
  var pass_error: Bool = false
  var finished_parse: Bool = false
  var finished_run: Bool = false
  var unknown_error: Bool = true

  let output: Writer = Writer

  var pointer: USize = 0
  let program: Array[SyntaxEvent val] = program.create()
  var variables: Map[String, F32] = variables.create()
  let call_stack: Array[USize] = call_stack.create()

  new create(
    coordinator': Coordinator,
    stream': OutStream
  ) =>
    coordinator = coordinator'
    stream = stream'

  be apply(syntax: SyntaxEvent) =>
    if pass_error then return end
    if finished_parse then
      coordinator.pass_error(this, "Cannot read syntactic structures after EOF")
      pass_error = true
      return
    end
    let syntax': SyntaxEvent val = consume syntax
    match syntax'
    | SyntaxEOF =>
      finished_parse = true
      _run()
    else
      program.push(syntax')
    end

  be _run() => None
    unknown_error = true
    try
      if pass_error then return end
      if finished_run then
        coordinator.pass_error(this, "Cannot run commands after end of program")
        pass_error = true
        return
      end
      if pointer == program.size() then
        _finish()
        return
      end
      let command: SyntaxEvent val = program(pointer)?
      match command

      | let c: SyntaxLabel val =>
        pointer = pointer + 1

      | let c: SyntaxCompilerLabel val =>
        pointer = pointer + 1

      | let c: SyntaxAttribution val =>
        pointer = pointer + 1
        match c.variable.index
        | None =>
          variables(c.variable.name) = _evaluate(c.expression)?
        | let index: Array[SyntaxExpression] val =>
          coordinator.pass_error(
            this,
            "Dimensioned variable attribution is unimplemented")
          pass_error = true
        end

      | let c: SyntaxPrint val =>
        pointer = pointer + 1
        for print in c.list.values() do
          match print
          | let s: String => output.write(s)
          | let e: SyntaxExpression val =>
            match e
            | let e': SyntaxExpressionUnary val =>
              if e'.operator is SyntaxInteger then
                output.write(_evaluate(e'.operand)?.u32().string())
              else output.write(_evaluate(e')?.string()) end
            else output.write(_evaluate(e)?.string()) end
          end
        end

      | let c: SyntaxGoto val =>
        var found: Bool = false
        for (count, command') in program.pairs() do
          match command'
          | let label: SyntaxLabel val =>
            if label.label == c.label then
              found = true
              pointer = count
              break
            end
          end
        end
        if not(found) then
          coordinator.pass_error(
            this,
            "Unknown label '" + c.label.string() + "'")
          pass_error = true
        end

      | let c: SyntaxCompilerGoto val =>
        var found: Bool = false
        for (count, command') in program.pairs() do
          match command'
          | let label: SyntaxCompilerLabel val =>
            if MatchStrings(label.label, c.label) then
              found = true
              pointer = count
              break
            end
          end
        end
        if not(found) then
          coordinator.pass_error(
            this,
            "Unknown compiler label '" + c.label + "'")
          pass_error = true
        end

      | let c: SyntaxIf val =>
        let left: F32 = _evaluate(c.left_expression)?
        let right: F32 = _evaluate(c.right_expression)?
        let result: F32 = left - right
        var new_pointer: USize = pointer
        var found: Bool = false
        for (count, command') in program.pairs() do
          match command'
          | let label: SyntaxLabel val =>
            if label.label == c.label then
              found = true
              new_pointer = count
              break
            end
          end
        end
        if not(found) then
          coordinator.pass_error(
            this,
            "Unknown label '" + c.label.string() + "'")
          pass_error = true
          return
        end
        pointer =
          if result < 0 then
            match c.comparator
            | SyntaxDifferent => new_pointer
            | SyntaxLesserThan => new_pointer
            | SyntaxLesserThanOrEqualTo => new_pointer
            else pointer + 1 end
          else
            if result > 0 then
              match c.comparator
              | SyntaxDifferent => new_pointer
              | SyntaxGreaterThan => new_pointer
              | SyntaxGreaterThanOrEqualTo => new_pointer
              else pointer + 1 end
            else
              match c.comparator
              | SyntaxEqualTo => new_pointer
              | SyntaxGreaterThanOrEqualTo => new_pointer
              | SyntaxLesserThanOrEqualTo => new_pointer
              else pointer + 1 end
            end
          end

      | let c: SyntaxCompilerIf val =>
        let left: F32 = _evaluate(c.left_expression)?
        let right: F32 = _evaluate(c.right_expression)?
        let result: F32 = left.sub(right)
        var new_pointer: USize = pointer
        var found: Bool = false
        for (count, command') in program.pairs() do
          match command'
          | let label: SyntaxCompilerLabel val =>
            if MatchStrings(label.label, c.label) then
              found = true
              new_pointer = count
              break
            end
          end
        end
        if not(found) then
          coordinator.pass_error(
            this,
            "Unknown compiler label '" + c.label + "'")
          pass_error = true
          return
        end
        pointer =
          if result < 0 then
            match c.comparator
            | SyntaxDifferent => new_pointer
            | SyntaxLesserThan => new_pointer
            | SyntaxLesserThanOrEqualTo => new_pointer
            else pointer + 1 end
          else
            if result > 0 then
              match c.comparator
              | SyntaxDifferent => new_pointer
              | SyntaxGreaterThan => new_pointer
              | SyntaxGreaterThanOrEqualTo => new_pointer
              else pointer + 1 end
            else
              match c.comparator
              | SyntaxEqualTo => new_pointer
              | SyntaxGreaterThanOrEqualTo => new_pointer
              | SyntaxLesserThanOrEqualTo => new_pointer
              else pointer + 1 end
            end
          end

      // | let c: SyntaxDim val =>

      // | let c: SyntaxUserDefinedFunctionDeclaration val =>

      | let c: SyntaxSubroutine val =>
        call_stack.push(pointer + 1)
        var found: Bool = false
        for (count, command') in program.pairs() do
          match command'
          | let label: SyntaxLabel val =>
            if label.label == c.subroutine then
              found = true
              pointer = count
              break
            end
          end
        end
        if not(found) then
          coordinator.pass_error(
            this,
            "Unknown label '" + c.subroutine.string() + "'")
          pass_error = true
        end

      | let c: SyntaxReturn =>
        try
          pointer = call_stack.pop()?
        else
          coordinator.pass_error(this, "Cannot return with empty call stack")
          pass_error = true
        end

      | let c: SyntaxRemark val =>
        pointer = pointer + 1

      else
        coordinator.pass_error(this, "Unimplemented command")
        pass_error = true
      end
      _run()
    else
      if unknown_error then
        coordinator.pass_error(this, "Unexpected error")
        pass_error = true
      end
    end

  fun ref _evaluate(exp: SyntaxExpression val): F32 ? =>
    try
      match exp
      | let e': SyntaxExpressionNumber val => e'.value
      | let e': SyntaxExpressionVariable val =>
        match e'.index
        | None =>
          try
            variables(e'.name)?
          else
            unknown_error = false
            coordinator.pass_error(
              this,
              "Cannot read variable '" + e'.name + "'")
            pass_error = true
            error
          end
        | let index: Array[SyntaxExpression] val =>
          unknown_error = false
          coordinator.pass_error(
            this,
            "Dimensioned variable read is unimplemented")
          pass_error = true
          error
        end
      | let e': SyntaxExpressionUnary val =>
        let e1: F32 = _evaluate(e'.operand)?
        match e'.operator
        | SyntaxNegation => e1.neg()
        | SyntaxSine => e1.sin()
        | SyntaxCosine => e1.cos()
        | SyntaxTangent => e1.tan()
        | SyntaxArctangent => e1.atan()
        | SyntaxExponential => e1.exp()
        | SyntaxAbsolute => e1.abs()
        | SyntaxLogarithm => e1.log()
        | SyntaxSquareRoot => e1.sqrt()
        | SyntaxInteger => e1.u32().f32()
        | SyntaxRandom => e1.mul(Rand(Time.seconds().u64()).real().f32())
        | let def: SyntaxUserDefinedFunctionCall val =>
          unknown_error = false
          coordinator.pass_error(
            this,
            "Function definition evaluation is unimplemented")
          pass_error = true
          error
        end
      | let e': SyntaxExpressionBinary val =>
        let e1: F32 = _evaluate(e'.left_operand)?
        let e2: F32 = _evaluate(e'.right_operand)?
        match e'.operator
        | SyntaxAdd => e1.add(e2)
        | SyntaxSubtract => e1.sub(e2)
        | SyntaxMultiply => e1.mul(e2)
        | SyntaxDivide => e1.div(e2)
        | SyntaxPower => e1.pow(e2)
        end
      end
    else
      if unknown_error then
        unknown_error = false
        coordinator.pass_error(this, "Invalid expression")
        pass_error = true
      end
      error
    end

  fun ref _finish() =>
    output.u8('\n')
    stream.writev(output.done())
    finished_run = true
