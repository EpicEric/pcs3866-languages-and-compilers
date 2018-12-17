use "collections"
use "debug"

/* Statement labels */

class SyntaxLabel
  let label: U32

  new create(label': U32) =>
    label = label'

/* Compiler labels */

class SyntaxCompilerLabel
  let label: String

  new create(label': String) =>
    label = label'

/* Expressions */

primitive SyntaxAdd
primitive SyntaxSubtract
primitive SyntaxMultiply
primitive SyntaxDivide
primitive SyntaxPower
type SyntaxBinaryOperator is
  ( SyntaxAdd
  | SyntaxSubtract
  | SyntaxMultiply
  | SyntaxDivide
  | SyntaxPower )

primitive SyntaxNegation
primitive SyntaxSine
primitive SyntaxCosine
primitive SyntaxTangent
primitive SyntaxArctangent
primitive SyntaxExponential
primitive SyntaxAbsolute
primitive SyntaxLogarithm
primitive SyntaxSquareRoot
primitive SyntaxInteger
primitive SyntaxRandom

class SyntaxUserDefinedFunctionCall // DEF FNx
  let name: String

  new create(name': String) =>
    name = name'

type SyntaxUnaryOperatorPrimitive is
  ( SyntaxNegation
  | SyntaxSine
  | SyntaxCosine
  | SyntaxTangent
  | SyntaxArctangent
  | SyntaxExponential
  | SyntaxAbsolute
  | SyntaxLogarithm
  | SyntaxSquareRoot
  | SyntaxInteger
  | SyntaxRandom )

type SyntaxUnaryOperator is
  ( SyntaxUnaryOperatorPrimitive
  | SyntaxUserDefinedFunctionCall iso )

class SyntaxExpressionNumber
  let value: F32

  new create(value': F32) =>
    value = value'

class SyntaxExpressionVariable
  let name: String
  let index: (None | Array[SyntaxExpression])

  new create(name': String, index': (None | Array[SyntaxExpression]) = None) =>
    name = name'
    index = index'

class SyntaxExpressionUnary
  let operand: SyntaxExpression
  let operator: SyntaxUnaryOperator

  new create(operand': SyntaxExpression, operator': SyntaxUnaryOperator) =>
    operand = operand'
    operator = consume operator'

class SyntaxExpressionBinary
  let left_operand: SyntaxExpression
  let right_operand: SyntaxExpression
  let operator: SyntaxBinaryOperator

  new create(
    left_operand': SyntaxExpression,
    right_operand': SyntaxExpression,
    operator': SyntaxBinaryOperator
  ) =>
    left_operand = left_operand'
    right_operand = right_operand'
    operator = operator'

type SyntaxExpression is
  ( SyntaxExpressionNumber
  | SyntaxExpressionVariable
  | SyntaxExpressionUnary
  | SyntaxExpressionBinary )

/* LET / READ+DATA attribution */

class SyntaxAttribution
  let variable: SyntaxExpressionVariable
  let expression: SyntaxExpression

  new create(
    variable': SyntaxExpressionVariable,
    expression': SyntaxExpression
  ) =>
    variable = variable'
    expression = expression'

/* PRINT */

class SyntaxPrint
  let list: Array[(String | SyntaxExpression)]

  new create(list': Array[(String | SyntaxExpression)]) =>
    list = list'

/* GOTO */

class SyntaxGoto
  let label: U32

  new create(label': U32) =>
    label = label'

/* Compiler GOTO */

class SyntaxCompilerGoto
  let label: String

  new create(label': String) =>
    label = label'

/* IF / FOR */

primitive SyntaxEqualTo
primitive SyntaxDifferent
primitive SyntaxGreaterThan
primitive SyntaxLesserThan
primitive SyntaxGreaterThanOrEqualTo
primitive SyntaxLesserThanOrEqualTo
type SyntaxComparator is
  ( SyntaxEqualTo
  | SyntaxDifferent
  | SyntaxGreaterThan
  | SyntaxLesserThan
  | SyntaxGreaterThanOrEqualTo
  | SyntaxLesserThanOrEqualTo )

class SyntaxIf
  let left_expression: SyntaxExpression
  let right_expression: SyntaxExpression
  let comparator: SyntaxComparator
  let label: U32

  new create(
    left_expression': SyntaxExpression,
    right_expression': SyntaxExpression,
    comparator': SyntaxComparator,
    label': U32
  ) =>
    left_expression = left_expression'
    right_expression = right_expression'
    comparator = comparator'
    label = label'

/* Compiler IF */

class SyntaxCompilerIf
  let left_expression: SyntaxExpression
  let right_expression: SyntaxExpression
  let comparator: SyntaxComparator
  let label: String

  new create(
    left_expression': SyntaxExpression,
    right_expression': SyntaxExpression,
    comparator': SyntaxComparator,
    label': String
  ) =>
    left_expression = left_expression'
    right_expression = right_expression'
    comparator = comparator'
    label = label'

/* DIM */

class SyntaxDim
  let variable: String
  let dimensions: Array[U32]

  new create(variable': String, dimensions': Array[U32]) =>
    variable = variable'
    dimensions = dimensions'

/* DEF FNx */

class SyntaxUserDefinedFunctionDeclaration
  let name: String
  let variable: String
  let expression: SyntaxExpression

  new create(name': String, variable': String, expression': SyntaxExpression) =>
    name = name'
    variable = variable'
    expression = expression'

/* GOSUB / RETURN */

class SyntaxSubroutine
  let subroutine: U32

  new create(subroutine': U32) =>
    subroutine = subroutine'

primitive SyntaxReturn

/* REM */

class SyntaxRemark
  let remark: String

  new create(remark': String) =>
    remark = remark'

/* Syntax blocks for semantic */

primitive SyntaxEOF

type SyntaxEvent is
  ( SyntaxLabel iso
  | SyntaxCompilerLabel iso
  | SyntaxAttribution iso
  | SyntaxPrint iso
  | SyntaxGoto iso
  | SyntaxCompilerGoto iso
  | SyntaxIf iso
  | SyntaxCompilerIf iso
  | SyntaxDim iso
  | SyntaxUserDefinedFunctionDeclaration iso
  | SyntaxSubroutine iso
  | SyntaxReturn
  | SyntaxRemark iso
  | SyntaxEOF )

actor SyntaxParserPass
  let coordinator: Coordinator
  let callback: {(SyntaxEvent)} val
  var pass_error: Bool = false
  var finished: Bool = false
  var unknown_error: Bool = true

  // Structured automaton
  var automaton: (ParserStructuredAutomaton | None) = None

  // READ / DATA
  let read_list: Array[SyntaxExpressionVariable iso] = read_list.create()
  let data_list: Array[SyntaxExpressionNumber iso] = data_list.create()

  // DIM ({identifier: [count_dim1, count_dim2, ...]})
  let dim_map: Map[String, Array[U32]] = dim_map.create()

  // FOR / NEXT ({var: (return_point, max_exp, step_exp)})
  let for_map: Map[String, (U32, SyntaxExpression, SyntaxExpression)] =
    for_map.create()

  // List of labels for each statement
  let label_list: Array[U32] = Array[U32]

  new create(
    coordinator': Coordinator,
    callback': {(SyntaxEvent)} val)
  =>
    coordinator = coordinator'
    callback = callback'
    automaton = ParserStructuredAutomaton(this)

  be apply(token: TokenEvent) =>
    if pass_error then return end
    if finished then
      coordinator.pass_error(this, "Cannot parse tokens after EOF")
      pass_error = true
      return
    end
    unknown_error = true
    try
      match automaton
      | let a: ParserStructuredAutomaton => a(consume token)?
      else
        coordinator.pass_error(this, "Unexpected structured automaton")
        pass_error = true
        return
      end
    else
      pass_error = true
      if unknown_error then
        coordinator.pass_error(this, "Unknown error")
      end
    end

  fun ref syntax_read(variable: SyntaxExpressionVariable iso) ? =>
    """
    Add variable to read_list and pop attributions if necessary.
    If variable is dimensioned, add multiple variables.
    """
    let name: String = CapitalizeString(variable.name)
    if dim_map.contains(name) then
      match variable.index
      | None =>
        // Multi-dimension variable
        let index_array: Array[U32] = index_array.create()
        let dimensions: Array[U32] = dim_map(name)?
        // Create zero-valued index array
        for _ in dimensions.keys() do
          index_array.push(1)
        end
        // Loop until we reach the value of dimensions
        repeat
          // Add new read command
          let index_array': Array[SyntaxExpression] iso =
            _create_expression_array(index_array)
          read_list.push(recover SyntaxExpressionVariable(
            name,
            consume index_array') end)
          // Iterate dimensions
          var i = index_array.size() - 1
          index_array(i)? = index_array(i)? + 1
          while (index_array(i)? > dimensions(i)?) and (i > 0) do
            index_array(i)? = 1
            i = i - 1
            index_array(i)? = index_array(i)? + 1
          end
        until index_array(0)? > dimensions(0)? end
      else
        // TODO: Check if dimensions match
        read_list.push(consume variable)
      end
    else read_list.push(consume variable) end
    _pop_read_data()?

  fun _create_expression_array(arr: Array[U32]):
    Array[SyntaxExpression] iso^
  =>
    let arr': Array[SyntaxExpression] iso = recover arr'.create() end
    for value in arr.values() do
      arr'.push(recover SyntaxExpressionNumber(F32.from[U32](value)) end)
    end
    consume arr'

  fun ref syntax_data(value: F32) ? =>
    """
    Add value to data_list and pop attributions if necessary.
    """
    data_list.push(recover iso SyntaxExpressionNumber(value) end)
    _pop_read_data()?

  fun ref _pop_read_data() ? =>
    """
    Convert a READ / DATA block into a LET block.
    """
    while (read_list.size() > 0) and (data_list.size() > 0) do
      let variable: SyntaxExpressionVariable iso = read_list.shift()?
      let value: SyntaxExpressionNumber iso = data_list.shift()?
      let event: SyntaxEvent = recover iso SyntaxAttribution(
        consume variable,
        consume value) end
      callback(consume event)
    end

  fun ref syntax_dim(variable: String, dimensions: Array[U32]) ? =>
    let variable': String = CapitalizeString(variable)
    if dim_map.contains(variable') then error end
    dim_map(variable') = dimensions

  fun ref syntax_for(
    variable: String,
    label: U32,
    max_exp: SyntaxExpression,
    step_exp: SyntaxExpression
  ) ? =>
    """
    Save FOR data in map
    """
    let variable': String = CapitalizeString(variable)
    if for_map.contains(variable') then error end
    for_map(variable') = (label, max_exp, step_exp)

  fun ref syntax_next(variable: String) ? =>
    """
    Convert a FOR / NEXT block into a LET / IF block:

    ForLabel:
      ...
      LET STEP = StepExp
      LET MAX = MaxExp
      LET Variable = Variable + STEP
      IF STEP > 0 THEN DescLoop
      IF Variable <= MAX THEN ReturnLoop
      GOTO EscapeLoop
    DescLoop:
      IF Variable >= MAX THEN ReturnLoop
    EscapeLoop:
      ...
    """
    let variable': String = CapitalizeString(variable)
    (let _, let for_data) = for_map.remove(variable')?
    let return_label: U32 = for_data._1
    let max_exp: SyntaxExpression iso = recover iso for_data._2 end
    let step_exp: SyntaxExpression iso = recover iso for_data._3 end
    let desc_loop_label: String = "DESC_" + variable'
    let loop_escape_label: String = "END_LOOP_" + variable'

    let step_attribution_event: SyntaxEvent = recover iso SyntaxAttribution(
      SyntaxExpressionVariable("STEP"),
      consume step_exp) end
    let max_attribution_event: SyntaxEvent = recover iso SyntaxAttribution(
      SyntaxExpressionVariable("MAX"),
      consume max_exp) end
    let step_addition_event: SyntaxEvent = recover iso SyntaxAttribution(
      SyntaxExpressionVariable(variable'),
      SyntaxExpressionBinary(
        SyntaxExpressionVariable(variable'),
        SyntaxExpressionVariable("STEP"),
        SyntaxAdd)) end
    let step_comparation_event: SyntaxEvent = recover iso SyntaxCompilerIf(
      SyntaxExpressionVariable("STEP"),
      SyntaxExpressionNumber(0),
      SyntaxGreaterThan,
      desc_loop_label) end
    let asc_loop_event: SyntaxEvent = recover iso SyntaxIf(
      SyntaxExpressionVariable(variable'),
      SyntaxExpressionVariable("MAX"),
      SyntaxLesserThanOrEqualTo,
      return_label) end
    let escape_loop_event: SyntaxEvent = recover iso SyntaxCompilerGoto(
      loop_escape_label) end
    let desc_label_event: SyntaxEvent = recover iso SyntaxCompilerLabel(
      desc_loop_label) end
    let desc_loop_event: SyntaxEvent = recover iso SyntaxIf(
      SyntaxExpressionVariable(variable'),
      SyntaxExpressionVariable("MAX"),
      SyntaxGreaterThanOrEqualTo,
      return_label) end
    let escape_label_event: SyntaxEvent = recover iso SyntaxCompilerLabel(
      loop_escape_label) end

    callback(consume step_attribution_event)
    callback(consume max_attribution_event)
    callback(consume step_addition_event)
    callback(consume step_comparation_event)
    callback(consume asc_loop_event)
    callback(consume escape_loop_event)
    callback(consume desc_label_event)
    callback(consume desc_loop_event)
    callback(consume escape_label_event)
