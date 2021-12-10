package aoc
package y2021

object Day10:
  import Parser.*

  private val brace   = char(')').as(Some(3))
  private val bracket = char(']').as(Some(57))
  private val curly   = char('}').as(Some(1197))
  private val angle   = char('>').as(Some(25137))
  private val closing = brace <|> bracket <|> curly <|> angle

  private lazy val mismatchedBarce: Parser[Option[Int]] =
    char('(') *> (parser <*> ((char(')') <|> eof).as(None) <|> closing)).map(_ orElse _)
  private lazy val mismatchedBracket: Parser[Option[Int]] =
    char('[') *> (parser <*> ((char(']') <|> eof).as(None) <|> closing)).map(_ orElse _)
  private lazy val mismatchedCurlie: Parser[Option[Int]] =
    char('{') *> (parser <*> ((char('}') <|> eof).as(None) <|> closing)).map(_ orElse _)
  private lazy val mismatchedAngle: Parser[Option[Int]] =
    char('<') *> (parser <*> ((char('>') <|> eof).as(None) <|> closing)).map(_ orElse _)

  private lazy val parser: Parser[Option[Int]] =
    (mismatchedBarce <|> mismatchedBracket <|> mismatchedCurlie <|> mismatchedAngle).many().map(_.flatten.headOption)

  def solve(input: String): Int =
    input.linesIterator
      .flatMap(line => parser.run(line))
      .sum

  private lazy val incompleteBarce: Parser[Long] =
    char('(') *> (parser2 <*> (eof.as(1L) <|> closing.as(0L))).map(_ * 5 + _)
  private lazy val incompleteBracket: Parser[Long] =
    char('[') *> (parser2 <*> (eof.as(2L) <|> closing.as(0L))).map(_ * 5 + _)
  private lazy val incompleteCurlie: Parser[Long] =
    char('{') *> (parser2 <*> (eof.as(3L) <|> closing.as(0L))).map(_ * 5 + _)
  private lazy val incompleteAngles: Parser[Long] =
    char('<') *> (parser2 <*> (eof.as(4L) <|> closing.as(0L))).map(_ * 5 + _)

  private lazy val parser2: Parser[Long] =
    (incompleteBarce <|> incompleteBracket <|> incompleteCurlie <|> incompleteAngles)
      .many()
      .map(_.foldLeft(0L)(_ * 5 + _))

  def solve2(input: String): Any =
    val result = input.linesIterator
      .filter(line => parser.run(line).isEmpty)
      .map(line => parser2.run(line))
      .toList
      .sorted
    println(result)
    result(result.length / 2)

  val input =
    """[<(<<([[[{<[{[[]()](<>())}<{()[]}<[]<>>>](<[()[]][()()]>)>[{{(()())(<>())}({()[]}{{}{}})}[(
      |{{{(({[<<[{[(<()()>[<>{}])<<()()>[[]]>]{{<(){}>{()[]}}([()<>]<<>{}>)}}([[<[][]>({}{})]<({}[])
      |<[[[(<[{{{(<({()<>)([][]))(<{}()>{[][]})>{[[{}()][[][]]]({{}[]}[()()])})}[{<([()()]{()<>}){([]
      |[{[(<<{({{<<<{<>[]}{{}}>[({}<>){<>}]>[<<{}()>{<>[]}>[(()())({}())]]>}}({[(<({}{})<<>{}>>{<()()><[
      |{{[{<<{[<{([({[]{}})])}>]}>><<<[[[<[([<>{}])<{[]{}}(<><>))]>]]({([{(<>())(()[])}<<()<>>>]<
      |((<<{<[{[((<([()[]])<{<>{}}>>((<{}[]><(){}>)[<()<>>(<>())])}[<{<()[]>{()()}}>])]([{<{<()[]>({}<>)}>}]{
      |{<[<<({{<<{(<<()()>((){})>[{<>()}{(){}}]){(<()()>){([]{})[()]}}}[([<<>[]>{{}}])]>>(<{([[[][]]({
      |<({{[[[[[<[(<((){})({})>)[<({}<>)(<>)>{{<>}}]]><[<{[[][]]<<>{}>}{(<>()){[]{}}}>{({()}<()[]
      |<<{<{{[[([<{{[(){}]<()[]>}<{<><>}[[][]]>}>[[[<()[]>]<{<>()}<[]{}>>]}]{<(<<<>{}>(<>[])>[{()(
      |({{(({<([(<{{[[][]]<{}<>>}{{[]<>}}}>[({{()<>}(<>[])}<<<>[]>>){[([]{})]<{()<>}{(){}}>}])[<[{[{}[]]}[
      |<([<{[{(<[[[((()[])[[]()]){[[]()](()<>)}][<(<>[]){()()}>{(<><>)[<>]}]]][((({{}[]}{[]()}))<[[{}<>]<[]
      |{[[([<[<{({{{[<>{}]{(){}}}{{[]{}}<{}<>>}}<(<()<>>{()[]})<(<>())[[]()]>>}[<[{[]<>}(()()))<<[]{}>{<
      |{<<<(([(((<[{(()[]){()<>}}]>)([<[<<><>>((){})]>][{[[{}[]][{}<>]]<<()<>>[{}[]]>}[[<<>{}>[{}[]]]<{[]()}>]]
      |{{[(<<<(({{<<{{}<>}[<>[]]>([[]<>]<{}{}>)>{({{}{}}{[]<>})}}}[[{<{<><>}(<>())>{[()]}}<[(<>{})
      |((({([{[[<{{([{}][{}<>]){[[]{}]{{}<>}}}[<{<>{}}><[[]{}]<<>()>>]}([(<()[]>[<>])[[[]()](())]
      |({{<[{{<(([<{<<>[]><{}{}>}[(<>{})<()[]>]><[[[][]](()())](<[]()>(<>[]))>]{<{<{}[]>[{}[]>}><[[[]{}]<[]{}
      |{{<[{<[<{[{((<[]<>>{{}[]})[([]<>)(()())])}][({[({}()){{}<>}]}({<[][]><<><>>}<[()<>]<[]<>>>))(<<<<>{}><(){}>
      |{[<[{<{((<[(<([][])[(){}]>{([]())<[][]>})<{<<><>>{<>}}>]<<[{{}()}{<><>}][[{}()]{[]<>)]>{[{[]<>}<{}{}>]((<>
      |<(<({(([{<[((<[]{}>[()[]]){((){})[(){}]})[[<[][]>{()<>}]({{}()})]]([(<<>()>{<>})<({}{})({}<>)>]((<<
      |<<<[<<[(<<{{(<<>()>([]()))[{(){}}{{}[]}]}{<[(){}]>{[<>[]]([]<>)}}}([<<<><>>([]<>)]]{{<()()>{{}[]}}[{<
      |{[<(<([([[<[{{<>{}}<{}()>}{<[]<>><<>()>}][{((){})[{}<>]}<{<>()}{<>{}}>]>{{<<[][]>[<>[]]><<()<>><[]<>>>}
      |[{({<[{{([[[{({}<>)[<>[])}[[[][]]({}())]]](<<<(){}>[<>]>({(){}})>[(({}()){{}{}})<<<>{}>{<>[]}>])])}
      |<[[<<((((<<([<()<>>{[]()}]({{}{}}<[]()>))>>{<<<{<>{}}>{(()[]){[][]}}>(({{}{}})[[<><>]{{}{}
      |({[({<[{{[(([{<>()}([][])](<<>{}><[]>))[{{[]<>}<<>()>><[<>()]>])]}<<{<([()<>]){{(){}}(()())}
      |((<{{<(<{<<({(<>{})}<[{}<>]{[]<>}>){{[<>]<<><>>}{[<>()]((){})}}>({({{}()}{()()}){[<>()]<<>{}>}}(<({}<>)([]())
      |{{[<{({{[[({<([]<>)(()<>)>([{}<>]{()()})}([{{}{}}<<>[]>]{<()[]>{()()}}))]<<[{[<>()]<{}[]]}[[[]()
      |[((<<{(<{(([{[<>{}]({}[])}([{}[]][()<>])])[[[(<>())([]<>)]]]){<([{[]{}}[<>()]]<[<>[]][<>{}]>)(<({}{})
      |((<([(<<([{(<<<>{}><<><>>>[{[]()}])[{[<>{}][[]{}]}]}([[{<>()}({}[])]<<{}>{(){}}>]{[<[]<>>[(){}]>})][<<((()()
      |<[<[<{({{{{({[[][]][<>[]]}<[[][]]>)[[<[]>((){})]]}<(<([]()){[][]}>[<<><>><[]<>>]]{(<<><>>)[<()[]>
      |{<[((<(<[({<({[]()}([]{}])<[<>{}]{{}<>}>>({<()()><<>{}>}{[<><>]<<>[]>})}{[((<>)[[]()])]})[({{[()[]]<
      |<{({[[{<{[<{([()[]][{}{}])}>({[<()()>(<><>)]<[()<>]([][])>}([<{}<>>(<>[])]({<>[]}(<>()))))]}(([{(([])
      |<<(([[[({(<({[{}()]<()()>}<[{}{}]<[]()>>){[(<>{})[<>[]]][(()<>)(<>())]}>(<<[()()]([]{})>[[[]{}]{(){}}]
      |({<{<<<{<<{(<<[]{}>([]{})><{{}}[()<>]>)}[([([]{})[{}[]]]{{<><>}[()<>]})]>{{<{<<>{}>}(<[]{}>[[]])>[<<()()><()
      |(<<{[<{<{(<{[[{}[]]<()[]>]([()][{}[]])}(({()[]})({<><>}<{}<>>))>[[[<[][]>{(){}}]][<[(){}][{}]
      |[[{{({[<<([{{[<><>]{<>{}}}}])><[{[{[[][]]{()<>>}{({}<>){[]}}]({{<>{}}<[]()>})}<{[<()<>>]([()[]][()])}{{<[]
      |[([({<[[[[<<<[<>][<><>]><{[][]}{<>[]}>><{[[]]<<>{}>}>>[[(({}<>){{}<>})[<()[]>[()[]]]]]>{([{{<>}<
      |{{[<[[<<{<{({{(){}}{(){}}}({<>()}{{}{}}))((({}())({}<>))([{}()][{}<>]))}>({(({()[]}){(<><>)<{}>}](<
      |[({[<{{([[{([{()<>}<<>{}>]{<[]{}>})}({{[<>{})<{}()>}[[[][]]<{}[]>]})]]{<{<[<{}()>]>{{{()}(<>
      |{[(([[(([<[(({()}<()<>>)<(<>[])({}[])>)]>])({[([[({}<>){<>()}](<{}<>>)](<((){})[<>[]])))([<([]())({}<>)>{[{}
      |{([[({[[(<<(<[<>()]{{}()}>)<<(()[])>>><(((<>{})[[][]])[[[][]]{<>[]}])({<<>{}><{}[]>}<{{}[]}>)>>[{(([{}
      |[(([<{[<<[<{<[{}{}]{[]()}><(<>())([]{})>}{[{[]()}[{}[]]]((()<>){()()})}>{([{<>[]}([]{})]<{[][]}({}{})>){{[[]{
      |[<{<[<[([{{{{<<><>>}((()()){[]()})}{{{{}<>}([]{})}([<><>]{()()])}}{{({<>{}}<{}{}>)[<<><>>({}())]}}}<
      |[<[[<{[((({(([<>{}]([][])){{[]()}<[][]>})[{[()<>}<()<>>}{<{}[]>}]}(<([<>{}]([]())){({}[])[<>{
      |[{{<{<[<{([({([]())([]{})}{<()[]>[()<>]])(([<>{}][()[]])(<{}{}><()<>>))])}>]({(<[<(([]<>)([]<>))<[()<>](()
      |<({<(([[<<[[{[{}()][()<>]}((<>[])[[]])]]{{<{{}{}}{<><>}><(<>)([]{}))}({(<>{})(<>[])}{<{}<>>{<>()}})}>({{{<
      |(({<[([(<<(<(<<><>>)({{}()})>{{[<>()]}}){{[[()[]]{<>{}}]({[]{}})}[([()<>]{{}()}){{[]{}}(<>[])}]}>>({(<(((
      |<[[{([<<[[(<<([][])[<>]>[[<>[]]]>)([<(()())<()[]>><[{}][[]]>])]((<[((){})[[]()]]<({}[]>[{}[]]>>
      |[(<<<<({<[{<<<(){}>{[]{}}>([{}{}]({}[]))><<<[]{}>(()[])>{[{}()](()<>)}>}][<<[(<>[])<(){}>]({(){}}((){
      |<[[[[{(<{([{((<>())[()<>]){<{}[]><{}()>}}][{{{<><>}[<>{}]}({{}{}}[<>{}])}[({(){}}[{}<>]){[{}[]]}]
      |(<<[<(<(<[[{<[<>()]<<>>>{{()<>}<[]{}>}}[(<[][]>{{}()})({()[]}(()[]))]]({((()())[{}])<<{}()){{
      |{{<([{{[(({([([][]){(){}}]){[{[]<>}(()<>)]<[{}]<<>>>}}<({{{}()}[{}()]}{<(){}>{<>[]}})<<[{}()]<<>()>]({
      |((<<{<{[[(<[{([]())(<><>)}](<<{}()>(()<>)>[((){})[[]{}]})>([(<[]<>><{}<>>)][<<{}()>>[({}[])([][])]]))<{((
      |[[<{{<[{({{[(<<>[]>[<><>])<{[]<>}<(){}>>]<[[{}<>]<{}()>]<<[][]><<>()>>>}<[({[]()})<(<><>)<<>{}
      |<(({[[[<{<[(<([][])(<>{})>{{{}}[{}()]})]((<{(){}}(<>[])>([{}[]]{<>{}})){{({})}{[{}[]]}})>}{[[(((<><>}(()()))
      |{[[{[({{[((<(({}{})){<[][]){{}()}}>){[({{}[]}([]{}))]{[<[]{}><[]{}>]}})({[{[[]{}]{()[]}}]<(<<>>)[{{}()}<{}<>
      |{[[<{({([(<[[{{}[]}[()[]]][[[][]]({}{})]]<[<[]<>>[{}()]]<<<>()>[()()]>>>>{[([[()()]<<>[]>]
      |<([[[[{([<{{(<[]{}>[[]]){<[]()>{()<>}}}[<<[][]>((){})>{{()<>}[[]{}]}]}>[{{{<()()><(){}>}<{()()}>}{[(()
      |{(<{<([<[(({{[{}[]]({}[])}{{[]()}{<><>}}})[<<({}())[()[]]>{[[][]]<()[]>}>[<<()<>>([]())><[(){})<()()>>]]
      |{[[(<([<(((([[{}{}]{[]()}]<(<>{})<[][]>>){{<{}{}><[][]>}[{()<>}[()[]]]})<(((<>{}){[]{}})[{[]()}
      |{{<<<{[({{{{([(){}}[<>{}])[<(){}><{}{}>]}}[<[{<>{}}{{}{}}](<[]{}>[[][]])>]}({<{{{}{}}<{}[]>}([[]][{}<>]
      |({{{(<[{{<[[(({}{})<[]<>>)([{}[]]{()[]})]((<()()>)[<[]{}>{<>[]}])])<[{(<{}>[[][]])<([]())[[]()]>}<
      |(((<{{{(((([({()<>})<<[]<>>[<>]>])[((<<>[]>({}))[[[]()]<<><>>])(<{[][]}[{}()]><{{}[]}[()()]>)
      |{[<[[<{<<({(<(<>()){[]()}><([]{})[<>{}]>)[[<{}[]><[]{}>][[{}[]]<[]<>>]]})>[<{<([()<>]{{}[]})<{{}[]}<{}<>>>
      |((({{[(((([{<(())({}<>)>{[[]{}][[][]]}}{<<{}<>>{{}[]}>{(<><>)({}<>)}}][{{[(){}](<>{})}{<()
      |{{<{({({[[<{{(()[])<()[]>}}[[{{}<>}(()[])][{{}{}}{{}<>}]]>{((<{}[]>{[]<>})({[]<>}{[][]}))[([[]<>]<()()>)<<<
      |[[{{{{{[[{[(<[{}()]{[]()}><<()()>[(){}]>)<[[{}<>]{<>[]}]>]([(<{}<>>[[][]]){(<><>)(()[])]]<[[
      |{({<[<{[<[[([[<><>][<><>]])[{{{}()}([]<>)}[[[]()][[][]]]]]<[<<()[]]{<>[]}><([])<<>()>>]{(<()<>><
      |({<<<{<<{({<{[()[]]{(){}}}{((){}){{}[]}}><<{(){}}><{<>()}<{}{}>}>}<{[[[]]([]{})](<()<>>(()<>))}>)}<<[({[<>
      |(<<<<{[{<({{{(<><>)<()[]>}{{[]()}{()()}}}<[<[]{}>(<>[])]{[[]<>]<[]<>>}>})[((([<>]{(){}})([(){}]<<>
      |(([(((((<<{<[<()<>><[]()>]([()[]][<>{}])>{(((){})({}[]))(<[]{}><[]<>>)}}{(<<(){}>[{}]>{{()())<()[
      |(({<([({(<{((<()()>{()[]}){<[][]>[[]<>]})[[{[]}{<><>}]<<[]><{}()>>]}(<(<[]{}><{}{}>)[(<><>){{}()}]>)>
      |{<<{[{[<{{{({({})<<>()>})<{[{}{}]}[[{}()]]>}}([{(({}{}}(<><>))([<>[]](<>()))}[{[{}<>]}(<<>{}>(()<>))]])}<
      |({[[{{{{[<{[([<>[]]{{}{}}><<<>[]>>]{[{()()}[()()]]}}((<[{}()][<>{}]>{([]()){()[]}})({[()<>](()[]
      |[[{{(({{({<{<<{}()>>{<<>()><{}()>}}{[{<>[]}<{}>]{<[]<>>[{}[]]}}>{[[[{}<>]({}{})]<[{}{}]<(){}}>][{({}
      |([[<<(<[<{{[[{(){}}<()[]>]{{()[]}}]({{[]{}}[(){}]](({}{})[[][]]))}{<[{<>[]}{{}<>}]{(<>{}){()()}}>}}<<{<(
      |{[<[(({[[(([[[{}()](<><>)]{{{}()}({}[])}])[<((()[])<()()>)<{<>{}}>><[{<><>}(()[])]>])](<({
      |{[{<<[(<{({<[[()<>]<[]{}>][(<>){{}<>}]>}<(<<<>[]>(()())>[(()<>)[[]<>]])<[<<>{}>[<><>]]{[<>{}]{[]}}>>)[
      |[{([{<([[{[([{{}<>}[{}[]]][({}{})({}{})]){({<>[]}(()()))}](<[(()[])([]{})][<()[]>{[]{}}]><{(<>())<(
      |([{({(((((({<({}<>)<<>[]>>{[<>[]][<><>]}}(<({}())<<>[]>>(([]<>)[<>{}])))[{({{}()}(()[])){<[]()><()(
      |{[{[{{<{<{<{(<()<>><()[]>)}<<<()[]><{}{}>>((<>[])<{}<>>)>><<(([]())[<>{}])({<>()}<{}>)>{([[]<>
      |([[(<<({{{{{[{()<>}[{}<>]]{{{}<>}<(){}>}}(([[][]][()()]))}({[{{}()}{()<>}]}<[([]<>)(<>())][(<>[]
      |{[<<{<<<<{<((<{}[]>[[]{}])([()<>](<>())))<{[()()]<(){}>>{<()[]>[{}{}]}>>}<[(<{{}{}}[{}{}]>)[([[]
      |<{{[(<(<{([{({{}[]})(<{}[]>{()<>})}]<[<<[]<>>([]<>)>{<[]{}><<>[]>}][[{()<>}[[]()]][{[]<>}(())]]>)([[[<{}
      |(<(<{<[({[<(<<{}<>>{{}()}>{(<>[])<()>})<{(<>())<{}()>}{[()()]<[]{}>}>><(({(){}}[[]{}]){{{}
      |[<[[[([{{<{[([<>]<[]()>){<()>[{}<>]}]{({[]()}[{}{}])[({}[])({}())]}}>}(<(<[[()[]]<(){}>]<<[][]>{()()}>>)((
      |[<<{<<([<{<(<(<>{}){()<>}>(({}{})({}{})))<<(<>[]){[][]]>{[{}{}]{()()}}>><[[[<><>]{[]{}}](<{}<
      |<{<{(<<((<{{[<<>()>[()<>]]{{<>()}(<>{}))}}>({[{{[]{}}[[][]]}({(){}}[[]()])][[<<>[]>[{}<>]]{(<>[])[(){
      |((({([<{[[<(({{}{}}[{}()])(([]()){{}{}})){<<[]<>>{{}()}>{{{}[]}{[]()}}}>[((<()[]>(()()))((()<>)([]{})))((
      |<(<<([{[(<<<{[{}<>]{()()}}(<<>>)>([([]()){[]<>}][[()<>][<>()]])>{[<((){})[{}[]]>{((){})({}())}][([<>[]]{[]
      |{[({{[<{<[<(<([]<>)(<>())>[<(){}>({}[])])>]>([<{[{[]}{{}<>}][({}<>)[[]<>]]}[{(<>[])(<><>)}({()<>>{[][]
      |<((((<<<{<[(<[{}{}]{{}}>){({<>()}{{}{}})[[{}<>]([]())]}]([<(()[])<{}()>>][(<[]{}>(<>{}))<(()()){[](
      |<[<<(({([[[<<<[]()>[()<>]>([<>()][()()])>[{(<>[])[<>]}{<{}()><[]{}}}]]]]([<{[<{}{}>{[]{}}]<<<><>>[()()]>}>
      |<[<{<{<({{{<<<[]<>>>((<>[])[{}()])][{<<>()><()()>}(<[][]>)]}<(<((){}){{}{}}>)>}})><{[{[{([{}{}]<[](
      |({<{<{{([[{[((<>[]))]}<<<({}{})>(<[]{}>({}[]))>{[(<>())([]<>)]{{<>{}}{()<>}}}>]([<({<><>}{{}<>})[[[]<>](
      |([[[([{[(<({([()[]])[{[][]}<{}<>>]}<(({}{}){()[]}){{<>[]}{(){}}}>)[<({<><>}{[]})(<(){}>(<><>))>{<[
      |[{{<<{<{(<<({[()()]<()[]>})[{(<>[]){()[]}}{((){})(<>[])}]>}){<[[[[<><>]<[][]>]]<([[][]](<>[]))((<>{})
      |<[((<{<(<[<[[<{}()>[[]<>]]([<>[]]<()>)]<<[<>[]]{()()}>]>([({()}((){}))]<{({}())[[]()]}(({}<>)<[]{}>)>
      |[{{<(<{({(<<(<()[]><{}<>>)<<<>>>><<(<>[])[{}[]]>>>({[{[]<>}]<<<>[]><{}()>>}<({{}}<()<>>)<[<>()][<><>]>>))})
      |([({({{{{<(<{{<>()}({}<>)}({{}<>}<[]<>>)>)>}[{(<{[[]<>][[]{}]}[[()<>]{<>}])){(<(<>[]){<>[]}><<()>[<
      |{({[{{(([[<({[{}{}]})><{<<<>[]><{}[]>><{()<>}<()()>>}(<[<>()]{()()}><[{}[]]>)>]]<[[((({}())<[]()>)<<()[]>{
      |[(<([{{([[<({{[]()}<(){}>}{({}<>)({}[])})>[(<{[][]}>({<>()}<[]<>>))]]{<{<<{}{}>[()[]]>[{[]<>}{()[]
      |([<((<<({([[([()()][()[]])[<<>{}>]]])}]>>)[<<(([[[(<<>()>)[{<><>}([][])]][((<><>)<[]{}>)[[()()]<{}{}
      |[[<{[[({[{{({[<>()][{}{}]}){[<()[]>][<()[]>[[]()]]}}}[[[([{}{}]{{}<>})[[{}[]]{(){}}]]]]]}{[{[<(<
      |[[<({[<{{<<[[({}[])][<{}<>>({}{})]]{(<<><>>{<>[]})}><<<{{}{}}{{}()}>{{<>}<<>>}]<<[()()]<[]<>>>([<>()]{[]<
      |<{{{([{[([{{({<><>}<[]<>>)}[({[]<>}){<{}())[<>[]]}]}{<[{{}{}}[{}{}]]>[[[()()][<>[]]]<{{}<>}
      |({[{{([<[<<{{<<>()><{}()>}}[[<<><>>(<>{})][{[]{}}]]><{(([]))<([]{})(<>[])>}(({<>{}}{()})(<{}{}]<<>>))>>]>[{
      |<(<([{{[[([[{<[]()>[[][]]}{[{}[]]<{}{}>}]<{<<>()><{}{}>}({{}})>]<((<()<>>[{}[]])({{}}<{}()>)){([[]<>]{<
      |(({{(((<<{<{<{{}[]}<[]()>>[<()<>><{}<>>]}>[[(([][])<[]<>>)][[<()()><[]>]{(<><>)<()()>}]]}[<[<{()<>}
      |(({{{{(<[<{([{{}()}<<>{}>][<{}[]>])}[{{{[]<>}[[]]}[<(){}>]}(<{[]()}{(){}}>{([]{}){[][]}})]>]>)}[([{[[[<<()<>
      |(<{<[([{[(((({()}<[]()>)>))<{<[(<>[]){<>()}][(<>())(<><>)]><[<()()>][((){})<<>()>]>}{{[({}{}""".stripMargin
