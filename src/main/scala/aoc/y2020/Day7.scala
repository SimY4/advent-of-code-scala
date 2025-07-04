package aoc.y2020

object Day7:
  private def parseLine(line: String): (String, List[(Int, String)]) =
    line.split(" contain ").toList match
      case bags :: "no other bags." :: Nil => bags.substring(0, bags.lastIndexOf(' ')) -> Nil
      case bags :: contains :: Nil         =>
        bags.substring(0, bags.lastIndexOf(' ')) ->
          contains
            .split(", ")
            .toList
            .map: bags =>
              bags.substring(0, bags.indexOf(' ')).toInt -> bags.substring(bags.indexOf(' ') + 1, bags.lastIndexOf(' '))

  def solve(input: String): Int =
    val bags = input.linesIterator.map(parseLine).toMap

    def loop(color: String, acc: Set[String] = Set.empty): Set[String] =
      val canContain = bags.view.filter((_, c) => c.map(_._2).contains(color)).keySet
      if canContain.isEmpty then acc + color
      else canContain.toList.map(loop(_, acc + color)).reduce(_ union _)

    (loop("shiny gold") - "shiny gold").size

  def solve2(input: String): Int =
    val bags = input.linesIterator.map(parseLine).toMap

    def loop(color: String): Int =
      val consists = bags(color)
      if consists.isEmpty then 0
      else consists.map((n, c) => n * (loop(c) + 1)).sum

    loop("shiny gold")

  val input =
    """dark maroon bags contain 2 striped silver bags, 4 mirrored maroon bags, 5 shiny gold bags, 1 dotted gold bag.
      |dark coral bags contain 4 pale blue bags, 3 wavy yellow bags, 4 vibrant tan bags, 3 striped purple bags.
      |striped aqua bags contain 1 pale aqua bag, 2 muted yellow bags, 4 pale maroon bags, 2 shiny coral bags.
      |wavy coral bags contain 4 pale purple bags, 2 bright olive bags.
      |bright aqua bags contain 5 mirrored purple bags, 1 dull maroon bag.
      |muted plum bags contain 1 dark beige bag.
      |pale cyan bags contain 5 dull gray bags, 3 posh olive bags, 2 striped silver bags.
      |muted aqua bags contain 3 muted black bags, 2 posh cyan bags.
      |wavy fuchsia bags contain 5 light gray bags, 3 wavy beige bags.
      |plaid orange bags contain 2 vibrant bronze bags, 3 pale silver bags, 1 shiny blue bag, 3 plaid maroon bags.
      |dotted salmon bags contain 2 clear lavender bags.
      |dark gray bags contain 3 plaid gray bags, 2 clear yellow bags, 5 posh gray bags.
      |vibrant aqua bags contain 4 posh salmon bags.
      |mirrored yellow bags contain 1 drab turquoise bag, 5 drab teal bags, 3 light cyan bags, 5 wavy gray bags.
      |muted indigo bags contain 2 pale cyan bags, 5 striped brown bags, 3 striped red bags.
      |striped white bags contain 4 pale maroon bags, 4 dull yellow bags, 3 mirrored white bags.
      |light beige bags contain 3 clear cyan bags, 5 dull gold bags, 4 dark olive bags.
      |mirrored turquoise bags contain 3 dull gray bags, 4 muted turquoise bags, 4 dull indigo bags.
      |clear lime bags contain 2 dotted salmon bags.
      |dull tomato bags contain 2 dim beige bags, 1 dotted brown bag, 3 faded magenta bags, 4 faded gray bags.
      |muted red bags contain 4 dark orange bags, 3 light black bags.
      |bright olive bags contain 4 pale green bags, 2 wavy red bags.
      |muted cyan bags contain 1 mirrored lavender bag, 1 shiny blue bag.
      |faded red bags contain 5 bright plum bags, 1 dull violet bag, 4 pale yellow bags, 3 pale coral bags.
      |shiny white bags contain 2 dark beige bags, 4 clear aqua bags.
      |dark tomato bags contain 3 pale yellow bags, 2 bright red bags.
      |clear bronze bags contain 1 dull salmon bag.
      |dark violet bags contain 4 dark purple bags.
      |dim indigo bags contain 5 plaid gray bags, 4 pale maroon bags, 2 bright maroon bags, 3 dark fuchsia bags.
      |striped beige bags contain 1 pale olive bag, 3 shiny purple bags, 1 dull salmon bag.
      |bright yellow bags contain 1 pale gold bag, 5 shiny maroon bags.
      |vibrant plum bags contain 5 dotted yellow bags, 2 plaid white bags, 5 drab red bags, 4 clear lavender bags.
      |clear olive bags contain 3 mirrored magenta bags.
      |bright lime bags contain 5 mirrored turquoise bags, 1 clear plum bag, 3 dull aqua bags, 1 drab green bag.
      |bright tan bags contain 1 vibrant lavender bag, 1 vibrant salmon bag, 1 dim green bag.
      |dull gold bags contain 5 dotted lavender bags.
      |dull crimson bags contain 5 shiny tomato bags.
      |dotted tomato bags contain 1 dark coral bag, 5 pale plum bags.
      |shiny gold bags contain 5 muted orange bags, 2 faded tan bags, 3 faded orange bags, 1 dull brown bag.
      |bright blue bags contain 1 plaid cyan bag, 4 dim magenta bags, 2 drab magenta bags.
      |shiny tomato bags contain 5 vibrant aqua bags, 3 clear tan bags.
      |pale silver bags contain 4 wavy orange bags, 4 dotted green bags, 3 drab silver bags.
      |faded indigo bags contain 5 striped gold bags, 3 clear olive bags.
      |faded gray bags contain 2 drab lime bags, 4 clear maroon bags, 1 mirrored silver bag, 1 shiny brown bag.
      |faded magenta bags contain 3 dim gold bags, 5 wavy lavender bags, 3 posh brown bags.
      |bright coral bags contain 3 drab tan bags, 4 pale beige bags, 4 clear turquoise bags, 1 faded white bag.
      |plaid salmon bags contain 1 striped tan bag, 3 pale blue bags.
      |drab aqua bags contain 3 wavy turquoise bags.
      |mirrored silver bags contain 1 dull brown bag, 4 pale black bags, 2 mirrored plum bags, 1 dotted red bag.
      |light salmon bags contain 4 dotted chartreuse bags.
      |wavy crimson bags contain 2 dim olive bags, 4 mirrored black bags, 4 faded salmon bags.
      |posh maroon bags contain 2 dark teal bags, 3 dim violet bags, 5 mirrored tan bags.
      |light lavender bags contain 3 vibrant turquoise bags, 1 pale orange bag, 4 shiny orange bags, 3 drab turquoise bags.
      |dark lavender bags contain 1 shiny salmon bag, 4 wavy cyan bags, 5 dim lavender bags.
      |shiny green bags contain 3 shiny brown bags, 5 dim coral bags.
      |pale violet bags contain 1 faded white bag.
      |dotted silver bags contain 2 pale blue bags.
      |posh olive bags contain 1 vibrant chartreuse bag, 4 posh salmon bags, 5 plaid coral bags.
      |vibrant gray bags contain 5 wavy yellow bags.
      |wavy brown bags contain 4 vibrant indigo bags, 2 vibrant green bags, 5 wavy blue bags, 3 dim magenta bags.
      |wavy teal bags contain 4 vibrant indigo bags, 2 vibrant turquoise bags.
      |mirrored black bags contain 5 vibrant turquoise bags, 1 drab salmon bag, 5 pale tan bags.
      |faded maroon bags contain 2 wavy crimson bags, 3 faded white bags.
      |mirrored salmon bags contain 4 striped purple bags, 1 vibrant black bag, 4 drab green bags.
      |faded brown bags contain 3 wavy turquoise bags.
      |drab tomato bags contain 3 pale brown bags.
      |dotted violet bags contain 5 plaid chartreuse bags, 5 shiny beige bags, 2 clear tan bags.
      |light orange bags contain 1 dull aqua bag, 2 clear silver bags, 3 bright tomato bags, 2 bright teal bags.
      |drab salmon bags contain 3 dark coral bags, 2 wavy brown bags, 1 striped purple bag, 4 dull gray bags.
      |dotted maroon bags contain 1 dark orange bag, 1 wavy brown bag.
      |striped chartreuse bags contain 2 striped yellow bags, 1 shiny orange bag, 1 dotted turquoise bag, 2 pale violet bags.
      |shiny tan bags contain 4 dim beige bags, 2 pale tomato bags.
      |plaid purple bags contain 4 light coral bags, 4 faded salmon bags.
      |wavy tan bags contain 2 vibrant tan bags.
      |pale lavender bags contain 3 dotted red bags, 1 bright green bag, 1 bright violet bag.
      |posh crimson bags contain 2 light white bags.
      |dark lime bags contain 2 plaid white bags, 2 bright gray bags.
      |light green bags contain 5 bright magenta bags, 1 light chartreuse bag.
      |light coral bags contain 3 pale black bags, 1 vibrant indigo bag, 1 wavy olive bag.
      |dark bronze bags contain 2 dim teal bags, 4 dark maroon bags.
      |light brown bags contain 5 dim white bags, 3 muted green bags, 1 dull white bag.
      |dim chartreuse bags contain 1 drab bronze bag, 2 mirrored maroon bags, 3 dark salmon bags, 5 light coral bags.
      |posh tan bags contain 2 plaid tan bags, 5 drab red bags, 5 vibrant salmon bags, 1 plaid salmon bag.
      |vibrant blue bags contain 2 dim turquoise bags, 4 plaid lime bags, 2 faded turquoise bags, 4 faded gold bags.
      |dark indigo bags contain 3 pale gray bags, 4 mirrored orange bags, 1 vibrant gray bag.
      |bright fuchsia bags contain 2 light cyan bags, 3 vibrant chartreuse bags, 4 wavy yellow bags, 2 vibrant olive bags.
      |posh white bags contain 4 light lime bags, 1 muted teal bag, 1 dull aqua bag.
      |clear blue bags contain 1 faded tan bag.
      |drab gold bags contain 5 mirrored beige bags.
      |posh blue bags contain 2 vibrant brown bags, 3 vibrant salmon bags.
      |shiny magenta bags contain 2 dotted fuchsia bags.
      |clear brown bags contain 3 posh olive bags, 1 drab silver bag, 5 dark purple bags.
      |muted brown bags contain 5 wavy yellow bags.
      |dim maroon bags contain 1 wavy crimson bag, 2 faded tan bags.
      |clear gold bags contain 5 dark aqua bags, 1 shiny coral bag, 2 dim fuchsia bags, 1 mirrored blue bag.
      |dull coral bags contain 4 faded salmon bags, 2 pale aqua bags, 5 dull aqua bags, 4 dull silver bags.
      |vibrant chartreuse bags contain 5 dull gray bags, 2 bright purple bags.
      |posh beige bags contain 2 dotted turquoise bags, 3 vibrant tan bags, 4 pale orange bags, 4 bright salmon bags.
      |bright white bags contain 3 dotted purple bags, 5 bright violet bags.
      |posh red bags contain 4 vibrant bronze bags, 4 wavy beige bags.
      |drab tan bags contain 2 dull indigo bags, 3 faded tan bags.
      |dull blue bags contain 5 dark maroon bags.
      |wavy plum bags contain 1 clear gray bag, 4 dark teal bags, 5 faded gray bags.
      |light tomato bags contain 4 muted red bags, 3 muted maroon bags, 1 dim teal bag.
      |posh purple bags contain 2 wavy black bags, 4 vibrant salmon bags.
      |dull lavender bags contain 5 posh tomato bags, 4 faded tan bags.
      |light silver bags contain 3 mirrored salmon bags, 4 pale yellow bags, 4 clear silver bags, 2 dim olive bags.
      |light black bags contain 2 wavy crimson bags, 1 dull salmon bag, 4 muted black bags.
      |light purple bags contain 2 vibrant purple bags, 2 vibrant aqua bags.
      |striped orange bags contain 3 clear aqua bags.
      |mirrored plum bags contain 3 vibrant aqua bags, 3 vibrant turquoise bags, 5 faded orange bags, 1 bright violet bag.
      |posh coral bags contain 5 drab yellow bags, 2 faded white bags.
      |clear tomato bags contain 3 shiny turquoise bags, 1 wavy cyan bag, 1 bright bronze bag, 5 light red bags.
      |posh bronze bags contain 3 dull orange bags, 4 dull salmon bags, 5 clear blue bags.
      |shiny crimson bags contain 4 drab green bags, 3 posh lime bags, 2 striped magenta bags, 2 bright teal bags.
      |drab chartreuse bags contain 1 wavy yellow bag, 4 mirrored beige bags, 4 dull gold bags, 5 dull plum bags.
      |striped tomato bags contain 3 plaid tan bags, 3 pale bronze bags, 4 dull yellow bags, 3 drab magenta bags.
      |wavy chartreuse bags contain 3 dim coral bags, 4 bright chartreuse bags, 5 pale plum bags.
      |posh chartreuse bags contain 4 dark purple bags, 3 muted magenta bags, 2 faded black bags.
      |dotted aqua bags contain 2 wavy green bags, 5 vibrant tomato bags.
      |dotted red bags contain 5 bright purple bags, 1 dotted gold bag, 5 mirrored magenta bags, 2 plaid indigo bags.
      |faded salmon bags contain 1 wavy yellow bag.
      |vibrant fuchsia bags contain 3 mirrored coral bags.
      |drab coral bags contain 5 mirrored brown bags, 5 dotted lavender bags, 1 drab bronze bag, 3 dim chartreuse bags.
      |posh aqua bags contain 3 bright olive bags.
      |dark chartreuse bags contain 5 plaid silver bags.
      |vibrant maroon bags contain 5 shiny brown bags, 2 faded black bags, 1 drab silver bag.
      |striped salmon bags contain 5 dark gray bags, 1 muted tomato bag.
      |posh gray bags contain 2 clear plum bags.
      |dotted gray bags contain 3 drab salmon bags, 1 dim coral bag.
      |clear aqua bags contain 4 dotted lavender bags, 1 striped purple bag, 3 light purple bags.
      |dark aqua bags contain 2 dark indigo bags, 5 bright bronze bags.
      |bright brown bags contain 2 plaid salmon bags, 4 faded indigo bags.
      |clear violet bags contain 3 pale cyan bags, 3 dull aqua bags, 4 pale brown bags, 5 dim plum bags.
      |wavy salmon bags contain 4 clear tomato bags, 4 bright bronze bags, 5 posh purple bags, 5 faded black bags.
      |drab green bags contain 4 mirrored maroon bags, 2 vibrant aqua bags, 4 dim olive bags.
      |posh silver bags contain 4 muted black bags, 5 pale cyan bags, 2 dark plum bags, 1 shiny bronze bag.
      |dull indigo bags contain 4 striped purple bags.
      |pale magenta bags contain 1 drab coral bag, 5 plaid blue bags, 1 wavy yellow bag, 5 dark salmon bags.
      |light olive bags contain 2 pale gold bags.
      |muted crimson bags contain 1 striped cyan bag, 1 vibrant bronze bag, 1 dull coral bag, 4 vibrant black bags.
      |plaid yellow bags contain 3 bright silver bags.
      |vibrant gold bags contain 5 vibrant tan bags.
      |bright beige bags contain 2 pale beige bags, 2 dark lavender bags, 1 dull teal bag.
      |faded white bags contain 2 pale brown bags, 3 muted orange bags, 3 dull indigo bags.
      |striped gray bags contain 5 pale chartreuse bags.
      |plaid brown bags contain 4 dull teal bags, 2 wavy beige bags.
      |wavy yellow bags contain no other bags.
      |dotted green bags contain 5 posh gray bags.
      |dull turquoise bags contain 3 faded coral bags, 2 bright green bags.
      |faded plum bags contain 1 dull fuchsia bag.
      |clear maroon bags contain 2 light purple bags, 2 dim crimson bags, 5 vibrant bronze bags.
      |posh green bags contain 1 plaid silver bag.
      |vibrant teal bags contain 5 dotted gold bags.
      |faded coral bags contain 3 dull aqua bags, 3 wavy orange bags, 3 drab chartreuse bags, 4 muted olive bags.
      |wavy beige bags contain 1 dark plum bag, 2 dull lavender bags, 2 drab green bags.
      |dim magenta bags contain 2 plaid coral bags, 2 faded orange bags.
      |striped teal bags contain 2 bright tan bags.
      |drab crimson bags contain 2 clear salmon bags, 2 clear orange bags, 1 striped yellow bag.
      |dotted yellow bags contain 4 dull gray bags.
      |mirrored tan bags contain 1 dotted fuchsia bag, 2 vibrant lavender bags.
      |wavy aqua bags contain 4 drab gray bags, 3 muted yellow bags, 5 shiny lavender bags, 2 plaid turquoise bags.
      |wavy gray bags contain 5 dim green bags, 1 plaid blue bag.
      |dull teal bags contain 3 mirrored black bags, 4 dark plum bags, 2 drab silver bags, 4 mirrored gray bags.
      |striped indigo bags contain 4 clear violet bags, 5 dotted yellow bags, 4 clear tomato bags.
      |dim brown bags contain 1 drab bronze bag, 5 shiny yellow bags, 1 pale olive bag, 2 drab silver bags.
      |mirrored teal bags contain 3 faded orange bags.
      |dull salmon bags contain 3 dark indigo bags, 2 striped gray bags.
      |dark tan bags contain 5 striped lime bags, 1 dark purple bag, 5 faded gold bags.
      |clear teal bags contain 3 dark crimson bags, 1 bright tomato bag.
      |dark olive bags contain 4 striped gold bags, 1 striped gray bag, 1 bright tomato bag.
      |posh indigo bags contain 1 mirrored maroon bag, 4 striped indigo bags.
      |drab yellow bags contain 5 dull teal bags, 2 plaid turquoise bags.
      |faded bronze bags contain 3 dull orange bags.
      |dull chartreuse bags contain 2 dull bronze bags, 3 light gold bags, 2 striped white bags.
      |posh orange bags contain 5 pale crimson bags, 1 bright blue bag, 5 clear green bags.
      |striped coral bags contain 4 pale gray bags.
      |clear tan bags contain 4 dark plum bags, 1 posh olive bag.
      |dotted tan bags contain 1 faded aqua bag, 3 pale brown bags, 5 dim chartreuse bags.
      |striped brown bags contain 5 dark black bags.
      |clear red bags contain 4 faded lime bags, 3 striped blue bags, 3 light fuchsia bags, 4 drab turquoise bags.
      |plaid gold bags contain 5 dim chartreuse bags.
      |plaid cyan bags contain no other bags.
      |muted tomato bags contain 2 pale blue bags, 2 shiny bronze bags, 1 light teal bag.
      |dotted crimson bags contain 2 dark maroon bags.
      |light turquoise bags contain 3 wavy orange bags, 5 drab red bags, 5 plaid yellow bags.
      |wavy cyan bags contain 1 dim magenta bag, 2 shiny blue bags.
      |dim coral bags contain 3 vibrant indigo bags, 4 vibrant turquoise bags.
      |dark blue bags contain 3 clear teal bags, 1 dim bronze bag, 4 light tan bags, 4 wavy indigo bags.
      |faded gold bags contain 2 posh salmon bags, 4 plaid white bags, 2 dull cyan bags.
      |vibrant turquoise bags contain no other bags.
      |dim fuchsia bags contain 3 shiny blue bags, 1 dark bronze bag, 2 wavy yellow bags.
      |muted blue bags contain 4 shiny violet bags, 5 muted tomato bags, 5 light salmon bags.
      |plaid bronze bags contain 4 dull green bags.
      |mirrored maroon bags contain 3 dark coral bags, 4 clear plum bags.
      |muted olive bags contain 2 dark brown bags, 5 bright white bags.
      |pale crimson bags contain 1 pale blue bag, 4 clear plum bags, 5 wavy yellow bags, 1 dotted purple bag.
      |striped crimson bags contain 4 muted teal bags.
      |dark orange bags contain 5 dark maroon bags, 2 drab cyan bags, 1 dotted chartreuse bag, 5 shiny green bags.
      |light white bags contain 5 faded gray bags, 1 dim beige bag, 5 bright violet bags.
      |plaid white bags contain 1 pale brown bag.
      |dim orange bags contain 3 faded blue bags.
      |vibrant green bags contain 4 posh salmon bags, 4 plaid cyan bags, 3 clear plum bags.
      |dotted beige bags contain 5 posh black bags, 2 mirrored yellow bags, 3 mirrored salmon bags.
      |dim black bags contain 2 vibrant cyan bags, 3 shiny turquoise bags, 2 dotted turquoise bags, 1 muted orange bag.
      |mirrored tomato bags contain 4 dotted plum bags.
      |dim violet bags contain 1 dotted chartreuse bag.
      |pale coral bags contain 5 dim silver bags, 3 clear gray bags.
      |vibrant lavender bags contain 3 dim bronze bags.
      |dark crimson bags contain 1 shiny brown bag.
      |bright orange bags contain 3 muted tomato bags, 2 pale black bags.
      |clear yellow bags contain 4 striped purple bags, 5 mirrored black bags, 4 posh gray bags.
      |posh violet bags contain 3 clear red bags.
      |plaid tan bags contain 1 pale cyan bag.
      |pale gold bags contain 5 striped olive bags.
      |muted lavender bags contain 1 clear crimson bag, 5 light white bags.
      |faded crimson bags contain 3 clear coral bags.
      |wavy blue bags contain no other bags.
      |faded purple bags contain 5 dark crimson bags.
      |dim silver bags contain 5 mirrored silver bags, 1 vibrant turquoise bag, 5 wavy turquoise bags.
      |pale gray bags contain 4 wavy yellow bags, 2 vibrant tan bags, 2 pale tan bags, 1 dark coral bag.
      |dim purple bags contain 2 striped silver bags, 1 plaid blue bag, 5 pale cyan bags.
      |striped purple bags contain no other bags.
      |plaid red bags contain 4 faded coral bags.
      |dull bronze bags contain 5 bright coral bags.
      |light teal bags contain 2 dotted green bags, 4 pale purple bags.
      |mirrored blue bags contain 2 striped silver bags.
      |muted green bags contain 2 drab red bags, 1 drab tan bag.
      |muted gray bags contain 2 muted coral bags.
      |light fuchsia bags contain 2 dim indigo bags.
      |shiny orange bags contain 2 bright green bags, 4 shiny lime bags, 1 striped red bag, 5 dull turquoise bags.
      |bright purple bags contain 1 striped purple bag, 3 shiny brown bags, 4 vibrant indigo bags.
      |dotted brown bags contain 2 faded blue bags, 5 shiny lavender bags, 4 clear lavender bags, 1 bright bronze bag.
      |light maroon bags contain 4 muted brown bags, 4 striped orange bags, 1 clear plum bag, 3 dull lavender bags.
      |wavy green bags contain 1 dull brown bag, 1 clear plum bag.
      |dull fuchsia bags contain 2 dark coral bags, 3 light coral bags.
      |dotted purple bags contain no other bags.
      |light plum bags contain 4 muted gold bags, 1 muted crimson bag.
      |light red bags contain 5 dull magenta bags, 2 wavy olive bags, 3 drab silver bags, 1 dim magenta bag.
      |faded green bags contain 3 dotted turquoise bags, 2 dull bronze bags, 2 muted green bags.
      |posh lime bags contain 3 faded brown bags, 4 wavy brown bags.
      |shiny black bags contain 2 plaid blue bags, 5 drab salmon bags.
      |muted teal bags contain 5 bright violet bags, 4 pale cyan bags.
      |light cyan bags contain 2 muted silver bags, 3 faded salmon bags, 5 shiny brown bags, 5 drab magenta bags.
      |faded beige bags contain 2 muted red bags.
      |mirrored aqua bags contain 4 faded silver bags, 4 dull maroon bags, 3 dull olive bags, 4 dim bronze bags.
      |muted silver bags contain 3 dotted cyan bags, 3 muted fuchsia bags, 5 plaid purple bags.
      |plaid teal bags contain 4 mirrored magenta bags, 3 dotted red bags.
      |bright teal bags contain 1 shiny brown bag, 4 vibrant tan bags.
      |dark red bags contain 3 wavy red bags, 4 dotted violet bags, 5 pale tomato bags.
      |muted maroon bags contain 1 plaid indigo bag.
      |dark fuchsia bags contain 4 bright teal bags, 4 dim blue bags, 5 vibrant lavender bags, 4 striped yellow bags.
      |dark white bags contain 1 drab green bag, 4 dotted chartreuse bags.
      |drab maroon bags contain 1 faded salmon bag, 1 dotted orange bag, 1 faded fuchsia bag.
      |dull violet bags contain 2 dotted tan bags, 4 pale cyan bags.
      |shiny violet bags contain 1 dull red bag, 4 wavy indigo bags, 1 clear violet bag, 2 dotted white bags.
      |posh teal bags contain 5 muted green bags, 1 dotted brown bag.
      |dotted teal bags contain 4 pale olive bags, 5 mirrored crimson bags.
      |dim olive bags contain 3 pale white bags, 1 posh olive bag, 3 striped orange bags, 1 striped silver bag.
      |shiny purple bags contain 3 shiny salmon bags, 5 posh plum bags, 1 striped white bag, 5 shiny black bags.
      |mirrored white bags contain 3 muted turquoise bags, 5 striped gray bags, 1 faded lime bag.
      |mirrored indigo bags contain 1 dim teal bag, 3 drab lavender bags, 1 plaid coral bag, 5 posh orange bags.
      |wavy orange bags contain 2 mirrored maroon bags, 1 striped silver bag, 2 wavy yellow bags, 2 dim crimson bags.
      |light indigo bags contain 2 vibrant cyan bags, 2 vibrant gray bags.
      |dark cyan bags contain 3 clear tomato bags.
      |dim lime bags contain 5 dull indigo bags, 1 wavy olive bag.
      |pale beige bags contain 2 muted red bags, 2 clear tomato bags, 3 posh olive bags.
      |plaid silver bags contain 4 dim plum bags, 1 dim beige bag, 3 plaid salmon bags.
      |wavy silver bags contain 1 plaid indigo bag.
      |wavy violet bags contain 4 posh chartreuse bags, 2 mirrored tomato bags.
      |clear beige bags contain 4 dark maroon bags.
      |vibrant cyan bags contain 1 striped purple bag, 4 muted black bags.
      |faded teal bags contain 4 dark blue bags, 1 pale brown bag.
      |drab gray bags contain 3 striped crimson bags, 2 shiny gold bags, 2 shiny magenta bags.
      |clear turquoise bags contain 5 light crimson bags, 4 dotted cyan bags.
      |plaid lime bags contain 4 dotted chartreuse bags, 2 plaid orange bags, 4 shiny fuchsia bags.
      |faded tan bags contain 5 vibrant green bags.
      |dotted indigo bags contain 3 light violet bags, 4 dull fuchsia bags.
      |muted turquoise bags contain 5 pale tan bags, 4 dim olive bags, 2 plaid indigo bags.
      |vibrant orange bags contain 4 dotted yellow bags, 4 plaid cyan bags.
      |drab fuchsia bags contain 1 dim plum bag, 1 striped red bag, 2 vibrant cyan bags.
      |clear green bags contain 1 dull magenta bag, 5 wavy blue bags, 1 vibrant olive bag, 1 vibrant tan bag.
      |light magenta bags contain 3 posh bronze bags.
      |dull black bags contain 4 drab bronze bags.
      |dull beige bags contain 3 dull lime bags, 2 wavy coral bags, 1 plaid yellow bag.
      |dotted plum bags contain 4 posh salmon bags, 3 vibrant chartreuse bags, 5 dim green bags.
      |vibrant salmon bags contain 4 dim purple bags.
      |mirrored crimson bags contain 2 faded tomato bags, 1 striped blue bag, 3 vibrant green bags, 3 striped turquoise bags.
      |dotted fuchsia bags contain 2 plaid coral bags, 5 dull brown bags, 1 dark maroon bag.
      |posh salmon bags contain no other bags.
      |dotted cyan bags contain 4 clear aqua bags, 5 plaid gray bags, 3 bright bronze bags.
      |mirrored bronze bags contain 3 vibrant black bags, 3 faded maroon bags, 1 drab coral bag, 1 muted black bag.
      |bright magenta bags contain 2 dim magenta bags, 3 dotted gold bags, 4 posh gold bags.
      |faded violet bags contain 1 light red bag.
      |dim yellow bags contain 1 dull orange bag, 1 muted purple bag, 2 faded indigo bags, 5 dotted plum bags.
      |wavy lime bags contain 1 clear blue bag.
      |mirrored coral bags contain 3 dull maroon bags, 1 striped orange bag.
      |wavy maroon bags contain 4 dark gold bags.
      |bright gold bags contain 3 dim orange bags, 5 dark gray bags, 2 clear fuchsia bags, 5 clear maroon bags.
      |dotted turquoise bags contain 3 dim olive bags, 3 light red bags, 3 dull fuchsia bags.
      |striped plum bags contain 4 dark tan bags, 2 mirrored teal bags, 1 striped brown bag.
      |shiny fuchsia bags contain 5 clear green bags, 5 muted tomato bags, 1 dark coral bag, 5 wavy blue bags.
      |vibrant white bags contain 1 dim olive bag, 2 bright silver bags, 2 muted blue bags.
      |dull cyan bags contain 2 clear teal bags, 4 plaid blue bags.
      |drab teal bags contain 4 clear crimson bags, 5 wavy lime bags, 4 plaid turquoise bags.
      |posh turquoise bags contain 5 plaid fuchsia bags, 5 pale blue bags, 3 vibrant green bags.
      |bright red bags contain 1 clear magenta bag, 4 striped aqua bags, 2 posh cyan bags, 1 dull salmon bag.
      |dark teal bags contain 3 dull yellow bags, 3 plaid white bags, 3 clear cyan bags, 4 striped red bags.
      |vibrant beige bags contain 3 drab silver bags, 4 wavy orange bags, 3 pale brown bags, 2 faded orange bags.
      |clear purple bags contain 5 faded indigo bags, 3 striped maroon bags, 4 vibrant plum bags, 3 light red bags.
      |pale lime bags contain 2 plaid blue bags, 1 bright violet bag, 5 faded bronze bags, 1 dotted plum bag.
      |shiny lavender bags contain 5 mirrored plum bags.
      |posh brown bags contain 1 drab yellow bag.
      |dark black bags contain 5 dull salmon bags, 3 pale silver bags, 2 vibrant turquoise bags.
      |shiny turquoise bags contain 3 plaid blue bags, 3 dim salmon bags, 5 dull gold bags.
      |dim green bags contain 3 vibrant turquoise bags, 5 faded tan bags.
      |pale maroon bags contain 3 dull fuchsia bags.
      |dull purple bags contain 2 vibrant aqua bags, 2 bright blue bags.
      |dull magenta bags contain 4 pale blue bags, 2 shiny bronze bags, 2 pale lavender bags, 3 pale chartreuse bags.
      |light gray bags contain 4 bright orange bags, 5 dull gray bags.
      |faded black bags contain 2 clear crimson bags, 4 posh gray bags.
      |plaid tomato bags contain 1 light bronze bag, 2 posh plum bags.
      |faded aqua bags contain 4 shiny gold bags, 5 pale tomato bags, 1 plaid blue bag, 1 faded coral bag.
      |muted tan bags contain 2 muted tomato bags, 2 light white bags, 4 wavy lavender bags, 2 dim turquoise bags.
      |dark turquoise bags contain 4 dotted silver bags, 3 vibrant chartreuse bags, 4 striped green bags, 2 dotted black bags.
      |dim lavender bags contain 3 dark indigo bags, 3 wavy orange bags, 5 faded orange bags, 2 striped orange bags.
      |plaid black bags contain 3 light violet bags, 3 mirrored bronze bags, 1 clear blue bag, 5 clear olive bags.
      |clear lavender bags contain 1 dim bronze bag.
      |pale blue bags contain no other bags.
      |mirrored magenta bags contain 1 vibrant tan bag, 1 wavy turquoise bag, 4 clear aqua bags.
      |shiny gray bags contain 3 wavy indigo bags, 5 striped gold bags, 1 drab cyan bag.
      |light bronze bags contain 3 dark aqua bags, 4 striped gold bags.
      |muted black bags contain 3 faded white bags, 5 pale crimson bags, 3 muted brown bags, 1 vibrant tan bag.
      |wavy purple bags contain 3 pale bronze bags, 5 shiny maroon bags, 4 dotted teal bags, 4 pale indigo bags.
      |muted orange bags contain 1 dotted chartreuse bag, 2 pale blue bags, 3 muted brown bags, 1 posh tomato bag.
      |wavy turquoise bags contain 2 drab silver bags, 2 vibrant purple bags, 3 faded tan bags.
      |dull plum bags contain 3 striped silver bags, 4 clear violet bags.
      |mirrored olive bags contain 4 shiny violet bags.
      |shiny teal bags contain 5 dim purple bags, 3 faded violet bags, 5 plaid crimson bags.
      |dotted coral bags contain 4 dull indigo bags, 5 muted silver bags, 1 faded white bag.
      |striped olive bags contain 5 posh tomato bags, 1 pale purple bag.
      |shiny beige bags contain 2 wavy red bags.
      |plaid aqua bags contain 2 muted teal bags.
      |shiny silver bags contain 1 dotted gold bag, 4 plaid tomato bags.
      |muted chartreuse bags contain 2 pale crimson bags, 2 mirrored black bags, 5 dark purple bags, 3 wavy tan bags.
      |shiny lime bags contain 1 light tan bag.
      |vibrant coral bags contain 1 dotted violet bag, 3 faded gray bags, 3 pale magenta bags.
      |clear orange bags contain 4 mirrored brown bags, 1 posh turquoise bag.
      |pale white bags contain 2 dotted gold bags, 3 dotted lavender bags.
      |dark green bags contain 5 clear tan bags.
      |pale olive bags contain 4 mirrored black bags, 1 dull magenta bag.
      |faded blue bags contain 5 dull magenta bags.
      |dull gray bags contain 4 vibrant indigo bags, 2 pale crimson bags, 2 clear plum bags.
      |dull lime bags contain 4 dim salmon bags.
      |bright green bags contain 1 drab silver bag, 5 bright bronze bags.
      |dim salmon bags contain 3 dotted silver bags.
      |dim red bags contain 1 vibrant green bag, 2 drab tan bags, 1 mirrored brown bag.
      |clear magenta bags contain 1 clear cyan bag, 3 light bronze bags.
      |dotted bronze bags contain 2 striped yellow bags, 3 dark maroon bags.
      |dotted blue bags contain 3 dotted tan bags, 4 muted violet bags.
      |bright cyan bags contain 4 pale turquoise bags, 4 dull yellow bags.
      |drab lavender bags contain 1 shiny green bag.
      |bright black bags contain 3 mirrored lime bags.
      |wavy black bags contain 1 faded indigo bag, 4 dim salmon bags, 1 drab aqua bag, 5 dull maroon bags.
      |clear gray bags contain 2 muted fuchsia bags, 4 clear yellow bags.
      |bright chartreuse bags contain 2 bright tomato bags.
      |posh black bags contain 5 dotted tan bags, 3 muted orange bags, 2 dim purple bags, 1 shiny cyan bag.
      |dull green bags contain 1 striped tan bag, 1 dull magenta bag.
      |mirrored green bags contain 3 posh salmon bags, 5 mirrored purple bags, 4 vibrant magenta bags.
      |dull olive bags contain 1 dim maroon bag, 4 striped gold bags, 2 shiny white bags, 3 clear tomato bags.
      |pale purple bags contain 3 dull salmon bags, 1 drab cyan bag, 5 bright green bags, 5 drab salmon bags.
      |mirrored lime bags contain 2 posh white bags.
      |drab blue bags contain 3 pale tomato bags, 1 shiny plum bag.
      |dim plum bags contain 2 vibrant indigo bags, 4 faded orange bags, 3 wavy cyan bags.
      |wavy magenta bags contain 1 muted fuchsia bag.
      |wavy lavender bags contain 3 shiny lime bags.
      |plaid beige bags contain 5 bright beige bags, 4 pale black bags.
      |mirrored gold bags contain 4 muted salmon bags, 3 striped black bags, 3 dotted red bags, 3 dim orange bags.
      |clear salmon bags contain 1 striped turquoise bag, 1 muted silver bag.
      |vibrant purple bags contain 5 vibrant aqua bags.
      |pale turquoise bags contain 4 wavy maroon bags.
      |dull brown bags contain 5 dotted purple bags, 5 vibrant turquoise bags.
      |drab lime bags contain 3 dim magenta bags.
      |vibrant black bags contain 1 drab magenta bag.
      |striped silver bags contain 2 shiny bronze bags, 5 striped purple bags.
      |dim turquoise bags contain 2 pale brown bags, 4 bright green bags, 1 drab salmon bag.
      |shiny red bags contain 4 light purple bags, 2 dull orange bags, 4 striped magenta bags, 3 dull red bags.
      |dotted magenta bags contain 3 faded silver bags, 3 dull purple bags, 5 dotted tan bags.
      |dotted lavender bags contain 2 vibrant indigo bags, 1 clear plum bag.
      |shiny indigo bags contain 1 dull lavender bag, 3 vibrant tan bags, 4 faded lime bags, 5 drab tan bags.
      |striped violet bags contain 1 muted tomato bag.
      |muted salmon bags contain 2 light cyan bags, 4 dull tomato bags, 2 pale chartreuse bags, 1 dotted gray bag.
      |mirrored fuchsia bags contain 4 faded silver bags, 4 plaid gray bags, 4 dotted purple bags.
      |light aqua bags contain 4 striped gold bags, 1 dark blue bag, 3 dim red bags, 2 wavy gold bags.
      |dark yellow bags contain 2 pale gray bags, 1 shiny blue bag, 4 faded turquoise bags.
      |striped fuchsia bags contain 4 muted magenta bags.
      |clear silver bags contain 2 plaid purple bags, 2 plaid indigo bags, 1 muted orange bag, 3 clear plum bags.
      |posh yellow bags contain 1 posh silver bag, 3 dotted gold bags.
      |dark magenta bags contain 1 drab violet bag, 2 bright purple bags, 4 shiny coral bags, 5 striped tan bags.
      |bright tomato bags contain 1 dotted purple bag, 2 dotted chartreuse bags, 2 wavy yellow bags.
      |bright plum bags contain 1 bright blue bag, 2 faded orange bags, 2 dim coral bags, 3 dotted cyan bags.
      |shiny maroon bags contain 3 dotted crimson bags, 5 bright teal bags, 5 dotted fuchsia bags, 4 dull green bags.
      |pale yellow bags contain 4 muted silver bags, 4 dull teal bags, 4 muted purple bags.
      |plaid fuchsia bags contain 3 dull lavender bags.
      |clear white bags contain 5 light turquoise bags.
      |striped lavender bags contain 2 plaid lime bags, 4 dark white bags.
      |posh cyan bags contain 3 faded coral bags, 5 light maroon bags, 3 faded indigo bags.
      |faded lime bags contain 2 wavy yellow bags.
      |striped red bags contain 1 plaid indigo bag.
      |light tan bags contain 4 wavy blue bags.
      |vibrant violet bags contain 4 striped purple bags, 4 light cyan bags, 5 faded tan bags, 4 light yellow bags.
      |dotted gold bags contain 4 vibrant indigo bags, 2 bright bronze bags, 2 drab silver bags, 5 muted brown bags.
      |shiny plum bags contain 1 faded lime bag, 4 faded orange bags, 2 vibrant beige bags, 2 striped orange bags.
      |striped cyan bags contain 5 clear beige bags.
      |striped yellow bags contain 3 dim teal bags, 5 clear maroon bags, 1 light cyan bag.
      |muted yellow bags contain 3 drab bronze bags, 2 pale gray bags.
      |clear indigo bags contain 1 clear beige bag, 2 bright silver bags, 1 dim salmon bag.
      |striped green bags contain 1 mirrored red bag, 3 shiny blue bags.
      |plaid plum bags contain 1 drab cyan bag, 5 light lime bags, 4 muted lavender bags.
      |pale green bags contain 2 dotted chartreuse bags, 3 plaid magenta bags.
      |wavy bronze bags contain 2 pale purple bags, 1 pale teal bag, 1 muted bronze bag, 1 mirrored beige bag.
      |clear black bags contain 4 vibrant brown bags, 5 pale tomato bags, 4 bright teal bags, 3 wavy brown bags.
      |pale fuchsia bags contain 3 pale beige bags, 5 plaid blue bags, 3 light violet bags, 3 striped blue bags.
      |vibrant magenta bags contain 5 posh plum bags, 2 mirrored turquoise bags, 2 light red bags, 2 pale plum bags.
      |drab olive bags contain 5 vibrant green bags, 4 dull turquoise bags, 4 muted fuchsia bags.
      |bright salmon bags contain 1 muted silver bag, 1 drab purple bag, 5 pale cyan bags.
      |shiny coral bags contain 4 wavy magenta bags, 5 dark olive bags.
      |mirrored cyan bags contain 3 dim chartreuse bags, 4 pale aqua bags.
      |plaid blue bags contain 5 bright purple bags.
      |vibrant tomato bags contain 5 faded tomato bags, 3 muted silver bags, 5 dotted turquoise bags.
      |mirrored brown bags contain 2 pale black bags.
      |shiny olive bags contain 3 plaid teal bags, 3 vibrant fuchsia bags.
      |shiny blue bags contain 3 dark coral bags, 4 bright purple bags, 2 pale cyan bags, 5 plaid indigo bags.
      |plaid crimson bags contain 5 wavy crimson bags, 5 clear crimson bags, 4 vibrant red bags.
      |striped black bags contain 1 faded chartreuse bag, 2 dull brown bags, 4 clear cyan bags, 1 light teal bag.
      |wavy gold bags contain 2 dull gray bags, 1 dark lavender bag, 3 pale gray bags.
      |faded turquoise bags contain 3 bright white bags, 4 pale bronze bags, 5 pale lavender bags.
      |dull maroon bags contain 2 bright orange bags, 5 vibrant bronze bags, 4 drab chartreuse bags.
      |striped bronze bags contain 2 pale lavender bags, 1 dull brown bag.
      |plaid violet bags contain 5 mirrored plum bags.
      |light violet bags contain 4 drab blue bags, 2 plaid beige bags, 3 pale crimson bags.
      |posh lavender bags contain 2 faded tomato bags, 4 dim crimson bags.
      |dark salmon bags contain 1 pale crimson bag.
      |dotted black bags contain 4 wavy lime bags, 1 faded salmon bag, 3 pale blue bags, 5 wavy blue bags.
      |shiny aqua bags contain 1 bright blue bag, 4 drab green bags, 2 light teal bags, 2 dotted green bags.
      |shiny salmon bags contain 3 dim magenta bags.
      |dark silver bags contain 1 shiny tan bag, 3 wavy yellow bags, 4 drab violet bags.
      |pale red bags contain 4 mirrored cyan bags, 4 clear gray bags, 2 dotted tan bags.
      |dark brown bags contain 1 plaid indigo bag.
      |drab cyan bags contain 2 wavy green bags, 2 pale gray bags, 2 dotted purple bags, 3 dark beige bags.
      |dim gray bags contain 2 bright salmon bags, 2 clear silver bags, 2 light turquoise bags, 4 drab plum bags.
      |pale black bags contain 1 plaid coral bag, 4 vibrant turquoise bags, 1 pale brown bag.
      |dotted olive bags contain 2 faded blue bags, 2 dull maroon bags, 3 dim fuchsia bags, 5 light fuchsia bags.
      |posh fuchsia bags contain 5 dark lavender bags.
      |pale orange bags contain 2 vibrant aqua bags, 2 vibrant green bags, 1 bright coral bag.
      |faded cyan bags contain 4 clear orange bags, 5 dull orange bags, 4 dotted bronze bags.
      |dim tan bags contain 2 dim chartreuse bags, 3 vibrant turquoise bags, 2 faded coral bags, 2 dim coral bags.
      |dim gold bags contain 2 wavy magenta bags.
      |dull aqua bags contain 2 wavy yellow bags.
      |posh tomato bags contain 1 muted brown bag.
      |dark gold bags contain 4 muted fuchsia bags, 4 light salmon bags.
      |clear chartreuse bags contain 3 light coral bags.
      |plaid chartreuse bags contain 1 light silver bag, 5 vibrant green bags, 3 wavy lavender bags, 4 vibrant turquoise bags.
      |mirrored red bags contain 5 plaid aqua bags, 4 clear cyan bags, 1 mirrored magenta bag.
      |dim beige bags contain 4 dull green bags, 2 dim green bags.
      |mirrored beige bags contain 1 drab magenta bag, 4 clear tan bags, 5 mirrored turquoise bags, 4 drab tan bags.
      |bright maroon bags contain 3 vibrant green bags, 5 shiny gold bags.
      |dim aqua bags contain 4 faded coral bags.
      |dim blue bags contain 3 posh silver bags.
      |drab silver bags contain 1 vibrant aqua bag, 5 vibrant tan bags.
      |drab turquoise bags contain 3 light gold bags, 1 striped blue bag, 2 dark maroon bags.
      |posh gold bags contain 5 dotted red bags.
      |muted violet bags contain 3 bright fuchsia bags, 1 muted tomato bag, 4 drab lime bags, 2 dim olive bags.
      |drab orange bags contain 5 vibrant brown bags, 5 striped bronze bags.
      |faded orange bags contain 4 bright bronze bags.
      |clear plum bags contain no other bags.
      |drab black bags contain 3 mirrored bronze bags, 1 dotted bronze bag, 5 light gray bags.
      |pale salmon bags contain 4 plaid coral bags, 5 wavy yellow bags, 2 light coral bags.
      |dim teal bags contain 5 dark brown bags, 5 dark crimson bags, 5 dull aqua bags.
      |bright silver bags contain 3 pale beige bags, 4 bright beige bags, 3 wavy maroon bags, 1 drab chartreuse bag.
      |dull white bags contain 4 dim chartreuse bags, 5 vibrant coral bags, 2 muted indigo bags.
      |wavy tomato bags contain 3 bright fuchsia bags, 2 dull fuchsia bags, 4 pale brown bags.
      |striped magenta bags contain 2 shiny white bags, 1 shiny brown bag, 2 bright tomato bags.
      |shiny chartreuse bags contain 3 mirrored green bags, 3 dark salmon bags.
      |vibrant silver bags contain 2 faded gray bags, 3 mirrored orange bags, 4 clear black bags, 2 pale lavender bags.
      |dull tan bags contain 3 light black bags, 4 faded brown bags, 2 pale bronze bags, 3 shiny lavender bags.
      |light gold bags contain 2 wavy beige bags, 5 plaid salmon bags, 3 shiny cyan bags.
      |posh plum bags contain 3 dim beige bags, 1 wavy magenta bag, 3 dull fuchsia bags, 4 dull gold bags.
      |dull red bags contain 1 striped teal bag, 2 dark violet bags, 1 shiny tomato bag, 1 striped yellow bag.
      |plaid lavender bags contain 2 light purple bags.
      |faded chartreuse bags contain 4 vibrant yellow bags.
      |shiny brown bags contain no other bags.
      |light crimson bags contain 3 wavy orange bags, 3 pale white bags, 4 vibrant turquoise bags.
      |mirrored chartreuse bags contain 2 muted maroon bags.
      |wavy white bags contain 3 striped maroon bags, 5 striped aqua bags, 4 dull fuchsia bags, 1 wavy orange bag.
      |plaid gray bags contain 1 muted brown bag, 3 shiny brown bags.
      |drab white bags contain 5 muted yellow bags.
      |mirrored gray bags contain 3 light tan bags.
      |bright lavender bags contain 1 drab lime bag, 4 plaid olive bags, 3 pale blue bags.
      |shiny yellow bags contain 5 muted orange bags.
      |pale bronze bags contain 4 mirrored coral bags, 2 muted red bags.
      |drab beige bags contain 2 plaid purple bags, 5 clear magenta bags, 3 faded coral bags, 2 clear beige bags.
      |vibrant yellow bags contain 2 mirrored lavender bags, 1 light orange bag.
      |vibrant red bags contain 3 vibrant salmon bags, 2 plaid teal bags, 2 dull gray bags, 2 vibrant gray bags.
      |wavy olive bags contain 1 shiny bronze bag, 5 plaid coral bags, 3 dotted gold bags, 1 pale brown bag.
      |muted white bags contain 3 plaid violet bags, 1 dim black bag.
      |faded olive bags contain 4 mirrored chartreuse bags.
      |muted gold bags contain 1 dim orange bag.
      |mirrored orange bags contain 3 light purple bags, 4 dull lavender bags, 1 bright bronze bag, 4 pale black bags.
      |light chartreuse bags contain 3 muted red bags, 3 bright violet bags.
      |pale teal bags contain 4 dim lime bags, 4 pale crimson bags, 2 clear lavender bags.
      |muted coral bags contain 3 dark green bags.
      |shiny bronze bags contain 1 pale blue bag.
      |dim crimson bags contain 1 vibrant indigo bag, 4 pale crimson bags.
      |striped gold bags contain 3 dotted gray bags.
      |light lime bags contain 4 mirrored teal bags, 4 dotted cyan bags.
      |posh magenta bags contain 3 posh salmon bags, 5 bright bronze bags, 5 mirrored maroon bags, 2 vibrant purple bags.
      |pale chartreuse bags contain 4 bright purple bags, 1 posh salmon bag.
      |mirrored violet bags contain 2 mirrored magenta bags, 1 dotted cyan bag, 2 dark beige bags, 1 mirrored plum bag.
      |shiny cyan bags contain 4 bright violet bags.
      |bright indigo bags contain 1 muted teal bag, 4 faded silver bags, 3 dim indigo bags.
      |striped maroon bags contain 5 shiny magenta bags.
      |muted bronze bags contain 2 drab salmon bags, 1 mirrored orange bag, 2 light coral bags, 4 clear plum bags.
      |dotted white bags contain 2 bright plum bags, 3 pale lavender bags, 2 muted red bags.
      |pale indigo bags contain 1 dull green bag, 5 drab olive bags, 5 dull salmon bags, 5 dark cyan bags.
      |dark plum bags contain 2 pale white bags, 5 dim plum bags.
      |clear crimson bags contain 4 wavy blue bags, 5 dim chartreuse bags, 1 plaid maroon bag, 4 dull cyan bags.
      |striped lime bags contain 2 bright chartreuse bags, 2 striped orange bags.
      |dark beige bags contain 5 mirrored black bags, 3 dim purple bags, 5 light purple bags, 5 dark brown bags.
      |clear cyan bags contain 4 mirrored plum bags, 3 dotted purple bags, 1 dull fuchsia bag, 5 bright teal bags.
      |plaid olive bags contain 5 posh tomato bags, 3 faded bronze bags, 4 pale white bags.
      |plaid indigo bags contain 4 drab silver bags, 2 dotted purple bags, 3 vibrant green bags, 2 vibrant aqua bags.
      |drab purple bags contain 1 shiny green bag, 5 wavy gray bags.
      |bright violet bags contain 4 posh salmon bags, 1 shiny bronze bag, 1 vibrant indigo bag.
      |dark purple bags contain 4 dotted purple bags, 3 striped yellow bags, 5 vibrant purple bags, 3 mirrored lavender bags.
      |dull silver bags contain 3 clear turquoise bags, 3 dim salmon bags.
      |drab red bags contain 2 muted tomato bags, 3 dim bronze bags, 2 mirrored black bags, 4 faded salmon bags.
      |light yellow bags contain 2 mirrored orange bags, 4 vibrant fuchsia bags, 5 drab purple bags.
      |dim white bags contain 5 mirrored white bags.
      |dotted lime bags contain 5 dotted magenta bags, 5 dark maroon bags.
      |striped blue bags contain 3 pale white bags.
      |pale tomato bags contain 3 dull teal bags, 4 vibrant green bags, 1 bright crimson bag.
      |mirrored lavender bags contain 1 wavy cyan bag, 2 drab cyan bags, 1 striped olive bag.
      |dull orange bags contain 2 pale salmon bags, 2 posh olive bags, 2 dark coral bags, 1 plaid coral bag.
      |pale tan bags contain 1 shiny bronze bag, 4 dim coral bags, 4 vibrant green bags, 4 dotted gold bags.
      |drab brown bags contain 3 striped teal bags.
      |dim bronze bags contain 1 drab magenta bag, 1 mirrored gray bag, 2 striped purple bags.
      |clear fuchsia bags contain 4 plaid brown bags, 4 dim bronze bags, 1 posh lime bag.
      |faded lavender bags contain 2 posh gray bags, 3 dark cyan bags, 4 muted tan bags.
      |bright turquoise bags contain 3 pale white bags.
      |muted lime bags contain 5 plaid olive bags, 4 muted orange bags.
      |vibrant bronze bags contain 5 dim silver bags, 4 dotted fuchsia bags, 1 vibrant gray bag, 1 muted brown bag.
      |drab violet bags contain 3 bright turquoise bags, 1 posh olive bag, 2 wavy olive bags.
      |drab plum bags contain 2 clear turquoise bags, 5 striped violet bags, 5 muted black bags.
      |vibrant tan bags contain 2 dull brown bags, 1 vibrant indigo bag, 1 dim crimson bag.
      |bright gray bags contain 2 wavy gray bags, 4 clear maroon bags.
      |faded fuchsia bags contain 4 faded tan bags, 1 clear silver bag, 1 faded tomato bag.
      |muted magenta bags contain 3 shiny green bags, 4 pale aqua bags.
      |plaid green bags contain 2 wavy red bags, 1 pale yellow bag, 5 posh black bags.
      |dotted orange bags contain 5 clear crimson bags, 2 bright gold bags, 2 dim violet bags, 3 faded gray bags.
      |faded tomato bags contain 4 drab coral bags, 5 bright lime bags, 2 light black bags, 2 muted olive bags.
      |pale aqua bags contain 3 light bronze bags.
      |wavy indigo bags contain 4 bright crimson bags.
      |drab magenta bags contain 1 plaid coral bag.
      |dim tomato bags contain 4 clear gold bags, 4 bright crimson bags, 4 light beige bags, 4 striped white bags.
      |striped turquoise bags contain 5 bright green bags, 5 dark bronze bags, 3 pale crimson bags.
      |vibrant lime bags contain 4 shiny maroon bags, 2 shiny plum bags, 3 dull green bags.
      |muted fuchsia bags contain 3 dim purple bags, 2 pale cyan bags, 4 pale gray bags, 3 drab salmon bags.
      |dim cyan bags contain 4 shiny salmon bags, 3 striped white bags, 3 plaid chartreuse bags.
      |mirrored purple bags contain 5 posh plum bags, 1 light purple bag, 2 plaid aqua bags, 5 striped gold bags.
      |plaid coral bags contain 4 vibrant indigo bags, 3 bright bronze bags, 1 dotted purple bag.
      |plaid maroon bags contain 3 wavy olive bags, 5 pale plum bags, 2 dark beige bags, 4 striped white bags.
      |plaid turquoise bags contain 2 pale tomato bags, 4 faded white bags, 4 bright teal bags.
      |drab bronze bags contain 1 mirrored brown bag.
      |clear coral bags contain 5 plaid maroon bags.
      |vibrant indigo bags contain 2 striped purple bags, 4 vibrant green bags, 3 dotted purple bags, 1 vibrant turquoise bag.
      |bright bronze bags contain 5 dotted purple bags, 4 shiny brown bags.
      |striped tan bags contain 2 light purple bags, 3 mirrored blue bags, 5 dim magenta bags.
      |pale brown bags contain 4 wavy green bags, 1 wavy yellow bag.
      |faded yellow bags contain 4 dotted red bags.
      |plaid magenta bags contain 5 striped magenta bags.
      |bright crimson bags contain 4 vibrant tan bags, 2 dotted gold bags, 5 striped purple bags.
      |dull yellow bags contain 3 clear cyan bags.
      |dotted chartreuse bags contain 5 dim coral bags, 1 dull gray bag, 2 posh tomato bags.
      |vibrant crimson bags contain 2 pale black bags, 5 mirrored silver bags, 1 clear beige bag, 5 drab yellow bags.
      |wavy red bags contain 2 drab silver bags, 3 shiny salmon bags.
      |light blue bags contain 4 posh black bags, 2 clear magenta bags.
      |vibrant brown bags contain 4 dim red bags.
      |drab indigo bags contain 3 shiny aqua bags, 4 dotted lavender bags, 4 dim brown bags, 2 faded purple bags.
      |pale plum bags contain 4 wavy blue bags, 3 posh bronze bags, 4 pale lime bags.
      |muted beige bags contain 5 bright bronze bags, 3 dull olive bags.
      |muted purple bags contain 4 striped purple bags.
      |faded silver bags contain 1 light purple bag, 3 bright tomato bags, 1 mirrored magenta bag.
      |vibrant olive bags contain 3 muted turquoise bags, 5 wavy blue bags, 1 dotted silver bag, 5 striped tan bags.""".stripMargin
