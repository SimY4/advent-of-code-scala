package aoc
package y2020

import scala.annotation.tailrec

object Day24 {
  @tailrec private def navigate(line: String, acc: Coord = Coord(0L, 0L)): Coord =
    line match {
      case _ if line.startsWith("e") =>
        navigate(line.substring(1), acc + Direction.Right.direction + Direction.Right.direction)
      case _ if line.startsWith("se") => navigate(line.substring(2), acc + Direction.DownRight.direction)
      case _ if line.startsWith("sw") => navigate(line.substring(2), acc + Direction.DownLeft.direction)
      case _ if line.startsWith("w") =>
        navigate(line.substring(1), acc + Direction.Left.direction + Direction.Left.direction)
      case _ if line.startsWith("ne") => navigate(line.substring(2), acc + Direction.UpRight.direction)
      case _ if line.startsWith("nw") => navigate(line.substring(2), acc + Direction.UpLeft.direction)
      case ""                         => acc
    }

  def solve(input: String): Int =
    input.linesIterator
      .map(navigate(_))
      .toList
      .groupBy(identity)
      .count((_, list) => (list.size & 1) > 0)

  extension (c: Coord)
    private def neighbours: List[Coord] =
      List(
        c + Direction.Right.direction + Direction.Right.direction,
        c + Direction.DownRight.direction,
        c + Direction.DownLeft.direction,
        c + Direction.Left.direction + Direction.Left.direction,
        c + Direction.UpRight.direction,
        c + Direction.UpLeft.direction
      )

  def solve2(input: String): Int = {
    val floor = input.linesIterator
      .map(navigate(_))
      .toList
      .groupMapReduce(identity)(_ => true)(_ ^ _)

    (1 to 100)
      .foldLeft(floor) { (acc, _) =>
        acc.toSeq
          .filter((_, v) => v)
          .flatMap((tile, v) => (tile -> v) :: tile.neighbours.map(n => n -> acc.getOrElse(n, false)))
          .map { (tile, v) =>
            val blacks = tile.neighbours.map(acc.getOrElse(_, false)).filter(identity).size
            (
              tile,
              if v then !(0 == blacks || blacks > 2)
              else 2 == blacks
            )
          }
          .groupMapReduce(_._1)(_._2)(_ || _)
      }
      .count((_, v) => v)
  }

  val input = """seesweseseeeeeeeeeenweeee
                |wswneseseseswseswseseseseswsesesesesese
                |senwnesenenesenwseseswnwnwwnewnenwnew
                |neseeseseseseseseseseseenwseseeseswe
                |neswnweweeeeeeweese
                |nenenwnenwnenenenenenenenenenenesenwne
                |sesesesesenesesesesenwseseseseseewee
                |eeeeesesweeweseeeeneeeeenw
                |neswwnesenenenenenwenenenenesenesenenwne
                |wseseeseenwsesesesesesesesesesesesese
                |newneeneeseenweenenee
                |eeeneeeeneneneneneswneeneenewenw
                |nwewwnwwwwnwwwwwswnwwnwwewnww
                |wnwswswwsewnenwnwnweneswnwseenenwnwwnw
                |enwnwseswnenenwnwnwnwnwnenwnwnenwnwnwnwnw
                |swsesenewnwsenewseneswswswseseswswsesw
                |seseswseseneeenwwseeseseswse
                |sweeeeneneweeeeeeeneeseee
                |newwwswwnwsewwsewsww
                |wwswsweneswewseswswswswnwnesweswnesw
                |neneneneeenewnwnewsweeneeseeneese
                |swwwswswsewseseswseswenesewseseene
                |nweeesewwwsweseee
                |wwnwwnwnesenwsenwnwsenwwnwwnwnwswsenene
                |esesenwseswnwwseneeseenwswswswswsesese
                |wswswswswswnwswnweswnwswnwswswseswswese
                |swswnwneswswswswswswsweswswswswnewswswsw
                |seneseeswswnwswseswseseseseswswsenesew
                |swseseseseseseseeswsesesesenwsesese
                |seswswseseswswenenwswseswseseswnwsewnwse
                |seswsesesesesesesewseneseneswseseseswsese
                |sesenewnenenewneneneneenenenenenenwnene
                |wnenwenwneswsewsesenwwewwswwswe
                |swwseswswswswneseswseseswneswswsesesewse
                |nesenenenwneneeswnewneneswnenenenenenene
                |esesewseeneseneseweenwnwneseeswewe
                |nwsewwnwnwnwnwnesesenww
                |nwnwnwnenewnenwnwnwnwnwsenwwnwnwnwenwnw
                |seenenewneeneneneneneneeewneneew
                |wseswwswswswswswswswnwswswwwneswewww
                |wsenwwnwseenwwwwwwwwwnwwwww
                |eenweeeeeeweswsweeene
                |newnwseseeeseeewenesewweseeee
                |neeseseeeswseeneswweeeseeseeee
                |seseeeseeseeeseeseeesesenwseesw
                |nweswseseeenenesesweesewsesesenw
                |seswnwnwneseneneswwnenenwewnesesesenwnw
                |nwneeeneneneneenenewsweneeeeswne
                |neswsenwwnwneswnwneswswneswenwnenwnw
                |swnwnenwwnewswwnwnwswwnwewwwnwnw
                |nenenwnenenenewneneneneseneneweenww
                |seneneneneneeneswnenenweewnenene
                |eeesenweweneeeseese
                |eneneeesewnenewenewnewseseneee
                |nenenewsenenenwwnenwneseneenenenwnesee
                |wswsewwwwwwnwwneswwswwwewwwsw
                |wswswswwswwwswwwewswswwseswnew
                |seenwwseseseseesesenwsesesesesesesewne
                |wswsenwwesewnwnwwnenesenwwseewnenww
                |senwwsenwswseseswswnwswneswswswswswsenwe
                |swneseseeseeeeesewnweeeeesee
                |seeseeweeseseseeeseeseseeenee
                |eseswneseseseseswwsesesenesesewse
                |eeneeewneeeeeenweeeeseenese
                |eeseswenwnwesewnwnwese
                |seswswwswswswswswswswswswswenwseseswsw
                |enwwnwewwnwswneswnwwnwnww
                |nwwnwenwnwwnwwnwnweenwnwwsenwnwnwnw
                |senwnesewwwnwwswsesesenwsenwwenwnwne
                |eswswswswwneseswseswswswseeswnesesewsww
                |wwwwwwwnwnwwwwwnesenwnwsenwwww
                |nwnwwenewnwnenenwnwnenwsenw
                |wwwwwnwwwwsewwsewnewwnwewww
                |wwnwwwnwwwwwwweswwwwwww
                |nwwwwwwnwwwswnwwwesewnwnwwew
                |wseweswswswswneswswsw
                |nwneswneenenenwwnweneneneneneeesesene
                |nwwwnwnwnwsenenwwnwnwswnwwwnenwnwse
                |ewwsenwwnwenwenwewwneswwsenwwsw
                |esesenwswswseseswsenesesewswseseseswse
                |nenwnwnenwnwneswnenwsweneseenenenenenwnw
                |sewwewwwwwwwwswwwwnewwsw
                |swswswswswswswswswswswswswswswneswww
                |swseseswnwewwswseeswnesweswseswwswswsw
                |wnwneseswenweneswswweneeseneenenw
                |wswseseenwsesenwseseewnesesesesesese
                |eseseewwnwsweneswnwswswwswsesewswesw
                |neeneneeswneswswwenwwwseneneneene
                |neneswnwsenwswnenwnenwnwnwswnw
                |neeenweeeneeneeesweeneeeene
                |nweeeeeeeeneeeneweenweeswsesw
                |swwwnwswweswnwwwewnewwwseswwwsw
                |seseseseseswsesesesesenese
                |eswneswneewswnwswswswewnweeswwse
                |seswseswswsenwneswnwseswswswseswnwnesesesw
                |eeweeswneenweeeneneeeneswseene
                |nenwnwswnwnwenwnwenenwwnwswnenwnenwnwnw
                |enwseeneneeeeweswneneenewnee
                |enewsewwwwneewwwswswwnwwew
                |eesweenwnwnwseeesenwseswneseseseswe
                |weseneseeseewnwwswnwneswswsenesewnw
                |nwnwsenwnewwwnwsenwnwnwwnww
                |nwnewnesweneneneneneswnenenwnenwenwenwnw
                |eswswwwswswnwswswswswwswenewswsww
                |neeneenenenwswnewwsweswswneneenene
                |nwseswenwnwsenwenwwswesewsesweesw
                |nwnweeswswnwnwswewnwnenwnwswenenew
                |swseenweswnesenwswwnwnwwnwnenwswsenenw
                |eeeeeneseesewesenwnweneeneenee
                |seseseseenweseeseseeswseseeseseenese
                |senwwwswswswswswwswswwswswenwswwswsw
                |eeneeneneeeneneneeeesenewneeneew
                |wnwnwwnwswsweeseswswswswswwwwswe
                |sesesesenewseeseseseseseseesesesewse
                |nenwnwenwnwneswnwnenwenww
                |nwswswswsewwneswswwwswneenewswwwwsw
                |eswenwsweeswswnwnwenwewewswnene
                |neesweneseeeeeneeeseeeeswnwesw
                |seseeneeweeweeeeseeeewee
                |nwnwnwnwnwnwnwnwnwnwnenwsenwnwnwnwnw
                |nenwnwwnwnwnenenwnenwnenenenwseswnenwnwnwse
                |wwnesenwneneeneeseneewswnwneneeese
                |senwnwnenenwneswnwnenweswweweneseswse
                |nwnwnwwnwnwswnwswnwwnenwnwnwwenenwwnw
                |eswnenenewnenenenesenenenwseewnenwnwsw
                |wsenenwwwwnwnwwwnwwnwnwwnwnwww
                |seswseswsenwswswseseswseneswsese
                |swswswswswswswneswwswnwseseswswswswswsesw
                |enwneswswswswnenenenenenenenwneenenene
                |wsesewswwwwwwwnwnwwwswwweww
                |enwwwsewwwsewwnwnwnwwwnenwww
                |eeeeeneswsweeeeeeeeenwee
                |nwwnewnwwswewnwnwnwnwsenwwnwsenwse
                |nwnwsewwwnwwnwseenwswswenwenenwnwwnw
                |seswswswswseswswseswswswneneswswswswswswsww
                |wswnwseswewwwswwwswswswwwswwww
                |wwnwwnwwwwsenwswenwe
                |seseseseswseseswnesesw
                |swswswswswseswneewnwswswswswsw
                |seswneesewseneewswseswwnewsenesesewnw
                |nwswwenwnwswneneesenewnwswnenw
                |seeswswswswswswwswswswswwneenwswswe
                |wswwseswnenwwseswneneeswsesweseene
                |eseseeneewseseseewseseeseeseesese
                |senwnwnwnwswswwneneenwnwnenwsenenwswnese
                |wnewswnwesewwneswsewswwsewwswnw
                |sewnwnwseseeewnwesenwwswnwwwsenwww
                |swenenwswnwnenwseswnwnwnenwnwnenwnwnesew
                |wswenewwwwnwnwenesweswweswseswsw
                |eseseeeswwnesesesesesewnweenweseswne
                |nwnwnwnwnwnwenwnwnwnwwnwnwnwnwwswew
                |eenwweneeeeeneneeeesweeesee
                |newsewwwwwwwwwwswwwnewwwew
                |wsewnewwwwewswswnwwnwwwneww
                |nenenwneseneenwsewneneswneewwnwnwnw
                |nenenenenenenenenwswnwseneneewnwne
                |newnenwnwneneenenenwnenw
                |swseseseneseswswswswswnwswswswwseswswseese
                |sewwwnewnwwwwenewwnwwwswwwnwse
                |weswseseseseeseseseseswseseseesewwsw
                |swesenenesweseeswneseeswnwseesesee
                |wswwneneseeeneeeseseseneswsenwswwne
                |wenewseswsenwnenenwnewnewsesenwnew
                |swswsweswswswswswswswswswnwswswwswswe
                |seenwseswswenwwsesenwseneswenw
                |nwswswswswswseswswnwseneswsesweswwsesw
                |nwseswseswsenwnesesesweenewnwswwswse
                |swswswseswsewnweswsesesesee
                |eeneenesenenwseeneneneneneneneswnenw
                |swweswsewswnwswwswwswswswswswswswsw
                |swswseswseswswswswswsene
                |enewswwsesesewnweeneeswwswesene
                |seeeeeseweseenweseseseseesenese
                |swwswnwneswnesenwnenesenesene
                |seeseneswwswewnwseeeeesenenwwnene
                |nesesenwswswnwswnwnewnwwsenenewe
                |neewneeseeeweseeswenwseseeeee
                |nenesewneseneneneewwnenenenwwe
                |swnenwnenwnwnwnwnenwnwnenwnwnenwswnwnwe
                |wswneswseewseneswswenwseswnwswneswwsw
                |neneeeeneeneneenweseneeeneneneew
                |seneswwwwwsesenwwnewnenwenwnwew
                |enwwnwnwnwwwnwwwwsenw
                |wnewwwwnewwsewwwwsewwsweww
                |nwwwnwsenwnwnwwwwnwwwnwewwnwww
                |swswwswswseseswwswwswneswnwswswwwswe
                |nwenenesweswneenesenwenw
                |eenwenesweeneweneeweneseeswsee
                |enwneneneeswnwnewneseeeeswnwewee
                |neneewnenewwnenenesenenesenene
                |nwnenwnwenwnwsenwnwnwnenwnewnenwseewnw
                |ewwsewwswewwnwneeseneswwsweewne
                |wwnwwnenwswnwwnwnwnwnwwnw
                |nwnwswnwnwnweswenwnwswnwnwenwwnwnenw
                |swwnewnwwnwswseswwsweewewwwswwsw
                |seswswswsewswseseswswneswswswwseneswwne
                |seseesesesenesesesweseseswesewnwenwe
                |swenenwnwnenwneneneneswnenenenwnenwnw
                |nesewnewsewnewenwswsenwneeneswnwswsee
                |nenwsenwnenwseseswnenenwwnwnwnwswnenene
                |seswwwwswwswnwwsw
                |eeeesweeseseneseeeenenwsesesew
                |nwseseseswseswseseseseseeseweseneseswne
                |neneseswenewswenwswswneseswnwwswnwene
                |nwnwnwwwewwnwnwnwnewnwwswnwwwsww
                |swwwwswwswswsewneswwswswswswwww
                |wwswnwwswswswnwswswswswswwwswwwese
                |nwnwnwwnwswnwnwnwnwwnwnweenwnwnwnwnwnwnw
                |nenwnenenenwnwnwnenenwswnwnwenwnwnwne
                |seneneneneneneneneenenenweesweneenenwne
                |senwenwenwsenwnwnwwnewnenwnwsenenenw
                |esenweesesesenwsenesweneeesewswnw
                |neneenewneeeneeneneneneneneneeswnee
                |senwwwneseseseneesesewseseseseeesew
                |nwwnenwnwnwwnwsewesewwwnwnwwsww
                |seseseseseswsesenese
                |neseenenwweswnwnewwsenenwnwwnenenw
                |nwwwwwwwwwnwnwwwsesewnwwwnw
                |nweeeeeeeneeswesewsesewneese
                |nwnwenwwseeneeswswwnwnenenesenenwnwswne
                |swsesenwesesenweeeeeseseseesweesese
                |swswswwsenwswwsweswswswswwwswswswswsw
                |swswnwseseseseswseeseseesenwneseseese
                |nwnwwsewnwnwwnwnwnwnwwwwnw
                |nwswswwswswsweswswswwswwswsewswwnw
                |swnwwseewswnwwswswwsw
                |nwwnweweseswsewneneswnwswsesenenenwsw
                |nwneeswneneeeswwswnenweneneneneene
                |nwnwnenwnenenewnwenesenwnwnenenwnwnew
                |seseseseseswwseseseseneswsesesw
                |nenenwneneewnenenwnenwswnwwsenwnwenew
                |eneneneseneeneneswseenwnenewswswenenwnw
                |newwswwswwseswenwwwnwnewsenwneew
                |nwwwnwswnwnwwsenwnwnenwswnwenwnwnwenw
                |neeneeneseseeneeeneewewewewe
                |swsweseswseneswsenesenenwswene
                |wswneeswseseneeesenweseeesesesesesee
                |nenwnenwnwnwnwnwneeswnenwnwnwnwsw
                |swswneswnwnweswnwswwswnweswsweswee
                |nenenenwnenwnwnenenenwswnwnwnwnenenwsee
                |wswseseneswwwswwswnwswswswswswswwswsw
                |newnwsesesesewseeneseesesenesewswnw
                |senwnwwwswnwnwnenwnwnwwnwnwnwnwnwwnenwnw
                |nenwnenenenwnwnwnewenwnwsenwnwnenwnww
                |nwnwwwwwwnwwsewnenwwwsewsewww
                |nwnwwnwwwnwwswwnwwwenwnw
                |swwnwswnwnwnwnenwewseswnwswwneneww
                |sewseseseeeseewnwneeweesenesese
                |swswwneswnwswswswswwswneswswsewswwwse
                |neswswwseswswswseeswswneswwswneseswsw
                |swseswseseeswsesesesesenewsewseswseswswse
                |seeenweeeseeeeenwsesenwnwseee
                |esesewseswswswswseswseswswnwnwseswsene
                |wwwnwwwnwwwnwnwnwnwnenwnwsesenwnwwnw
                |swswsewswseswneswswswseeseswswseswswsw
                |newseeseeeseseeneeeeseeeewe
                |neswneswwenwesweneenenesenenweeesw
                |neeeneneneneeeeweneneneeeswnene
                |eseseswseseseseseneseeneseswenwwsese
                |neneneneneeneneenwneneneneeneneswswenw
                |senwnewnweneeneswseneswwenenewnene
                |neseeewneenesesewsenwswsenwswsenese
                |nwswnenwnwnwwenwsesenwwsewnwenese
                |swswwswneswwswswwswswswwenwswwwsw
                |nwneneneneneeneneswneeswnenenenenenenenene
                |neseswswswseseneswseseswswseswswswswsenwse
                |neswnenenesweswswnenenwneneenwsenwnene
                |swseseseswswwswswwweneswneesewwne
                |enwnwnwnwnwnwnwwenwswnwwnwwnwenwnwnw
                |wwwswwswwwwwswweswwswwwee
                |seneeeneeneneneswneswnwenwnenwnwswswse
                |nwnwnwnwnwnwwnwnwnwnenwnenwenwnwnwswnwnw
                |wwseneneseneswnwneeneneswne
                |nwwenewswnwsewswswewnwnwnenwwnwwwe
                |sesesesenwseswsesenesesesesesesesesesese
                |swenweseeeeesesenweseeseneseswsee
                |swenwswseneswwseswenwneswswwswnwswe
                |wswnwwwwwwwwwnwwewswwwwwwe
                |eseseweneeseeeswseeeseeeseenese
                |swneswnesweeewnweswnwswnenwswswswsww
                |nwnwnwseswnwewnwnw
                |neneswnweeeeneeeneneewneneneenene
                |eseseweseswsenwswesesenwsesewesenewsw
                |sesesesesewnenwwswsesesweswsesewee
                |swsewswseeswnwswseswswswswswswswswswesw
                |sewnwnwswnwwnwnwnwneswnwnenwnwnwewnwnw
                |wnwswnenenenenwneneenene
                |nwwneswswneenwnwenwnwnenwnwnenenenwnenw
                |newswswwsweenwnesenwnwsenesweeswsw
                |nwnwnwnwnwnwnwnwnwnwsenwnwnwnwsenwnwnwnw
                |nwnwwnwseenwnwwnwnwnwnwwnwnwnwwww
                |wwnwseeeneenweswswwenenesewsesenw
                |swswsesesenwseeswswswsesenwswseseseseswsw
                |wwswwwnesenewwwwswwwwnwneww
                |seseseswnwswswswswseswswswseneswnwsesw
                |weswwswwwewwenwswwwwwnwwnwww
                |eeeeeneweeeneeeeewseewee
                |seseseswneseseseneneneseseseswwsewsesw
                |neneneneneneneneneeeneneneswneewseenw
                |wnenwnwwswwnwnwewwwwnwnwswwwww
                |swenwnwnwnenwnwnenwnenwnwnenwnwnwnwnwne
                |eeenesweeeenweneeneweseeeswne
                |ewnweeeswwseneneene
                |neneswneneneneneneswnwenenenwnw
                |sewwwwenwnenwwwwwsewwwwnwse
                |wwwsesesewwnesenenwnewnwwsewwsewne
                |swswwwnewwswwswwswnewwwwwswsww
                |seseneweweswneneswseeeeswwenwnwne
                |eseseseesesweswseseseseeseneeeenw
                |neneneeeenewnenenenenwneswneneneneenene
                |nwewewwwsenewswsewswsenenewswwsene
                |neneneswnenwnwneneeeneneneeneswnenene
                |enenenwnwnwnwnwswnenwnwnwnwnenenenenwnw
                |wnewswwwswswswwswswswswswwww
                |swswswswneswswsweswnwswswswswswneseswsw
                |eeeeeeeeenweeeesw
                |seseswseseswsesesesesewsesene
                |wwwwnewswnwsenwwwwsewnewwwww
                |newsenenenenesenwenwneneneneenewnenesw
                |swswswswswswswswnweswswnesenwswswswswsesw
                |newnenenenenenenwesenenewswswnesene
                |wswnwnwnwnwseswenwseeenwnw
                |eewseseseeseseseneseeeneewsewe
                |nenenenenenenesenwenwenewnenenenwnenesw
                |wsenwsewnwwwenweseswnwnenewnwswwnw
                |seswenwswswswswswnwswswswswswswswswsesw
                |nwwnwwnwwnwnwwwswnwnwwenw
                |neenenenenenwenwwnenwnwnenewnenenene
                |eeeeneeneenwnewseeeneeeseene
                |esenwwnwsenwnwnwnwnwnwnenwnenwnwnwnwswnw
                |eeeseeseeeseeseeseeesesewsesenew
                |wweeswnewwnwsewnwwwwwwseewsw
                |seswwwwewwnwewswwswenwwwnene
                |eseeeseseseewseseeseeeesenesese
                |eenwesenwweeneswwnewneeeesenee
                |eeeneeneneswseneenenenenw
                |seseseswswseswseswesenwweswneswseswse
                |nwsewneenenwnenenenenenwnenenenewnenene
                |nwnenwnwnwnwnenwnwnwswswswnwnwnwnwnwnwe
                |neswnwnwnwenewwwnewwnwswswesenwwne
                |nwsewnwwnwnwwsenenenenwnwswwnwswnwnww
                |eeeeeeweeeseeseneeeenweene
                |seswswwswwwwwwswneswswswwnwswwwsw
                |nwnwnwnwwnwnwnwnwnenwnweswnwnwnwnwnwne
                |swweswswswswswwswswswswswwswwnweswwsw
                |nesweewnwswnenwnwnenenwnenwseeswnene
                |neseseseeeeeeswsesewseeneeeesese
                |swnwswseswneswswswswswswswswswwswseswsese
                |swwwenwnwnwwnwsenwwnwnewnwwwwnwnww
                |senwseseseesesesesesesesesese
                |senwnwnwnesewnweenwwnenenwswswnwswnenwnw
                |enenweenwnwweseesweeseseeesw
                |swseswwswenwwswsweswswswsweswweswsw
                |nenwnenenenewseswnesenwnwwneweeneese
                |esesesesesewseneseseseseseseswsesesese
                |wewnwnewsewwwwwswswwwwseenwew
                |nweeenwswsenwnewwewwnwswnwwnwnww
                |sewnwwnwwnwnesenewnenesenwnwseeenwne
                |enenewseeeeeneeeenene
                |eeeeeneeneeesenweeeeneeeesw
                |neeswwseewwneewswswnenwwswwwww
                |wswwswwwseswswwswswneww
                |nwnwnwwnenenesesesenwnwwwnwsewnwnwwnw
                |swneneenwswenwnwweswwwwnweesewse
                |nwnwnwnenwnwnwnenwnwnwnwnenwnwwnwesenenw
                |wswneswswwswswwneewwnwnwweseewne
                |swnenwsenwnwnenwnesenwnwnwnwnwnwnwnwnwnw
                |esweswsweeseeseswenwseenwenwnwnwwne
                |neswseeeeswsesenw
                |swnwswnwsesweswseswsweseswswswswswswsww
                |neswneweneneenwenweeswswnwweee
                |enwswnwenwnewswwnwnwnwnwesewnwnwesw
                |swswseswswswseseseseseseseswswswseweswne
                |eneesenweeeeeneeeenwneeswnenee
                |neneneneneenenwnenwnenenewsenenenenewne
                |neneseeswneneeneenenewneneenenenenw
                |seeswseseswseseeeenwenwseseeesenwee
                |nwnwswwnwneenwnwswsesenwnwseseeeswnw
                |nwesewnenwewseeneneeeewneeswenee
                |swseswseswswewenwswswneswswswnwnwseswse
                |eenweesesweeeseeweeseeeseeee
                |nenenenwnwnenenenewneneeswnenenenenenenw
                |sweesesenweseeeseeseeee
                |nwsenewenwnwnenenenenw
                |eneeweseneeeeeeeeeesweseese
                |nwnwnwwnwnenwnwwnwnwnwnwnwwsesewwnwnw
                |neswswswswseswswswswweswwswswsw
                |nwnenwewsenwnenwnwnwnwnwwnwnenwnwnwnwnwnw
                |seneesewsenesesesesesesesesesewsewsese
                |nenenweeneneenewwseeeenewseswnenene
                |nesesenwseeseseewneseseseewswseee
                |swwswswswswswwwwsweswswnwsww
                |sesewseseseseseesesesesewseesesesesese
                |swnwswwswwwsewseswwwneswwswwsww
                |seswswnewswswswswswswseswseeseseseseswsw
                |eeenwesesenwseseesesewweeseesee
                |swswswswswswswwswneswswseseswewswswsw
                |senwwnwnwnwnwnenwnwnwnwnwnwnwsenwnwwswenw
                |nweesweeeneeneeeneneeeewnenene
                |enwwwnwnwwnwnwwnwwnwnesenwsww
                |swseseneeeweesenwseeenewesewnenew
                |neesesesewseseesesese
                |esesenwseseswseesesesesesesesese
                |swswswswswneseseswswseseswswsenwswswsesww
                |ewwsesesewesenwnenw
                |wswwnwwnwwnenwnwnwsenwwwewnwnwnwnwnw
                |neseseeseweseseseseseesewseseesee
                |eeeneesenwesewnwsweweeeeenesw
                |neneeneeneeneeeeeeneewesenesw
                |eeneneeneneeneewneneseneneneenenwe
                |wnwnenenwnwnwsenenwnwnwnwnwneseewnwnw
                |sweswswswswswswwswnwswswswswsw
                |swneswwswwwswwswwwswswwswsw
                |wneewswneswswswseswwwnwwwwswswsenw
                |wneseneeeeneeeeeeeewneesww
                |swneseenesesweswseseneseseeenwsesese
                |nenwnwnesenenwnenwnwsenenwnenenwnwnenenwnw
                |eneneswneneneeeneene
                |neenewnenwnwnenenenwneswnenwnenenwnwnenw
                |wwwewwwwwwwsewwwnwwwwww
                |eeeeeewsesweeeeeeeenenwesw
                |wnwswwsewwswnesenesenewwwswwwesww
                |nesenwwwswwwsew
                |wnwnwneswneswnwnwnwnwnwnenwsenwnwnwesenw
                |nwneseenwwnenwnenenewnw
                |eseseeeesweeneeseeweeeseeese
                |swswseswseseseseseseswsesenewswswsenesesw
                |nwenenenenwneenenesweeeseseneneswnenww
                |wwsenesenwnenenweneneswnenwsenwwswe
                |nenenenesenenenenenenwwnenesweeneenenw
                |neswsenewnenenenewneeneseneenewnwneesw
                |swsweswswseswseswswswswseswsweswww
                |wnewwewwseswwwwwwwsenenwwsew
                |seswswnewswwwnewwweswwswwswswwsw
                |wnesweenenenwnwesewsweweeeswnw
                |wnwnwwswnwnenwwwnwnwwnwenwnwwnwnwnw
                |neswswseswwswswnwwsewseswnewwswswswsw
                |neesenwwseneswnwwese
                |wnwnwnwswnwwswnewwnwnewnwnwswnwenww
                |nwnenwswnwnenesweneneswnwneeswswnwnee
                |sesesenwseseseseseseswseseesesesesesewse
                |wwwwswewwwwnewwseswswswwswww
                |nenenwneswnwnenwnwnenenenw
                |eswwwnwnenwwwwwewswwwswnwwnew
                |wnwswwsweswswnwnwswseweswswswsweswww
                |swswswswswneswswswneswswswswswswswseswsww
                |wnwnwnwwwnwnwsenwewnwsenewnwnewsw
                |seesesesenwseswseswswsesenwseseswseswse
                |eneswsewesenwwneswwswseswswswwswswsw
                |seswnwenwsesewnwesesesesesweenenwe
                |eswseeweeswwsenwesenweeewnwe
                |seswnwnwewnwnwnwswnwenwnw
                |swwnwwswwwswwswsewswwwwwweswne
                |enwnwnwnewnwnwwnewwswwnwswsenewswsw
                |nenwnenenwnwnwnenewenenwewenwwswse
                |swewswwwswnewnwswnwseswwwswwwswsw
                |eeeeeseeewswswswnenwnweeneneene
                |nwswswswswswewseswswenwseswsweseswswne
                |neeeswneneneneneeneneneswnenwneeene
                |neenenenwnenenesweneseneeesewneseew
                |nwenenenenenwnesenesewnenenenenenewne
                |weseswwneesenewwsenwnwwsweswwsw
                |nwnwswwnewnwnwnwwwsenwwwswwneww
                |nwnwsenwnenenwnenwneenwnwswnewnwnenw
                |nenwneseswnwswwswwwnweseesesewnwsene
                |swswswswswswwsweswwswswwnwswswneswse
                |seswsenwsesweseseseseswseseseswseswswse
                |seswseseseseseseseseeseseswsenwswsesese
                |wwnenwwnwnwwwnwwwwnwnwwwwwse
                |seesesewsesesenwesese
                |swnenwwnwswsesweswnwnenesewswewwse
                |seseseswsewwwesesenwsesesweneswenw
                |senwnwnwwnwnwwnwnwswesenwnwwnwwnwe
                |wnwwwwwwwwnwwewwwwwwew
                |neeeneeenesenweeeeneneneeee
                |swwwseeeneenweeneeeenwseswnwswse
                |wwnwwswswswewseswswwswwswwnwwe
                |wswneeeeneewnwneneseseseeewwew
                |seseeseeswswseesewsenwesesenwse
                |swseneseseseseswswwesesenwswswseseswnese
                |wwnewwnwwswwnwwwnw
                |nwneswnwswswswswswseewswswswswswswesw
                |seeneseneseseweswwwsesenwwneseesee
                |wnwsenesesesesesesesenwseseseswsesese
                |sewwwwewswwwwwsewwwnwnw
                |wswswsewneeswswswswswswwswwswwwswsww
                |eeenenwwneneneneneweweneewneswnee
                |nwwnwsenwwwswwwwnwnwwnwnwnwnwnewnw
                |swswswswseswswswneswswswseswswswswswswnwsw
                |sewnenenenwnenenenenenenenenenenesenwnene
                |swnweswnwnwsenwnenenwswnwswweeeswenw
                |wnenenwneenwseswnwwswnwswwwswwnwne""".stripMargin
}
