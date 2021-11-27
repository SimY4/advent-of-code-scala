package aoc.y2020

object Day10:

  def solve(input: String): Int =
    val adapters = input.linesIterator.map(_.toInt).toList.sorted

    val res = adapters
      .zip(adapters.tail)
      .map((f, s) => s - f)
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .toMap

    (res(1) + 1) * (res(3) + 1)

  def solve2(input: String): Long =
    val adapters = input.linesIterator.map(_.toInt).toList.sorted

    val arr = Array(1L, 0L, 0L, 0L)
    adapters.foldLeft(0) { (acc, ad) =>
      val d = ad - acc
      System.arraycopy(arr, 0, arr, d, arr.size - d)
      (0 until d).foreach(arr(_) = 0)
      arr(0) = arr.sum
      ad
    }
    arr(0)

  val input = """152
                |18
                |146
                |22
                |28
                |133
                |114
                |67
                |19
                |37
                |66
                |14
                |90
                |163
                |26
                |149
                |71
                |106
                |46
                |143
                |145
                |12
                |151
                |105
                |58
                |130
                |93
                |49
                |74
                |83
                |129
                |122
                |63
                |134
                |86
                |136
                |166
                |169
                |159
                |3
                |178
                |88
                |103
                |97
                |110
                |53
                |125
                |128
                |9
                |15
                |78
                |1
                |50
                |87
                |56
                |89
                |60
                |139
                |113
                |43
                |36
                |118
                |170
                |96
                |135
                |23
                |144
                |153
                |150
                |142
                |95
                |180
                |35
                |179
                |80
                |13
                |115
                |2
                |171
                |32
                |70
                |6
                |72
                |119
                |29
                |79
                |27
                |47
                |107
                |73
                |162
                |172
                |57
                |40
                |48
                |100
                |64
                |59
                |175
                |104
                |156
                |94
                |77
                |65""".stripMargin

