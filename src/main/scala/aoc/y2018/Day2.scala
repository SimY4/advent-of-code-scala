package aoc.y2018

object Day2:
  def solve(input: String): Int =
    val (fst, snd) = input.linesIterator.foldLeft(0 -> 0):
      case ((accX, accY), line) =>
        val grouped = line.toSeq.groupBy(identity).values.map(_.size)
        val (x, y)  = if grouped.exists(_ == 2) then (accX + 1, accY) else (accX, accY)
        if grouped.exists(_ == 3) then (x, y + 1) else (x, y)
    fst * snd

  def solve2(input: String): String =
    (for
      line1 <- input.linesIterator
      line2 <- input.linesIterator
      if line1 < line2
      indexes = line1
        .zip(line2)
        .zipWithIndex
        .collect:
          case ((l, r), i) if l != r => i
      if indexes.size == 1
      index <- indexes.headOption
    yield line1.substring(0, index) + line1.substring(index + 1)).next

  val input = """uqcipadzntnheslgvjjozmkfyr
                |uqcipadzwtnhexlzvxjobmkfkr
                |cqcipadpwtnheslgyxjobmkfyr
                |ubnipadzwtnheslgvxjobmkfyw
                |uqcisadzwtnheslgvxjfbmkfor
                |uqcisaezwtnheslgvxkobmkfyr
                |uqcguadzwtnheslgvxjobmkfir
                |uqcipadzmtnhesldvxdobmkfyr
                |uqcipadzwtzheslgdxjtbmkfyr
                |uquipadzwtcheslgvxjobmkfbr
                |uqctpadzwtnhesjbvxjobmkfyr
                |ueciparzwtnheslgvxjobmkfyx
                |uqcipadzwtnhessgvxjkbmkfkr
                |uqcipamzwtnheslgvxioamkfyr
                |uciizadzwtnheslgvxjobmkfyr
                |uqcieadzwtnhesfgvxeobmkfyr
                |fqcipadzwtnreslgvkjobmkfyr
                |uqcipadzrtnherlgvxjobmklyr
                |uqcypadzwtnheslgvxjobmkxfr
                |uqcipadzwtnhemlgvxjobmvfur
                |uwciuadzwwnheslgvxjobmkfyr
                |uqcipadzwtnhcscgvxjobmkuyr
                |upripadzwtnheslovxjobmkfyr
                |uqcipadzltnheslgvxjobmkftc
                |uqcipadzwtnheslgvgjobmifsr
                |uqoipadzwtnheslgvxjosmkfkr
                |uqcipadzwtbhesrqvxjobmkfyr
                |uqcipadzwtnheslpvxjobmhfyx
                |uhcinadzwtnheslgvxjybmkfyr
                |uqcipadzwtnhhslgvxjabmkbyr
                |uecipadzwtnheslgvxjobqyfyr
                |uqcipadfwtnheslwvxjobgkfyr
                |uqcipadzvtnheshgvxzobmkfyr
                |fqcipadzwtcheslgvxjobmkfyt
                |uecipadzwtnheslgpxjbbmkfyr
                |uqclpadzwtnheslgvnjobukfyr
                |qqciprdzetnheslgvxjobmkfyr
                |uqcipahpwtnheslgvxjtbmkfyr
                |uqcidadzwtnhesljvxyobmkfyr
                |uqciradswtnqeslgvxjobmkfyr
                |uqcipadzwtrhmslgvxjobmkfyf
                |urcipadzjtnheslgvxfobmkfyr
                |uqcipadzwznheslgvxjobmkfcv
                |uqcipadowtnheslgyxjobmkfym
                |uqcigadzwtnheslgvxjoomkmyr
                |uqjipafzwtnheslgvejobmkfyr
                |uqcioadzwtnhhslgvxzobmkfyr
                |uqcgpadkwtnheslgvxjobhkfyr
                |ufciiadewtnheslgvxjobmkfyr
                |uqoipadzwtnheslgvxjllmkfyr
                |uqcipadzutnheslgwxxobmkfyr
                |uqcipadzwtlheslgaxjobmkfwr
                |uqcbpadzutnheslgvxjbbmkfyr
                |uucipadzwvnhesngvxjobmkfyr
                |uqcifadzwtnceslgvxjoumkfyr
                |ujcipadzwteheslgvxjobmkfyj
                |uqcipadzwtnheslqvxjobmkuyp
                |uqcipadzwtnheslgvxjoxmkxyw
                |uqcipaduwtnheslgvujmbmkfyr
                |uicipadnwtnheslgvxjobmbfyr
                |uqcipadzwteheslgvxjobbmfyr
                |uqcipadzwgnneslgvxjobmklyr
                |uqcipadzxtnhwslgvbjobmkfyr
                |uqcipaxwwtnheslxvxjobmkfyr
                |uocipadzwtnheslgvxjobqdfyr
                |uqciaauzwtnheslgtxjobmkfyr
                |uncipagzwtnkeslgvxjobmkfyr
                |uqcipadzwtnhehlgvxjohdkfyr
                |uqcipadzwtnheslgvxjobmspyz
                |uccipadzwtnhvsltvxjobmkfyr
                |uacipagzwtnheslgvxjoqmkfyr
                |tqcipaduwtnheslgvxjobmmfyr
                |uqcipadzwtnheslgvxqebmifyr
                |uecipadthtnheslgvxjobmkfyr
                |uocipadzwtnhdslgvkjobmkfyr
                |uqcipadtwtnheslgvxhobmufyr
                |uqkipadzwtnleslgtxjobmkfyr
                |uqcipadzjunheslgvxjobmnfyr
                |ubcipadzwtvheslgvxjobmkfyf
                |uqcipadzwpfheslgvxjsbmkfyr
                |uocipadzwtndeslgvxjobmmfyr
                |uqcipadzwtnheslgtxjobhkfyq
                |uqcipadzwtrheslgvxjobmyfya
                |uqcipadzwtvheslgvxjolgkfyr
                |uqcipidzwtaheslgvxjobmkfxr
                |uyzixadzwtnheslgvxjobmkfyr
                |uqyihadzwtnhedlgvxjobmkfyr
                |uqcipadzwtnhesltvejobqkfyr
                |uqciptdzwtnheslgyxlobmkfyr
                |uqcipzdzwtnhzslgvxjosmkfyr
                |uqcipadzwtnbeslgexjobmkfvr
                |uqcipadzwtnheslcwxjobmkkyr
                |uqcapadzwcnheslgvxjolmkfyr
                |uqcjpadzwtnhejlgvxjxbmkfyr
                |uqcipadwwtxweslgvxjobmkfyr
                |uqmipadzwtnhezlgvxjobmkyyr
                |uqcipubzwtnpeslgvxjobmkfyr
                |uecvpadzwtnheslgvxjocmkfyr
                |uqcipadzwfnheslgvxjibmkdyr
                |uqcipadzwtnheslgvxvfbykfyr
                |uqcipadzwtnheslgvgjoimkfyt
                |dqcqpaqzwtnheslgvxjobmkfyr
                |uqcipbdzwtnheslgvxjobmkghr
                |jqcipadzwtnheslgvxjgbmkzyr
                |uqcipadzwtnheslgvxqkqmkfyr
                |uqcipadzptnheslgvxjxbokfyr
                |uucijadzwtwheslgvxjobmkfyr
                |uccfpadzwtnheslgvxjobpkfyr
                |uqcipadzwtnheslgvxjobakeyq
                |uqcipadzwtnheolgvxqobjkfyr
                |imiipadzwtnheslgvxjobmkfyr
                |uqcehadzwtnheslgvxjobmkuyr
                |uqcipadzztnheslgvxjorokfyr
                |rqcixadzwtnheelgvxjobmkfyr
                |uqcipadzwtzheslgvxjodmkfyi
                |uqcipaezwtnwuslgvxjobmkfyr
                |uqcipadzwtnheslggxjobjkfyq
                |uqcipadzwkghesagvxjobmkfyr
                |uqcypqdzwtnheslgvxjobakfyr
                |iqcipadzwtnhezltvxjobmkfyr
                |uxcimadzwtnheslgvxjobmxfyr
                |uqcipaizwtvhwslgvxjobmkfyr
                |uqcipafzwtnheslgvxjpbmkfym
                |uqcipadzwinheslgvxlobmpfyr
                |uqcupadzwtnheslkvxmobmkfyr
                |uqcapadzwtnhesrgvxjobmkfsr
                |urcipafzwtnheslgvxjobmkfur
                |uqcipaczwtnheslgvbjobmknyr
                |uqcizadzztgheslgvxjobmkfyr
                |uqcipfdzwtnhesxgvxjobmkfyw
                |uqcipbdzwtnhyslgvxjobmcfyr
                |uqcipadzwanhezlgvxjobmkfwr
                |uvcipadzwtnheslgvxjbkmkfyr
                |uqcipajzwtnseslgvxjobmkfyq
                |uqcipvdzwtnheslgvmlobmkfyr
                |uqcipadzdgnheslgmxjobmkfyr
                |uqcipddzwtnhestgvpjobmkfyr
                |umcipadzwtdheslgvxjzbmkfyr
                |uqciuwdzwtnheslgvxjobmkflr
                |uqcipadzwtnheslgsxabbmkfyr
                |uceipadzwtnheslgvxjobgkfyr
                |mqcipadzwtnhesrgvxjobmjfyr
                |aqcipadvwtnheslgvxjobmkryr
                |uqsipadzwtnofslgvxjobmkfyr
                |uqcixadzwtfheslgvxjzbmkfyr
                |uqcipadnwfnheslgvxjohmkfyr
                |uqcivadzwtnheslfvxjobmkfyz
                |uqciprdzwtnheslgvxjobmkjir
                |uqcipadhbtnheslgvxjoxmkfyr
                |fqcipadzwtnhesfgvxjobmkfye
                |uqoipqdzwtnheqlgvxjobmkfyr
                |uqcipadzwtnhesltvxmobmkzyr
                |uqcipadzwtnhebqgvsjobmkfyr
                |uqcipadzwtnheslglxjobmfbyr
                |gqcipadzwtgheslgvxjobwkfyr
                |uqcipadzwtnheslgfxjzbmlfyr
                |ujcnpadzwtnheslrvxjobmkfyr
                |ujcivadzwtnheglgvxjobmkfyr
                |uqcitadzwgnheslgvxjofmkfyr
                |uqcipahzatnhmslgvxjobmkfyr
                |uqzipaizwtnheslgvujobmkfyr
                |uqcipadzltnheylgvnjobmkfyr
                |uqcidadzwtnhwsljvxyobmkfyr
                |uqcipadzwtihetlgvxjobhkfyr
                |oqcipabzwtnheslgvfjobmkfyr
                |uqcipadzwtnveslgvxjobzkfzr
                |uqcipadzwtjheslgqxjobmlfyr
                |uqcnpadzztnheslgvxjobmkoyr
                |uqciuadzwonheslgvxjobmkfyz
                |tqcipadzwtnheslgvxaobmqfyr
                |uqcipadtwtnhqslgvxjobmkeyr
                |uqcipadzwbnheslgvajobmsfyr
                |ubcopadzwtnhgslgvxjobmkfyr
                |uqcipydzwtwheslgvxjobakfyr
                |cqbijadzwtnheslgvxjobmkfyr
                |uscipadowtnheslgvxjobmkfcr
                |uqcipadzwtgheslnvxjobskfyr
                |uqcipzdzwtnzeslgkxjobmkfyr
                |uqcipawzwtnhrslgbxjobmkfyr
                |uqcipadzatchyslgvxjobmkfyr
                |uqcipadzotnpeslgvxjobmjfyr
                |uqcipagzwtnheslgvxjobmvfyt
                |uqcipadzwhnheslgvxyobmkfyo
                |uqcipadzwtnheslgmqjobmkfyc
                |uqcupadzwgnheslgvcjobmkfyr
                |uqcipabzwbnheslgvxjobmkwyr
                |uqciiadzwtnheslgvxjobmkfmz
                |uqkipauzwtnheslgvxjjbmkfyr
                |uqcipidzetnheslgvxjobmkfyi
                |uqcipadzwtnheslgqxjokmkfmr
                |uqcipadzqtnhesllvxjobmkfyk
                |uqccpadzwtnheslgmxsobmkfyr
                |uqcipadzwteheslgvljfbmkfyr
                |uqcipadxwinheslgaxjobmkfyr
                |uqcipadzwtnheslhvxyobmkfjr
                |aqcipadzwnnheslgvxjqbmkfyr
                |uvcipadzwtnheszgvxjobmkfyg
                |uqcipahzmtnheslgvxjobmkfir
                |ukcipadzbtnheslgvxjobmkfyb
                |uqcipadzwtnhemlgvqjobmkfpr
                |uqcipadzwtnheslgvmeobmkfpr
                |uqciphdrwtnheslgvxjobmkfyw
                |uqcipadzwtnheslevxqobzkfyr
                |uqcipadzwknzeslgvxnobmkfyr
                |wqcipadzwjnheslgvxjobbkfyr
                |uqcipadzwtdheslgvmjobmkjyr
                |uqvipadzwtnhextgvxjobmkfyr
                |uqhipadzwtnheslwvxjzbmkfyr
                |uqcipadzwtnherlgsxjobmksyr
                |uqcipadzwtnhesqgvxjotmvfyr
                |udcipadzwtnhekwgvxjobmkfyr
                |uqcjprdzwtnheslgvxjobmkfpr
                |uqcipadzatnheclgvqjobmkfyr
                |uqcbpadzctnheslqvxjobmkfyr
                |uqcipadzqtnhesluvxjobrkfyr
                |uqcipadzwtnhcslgvxjoomwfyr
                |uqcppadzwxnheslgwxjobmkfyr
                |uqcipadcwtnheslrvxjdbmkfyr
                |ukcipadzwtnhhslgvxjobmkgyr
                |uqckpadzwtnheslgvxjokmkiyr
                |uqcspadzwtjheslgvxjobmkfjr
                |uqcipadpwtnhsslgvxjobmkfyu
                |uqcepadzwtnheilgvbjobmkfyr
                |jqcipadiwtnheslgvxjobmkjyr
                |uqcipadzrtnseslgqxjobmkfyr
                |sqmipadzwtnhewlgvxjobmkfyr
                |uqcieadzhtnheslgvgjobmkfyr
                |uqcipadzwkwhewlgvxjobmkfyr
                |uqcipadzwtzheslgvxjpbqkfyr
                |uzcipadzjtnheslgvxjobmlfyr
                |uqcipadzwtnheslnvxjobmkfee
                |uqciyanzwtnheslgvxjoimkfyr
                |uqcipadqwtnheswghxjobmkfyr
                |uycipadzwtnheslovxjobmofyr
                |uqcipadzwtnheslgvxcozmxfyr
                |uqmipadzwtnxezlgvxjobmkfyr
                |uqcipadzftnheslgvxjotmkffr
                |aqcipaizwtnhesagvxjobmkfyr
                |uqcipcdzwtnheslgoajobmkfyr
                |uqcypadgwtnhesbgvxjobmkfyr
                |uqcipcdzwtnheslgvxjebmkfyb
                |uhcvpadzwtnheslgvxjobzkfyr
                |uqcipadzwtnpesagvxmobmkfyr
                |uqcipadzwtnidslgvxjobmkfor
                |uqcipadkwtnhesigvxjzbmkfyr
                |uqcypadlwtnheslsvxjobmkfyr
                |qqcipadzwtnheswgvxjobmkoyr
                |uqcipadzwtnheslgvxjhbmmcyr
                |uqcipadzwtnhesogvxjormkfmr
                |uqcipadzwtnhetcgvxgobmkfyr""".stripMargin
