package aoc.y2019

object Day5
  def runProgram(opCodes: List[Int], inputs: LazyList[Int]): LazyList[Int] = 
    case class ProgramState(pointer: Int, opCodes: List[Int], inputs: LazyList[Int], output: Option[Int])
    LazyList.iterate(ProgramState(0, opCodes, inputs, None)) { case ProgramState(pointer, opCodes, inputs, _) =>
      opCodes.drop(pointer) match
        case 1 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, opCodes(x) + opCodes(y)), inputs, None)
        case 101 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, x + opCodes(y)), inputs, None)
        case 1001 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, opCodes(x) + y), inputs, None)
        case 1101 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, x + y), inputs, None)
        case 2 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, opCodes(x) * opCodes(y)), inputs, None)
        case 102 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, x * opCodes(y)), inputs, None)
        case 1002 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, opCodes(x) * y), inputs, None)
        case 1102 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, x * y), inputs, None)
        case 3 :: x :: _ => ProgramState(pointer + 2, opCodes.updated(x, inputs.head), inputs.tail, None)
        case 4 :: x :: _ => ProgramState(pointer + 2, opCodes, inputs, Some(opCodes(x)))
        case 104 :: x :: _ => ProgramState(pointer + 2, opCodes, inputs, Some(x))
        case 5 :: x :: y :: _ => ProgramState(if opCodes(x) != 0 then opCodes(y) else pointer + 3, opCodes, inputs, None)
        case 105 :: x :: y :: _ => ProgramState(if x != 0 then opCodes(y) else pointer + 3, opCodes, inputs, None)
        case 1005 :: x :: y :: _ => ProgramState(if opCodes(x) != 0 then y else pointer + 3, opCodes, inputs, None)
        case 1105 :: x :: y :: _ => ProgramState(if x != 0 then y else pointer + 3, opCodes, inputs, None)
        case 6 :: x :: y :: _ => ProgramState(if opCodes(x) == 0 then opCodes(y) else pointer + 3, opCodes, inputs, None)
        case 106 :: x :: y :: _ => ProgramState(if x == 0 then opCodes(y) else pointer + 3, opCodes, inputs, None)
        case 1006 :: x :: y :: _ => ProgramState(if opCodes(x) == 0 then y else pointer + 3, opCodes, inputs, None)
        case 1106 :: x :: y :: _ => ProgramState(if x == 0 then y else pointer + 3, opCodes, inputs, None)
        case 7 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, if opCodes(x) < opCodes(y) then 1 else 0), inputs, None)
        case 107 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, if x < opCodes(y) then 1 else 0), inputs, None)
        case 1007 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, if opCodes(x) < y then 1 else 0), inputs, None)
        case 1107 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, if x < y then 1 else 0), inputs, None)
        case 8 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, if opCodes(x) == opCodes(y) then 1 else 0), inputs, None)
        case 108 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, if x == opCodes(y) then 1 else 0), inputs, None)
        case 1008 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, if opCodes(x) == y then 1 else 0), inputs, None)
        case 1108 :: x :: y :: z :: _ => ProgramState(pointer + 4, opCodes.updated(z, if x == y then 1 else 0), inputs, None)
        case _ => ProgramState(-1, Nil, inputs, None)
    }
      .takeWhile(_.pointer >= 0)
      .collect { case ProgramState(_, _, _, Some(output)) => output }
    

  def solve(input: String): String = 
    runProgram(input.split(",").map(_.toInt).toList, LazyList(1)).mkString
    

  def solve2(input: String): String = 
    runProgram(input.split(",").map(_.toInt).toList, LazyList(5)).mkString
  
  val input = "3,225,1,225,6,6,1100,1,238,225,104,0,1101,65,73,225,1101,37,7,225,1101,42,58,225,1102,62,44,224,101,-2728,224,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,1,69,126,224,101,-92,224,224,4,224,1002,223,8,223,101,7,224,224,1,223,224,223,1102,41,84,225,1001,22,92,224,101,-150,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,80,65,225,1101,32,13,224,101,-45,224,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,1101,21,18,225,1102,5,51,225,2,17,14,224,1001,224,-2701,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,101,68,95,224,101,-148,224,224,4,224,1002,223,8,223,101,1,224,224,1,223,224,223,1102,12,22,225,102,58,173,224,1001,224,-696,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,1002,121,62,224,1001,224,-1302,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1008,226,677,224,102,2,223,223,1005,224,329,1001,223,1,223,7,677,226,224,102,2,223,223,1006,224,344,1001,223,1,223,1007,226,677,224,1002,223,2,223,1006,224,359,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,374,1001,223,1,223,108,677,677,224,102,2,223,223,1006,224,389,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,404,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,419,101,1,223,223,8,677,226,224,1002,223,2,223,1005,224,434,101,1,223,223,107,677,677,224,1002,223,2,223,1006,224,449,101,1,223,223,7,677,677,224,1002,223,2,223,1006,224,464,101,1,223,223,1107,226,226,224,102,2,223,223,1006,224,479,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,494,101,1,223,223,108,226,677,224,1002,223,2,223,1006,224,509,101,1,223,223,1108,226,677,224,102,2,223,223,1006,224,524,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,539,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,554,101,1,223,223,8,677,677,224,102,2,223,223,1005,224,569,101,1,223,223,107,226,677,224,102,2,223,223,1005,224,584,101,1,223,223,1108,226,226,224,1002,223,2,223,1005,224,599,1001,223,1,223,1008,677,677,224,1002,223,2,223,1005,224,614,101,1,223,223,1107,226,677,224,102,2,223,223,1005,224,629,101,1,223,223,1108,677,226,224,1002,223,2,223,1005,224,644,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,659,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,674,101,1,223,223,4,223,99,226"