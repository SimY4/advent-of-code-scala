package aoc

private opaque type Parser[+A] = String => Option[(A, String)]

private object Parser:
  def char(ch: Char): Parser[Char] = { input =>
    for
      head <- input.headOption
      if head == ch
    yield (head, input.tail)
  }

  def literal(literal: String): Parser[String] = { input =>
    if input.startsWith(literal) then Some(literal, input.substring(literal.length))
    else None
  }

  def span(p: Char => Boolean): Parser[String] = { input =>
    val matches = input.takeWhile(p)
    if matches.isEmpty then None
    else Some(matches, input.substring(matches.length))
  }

  val any: Parser[Char] = { input => input.headOption.map((_, input.tail)) }

  def not(first: Char, rest: Char*): Parser[Char] = { input =>
    for
      ch <- input.headOption
      if first != ch && !rest.contains(ch)
    yield (ch, input.tail)
  }

  def nothing[A](empty: A): Parser[A] = Some(empty, _)

  val eof: Parser[Unit] = { input => Option.when(input.isEmpty)(((), input)) }

  extension [A](p: Parser[A])
    def as[B](b: => B): Parser[B] = p.map(_ => b)

    def many(separatedBy: Parser[Any] = nothing("")): Parser[List[A]] =
      (p <*> (separatedBy *> p).many()).map(_ :: _) <|> nothing(Nil)

    def map[B](f: A => B): Parser[B] = { input =>
      p(input).map((a, rest) => (f(a), rest))
    }

    def optional: Parser[Option[A]] = p.map(Some(_)) <|> nothing(None)

    def run(input: String): A =
      p(input) match
        case Some(res, "")  => res
        case Some(res, rem) => sys.error(s"parsed string didn't match. parsed: $res, rest: '$rem'")
        case None           => sys.error("parsed string didn't match.")

    def <|>(other: => Parser[A]): Parser[A] = { input =>
      p(input).orElse(other(input))
    }

    def <*>[B](other: => Parser[B]): Parser[(A, B)] =
      p.andThen { op =>
        op.flatMap { (a, rest) =>
          other(rest).map((b, rest2) => ((a, b), rest2))
        }
      }

    def <*(other: => Parser[Any]): Parser[A] =
      (p <*> other).map(_._1)

    def *>[B](other: => Parser[B]): Parser[B] =
      (p <*> other).map(_._2)
