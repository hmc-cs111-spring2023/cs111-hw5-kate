package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

trait RegularLanguage

case class Empty() extends RegularLanguage
case class Epsilon() extends RegularLanguage
case class Character(c: Char) extends RegularLanguage
case class Union(re1: RegularLanguage, re2: RegularLanguage) extends RegularLanguage
case class Concat(re1: RegularLanguage, re2: RegularLanguage) extends RegularLanguage
case class Star(re: RegularLanguage) extends RegularLanguage

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = lang match
  case Concat(re1, re2) if re1 == Epsilon => re2
  case Concat(re1, re2) if re2 == Epsilon => re1
  case Concat(re1, re2) if re1 == Empty => re2
  case Concat(re1, re2) if re2 == Empty => re1
  case Concat(re1, re2) => Concat(simplify(re1), simplify(re2))
  case Union(re1, re2) if re1 == Empty => re2
  case Union(re1, re2) if re2 == Empty => re1
  case Union(re1, re2) => Union(simplify(re1), simplify(re2))
  case Star(re) if re == Epsilon => Epsilon()
  case Star(re) if re == Empty => Empty()
  case Star(re) => Star(simplify(re))
  case _ => lang

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = ???

/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = ???

/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
