import Utils.readString

val in = readString("2020/day4/input")
val sample = readString("2020/day4/sample")

def splitPasswords(in: String): Seq[String] =
  in.split("\n\n").map(_.replace("\n", " "))

def parsePassword(p: String): Map[String, String] = {
  val pattern = """(\w*?):(.*?)(?:\s|$)""".r
  pattern.findAllMatchIn(p).foldLeft(Map.empty[String, String]) {
    case (res, matches) => res.updated(matches.group(1), matches.group(2))
  }
}

def hasRequiredFields(password: Map[String, String]): Boolean = {
  val required = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  required.forall(password.contains)
}

def solveFirst(passwords: String): Int =
  splitPasswords(passwords).map(parsePassword).count(hasRequiredFields)

assert(solveFirst(sample) == 2)
solveFirst(in)

def validYear(lowerBound: Int, upperBound: Int)(year: String): Boolean =
  year.length == 4 && year.toIntOption.exists(y => y >= lowerBound && y <= upperBound)

def validHeight(height: String): Boolean = {
  val res = for {
    matches <- """(\d+)(cm|in)""".r.findFirstMatchIn(height)
    List(amount, unit) = matches.subgroups
    amount <- amount.toIntOption
  } yield unit match {
    case "cm" => amount >= 150 && amount <= 193
    case "in" => amount >= 59 && amount <= 76
  }
  res.getOrElse(false)
}

def validHexColor(color: String): Boolean =
  "#[a-f0-9]{6}".r.matches(color)

def validPassportNumber(number: String): Boolean =
  "[0-9]{9}".r.matches(number)

def validEyeColor(color: String): Boolean =
  Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(color)

def validPassword(password: Map[String, String]): Boolean =
  password.get("byr").exists(validYear(1920, 2002)) &&
    password.get("iyr").exists(validYear(2010, 2020)) &&
    password.get("eyr").exists(validYear(2020, 2030)) &&
    password.get("hgt").exists(validHeight) &&
    password.get("hcl").exists(validHexColor) &&
    password.get("ecl").exists(validEyeColor) &&
    password.get("pid").exists(validPassportNumber)

def solveSecond(input: String): Int =
  splitPasswords(input).map(parsePassword).count(validPassword)

val validPasswords = readString("2020/day4/valid")
val invalidPasswords = readString("2020/day4/invalid")

assert(solveSecond(validPasswords) == 4)
assert(solveSecond(invalidPasswords) == 0)
solveSecond(in)
