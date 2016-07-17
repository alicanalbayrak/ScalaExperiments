

object maps {


  val romanNumerals = Map('I' -> 1, "V" -> 5, "X" -> 10)

  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")


  // Throws no such element
  //  capitalOfCountry("Andorra")

  capitalOfCountry get "Andorra"

  val cap1 = capitalOfCountry withDefaultValue "unkown"
  capitalOfCountry get "Andorra"

}