object ProblemTwo {

  case class Promotion(code: String, notCombinableWith: Seq[String])

  case class PromotionCombo(promotionCodes: Seq[String])

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    allPromotions
      .map(p => allPromotions.filterNot(_.notCombinableWith.contains(p.code)))
      .flatMap(b => b
        .map(p => p.code -> b.filterNot(_.notCombinableWith.contains(p.code)))
      )
      .filterNot(b => b._2.exists(p => b._2.flatMap(_.notCombinableWith).contains(p.code)))
      .map(p => PromotionCombo(p._2.map(_.code)))
      .distinctBy(combo => combo.promotionCodes)
  }

  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    allCombinablePromotions(allPromotions).filter(_.promotionCodes.contains(promotionCode))
  }

  def main(args: Array[String]): Unit = {
    val promotions = Seq(
      Promotion("P1", Seq("P3")), // P1 is not combinable with P3
      Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
      Promotion("P3", Seq("P1")), // P3 is not combinable with P1
      Promotion("P4", Seq("P2")), // P4 is not combinable with P2
      Promotion("P5", Seq("P2")) // P5 is not combinable with P2
    )

    println("All combined:")
    println(allCombinablePromotions(promotions))

    println("\nP1:")
    println(combinablePromotions("P1", promotions))

    println("\nP3:")
    println(combinablePromotions("P3", promotions))
  }
}
