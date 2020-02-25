
object ProblemOne {

  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

  case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    prices.map(
      price => BestGroupPrice(price.cabinCode, price.rateCode, price.price, rates.find(_.rateCode == price.rateCode).get.rateGroup)
    )
      .groupBy(best => (best.cabinCode, best.rateGroup))
      .map(_._2.minBy(_.price))
      .toSeq
      .sortBy(best => (best.cabinCode, best.rateCode))
  }

  def main(args: Array[String]): Unit = {
    val rates = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val cabinPrices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    println(getBestGroupPrices(rates, cabinPrices))
  }
}