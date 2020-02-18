import java.sql.Timestamp
case class BankOperation(eventData: Timestamp, userId: Int, operation: Double, title: String, source: Int, gender: Int)
object Main extends App {
  val systemId = 0
  val man = "man"
  val woman = "woman"
  val userIdToGender = Map(1 -> man, 2 -> woman)
  val operations = Set("cinema", "food purchase")
  val operationType = "food purchase"
  val bankOperations = List(
    BankOperation(Timestamp.valueOf("2019-05-05 13:33:50.234"), 0, 500, "input", systemId, 1),
    BankOperation(Timestamp.valueOf("2019-05-05 14:33:50.234"), 1, 1500, "input", systemId, 2),
    BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 0, 300, "transfer", 1, 1),
    BankOperation(Timestamp.valueOf("2019-05-05 17:00:00.555"), 1, -300, "transfer", 0, 2),
    BankOperation(Timestamp.valueOf("2019-05-05 18:00:00.555"), 1, -1000, "food purchase", 3, 2),
    BankOperation(Timestamp.valueOf("2019-05-06 10:21:42.234"), 0, 2000, "input", systemId, 1),
    BankOperation(Timestamp.valueOf("2019-05-06 10:40:20.555"), 0, -1000, "transfer", 1, 1),
    BankOperation(Timestamp.valueOf("2019-05-05 10:40:20.555"), 1, 1000, "transfer", 0, 2),
    BankOperation(Timestamp.valueOf("2019-05-06 12:03:21.234"), 1, -600, "food purchase", 3, 2),
    BankOperation(Timestamp.valueOf("2019-05-06 16:54:10.234"), 0, -400, "food purchase", 3, 1),
    BankOperation(Timestamp.valueOf("2019-05-06 20:00:10.234"), 0, -500, "cinema", 4, 1),
    BankOperation(Timestamp.valueOf("2019-05-06 22:41:12.234"), 0, -300, "food purchase", 3, 1),
    BankOperation(Timestamp.valueOf("2019-05-06 23:30:14.234"), 1, -250, "cinema", 4, 2),
    BankOperation(Timestamp.valueOf("2019-05-07 01:54:21.234"), 1, 3000, "input", systemId, 2),
    BankOperation(Timestamp.valueOf("2019-05-07 08:41:38.234"), 2, 1400, "input", systemId, 1),
    BankOperation(Timestamp.valueOf("2019-05-07 09:26:35.234"), 0, -450, "food purchase", 3, 1),
    BankOperation(Timestamp.valueOf("2019-05-07 09:35:38.234"), 1, -500, "food purchase", 3, 2),
    BankOperation(Timestamp.valueOf("2019-05-07 10:20:05.234"), 2, -200, "cinema", 4, 1),
    BankOperation(Timestamp.valueOf("2019-05-07 10:20:05.234"), 2, -800, "food purchase", 3, 1)
  )
  def Amount(bankOperations: List[BankOperation]): Map[Int, Double] = {
    bankOperations.groupBy(_.userId).map { case (userId, data) =>
      val operation = data.map(_.operation).sum
      (userId, operation)
    }
  }
  def FrequentOperation(bankOperation: List[BankOperation]): List[(String, Int)] = {
    bankOperation.groupBy(_.title).map{case (title, data) =>
    val countTitle = data.map(_.title).size
      (title, countTitle)
    }.toList.sortBy{ case (_, countTitle) => countTitle}.reverse
  }
  def CostsForMenAndWomen(bankOperations: List[BankOperation], genders: Map[Int, String]): Map[Option[String], Double] = {
    bankOperations.filter(_.operation < 0).groupBy(_.gender).map { case (gender, data) =>
      val costs = data.map(_.operation).sum
      (genders.get(gender), costs)
    }
  }
  def CostsByCategory(bankOperations: List[BankOperation], operations: Set[String]): Map[Int, Double] = {
    bankOperations.filter(data => operations.contains(data.title)).
      groupBy(_.userId).map { case (id, data) =>
      val costsByCategory = data.filter(_.operation < 0).map(_.operation).sum
      (id, costsByCategory)
    }
  }
  def OperationInPeriodOfTime(bankOperations: List[BankOperation], operationType: String): Map[Int, String] = {
    bankOperations.filter(_.title == operationType).groupBy(_.title).map { case (_, data) =>
      val night = data.filter(_.eventData.getHours >= 0).filter(_.eventData.getHours < 6).size
      val morning = data.filter(_.eventData.getHours >= 6).filter(_.eventData.getHours < 12).size
      val day = data.filter(_.eventData.getHours >= 12).filter(_.eventData.getHours < 18).size
      val evening = data.filter(_.eventData.getHours >= 18).filter(_.eventData.getHours <= 23).size
      val mapTime = Map((night, "night"), (morning, "morning"), (day, "day"), (evening, "evening"))
      mapTime.max
    }
  }
  print("Остаток по каждому Id -> ") -> println(Amount(bankOperations))
  print("Наиболее частые операции -> ") -> println(FrequentOperation(bankOperations))
  print("Траты мужчин и женщин -> ") -> println(CostsForMenAndWomen(bankOperations, userIdToGender))
  print("Траты каждого пользователя по заданным категориям ->") -> println(CostsByCategory(bankOperations, operations))
  print("Максимальное количество операций заданного типа в определенный промежуток времени ->") -> println(OperationInPeriodOfTime(bankOperations, operationType))
}