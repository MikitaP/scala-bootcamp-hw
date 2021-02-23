package typeclass.v2

object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)
}

object Task2 {
  trait Show[T] { // fancy toString
    def show(entity: T): String
  }

  final case class User(id: String, name: String)

  object Show {
    def show[T](value: T)(implicit printer: Show[T]): String =
      printer.show(value)
  }

  implicit object UserPrinter extends Show[User] {
    override def show(user: User): String =
      s"User with id:${user.id} and name:${user.name}"
  }

  implicit class ShowEnrichment[T](value: T) {
    def show(implicit printer: Show[T]): String = printer.show(value)
  }

  User("1", "Oleg").show
}

object Task3 {
  type Error = String
  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }

  final case class User(id: String, name: String)

  object Parse {
    def parse[T](value: String)(implicit parser: Parse[T]): Either[Error, T] =
      parser.parse(value)
  }
  implicit object UserParser extends Parse[User] {
    override def parse(entity: String): Either[Error, User] =
      Left("Not user type")
  }

  implicit class ParserEnrichment[T](value: String) {
    def parse[T](implicit parser: Parse[T]): Either[Error, T] =
      parser.parse(value)
  }
  "lalala".parse[User]
}

object Task4 {
  // TODO: design a typesafe equals so i can do a === b, but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`
}

//object AdvancedHomework {
//  // TODO: create a typeclass for flatMap method
//}
