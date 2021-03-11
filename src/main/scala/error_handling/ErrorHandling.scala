package error_handling

import cats.data.ValidatedNec
import cats.syntax.all._
import error_handling.ErrorHandling.Card._

import java.time.YearMonth
import java.time.format.DateTimeFormatter
import scala.util.Try

// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.
object ErrorHandling {

  object Card {
    final case class CardHolderName(name: String) extends AnyVal
    final case class CardNumber(number: String) extends AnyVal
    final case class CardExpirationDate(date: YearMonth) extends AnyVal
    final case class CardSecurityCode(code: String) extends AnyVal
  }

  final case class PaymentCard(cardHolderName: CardHolderName,
                               cardNumber: CardNumber,
                               cardExpirationDate: CardExpirationDate,
                               cardSecurityCode: CardSecurityCode)

  sealed trait ValidationError
  object ValidationError {

    final case object InvalidCardHolderNameLength extends ValidationError {
      override def toString: String =
        "Cardholder name should contain from 6 to 25 characters"
    }

    final case object InvalidCardHolderNameHasSpecial extends ValidationError {
      override def toString: String =
        "Cardholder name shouldn't contain special characters"
    }

    final case object InvalidCardNumber extends ValidationError {
      override def toString: String =
        "Card number should contain exactly 16 digits"
    }

    final case object InvalidCardSecurityCode extends ValidationError {
      override def toString: String =
        "Card security code should contain exactly 3 digits"
    }

    final case object InvalidCardExpirationDateFormat extends ValidationError {
      override def toString: String =
        "Card expiration date format should be in mm/yy"
    }
  }

  object PaymentCardValidator {

    import ValidationError._

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateCardHolderName(
      cardHolderName: String
    ): AllErrorsOr[CardHolderName] = {

      def validateCardHolderNameLength: AllErrorsOr[CardHolderName] =
        if (cardHolderName.length >= 6 && cardHolderName.length <= 25)
          CardHolderName(cardHolderName).validNec
        else InvalidCardHolderNameLength.invalidNec

      def validateCardHolderNameContents: AllErrorsOr[CardHolderName] =
        if (cardHolderName.matches("^[a-zA-Z0-9]+$"))
          CardHolderName(cardHolderName).validNec
        else InvalidCardHolderNameHasSpecial.invalidNec

      validateCardHolderNameLength.productR(validateCardHolderNameContents)
    }

    def validateCardNumber(cardNumber: String): AllErrorsOr[CardNumber] = {
      if (cardNumber.length == 16) CardNumber(cardNumber).validNec
      else InvalidCardNumber.invalidNec
    }

    private def validateCardExpirationDateFormat(
      expirationDate: String
    ): AllErrorsOr[CardExpirationDate] = {
      Try(
        YearMonth
          .parse(expirationDate, DateTimeFormatter.ofPattern("MM/yy"))
      ).toOption match {
        case Some(value) => CardExpirationDate(value).validNec
        case _           => InvalidCardExpirationDateFormat.invalidNec
      }
    }

    def validateCardSecurityCode(
      cardSecurityCode: String
    ): AllErrorsOr[CardSecurityCode] = {
      if (cardSecurityCode.length == 3)
        CardSecurityCode(cardSecurityCode).validNec
      else InvalidCardSecurityCode.invalidNec
    }

    def validate(name: String,
                 number: String,
                 expirationDate: String,
                 securityCode: String,
    ): AllErrorsOr[PaymentCard] =
      (
        validateCardHolderName(name),
        validateCardNumber(number),
        validateCardExpirationDateFormat(expirationDate),
        validateCardSecurityCode(securityCode)
      ).mapN(PaymentCard)
  }
}
