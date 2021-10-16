package com.validation

import scala.util.{Try, Success, Failure}

object FreeValidation extends App {

  sealed trait Free[ITEMS[_], IN] { // 'ITEMS'='F'  |  'IN'='A'  |  'OUT'='B'
    def flatMap[OUT](funcIn: IN => Free[ITEMS, OUT]): Free[ITEMS, OUT] = this match {
      case Return(in) =>
        funcIn(in)
      case FlatMap(elements, funcElement) =>
        FlatMap(elements, funcElement.andThen(_.flatMap(funcIn)) ) //TODO: функция 'andThen' - объединяет два экземпляра старую Function-1 в новую Function-1
    }

    def map[OUT](funcIn: IN => OUT): Free[ITEMS, OUT] = {
      flatMap(in => Return( funcIn(in) ))
    }
  }

  case class Return[ITEMS[_], IN] (in: IN) extends Free[ITEMS, IN]                                                             //TODO:  Return (in: IN)
  case class FlatMap[ITEMS[_], ITEM, IN] (elements: ITEMS[ITEM], funcElement: ITEM => Free[ITEMS, IN]) extends Free[ITEMS, IN] //TODO:  FlatMap (elements: ITEMS[ITEM], funcElement: ITEM => Free[ITEMS, IN])

  implicit def liftITEMS[ITEMS[_], IN] (elements: ITEMS[IN]): Free[ITEMS, IN] = FlatMap(elements, Return.apply)                //TODO: liftITEMS (elements: ITEMS[IN]): Free[ITEMS, IN]


  //
  abstract class Error (errorCode: Int, errorMsg: String) {
    override def toString: String =
//      s"""errorCode: $errorCode
//         |errorMsg: \"$errorMsg\"
//         |""".stripMargin
      s"""errorMsg: \"$errorMsg\"""".stripMargin
  }
  case object AgeError extends Error (errorCode = 0, errorMsg = "Illegal Age (Age must be over 17)")
  case object NameError extends Error (errorCode = 1, errorMsg = "Illegal Name (Name must not be empty)")

  //
  sealed trait Validator[A] {
    def validate: Option[Error]
    def unbox: A
  }

  case class NameValidator(name: String) extends Validator[String] {
    def validate = if (name.isEmpty) Some(NameError) else None
    def unbox: String = name
  }

  case class AgeValidator(age: Int) extends Validator[Int] {
    def validate = if (age >= 18) None else Some(AgeError)
    def unbox: Int = age
  }

  // ///
  case class Person(name: String, age: Int)
//  val person = Person("", 20) // errorMsg: "Illegal Name (Name must not be empty)"
//  val person = Person(null, 20) // NullPointerException
//  val person = Person("John", 0) // errorMsg: "Illegal Age (Age must be over 17)"
  val person = Person("", 17) // AgeError  NameError
//  val person = Person("John", -1) // AgeError
//  val person = Person("John", 20) // save John at age 20

  val validation = for {
    _ <- NameValidator(person.name)
    _ <- AgeValidator(person.age)
  } yield ()

  sealed trait Executor[F[_]] {
    def exec[A](fa: F[A]): Option[Error]
    def unbox[A](fa: F[A]): A
  }

  val interpreter  = new Executor[Validator] {
    override def unbox[A](fa: Validator[A]) = fa.unbox
    override def exec[A](fa: Validator[A]) = fa.validate
  }

  // ///
  def validate[F[_], A](prg: Free[F, A], interpreter : Executor[F]): List[Error] = {
    def go(errorList: List[Option[Error]], prg: Free[F, A]): List[Option[Error]] = prg match {
      case Return(e) => errorList
      case FlatMap(sub, cont) => go(interpreter.exec(sub) :: errorList, cont(interpreter.unbox(sub)))
    }
    go(List.empty[Option[Error]], prg).flatten
  }

  def save(p: Person): Boolean = {
    println( s"save ${p.name} at age ${p.age}" )
    true
  }

  // ///
  validate(validation, interpreter) match {
    case Nil => save(person)
    case errors => errors.foreach( println )
  }
}
