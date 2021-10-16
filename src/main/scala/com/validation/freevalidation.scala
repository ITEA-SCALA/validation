package com.validation

import scala.util.{Try, Success, Failure}

object FreeValidation extends App {

  sealed trait Free[F[_], A] {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => f(a)
      case FlatMap(sub, cont) => FlatMap(sub, cont andThen (_ flatMap f))
    }

    def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
  }
  final case class Return[F[_], A](a: A) extends Free[F, A]
  case class FlatMap[F[_], I, A](sub: F[I], cont: I => Free[F, A]) extends Free[F, A]

  implicit def liftF[F[_], A](fa: F[A]): Free[F, A] = FlatMap(fa, Return.apply)


  //
  abstract class Error (errorCode: Int, errorMsg: String)
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
//  val person = Person("", 20) // NameError
//  val person = Person(null, 20) // NullPointerException
//  val person = Person("John", 0) // AgeError
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
