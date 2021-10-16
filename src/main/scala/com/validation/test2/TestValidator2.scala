package com.validation.test2

import scala.{None => SNone}

object TestValidator2 extends App {

  /**
   * Каковы ограничения на вывод 'типов высшего рода' в Scala?
   * ***
   * @see https://coderoad.ru/15303437/Каковы-ограничения-на-вывод-типов-высшего-рода-в-Scala
   *      1. Мономорфный тип — когда простой, непараметризованный тип данных (например Int) имеет вид '_'
   *                           case class Base[A] (a: A) // An identity functor
   *      2. Полиморфный тип 1-го ранга — это конструктор типов, который принимает тип, (например F[_]) имеет вид '_[_]'
   *                                      Когда мы не можем использовать его в качестве типа значения, не указав тип, который он содержит
   *      3. Полиморфный тип 2-го ранга — это конструктор типов, который принимает конструктор типов, у него есть два параметра 'F[_]' и 'A'
   *                                      Количество параметров типа здесь не имеет значения, но их виды имеют значение
   *                                      все что имеет ранг два или выше — называем ТИПАМИ БОЛЕЕ ВЫСОКОГО РОДА
   *                                      case class Recursive[F[_], A](fa: F[A]) // pair functor
   *
   * Scala в целом не имеет проблем с типами более высокого рода.
   * Но у него есть проблемы с частичным применением параметров типа при работе с типами более высокого рода.
   * Вот в чем проблема: Recursive[F[_], A] имеет два параметра типа.
   * Если бы Scala имел списки параметров типа карри, я бы определенно использовал это здесь:
   *
   * case class Base[A](a: A)
   * case class Recursive[F[_]][A](fa: F[A]) // curried!
   *
   * def main(args: Array[String]): Unit = {
   *    val one = Base(1)          // Base[Int]
   *    val two = Recursive(one)   // Recursive[Base][Int]
   *    val three = Recursive(two) // Recursive[Recursive[Base]][Int]
   *    println(three)
   * }
   *
   * Увы, это не так ( см. https://github.com/scala/bug/issues/4719 )
   * Мы можем кодировать блоки параметров нескольких типов на методы в текущей Scala, хотя и неэффективно и неуклюже
   *    def foo[A1, A2][B1, B2] (p1: (A1, B1)): (A2, B2) = ...
   * можно закодировать как,
   *    def foo[A1, A2] = new {
   *       def apply[X1, X2, B1, B2] (p1: (A1, B1)) (implicit ev: X1 =:= A1): (A2, B2) = ...
   *    }
   *
   *
   *
   * @see https://coderoad.ru/29113645/Как-работать-с-дженериками-вложенных-типов-в-scala
   *
   *      Scala имеет некоторую черную магию, когда дело доходит до дженериков, которые могут позволить указать, что 'F' действительно является типом, вложенным в T',
   * но я не могу понять, как это будет сделано:
   *
   * Возьмите тип 'T', чтобы представить RDD[F].
   * Вы видите здесь проблему?
   * Здесь становится неприятно заставлять пользователей определять оба типа таким образом:
   *      class MySource extends DataSource[RDD[String], String]
   * RDD и DStream не наследуются от одного и того же признака или базового класса.
   * Дело в том, что 'T' также может быть DStream[String].
   */
  class ClazzA[A[B[C]]]

//  class ClazzB[A[B[C]]] extends ClazzA[A[B[C]]] //TODO: ошибка компиляции
//  class ClazzB[A[B[C]]] extends ClazzA //TODO: можно так
  class ClazzB[A[B[C]]] extends ClazzA[A] //TODO: или можно так


  case class Base[A](a: A)
  case class Recursive[F[_], A](fa: F[A])

  val one1 = Base(1)
  val two1 = Recursive(one1)
  val three1 = Recursive(two1)
  println(three1) // Recursive(Recursive(Base(1)))

  val one2 = Base(1)
  val two2 = Recursive(one2, "one")
  val three2 = Recursive(two2, "two")
  println(three2) // Recursive((Recursive((Base(1),one)),two))



  class Error (errorCode: Int, errorMsg: String) {
    override def toString: String =
      s"""errorMsg: \"$errorMsg\"""".stripMargin
  }
  case object AgeError extends Error (errorCode = 0, errorMsg = "Illegal Age (Age must be over 17)")
  case object NameError extends Error (errorCode = 1, errorMsg = "Illegal Name (Name must not be empty)")




  // Предположим, нам нужно проверить имя и возраст человека перед сохранением человека в базе данных
  case class Person(name: String, age: Int){
    def validateName= {
      if (name.isEmpty) Some(NameError)
      else SNone
    }
    def validateAge = {
      if (age < 18) Some(AgeError)
      else SNone
    }
  }

  def save(person: Person): Boolean = {
    println(s"save ${person.name} at age ${person.age}")
    true
  }

//  val person = Person("Michael", 20) // save Michael at age 20
//  val person = Person("Michael", 0) // validateAge: Some(errorMsg: "Illegal Age (Age must be over 17)")
//  val person = Person("", 20) // validateName: Some(errorMsg: "Illegal Name (Name must not be empty)")
  val person = Person("", 0)
  for {
//    name <- {
//      if (person.validateName != SNone) println("validateName: " + person.validateName)
//      person.validateName
//    }
//    age <- {
//      if (person.validateAge != SNone) println("validateAge: " + person.validateAge)
//      person.validateAge
//    }
//
    errorMsg <- {
      if (person.validateName != SNone) {
        println("validateName: " + person.validateName)
      }
      if (person.validateAge != SNone) {
        println("validateAge: " + person.validateAge)
      }
      if (person.validateName != SNone) person.validateName else
      if (person.validateAge != SNone) person.validateAge else SNone
    }
//  } yield save(person)
  } yield {
    println( "p = " + errorMsg )
  }

}
