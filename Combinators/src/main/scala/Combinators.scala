import scala.util.Random

object Combinators {
  def main(args: Array[String]): Unit = {
    //representation of combinators and methods for their reduction
    sealed trait Combinator

    sealed trait CombResult {
      def substitute(combs: List[Combinator])
      : Combinator
    }

    final case class App(
                          c1: Combinator,
                          c2: Combinator,
                        ) extends Combinator

    final case class VarResult(
                                index: Int
                              ) extends CombResult {
      def substitute(combs: List[Combinator])
      : Combinator =
        combs(index)
    }

    final case class Const(
                            arity: Int,
                            result: CombResult,
                          ) extends Combinator {
      def apply(args: List[Combinator])
      : Option[Combinator] =
        if (args.size != arity) None
        else Some(result.substitute(args))
    }


    final case class AppResult(
                                r1: CombResult,
                                r2: CombResult,
                              ) extends CombResult {
      def substitute(combs: List[Combinator])
      : Combinator =
        App(r1.substitute(combs), r2.substitute(combs))
    }


    //reduction function
    var index = 0: Int;

    def RandomCombinator2(indicator: Int): String = {
      var r: Int = Random.nextInt(3)
      indicator match {
        case 1 => r match {
          case 0 => {
            var indicator: Int = Random.nextInt(2); "Const(1,VarResult(" + index + ")).apply(List(" + RandomCombinator2(indicator) + "))"
          }
          case 1 => {
            var indicator: Int = Random.nextInt(2); "Const(2,AppResult(VarResult(" + index + "),VarResult(" + (index + 1) + "))).apply(List(" + RandomCombinator2(indicator) + "," + RandomCombinator2(indicator) + "))"
          }
          case 2 => {
            var indicator: Int = Random.nextInt(2); "Const(3,AppResult(AppResult(VarResult(" + index + "),VarResult(" + (index + 2) + ")),AppResult(VarResult(" + (index + 1) + "),VarResult(" + (index + 2) + ")))).apply(List(" + RandomCombinator2(indicator) + "," + RandomCombinator2(indicator) + "," + RandomCombinator2(indicator) + "))"
          }
        }
        case 0 => {
          index = index + 1; "Const(1,VarResult(" + index + "))"
        }
      }
    }

    print(RandomCombinator2(1))

    //representation of combinators

    var i = 0: Int
    var X = VarResult(0)
    var I = Const(1, VarResult(i))
    var K = Const(2, AppResult(VarResult(i), VarResult(i + 1)))
    var S = Const(3, AppResult(AppResult(VarResult(i), VarResult(i + 2)), AppResult(VarResult(i + 1), VarResult(i + 2))))


    print(Const(2, AppResult(VarResult(0), VarResult(1))).apply(List(Const(1, VarResult(1)), Const(1, VarResult(2)))))
    print(Const(1, VarResult(0)).apply(List(Const(1, VarResult(0)))))
    print(K.apply(List(App(Const(1, VarResult(0)), Const(1, VarResult(0))), Const(1, VarResult(0)))))
    print(I.apply(List(I)))
    print(I.apply(List(Const(1, VarResult(0)))))
    print(I.apply(List(Const(1, VarResult(i)))))
    print(Const(1, VarResult(i)).apply(List(Const(1, VarResult(i))))) // I
    print(Const(2, AppResult(VarResult(i), VarResult(i + 1))).apply(List(Const(1, VarResult(i)), Const(1, VarResult(i))))) // K
    print(Const(3, AppResult(AppResult(VarResult(i), VarResult(i + 2)), AppResult(VarResult(i + 1), VarResult(i + 2)))))
    print(I.apply(List(Const(1, VarResult(i)))))
    print(Const(3, AppResult(AppResult(VarResult(i), VarResult(i + 2)), AppResult(VarResult(i + 1), VarResult(i + 2)))).apply(List(Const(1, VarResult(1)), Const(1, VarResult(2)), Const(1, VarResult(3)))))
  }

}
