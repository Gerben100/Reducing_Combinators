
/*
import scala.util.parsing.combinator._
import scala.util.parsing

sealed trait Name
sealed trait Combinator
case class Var(name: Name) extends Combinator
case class Lam(arg: Var, body: Combinator) extends Combinator
case class App(f: Combinator, arg: Combinator) extends Combinator
case class Index(i:Int, name:String) extends Name
case class Literal (name:String) extends Name

var x="x":String
var y="y":String
var z="y":String

/*val I = Lam (Var(x),  Lam(Var(x)  , Var(y)))
val K = Lam (Var(x), (Lam (Var(y) , Var (x))))
val S = Lam (Var(x) ,(Lam (Var(y) ,(Lam (Var(z) , (App (Var (x), App (Var(z),(App (Var (y) ,Var (z)))))))))))
val B = Lam (Var(x) ,(Lam (Var(y) ,(Lam (Var(z) , (App (Var (x),(App (Var (y) ,Var (z))))))))))
*/
//val k = Lam(Var(x), Lam(Var(y), Var(x)))
//val s = Lam(Var(x), Lam(Var(y), Lam(Var(z), Var(x))))


def reduce(term: Combinator, i: Int, body: Combinator): Combinator = body match {
    case Var(name) => name match {
      case Index(j, _) if i == j => term
      case _ => Var(name)
    }
    case Lam(arg, body) => Lam(arg, reduce(term, i + 1, body))
    case App(f, arg) => App(reduce(term, i, f), reduce(term, i, arg))
    case _ => body
  }

val beta: Combinator => Option[Combinator] = {
  case App(Lam(Var(Index(i,name)), body), term) =>
    Some(reduce(term, i, body))
  case _ =>
    None
}



val b = beta(App(Lam(Var(Index(0,"a")),Var(Index(1,"x"))),Var(Index(1,"x"))))

print(b)

val n,s,k,i :Int
val r = scala.util.Random
print(r.nextInt)




/*class Printer {
  def apply(expr: Combinator): String = expr match {
    case Lam(arg, body) => p"lam.$arg.$body"
    case App(f, arg) => p"$f $arg"
    case Var(name) => s"$name"
  }

  implicit class PrettyPrinting(val sc: StringContext) {
    def p(args: Combinator*) = sc.s((args map parensIfNeeded): _*)
  }

  def parensIfNeeded(expr: Combinator) = expr match {
    case Var(name) => name
    case _ => "(" + apply(expr) + ")"
  }
}*/

/////////////////////////////////////////////////

sealed trait Combimator

final case class Const(
   arity: Int,
   result: CombResult,
)  extends Combinator {
  def apply(args: List[Combinator])
  : Option[Combinator] =
    if (args.size != arity) None
    else Some()
}

final case class App(
  с1: Combinator,
  c2: Combinator,
) extends Combinator

sealed trait CombResult {
  def substitute(combs: List[Combinator])
    : Combinator
}

final case class VarResult(
  index: Int
) extends CombResult {
  def substitute(combs: List[Combinator])
    : Combinator =
    combs(index)
}

final case class AppResult(
  r1: CombResult,
  r2: CombResult,
) extends CombResult {
  def substitute(combs: List[Combinator])
  : Combinator =
    App(r1.substitute(combs), r2.substitute(combs))
}


import scala.util.Random

val x: Int = Random.nextInt(4)
var l="": String

def reduce(x:Int, l:String): String = x match {
  case 0 => l + "I"
  case 1 => l + "K"
  case 2 => l + "S"
}

for (counter <- 1 to x) {
  val y: Int = Random.nextInt(3)
  println(y);
  y match {
    case 0 => l = l + "I"
    case 1 => l = l + "K"
    case 2 => l = l + "S"
  }
  println(l)
}
*/
/*
import scala.util.Random

val r = new scala.util.Random(3)
val indicator = new scala.util.Random(1)
var l="";






def RandomCombinator1(): String = {
  var i: Int = Random.nextInt(3)
  i match {
    case 0 => "I"
    case 1 => "K"
    case 2 => "S"
  }
}
print(RandomCombinator1())
print(RandomCombinator1())
print(RandomCombinator1())
print(RandomCombinator1())


/*
def RandomCombinator2(indicator:Int): String = {
  var r: Int = Random.nextInt(3)
  indicator match {
    case 1 => r match {
      case 0 => {var indicator: Int = Random.nextInt(2); "I(" + RandomCombinator2(indicator) + ")"}
      case 1 => {var indicator: Int = Random.nextInt(2); "K(" + RandomCombinator2(indicator) + "," + RandomCombinator2(indicator) + ")"}
      case 2 => {var indicator: Int = Random.nextInt(2); "S(" + RandomCombinator2(indicator) + "," + RandomCombinator2(indicator) + "," + RandomCombinator2(indicator) + ")"}}
    case 0 => {index=index+1;"X"+index}
  }
}*/




var index=0:Int;

def RandomCombinator2(indicator:Int): String = {
  var r: Int = Random.nextInt(3)
  indicator match {
    case 1 => r match {
      case 0 => {var indicator: Int = Random.nextInt(2); "Const(1,VarResult(" + index + ")).apply(List(" + RandomCombinator2(indicator) + "))"}
      case 1 => {var indicator: Int = Random.nextInt(2); "Const(2,AppResult(VarResult(" + index + "),VarResult(" + (index + 1)+ "))).apply(List(" + RandomCombinator2(indicator) + "," + RandomCombinator2(indicator) + "))"}
      case 2 => {var indicator: Int = Random.nextInt(2); "Const(3,AppResult(AppResult(VarResult(" + index + "),VarResult(" + (index + 2) + ")),AppResult(VarResult(" + (index + 1) + "),VarResult(" + (index + 2) + ")))).apply(List(" + RandomCombinator2(indicator) + "," + RandomCombinator2(indicator) + "," + RandomCombinator2(indicator) + "))"}}
    case 0 => {index=index+1; "Const(1,VarResult(" + index + "))"}
  }
}



  print( RandomCombinator2(1))
  index=0
  print( RandomCombinator2(1))
  index=0
  print( RandomCombinator2(1))
  index=0
  print( RandomCombinator2(1))
  index=0
  print( RandomCombinator2(1))
  index=0
  print( RandomCombinator2(1))
  index=0
  print( RandomCombinator2(1))
  index=0
  print( RandomCombinator2(1))
index=0
print( RandomCombinator2(1))



for (counter <- 1 to 5) {
  println( RandomCombinator2(1))
  index=0
}







def RandomCombinator1(r:Random): String = r match {
  case 0 => "I"
  case 1 => "K"
  case 2 => "S"
}



/*for (counter <- 1 to x) {
  val y: Int = Random.nextInt(3)
  println(y);
  y match {
    case 0 => l = l + "I"
    case 1 => l = l + "K"
    case 2 => l = l + "S"
  }
  println(l)
}
*/*/


