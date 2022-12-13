object Main extends App {
  def compteMots(chaine: String): Int =  chaine.trim() match {
    case "" => 0
    case _ => {
      if(chaine.contains(" ")) {
        1 + compteMots(chaine.substring(chaine.indexOf(" ")+1))
      } else {
        1
    }
  }
  }

  println("Exo 1 \n")

  println("Q1")
  println( compteMots("Hello world !"))
  println(compteMots(""))
  println(compteMots(" "))
  println(compteMots("Hello"))
  println(" \n")
  def egaux(a: List[Int], b: List[Int]): Boolean = {
    def helper(a: List[Int], b: List[Int]): Boolean = (a,b) match {
      case (Nil, Nil) => true
      case (Nil, _) => false
      case (_, Nil) => false
      case (xh:: xt, yh :: yt) => if(xh== yh) helper(xt, yt) else false
    }
    helper(a,b)
  }

  println("Q2")
  println(egaux(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 3 :: Nil))
  println(egaux(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 4 :: Nil))
  println(egaux(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 3 :: 4 :: Nil))
  println(" \n")
def min(a: List[Int]): Int = {
  def helper(a: List[Int], min: Int): Int = a match {
    case Nil => min
    case xh :: xt => if(xh < min) helper(xt, xh) else helper(xt, min)
  }
  helper(a, a.head)
}
println("Q3")
println(min(9 :: 2 :: 3 :: Nil));
println(" \n")

println("Exo 2 \n")
sealed trait Liste[+A]

case class NonVide[+A](tete: A, queue: Liste[A]) extends Liste[A]

object Vide extends Liste[Nothing]

class A;
class B extends  A;


println("Q1")

object Liste {
  def apply[A](first: A, others: A*): Liste[A] = {
    NonVide(first, others.toList match {
      case Nil => Vide
      case xh :: xt => apply(xh, xt: _*)
    })
  }
  // def map[B](f: A => B): Liste[B] = {
  //    NonVide(f)
  // }
}

println("Q1")
println("\n")
println(Liste(1, 2, 3, 4, 5, 6)) // == NonVide(1, NonVide(2, NonVide(3, NonVide(4, NonVide(5, NonVide(6, Vide))))))

println(Liste(1)) // == NonVide(1, Vide)

println(Liste(1, 2, 3)) // == NonVide(1, NonVide(2, NonVide(3, Vide)))

println("Q2")
println("\n")
//println((Vide: Liste[Int]).map[Int](x => x + 1)) // ==  Vide

//println(Liste(1, 2, 3, 4).map(x => x + 1)) // == Liste(2, 3, 4, 5)

println("Ex 3\n")

sealed trait Article

final case class Regular(name: String, category: String, price: Double) extends Article

// discount est compris entre 0 et 1
final case class Discounted(name: String, category: String, price: Double, discount: Float) extends Article

println("Q1")
def applyDiscount(article: Discounted): Double = {
  article.price * (1 - article.discount)
}

println("Q2")
def price(article: Article): Double = {
  article match {
    case Regular(_, _, price) => price
    case Discounted(_, _, price, discount) => price * (1 - discount)
  }
}

println("Q3")
def cartAmount(articles: List[Article]): Double = {
  articles match {
    case Nil => 0
    case xh :: xt => price(xh) + cartAmount(xt)
  }
}

val articles: List[Article] = List(
  Regular(name = "Biscuits", category = "food", price = 2.0),
  Discounted(name = "Monitor", category = "tech", price = 119.99, discount = 0.1f),
  Discounted(name = "Mouse", category = "tech", price = 25.50, discount = 0.2f),
  Regular(name = "dress", category = "clothes", price = 49.90)
)

println(cartAmount(articles))


println("Q4")

def applyCoupon(coupon: Float, category: String)(article: Article): Article = {
  article match {
    case Regular(name, category, price) => Discounted(name, category, price, coupon)
    case Discounted(name, category, price, discount) => Discounted(name, category, price, discount + coupon)
  }
}
}

