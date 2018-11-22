package calculation

import scala.annotation.tailrec

object Rational {
  trait Rational extends ValueType {
    def flatMap(f: (Int, Int) => Rational): Rational
    def unit(a: Int, b: Int): Rational
    def map(f: (Int, Int) => (Int, Int)) = flatMap {
      case (x, y) =>
        val (a, b) = f(x, y)
        unit(a, b)
    }
    def transform(that: Rational)(f: (Int, Int, Int, Int) => (Int, Int)): Rational =
      this.flatMap((x, y) => that.map((a, b) => f(x, y, a, b)))

    def + (that: Rational): Rational = transform(that) {
      case (x, y, a, b) => (x * b + a * y, y * b)
    }

    def - (that: Rational): Rational = transform(that) {
      case (x, y, a, b) => (x * b - a * y, y * b)
    }

    def * (that: Rational): Rational = transform(that) {
      case (x, y, a, b) => (x * a, y * b)
    }

    def / (that: Rational): Rational = this * that.reciprocal

    def ^ (that: ValueType): ValueType = that match {
      case RationalExpr(n, 1) => if(n > 0) map {
        case (x, y) => (Math.pow(x, n).toInt, Math.pow(y, n).toInt)
      } else (this ^ RationalExpr(-n)).reciprocal
      case _ => DoubleValue(Math.pow(this.value, that.doubleValue))
    }

    def reciprocal: Rational = map {
      case (a, b) => (b, a)
    }

    def value: Double

    override def doubleValue: Double = value

    override def abs: ValueType = map {
      case (a, b) => (Math.abs(a), b)
    }

    def unary_- = map {
      case (x, y) => (-x, y)
    }

    def switchOps(that: ValueType)(f: Rational => Rational)(g: Double => Double): ValueType = that match {
      case DoubleValue(x) => DoubleValue(g(x))
      case x: Rational => f(x)
    }

    override def +(that: ValueType): ValueType = switchOps(that)(this + _)(this.doubleValue + _)
    override def -(that: ValueType): ValueType = switchOps(that)(this - _)(this.doubleValue - _)
    override def *(that: ValueType): ValueType = switchOps(that)(this * _)(this.doubleValue * _)
    override def /(that: ValueType): ValueType = switchOps(that)(this / _)(this.doubleValue / _)
//
//    override def *(that: ValueType): ValueType = this * that
//    override def +(that: ValueType): ValueType = this + that
//    override def -(that: ValueType): ValueType = this - that
//    override def /(that: ValueType): ValueType = this / that
  }
  case class RationalExpr(a: Int, b: Int = 1) extends Rational {
    override def unit(a: Int, b: Int): Rational = RationalExpr.apply(a, b)
    override def flatMap(f: (Int, Int) => Rational): Rational = f(a, b)

    override def toString: String = if (b == 1) a.toString else f"$a/$b"

    override def value: Double = a.toDouble / b
  }
  object RationalExpr {
    def apply(a: Int, b: Int = 1): Rational = if (b == 0) Infinity else {
      val g = gcd(a, b)
      val na = a / g
      val nb = b / g
      if (nb > 0) new RationalExpr(na, nb)
      else new RationalExpr(-na, -nb)

    }
    @tailrec
    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    def lcm(a: Int, b: Int) = a / gcd(a, b) * b
  }
  object Infinity extends Rational {
    override def unit(a: Int, b: Int): Rational = Infinity
    override def flatMap(f: (Int, Int) => Rational): Rational = Infinity
    override def toString: String = "NaN"

    override def value: Double = Double.NaN
  }

  implicit def toDouble(r: Rational): Double = r.value
}