package protocols

import protocols.DoubleValue.DoubleValue
import protocols.Rational.{Rational, RationalExpr}

import scala.language.implicitConversions

object Complex {
  trait Complex extends ValueType {
    def imaginary: ValueType
    def real: ValueType
    def modulus: ValueType
    def argument: ValueType
    def reflection: Complex

    override def toBoolean: Boolean = modulus.toBoolean

    override def toComplex: Complex = this

    override def doubleValue: Double = throw new ArithmeticException("You are not supposed to convert")

    override def typeName: String = {
      val rect = toRectangular
      if (rect.y != 0) "Complex"
      else rect.y.typeName
    }

    override def reciprocal: ValueType = RationalExpr(1) / this

    override def abs: ValueType = modulus
    def toRectangular: RectangularComplex
    def toAngle: AngleComplex

    override def equals(obj: Any): Boolean =
      obj match {
        case x: Complex => x.toRectangular == obj || x.toAngle == obj
        case vt: ValueType => vt.toComplex == this
        case _ => false
      }
  }

  case class RectangularComplex(x: ValueType, y: ValueType) extends Complex { // x + yi
    override def unary_- : ValueType = RectangularComplex(-x, -y)
    override def imaginary: ValueType = y

    override def real: ValueType = x

    override def modulus: ValueType = (x * x + y * y).sqrt

    override def argument: ValueType = DoubleValue {
      if (x.sgn == 0) {
        if (y.sgn > 0) Math.PI / 2
        else if (y.sgn < 0) - Math.PI / 2
        else 0
      }
      else if(x.sgn > 0) Math.atan(y / x)
      else {
        Math.atan(y / x) + Math.PI * y.sgn
      }
    }


    override def reflection: Complex = RectangularComplex(x, -y)

    override def toRectangular: RectangularComplex = this

    override def toAngle: AngleComplex = AngleComplex(modulus, argument)

    override def toString: String = {
      if (modulus == 0) "0"
      else {
        val realPart = if (x == 0) "" else s"$x"
        val imPart = if (y == 0) "" else (y.abs match {
          case d if d == 1 => "i"
          case d => s"${d}i"
        })
        val opPart = if(y.sgn < 0) "-" else if (realPart.length > 0) "+" else ""
        s"(${List(realPart, opPart, imPart) filter (_.length != 0) mkString " "})"
      }

    }

    override def +(that: ValueType): ValueType = {
      val tr = that.toComplex.toRectangular
      RectangularComplex(x + tr.x, y + tr.y)
    }

    override def -(that: ValueType): ValueType = {
      val tr = that.toComplex.toRectangular
      RectangularComplex(x - tr.x, y - tr.y)
    }

    override def *(that: ValueType): ValueType = that match {
      case RectangularComplex(a, b) => RectangularComplex(x * a - y * b, x * b + y * a)
      case d: AngleComplex => d * x
    }

    override def /(that: ValueType): ValueType = toAngle / that

    override def ^(that: ValueType): ValueType = toAngle ^ that
  }

  case class AngleComplex(a: ValueType, theta: ValueType) extends Complex { // A ∠θ
    override def unary_- : ValueType = AngleComplex(a, theta + Math.PI)

    override def imaginary: ValueType = a * theta.sin

    override def real: ValueType = a * theta.cos

    override def modulus: ValueType = a

    override def argument: ValueType = theta

    override def reflection: Complex = toRectangular.reflection

    override def toRectangular: RectangularComplex = RectangularComplex(real, imaginary)

    override def toAngle: AngleComplex = this

    override def toString: String = s"$a∠$theta"

    override def +(that: ValueType): ValueType = toRectangular + that

    override def -(that: ValueType): ValueType = toRectangular - that

    override def *(that: ValueType): ValueType = {
      val tr = that.toComplex.toAngle
      AngleComplex(a * tr.a, theta + tr.theta)
    }

    override def /(that: ValueType): ValueType = {
      val tr = that.toComplex.toAngle
      AngleComplex(a / tr.a, theta - tr.theta)
    }

    override def ^(that: ValueType): ValueType = {
      val tr = that.toComplex.toAngle
      AngleComplex(a * tr.a, theta - tr.theta)
    }
  }


  object AngleComplex {
    def apply[T, U](a: T, theta: U)(implicit fa: T => ValueType, ft: U => ValueType) :AngleComplex =
      apply(fa(a), ft(theta))
    def apply(a: ValueType, theta: ValueType): AngleComplex =
      if (theta > -Math.PI && theta <= Math.PI) new AngleComplex(a, theta)
      else if (theta < 0) {
        val nTheta = DoubleValue(theta.doubleValue + Math.PI * 2)
        AngleComplex(a, nTheta)
      }
      else {
        val nTheta = DoubleValue(theta.doubleValue - Math.PI * 2)
        AngleComplex(a, nTheta)
      }
  }

  implicit def fromNumber(n: Int): Rational = RationalExpr(n)
  implicit def fromDouble(n: Double): ValueType = new DoubleValue(n)
  implicit def toDouble(n: ValueType): Double = n.doubleValue

}
