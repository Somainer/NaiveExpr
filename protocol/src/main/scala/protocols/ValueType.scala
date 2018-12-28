package protocols

import protocols.DoubleValue.DoubleValue
import protocols.Rational.RationalExpr

trait ValueType extends Ordered[ValueType]{
  def + (that: ValueType): ValueType
  def - (that: ValueType): ValueType
  def * (that: ValueType): ValueType
  def / (that: ValueType): ValueType
  def ^ (that: ValueType): ValueType
  def pow: ValueType => ValueType = ^
  def unary_- : ValueType
  def doubleValue: Double
  def sqrt: ValueType = DoubleValue(Math.sqrt(doubleValue))
  def abs: ValueType
  def sin: ValueType = DoubleValue(Math.sin(doubleValue))
  def cos: ValueType = DoubleValue(Math.cos(doubleValue))
  def tan: ValueType = DoubleValue(Math.tan(doubleValue))
  def ln: ValueType = DoubleValue(Math.log(doubleValue))
  def log(that: ValueType) = DoubleValue(ln.doubleValue / Math.log(that.doubleValue))
  def reciprocal: ValueType
  def toRationalInt: Rational.Rational = this match {
    case DoubleValue(v) => RationalExpr(v.toInt)
    case r: RationalExpr => r
  }
  def toComplex: Complex.Complex = Complex.RectangularComplex(this, RationalExpr(0))

  def sgn: Int = compare(DoubleValue(0)).signum

  def toBoolean: Boolean

  override def compare(that: ValueType): Int = this.doubleValue compareTo that.doubleValue

  def toDoubleValue: DoubleValue = DoubleValue(doubleValue)

  def typeName: String

  override def equals(obj: scala.Any): Boolean =
    if(super.equals(obj)) true
    else obj match {
      case x: Double => this.doubleValue == x
      case x: Int => this == RationalExpr(x)
      case x: Complex.Complex => x == obj
      case x: ValueType => x.doubleValue == doubleValue
      case _ => false
    }

  override def toString: String = this.doubleValue.toString
}
