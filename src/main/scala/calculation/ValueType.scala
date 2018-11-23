package calculation

import calculation.Rational.RationalExpr

trait ValueType {
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
  def reciprocal: ValueType
  def toRationalInt: Rational.Rational = this match {
    case DoubleValue(v) => RationalExpr(v.toInt)
    case r: RationalExpr => r
  }

  override def equals(obj: scala.Any): Boolean =
    if(super.equals(obj)) true
    else obj match {
      case x: Double => this.doubleValue == x
      case x: Int => this == RationalExpr(x)
      case x: ValueType => x.doubleValue == doubleValue
      case _ => false
    }

  override def toString: String = this.doubleValue.toString
}
