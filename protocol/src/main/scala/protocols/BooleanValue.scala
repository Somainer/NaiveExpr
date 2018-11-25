package protocols

import protocols.Rational.RationalExpr

object BooleanValue {
  implicit class BooleanValue(value: Boolean) extends ValueType {
    override def +(that: ValueType): ValueType = toRationalInt + that
    override def -(that: ValueType): ValueType = toRationalInt - that
    override def *(that: ValueType): ValueType = toRationalInt * that
    override def /(that: ValueType): ValueType = toRationalInt / that

    override def unary_- : ValueType = - value

    override def reciprocal: ValueType = toRationalInt.reciprocal

    override def doubleValue: Double = toRationalInt.doubleValue

    override def abs: ValueType = toRationalInt.abs

    override def sqrt = toRationalInt.sqrt

    override def ^(that: ValueType):ValueType = toRationalInt ^ that

    override def toDoubleValue: DoubleValue.DoubleValue = DoubleValue(doubleValue)

    override def toRationalInt: Rational.Rational = RationalExpr(if (value) 1 else 0)

    def getValue = value

    override def toBoolean: Boolean = value

    override def equals(obj: Any): Boolean =
      if(super.equals(obj)) true
      else obj match {
        case BooleanValue(`value`) => true
        case x if x == value => true
        case x => x == this
      }

    override def typeName: String = "Boolean"

    override def toString: String = value.toString

  }

  def apply(value: Boolean): BooleanValue = new BooleanValue(value)

  def unapply(arg: BooleanValue): Option[Boolean] = Some(arg.getValue)
}
