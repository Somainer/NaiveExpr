package calculation

object DoubleValue {
  implicit class DoubleValue(value: Double) extends ValueType {
    override def +(that: ValueType): ValueType = value + that.doubleValue
    override def -(that: ValueType): ValueType = value - that.doubleValue
    override def *(that: ValueType): ValueType = value * that.doubleValue
    override def /(that: ValueType): ValueType = value / that.doubleValue

    override def unary_- : ValueType = - value

    override def reciprocal: ValueType = 1.0 / value

    override def doubleValue: Double = value

    override def abs: ValueType = Math abs value

    override def sqrt = Math sqrt value

    override def ^(that: ValueType):DoubleValue = Math.pow(value, that.doubleValue)

    override def equals(obj: Any): Boolean =
      if(super.equals(obj)) true
      else obj match {
        case DoubleValue(`value`) => true
        case x if x == value => true
        case x: Rational.Rational => x.doubleValue == value
        case x => x == this
      }

    override def typeName: String = "Float"

  }

  def apply(value: Double): DoubleValue = new DoubleValue(value)

  def unapply(arg: DoubleValue): Option[Double] = Some(arg.doubleValue)
}
