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


  }

  def apply(value: Double): DoubleValue = new DoubleValue(value)

  def unapply(arg: DoubleValue): Option[Double] = Some(arg.doubleValue)
}
