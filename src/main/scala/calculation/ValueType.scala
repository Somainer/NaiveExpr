package calculation

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

  override def toString: String = this.doubleValue.toString
}
