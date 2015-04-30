package com.mchange.sc.v2.restrict;

object RestrictedInt {
  type ShieldType = AnyVal with RestrictedType.Element[Int];

  // 2.11 compiler inadequacy, does not recognize ShieldType as an extendable type, alas.

  object AnyInt extends RestrictedInt[AnyInt] {
    override def contains( value : Int ) : Boolean = true;
    override protected def create( value : Int ) = new AnyInt( value );
  }
  class AnyInt private ( val widen : Int ) extends AnyVal with RestrictedType.Element[Int];

  object UnsignedInt extends Unsigned[UnsignedInt] {
    override protected def create( value : Int ) = new UnsignedInt( value );
  }
  class UnsignedInt private ( val widen : Int ) extends AnyVal with RestrictedType.Element[Int];

  abstract class MinUntil[SHIELD <: ShieldType]( val MinValueInclusive : Int, val MaxValueExclusive : Int ) 
      extends RestrictedInt[SHIELD] with RestrictedType.MinUntil {
    require( MaxValueExclusive > MinValueInclusive );
    override def contains( value : Int ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: ShieldType]( max : Int ) extends RestrictedInt.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: ShieldType]( bitLength : Int ) extends RestrictedInt.ZeroUntil[SHIELD]( AnyInt(1 << bitLength).widen );
  abstract class UnsignedWithByteLength[SHIELD <: ShieldType]( byteLength : Int ) extends UnsignedWithBitLength[SHIELD]( byteLength * 8 );
  abstract class Unsigned[SHIELD <: ShieldType] extends ZeroUntil[SHIELD]( Int.MaxValue );
}
trait RestrictedInt[SHIELD <: RestrictedInt.ShieldType] extends RestrictedType[CommonConversions.IntegralToInt.type,Int,SHIELD];
