package com.mchange.sc.v2.restrict;

object RestrictedInt {
  object AnyInt extends RestrictedInt[AnyInt] {
    override def contains( value : Int ) : Boolean = true;
    override def create( value : Int ) = new AnyInt( value ); 
  }
  class AnyInt private ( val value : Int ) extends AnyVal with RestrictedType.Element[Int];

  object UnsignedInt extends Unsigned[UnsignedInt] {
    override def create( value : Int ) = new UnsignedInt( value ); 
  }
  class UnsignedInt private ( val value : Int ) extends AnyVal with RestrictedType.Element[Int];

  abstract class MinUntil[SHIELD <: AnyVal]( val MinValueInclusive : Int, val MaxValueExclusive : Int ) extends RestrictedInt[SHIELD] with RestrictedType.MinUntil {
    override def contains( value : Int ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: AnyVal]( max : Int ) extends RestrictedInt.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: AnyVal]( bitLength : Int ) extends RestrictedInt.ZeroUntil[SHIELD]( AnyInt(1 << bitLength).value );
  abstract class UnsignedWithByteLength[SHIELD <: AnyVal]( byteLength : Int ) extends UnsignedWithBitLength[SHIELD]( byteLength * 8 );
  abstract class Unsigned[SHIELD <: AnyVal] extends ZeroUntil[SHIELD]( Int.MaxValue );
}
trait RestrictedInt[SHIELD <: AnyVal] extends RestrictedType[CommonConversions.IntegralToInt.type,Int,SHIELD];
