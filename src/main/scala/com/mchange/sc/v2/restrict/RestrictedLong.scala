package com.mchange.sc.v2.restrict;

object RestrictedLong {
  type ShieldType = AnyVal with RestrictedType.Element[Long];

  // 2.11 compiler inadequacy, does not recognize ShieldType as an extendable type, alas.

  object AnyLong extends RestrictedLong[AnyLong] {
    override def contains( value : Long ) : Boolean = true;
    override def create( value : Long ) = new AnyLong( value ); 
  }
  class AnyLong private ( val value : Long ) extends AnyVal with RestrictedType.Element[Long];

  object UnsignedLong extends Unsigned[UnsignedLong] {
    override def create( value : Long ) = new UnsignedLong( value ); 
  }
  class UnsignedLong private ( val value : Long ) extends AnyVal with RestrictedType.Element[Long];

  abstract class MinUntil[SHIELD <: ShieldType]( val MinValueInclusive : Long, val MaxValueExclusive : Long ) 
      extends RestrictedLong[SHIELD] with RestrictedType.MinUntil {
    require( MaxValueExclusive > MinValueInclusive );
    override def contains( value : Long ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: ShieldType]( max : Long ) extends RestrictedLong.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: ShieldType]( bitLength : Int ) extends RestrictedLong.ZeroUntil[SHIELD]( AnyLong(1 << bitLength).value );
  abstract class UnsignedWithByteLength[SHIELD <: ShieldType]( byteLength : Int ) extends UnsignedWithBitLength[SHIELD]( byteLength * 8 );
  abstract class Unsigned[SHIELD <: ShieldType] extends ZeroUntil[SHIELD]( Long.MaxValue );
}
trait RestrictedLong[SHIELD <: RestrictedLong.ShieldType] extends RestrictedType[CommonConversions.IntegralToLong.type,Long,SHIELD];
