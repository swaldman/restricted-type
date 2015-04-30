package com.mchange.sc.v2.restrict;

object RestrictedShort {
  type ShieldType = AnyVal with RestrictedType.Element[Short];

  // 2.11 compiler inadequacy, does not recognize ShieldType as an extendable type, alas.

  object AnyShort extends RestrictedShort[AnyShort] {
    override def contains( value : Short ) : Boolean = true;
    override def create( value : Short ) = new AnyShort( value ); 
  }
  class AnyShort private ( val value : Short ) extends AnyVal with RestrictedType.Element[Short];

  object UnsignedShort extends Unsigned[UnsignedShort] {
    override def create( value : Short ) = new UnsignedShort( value ); 
  }
  class UnsignedShort private ( val value : Short ) extends AnyVal with RestrictedType.Element[Short];

  abstract class MinUntil[SHIELD <: ShieldType]( val MinValueInclusive : Short, val MaxValueExclusive : Short ) extends RestrictedShort[SHIELD] with RestrictedType.MinUntil {
    require( MaxValueExclusive > MinValueInclusive );
    override def contains( value : Short ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: ShieldType]( max : Short ) extends RestrictedShort.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: ShieldType]( bitLength : Int ) extends RestrictedShort.ZeroUntil[SHIELD]( AnyShort(1 << bitLength).value );
  abstract class UnsignedWithByteLength[SHIELD <: ShieldType]( byteLength : Int ) extends UnsignedWithBitLength[SHIELD]( byteLength * 8 );
  abstract class Unsigned[SHIELD <: ShieldType] extends ZeroUntil[SHIELD]( Short.MaxValue );
}
trait RestrictedShort[SHIELD <: RestrictedShort.ShieldType] extends RestrictedType[CommonConversions.IntegralToShort.type,Short,SHIELD];
