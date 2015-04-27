package com.mchange.sc.v2.restrict;

object RestrictedShort {
  object AnyShort extends RestrictedShort[AnyShort] {
    override def contains( value : Short ) : Boolean = true;
    override def create( value : Short ) = new AnyShort( value ); 
  }
  class AnyShort private ( val value : Short ) extends AnyVal with RestrictedType.Element[Short];

  object UnsignedShort extends Unsigned[UnsignedShort] {
    override def create( value : Short ) = new UnsignedShort( value ); 
  }
  class UnsignedShort private ( val value : Short ) extends AnyVal with RestrictedType.Element[Short];

  abstract class MinUntil[SHIELD <: AnyVal]( val MinValueInclusive : Short, val MaxValueExclusive : Short ) extends RestrictedShort[SHIELD] with RestrictedType.MinUntil {
    override def contains( value : Short ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: AnyVal]( max : Short ) extends RestrictedShort.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: AnyVal]( bitLength : Int ) extends RestrictedShort.ZeroUntil[SHIELD]( AnyShort(1 << bitLength).value );
  abstract class UnsignedWithByteLength[SHIELD <: AnyVal]( byteLength : Int ) extends UnsignedWithBitLength[SHIELD]( byteLength * 8 );
  abstract class Unsigned[SHIELD <: AnyVal] extends ZeroUntil[SHIELD]( Short.MaxValue );
}
trait RestrictedShort[SHIELD <: AnyVal] extends RestrictedType[CommonConversions.IntegralToShort.type,Short,SHIELD];
