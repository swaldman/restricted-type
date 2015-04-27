package com.mchange.sc.v2.restrict;

object RestrictedByte {
  object AnyByte extends RestrictedByte[AnyByte] {
    override def contains( value : Byte ) : Boolean = true;
    override def create( value : Byte ) = new AnyByte( value ); 
  }
  class AnyByte private ( val value : Byte ) extends AnyVal with RestrictedType.Element[Byte];

  object UnsignedByte extends Unsigned[UnsignedByte] {
    override def create( value : Byte ) = new UnsignedByte( value ); 
  }
  class UnsignedByte private ( val value : Byte ) extends AnyVal with RestrictedType.Element[Byte];

  abstract class MinUntil[SHIELD <: AnyVal]( val MinValueInclusive : Byte, val MaxValueExclusive : Byte ) extends RestrictedByte[SHIELD] with RestrictedType.MinUntil {
    override def contains( value : Byte ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: AnyVal]( max : Byte ) extends RestrictedByte.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: AnyVal]( bitLength : Int ) extends RestrictedByte.ZeroUntil[SHIELD]( AnyByte(1 << bitLength).value );
  abstract class Unsigned[SHIELD <: AnyVal] extends ZeroUntil[SHIELD]( Byte.MaxValue );
}
trait RestrictedByte[SHIELD <: AnyVal] extends RestrictedType[CommonConversions.IntegralToByte.type,Byte,SHIELD];
