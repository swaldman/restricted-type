package com.mchange.sc.v2.restrict;

object RestrictedByte {
  type ShieldType = AnyVal with RestrictedType.Element[Byte];

  // 2.11 compiler inadequacy, does not recognize ShieldType as an extendable type, alas.

  object AnyByte extends RestrictedByte[AnyByte] {
    override def contains( value : Byte ) : Boolean = true;
    override protected def create( value : Byte ) = new AnyByte( value ); 
  }
  class AnyByte private ( val widen : Byte ) extends AnyVal with RestrictedType.Element[Byte];

  object UnsignedByte extends Unsigned[UnsignedByte] {
    override protected def create( value : Byte ) = new UnsignedByte( value ); 
  }
  class UnsignedByte private ( val widen : Byte ) extends AnyVal with RestrictedType.Element[Byte];

  abstract class MinUntil[SHIELD <: ShieldType]( val MinValueInclusive : Byte, val MaxValueExclusive : Byte ) 
      extends RestrictedByte[SHIELD] with RestrictedType.MinUntil {
    require( MaxValueExclusive > MinValueInclusive );
    override def contains( value : Byte ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: ShieldType]( max : Byte ) extends RestrictedByte.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: ShieldType]( bitLength : Int ) extends RestrictedByte.ZeroUntil[SHIELD]( AnyByte(ONE << bitLength).widen );
  abstract class Unsigned[SHIELD <: ShieldType] extends ZeroUntil[SHIELD]( Byte.MaxValue );
}
trait RestrictedByte[SHIELD <: RestrictedByte.ShieldType] extends RestrictedType[CommonConversions.IntegralToByte.type,Byte,SHIELD];
