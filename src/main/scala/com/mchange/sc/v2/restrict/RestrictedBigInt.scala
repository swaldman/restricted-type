package com.mchange.sc.v2.restrict;

object RestrictedBigInt {
  type ShieldType = AnyVal with RestrictedType.Element[BigInt];

  // 2.11 compiler inadequacy, does not recognize ShieldType as an extendable type, alas.

  object AnyBigInt extends RestrictedBigInt[AnyBigInt] {
    override def contains( value : BigInt ) : Boolean = true;
    override protected def create( value : BigInt ) = new AnyBigInt( value ); 
  }
  final class AnyBigInt private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];

  object UnsignedBigInt extends Unsigned[UnsignedBigInt] {
    override protected def create( value : BigInt ) = new UnsignedBigInt( value ); 
  }
  final class UnsignedBigInt private ( val widen : BigInt ) extends AnyVal with RestrictedType.Element[BigInt];

  abstract class MinUntil[SHIELD <: ShieldType]( val MinValueInclusive : BigInt, val MaxValueExclusive : BigInt ) extends RestrictedBigInt[SHIELD] with RestrictedType.MinUntil {
    require( MaxValueExclusive > MinValueInclusive );
    override def contains( value : BigInt ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: ShieldType]( max : BigInt ) extends RestrictedBigInt.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: ShieldType]( bitLength : Int ) extends RestrictedBigInt.ZeroUntil[SHIELD]( AnyBigInt(1 << bitLength).widen );
  abstract class UnsignedWithByteLength[SHIELD <: ShieldType]( byteLength : Int ) extends UnsignedWithBitLength[SHIELD]( byteLength * 8 );
  abstract class Unsigned[SHIELD <: ShieldType] extends RestrictedBigInt[SHIELD] {
    override def contains( value : BigInt ) : Boolean = value >= 0;
    override def mathRep = "[0,\u221E)";
  }
}
trait RestrictedBigInt[SHIELD <: RestrictedBigInt.ShieldType] extends RestrictedType[CommonConversions.IntegralToBigInt.type,BigInt,SHIELD];
