/*
 * Distributed as part of restricted-type v0.0.1
 *
 * Copyright (C) 2015 Machinery For Change, Inc.
 *
 * Author: Steve Waldman <swaldman@mchange.com>
 *
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of EITHER:
 *
 *     1) The GNU Lesser General Public License (LGPL), version 2.1, as 
 *        published by the Free Software Foundation
 *
 * OR
 *
 *     2) The Eclipse Public License (EPL), version 1.0
 *
 * You may choose which license to accept if you wish to redistribute
 * or modify this work. You may offer derivatives of this work
 * under the license you have chosen, or you may provide the same
 * choice of license which you have been offered here.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * You should have received copies of both LGPL v2.1 and EPL v1.0
 * along with this software; see the files LICENSE-EPL and LICENSE-LGPL.
 * If not, the text of these licenses are currently available at
 *
 * LGPL v2.1: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html
 *  EPL v1.0: http://www.eclipse.org/org/documents/epl-v10.php 
 * 
 */

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
