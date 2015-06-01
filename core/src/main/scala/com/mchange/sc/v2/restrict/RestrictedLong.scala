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

object RestrictedLong {
  type ShieldType = AnyVal with RestrictedType.Element[Long];

  // 2.11 compiler inadequacy, does not recognize ShieldType as an extendable type, alas.

  object AnyLong extends RestrictedLong[AnyLong] {
    override def contains( value : Long ) : Boolean = true;
    override protected def create( value : Long ) = new AnyLong( value ); 
  }
  class AnyLong private ( val widen : Long ) extends AnyVal with RestrictedType.Element[Long];

  object UnsignedLong extends Unsigned[UnsignedLong] {
    override protected def create( value : Long ) = new UnsignedLong( value ); 
  }
  class UnsignedLong private ( val widen : Long ) extends AnyVal with RestrictedType.Element[Long];

  abstract class MinUntil[SHIELD <: ShieldType]( val MinValueInclusive : Long, val MaxValueExclusive : Long ) 
      extends RestrictedLong[SHIELD] with RestrictedType.MinUntil {
    require( MaxValueExclusive > MinValueInclusive );
    override def contains( value : Long ) : Boolean = value >= MinValueInclusive && value < MaxValueExclusive;
  }
  abstract class ZeroUntil[SHIELD <: ShieldType]( max : Long ) extends RestrictedLong.MinUntil[SHIELD](0, max );
  abstract class UnsignedWithBitLength[SHIELD <: ShieldType]( bitLength : Int ) extends RestrictedLong.ZeroUntil[SHIELD]( AnyLong(ONE << bitLength).widen );
  abstract class UnsignedWithByteLength[SHIELD <: ShieldType]( byteLength : Int ) extends UnsignedWithBitLength[SHIELD]( byteLength * 8 );
  abstract class Unsigned[SHIELD <: ShieldType] extends ZeroUntil[SHIELD]( Long.MaxValue );
}
trait RestrictedLong[SHIELD <: RestrictedLong.ShieldType] extends RestrictedType[CommonConversions.IntegralToLong.type,Long,SHIELD];
