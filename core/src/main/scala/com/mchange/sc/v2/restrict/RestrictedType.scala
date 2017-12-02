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

package com.mchange.sc.v2.restrict

import scala.math.Ordering

object RestrictedType {
  trait MinUntil {
    self : RestrictedType[_,_,_] =>

    val MinValueInclusive : Any;
    val MaxValueExclusive : Any;
    override def mathRep : String = s"[${MinValueInclusive},${MaxValueExclusive})"
  }
  object Element {
    class ElementOrdering[BELLY, T <: Element[BELLY]]()( implicit io : Ordering[BELLY] ) extends Ordering[T] {
      def compare( x : T, y : T ) = io.compare( x.widen, y.widen )
    }

    /*
     * This method seems too hard for the compiler to resolve as an implicit. we get "divergent implicit expansion" errors
     * 
     * You can call the method explicitly, as in
     * 
     *    implicit val ordering = RestrictedType.Element.ordering[Tuple7,MyType]
     * 
     *    val ts = immutable.TreeSet.empty[Tuple7]
     * 
     * For now we define some easier-to-resolve special cases as implicits, below.
     */ 
    def ordering[BELLY, T <: Element[BELLY]]( implicit io : Ordering[BELLY] ) : Ordering[T] = new ElementOrdering[BELLY,T]()(io)

    implicit def byteOrdering[T <: Element[Byte]] = ordering[Byte,T]
    implicit def shortOrdering[T <: Element[Short]] = ordering[Short,T]
    implicit def intOrdering[T <: Element[Int]] = ordering[Int,T]
    implicit def longOrdering[T <: Element[Long]] = ordering[Long,T]
    implicit def bigIntOrdering[T <: Element[BigInt]] = ordering[BigInt,T]
  }
  trait Element[BELLY] extends Any {
    def widen : BELLY;
    def unwrap : BELLY = widen;

    override def toString : String = {
      import com.mchange.lang.ByteUtils;

      val valueStr = widen match {
        case ba : Array[Byte]                                                 => s"0x${ByteUtils.toLowercaseHexAscii( ba )}";
        case bs : Seq[Byte @unchecked] if (bs.forall( _.isInstanceOf[Byte] )) => s"0x${ByteUtils.toLowercaseHexAscii( bs.toArray )}";
        case other                                                            => String.valueOf( other );
      }
      s"${this.getClass.getSimpleName()}(${valueStr})"
    }
  }
  trait Converter[+SEARCHME,T,BELLY] {
    def convert( t : T ) : BELLY; // may throw a CannotConvertException, implies that t cannot be converted, and so cannot belong to any restriction on BELLY
  }

  private val RecoverFalse : PartialFunction[Throwable,Boolean] = { case e : CannotConvertException => false }
  private val RecoverIAE   : PartialFunction[Throwable,Nothing] = { case e : CannotConvertException => throw new IllegalArgumentException( e.getMessage, e ) }
}
trait RestrictedType[SEARCHME, BELLY, SHIELD <: AnyVal with RestrictedType.Element[BELLY]] {
  import RestrictedType.{Converter, RecoverFalse, RecoverIAE};

  protected def create( b : BELLY ) : SHIELD;

  def contains( b : BELLY ) : Boolean;
  def contains[T]( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] ) : Boolean = try this.contains( converter.convert( xb ) ) catch RecoverFalse;

  // the converter below enables tests of the SHIELD type 
  //
  // alas, with widening and a superfluous test, but it will hopefully be rare to ask 
  // whether a thing known at compile time to be a RestrictedTypeFoo 
  // is in fact an element of RestrictedTypeFoo
  //
  // there ought to be some way to encode the tautological truth of this comparison at
  // compile time. i have tried to find it, but failed.
  //
  //   def contains[T]( mustBeShield : T )( implicit evidence : T <:< SHIELD ) : Boolean = true;
  //
  // should work but doesn't. Even though it would find no converter for SHIELD (absent the converter just added below)
  // when a call to contains( ... ) is made, the compiler complains about being required to supply ambiguous implicits, 
  // the converter or evidence param, and dies: "error: ambiguous reference to overloaded definition"

  implicit object WidenConverter extends Converter[SEARCHME,SHIELD,BELLY] {
    def convert( shield : SHIELD ) : BELLY = shield.widen
  }

  final def apply[T]( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] ) : SHIELD = {
    apply( converter.convert( xb ) )
  }

  final def apply( b : BELLY ) : SHIELD = {
    val xb = pretransform(b)
    require( xb elem_!: this );
    create( xb );
  }

  /**
   *  This method is very much like apply, but the "type-check" is an elidable
   *  assert. Should only be used when it would be an internal, program error
   *  for a noncompilant value to be passed.
   */  
  final def assert[T]( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] ) : SHIELD = {
    assert( converter.convert( xb ) )
  }

  /**
   *  This method is very much like apply, but the "type-check" is an elidable
   *  assert. Should only be used when it would be an internal, program error
   *  for a noncompilant value to be passed.
   */  
  final def assert( b : BELLY ) : SHIELD = {
    val xb = pretransform(b)
    Predef.assert( xb elem_!: this );
    create( xb );
  }

  protected def pretransform( b : BELLY ) : BELLY = b

  //final def unsafe( b : BELLY ) : SHIELD = create(b);

  /**
   *  You'll generally want to override this into some concise notation
   *  that describes your restricted type.
   */  
  def mathRep : String = simpleName;

  /**
   *  You usually won't need to override this, but may if you want!
   */  
  val simpleName : String = try { 
    this.getClass.getSimpleName.filter( _ != '$' ) 
  } catch { // workaround Scala bug, "java.lang.InternalError: Malformed class name"
    case e : Throwable => {
      val fqcn = this.getClass.getName;
      fqcn.reverse.dropWhile( _ == '$' ).takeWhile( "$.".indexOf(_) < 0 ).reverse
    }
  }

  override def toString : String = {
    val mr = mathRep;
    if ( mr == simpleName ) simpleName else s"${simpleName}: ${mathRep}";
  }
  def notMemberMessage( a : Any ) = s"${a} \u2209 ${mathRep}";
  def badValueMessage( a : Any ) = "Bad value: " + notMemberMessage( a )

  private def badMessage( a : Any ) : Nothing = throw new IllegalArgumentException( badValueMessage( a ) )

  // convenience aliases of contain, including versions that throw well-messaged IllegalArgumentException rather than return false.
  final def elem_:  ( b : BELLY ) : Boolean = this.contains( b );
  final def elem_!: ( b : BELLY ) : Boolean = this.contains( b ) || badMessage( b );

  final def elem_:[T]  ( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] ) : Boolean = try this.elem_:( converter.convert( xb ) )  catch RecoverFalse;
  final def elem_!:[T] ( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] ) : Boolean = try this.elem_!:( converter.convert( xb ) ) catch RecoverIAE;

  // support the mathematical is-element-of-symbol, why not? http://www.fileformat.info/info/unicode/char/2208/index.htm
  final def \u2208: ( b : BELLY )  : Boolean = this.elem_:( b );
  final def \u2208!: ( b : BELLY ) : Boolean = this.elem_!:( b );

  final def \u2208:[T] ( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] )  : Boolean = this.elem_:( xb )( converter );
  final def \u2208!:[T] ( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] ) : Boolean = this.elem_!:( xb )( converter );

  // and just to be gratuitous, support is-not-element-of
  final def \u2209: ( b : BELLY )  : Boolean = !this.elem_:( b );
  final def \u2209:[T] ( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] )  : Boolean = !this.elem_:( xb )( converter );
}
