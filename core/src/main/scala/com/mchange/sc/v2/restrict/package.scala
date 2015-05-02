package com.mchange.sc.v2;

package object restrict {
  class CannotConvertException( message : String, cause : Throwable = null) extends Exception( message, cause );

  def cannotConvert( value : Any, target : Any ) : Nothing = throw new CannotConvertException( s"Cannot convert ${value} to ${target}" );

  private[restrict] val ONE = BigInt(1);
}
