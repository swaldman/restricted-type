package com.mchange.sc.v2.restrict;

import org.scalacheck._

package object scalacheck {

  type ShieldType[BELLY] = AnyVal with RestrictedType.Element[BELLY];

  private def gen[BELLY, SHIELD <: ShieldType[BELLY]]( factory : RestrictedType[_, BELLY, SHIELD ] )( implicit arb : Arbitrary[BELLY] ) : Gen[SHIELD] = {
    val gen : Gen[BELLY] = Arbitrary.arbitrary( arb );
    gen.filter( _ elem_: factory ).map( factory(_) )
  }

  private[scalacheck] def arbitrary[BELLY, SHIELD <: ShieldType[BELLY]]( factory : RestrictedType[_, BELLY, SHIELD ] )( implicit arb : Arbitrary[BELLY] ) : Arbitrary[SHIELD] = {
    Arbitrary( gen( factory )( arb ) )
  }

  private[scalacheck] def arbitraryFromGen[BELLY, SHIELD <: ShieldType[BELLY]]( factory : RestrictedType[_, BELLY, SHIELD ], gen : Gen[BELLY] ) : Arbitrary[SHIELD] = {
    arbitrary( factory )( Arbitrary( gen ) )
  }

  implicit class Decorator[BELLY, SHIELD <: ShieldType[BELLY]]( val factory : RestrictedType[_, BELLY, SHIELD ] ) extends AnyVal {
    def arbitrary( implicit arb : Arbitrary[BELLY] ) = scalacheck.arbitrary( factory );
    def arbitraryFromGen( gen : Gen[BELLY] )         = scalacheck.arbitraryFromGen( factory, gen );
  }
}
