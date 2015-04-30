package com.mchange.sc.v2.restrict;

object RestrictedType {
  trait MinUntil {
    self : RestrictedType[_,_,_] =>

    val MinValueInclusive : Any;
    val MaxValueExclusive : Any;
    override def mathRep : String = s"[${MinValueInclusive},${MaxValueExclusive})"
  }
  trait Element[+BELLY] extends Any {
    def value : BELLY;
    def unwrap : BELLY = value;

    override def toString : String = {
      import javax.xml.bind.DatatypeConverter;

      val valueStr = value match {
        case ba : Array[Byte]                                                 => s"0x${DatatypeConverter.printHexBinary( ba )}";
        case bs : Seq[Byte @unchecked] if (bs.forall( _.isInstanceOf[Byte] )) => s"0x${DatatypeConverter.printHexBinary( bs.toArray )}";
        case other                                                            => String.valueOf( other );
      }
      s"${this.getClass.getSimpleName()}(${valueStr})"
    }
  }
  trait Converter[+SEARCHME,-T,+BELLY] {
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
    def convert( shield : SHIELD ) : BELLY = shield.value
  }

  final def apply[T]( xb : T )( implicit converter : Converter[SEARCHME,T,BELLY] ) : SHIELD = {
    apply( converter.convert( xb ) )
  }

  final def apply( b : BELLY ) : SHIELD = {
    require( b elem_!: this );
    create( b );
  }

  /**
   *  This method is very much like apply, but the "type-check" is an elidable
   *  assert. Should only be used when it would be an internal, program error
   *  for a noncompilant value to be passed.
   */  
  final def assert( b : BELLY ) : SHIELD = {
    require( b elem_!: this );
    create( b );
  }

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
