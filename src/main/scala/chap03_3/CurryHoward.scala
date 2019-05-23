package chap03_3

object CurryHoward {

  //
  // ────────────────────────────────────────────────────────────────────────────── I ──────────
  //   :::::: F O R M U L A S   A N D   A X I O M S : :  :   :    :     :        :          :
  // ────────────────────────────────────────────────────────────────────────────────────────
  //

  // forall A. A => A
  def identity[A]: A => A =
    a => a
  // if you have A -> you have A

  // forall A. A => 1
  def toUnit[A]: A => Unit =
    _ => Unit
  // if A then true - Unit is true in Type subspace (if there is such a thing)

  // forall A B. A => A + B
  def inLeft[A, B]: A => Either[A, B] =
    a => Left(a)
  // if you have A -> you have A or B

  // forall A B. A x B => A
  def first[A, B]: (A, B) => A = {
    case (a, _) => a
    // if you have A and B -> you have A
  }

  // forall A B. A => (B => A)
  def const[A, B]: A => B => A =
    a => _ => a
  // if you have A -> A can be returned from *any* function
  // another way to look at it
  // A implies *any* function that returns A

  // ─── PERSPECTIVE ON IMPLICATION ─────────────────────────────────────────────────
  //
  // => (implication) is looked at in type logic as
  // A => B
  // If you have *proof* of A, you have *proof* of B.
  // A *proof* of A is any *value* of type A. Which simplifies
  // further A => B to if you have a *value* of type A then you
  // have a *value* of type B

  //
  // ─── CORRESPONDENCE WITH ARITHMETIC ─────────────────────────────────────────────
  //
  // Equality is Biconditional <=>, <->, Read isomorphism below
  //
  // Identity
  // A x 1 = A
  //
  // Associativity
  // (A x B) x C = A x (B x C)
  // (A + B) + C = A + (B + C)
  //
  // Distribution of product over sum
  // A x (B + C) = (A x B) + (A x C)

  //
  // ─── TYPE ISOMORPHISM ───────────────────────────────────────────────────────────
  //
  // Types A and B are isomorphic, A = B, if there is a 1-to-1
  // correspondence b/w sets of values of these types
  // -> Need to to find two functions f: A => B and g: B => A
  //    such that f . g = id, id of B and g . f = id, id of A
  //
  // The interpretation of this is that values of one type can
  // be encoded in the other, there is no *loss* of information
  // going from A => B and B => A.

  // computivity is too trivial
  // a x b = b x a -> swap function for tuples
  // a + b = b + a -> swap function for Either

  // 1 a x 1 == a
  //
  // forall a. a x 1 -> a
  def a1[A]: ((A, Unit)) => A = { case (a, _) => a }
  // forall a. a -> a x 1
  def a2[A]: A => (A, Unit) = (_, Unit)

  // 2 (a x b) x c == a x (b x c)
  //
  // forall a b c. (a x b) x c -> a x (b x c)
  def b1[A, B, C]: (((A, B), C)) => (A, (B, C)) = {
    case ((a, b), c) => (a, (b, c))
  }
  // forall a b c. a x (b x c) -> (a x b) x c
  def b2[A, B, C]: ((A, (B, C))) => ((A, B), C) = {
    case (a, (b, c)) => ((a, b), c)
  }

  // 3 (a + b) x c == a x c + b x c
  // Distribution of conjunction over disjunction
  //
  // forall a b c. (a + b) x c -> a x b + b x c
  def c1[A, B, C]: ((Either[A, B], C)) => Either[(A, C), (B, C)] = {
    case (Left(a), c)  => Left((a, c))
    case (Right(b), c) => Right(b, c)
  }
  // forall a b c. a x b + b x c -> (a + b) x c
  def c2[A, B, C]: Either[(A, C), (B, C)] => (Either[A, B], C) = {
    case Left((a, c))  => (Left(a), c)
    case Right((b, c)) => (Right(b), c)
  }

  // 4 (a x b) + c /= (a + c) x (b + c)
  // Distribution of disjunction over conjunction
  //
  // forall. a b c. (a x b) + c -> (a + c) x (b + c)
  def d1[A, B, C]: Either[(A, B), C] => (Either[A, C], Either[B, C]) = {
    case Left((a, b)) => (Left(a), Left(b))
    case Right(c)     => (Right(c), Right(c))
  }
  // forall a b c. (a + c) x (b + c) -> (a x b) + c
  def d2[A, B, C]: ((Either[A, C], Either[B, C])) => Either[(A, B), C] = {
    case ((Left(a), Left(b))) => Left((a, b))
    case ((Left(a), Right(c))) =>
      Right(c) // loosing information here! `a` is discarded
    case ((Right(c), Left(b)))   => Right(c)
    case ((Right(c), Right(c2))) => Right(c)
  }

  // 5 (a + b) -> c = (a -> c) x (b -> c)
  // This is the definition of sum type a + b for arbitary a and b.
  // Further explaination in wadler's category theory video.
  // Given two arrows f g going from a -> c and b -> c, there exists
  // a third arrow going from the sum type of a, b to c. a + b -> c
  // this arrow is composition of f, g not normal but conceptual
  //
  // forall a b c. (a + b -> c) ->> (a -> c) x (b -> c)
  def e1[A, B, C]: (Either[A, B] => C) => (A => C, B => C) = { fn =>
    (
      (a: A) => fn(Left(a)),
      (b: B) => fn(Right(b))
    )
  }
  // forall a b c. (a -> c) x (b -> c) ->> (a + b -> c)
  def e2[A, B, C]: ((A => C, B => C)) => (Either[A, B] => C) = {
    case (atoc, btoc) => {
      case Left(a)  => atoc(a)
      case Right(b) => btoc(b)
    }
  }

  //
  // ─── LOGIC CH VS ARITHEMATIC CH FOR ELEMENTARY ALGEBRAIC TYPES ──────────────────
  //
  // A + B = Union of two disjoint (no element in common) sets.
  //         Hence size of |A + B| = |A| + |B|
  // A x B = Cartesian product of two sets
  //         Hence size of |A x B| = |A| x |B|
  //
  // A -> B = Set of all maps between A and B
  //          Hence |B|^|A|. For each value of A
  //          the number of permutations of values of B
  // Example: Ordering -> Bool
  //  Ordering {LT, GT, EQ} = 3
  //  Bool {T, F} = 2
  //  _ _ _ Three places to fill from items totaling 2
  //  1 1 1
  //  1 1 2
  //  1 2 1
  //  .....
  // Suffice it to say for each position of 3 we can chose 2 elements
  // This permuation allows repition for each position _ _ _.
  // 2 x 2 x 2 = 2 ^ 3 = |B| ^ |A|
  //
  // Type Equivalence requires sizes of both types to be equal. ie
  // if a = b then |a| = |b | . 1-to-1 mapping b/w them.
  //
  // ─── ARITHEMATIC IDENTIFIES MOSTLY CORRESPOND TO TYPE IDENTITIES ────────────────
  //
  // Given that a^c corresponds to a function from c -> a.
  //
  // (a^c)(b^c) = (ab)^c
  // (c -> a, c -> b) = (c -> (a, b))
  // English: Given a function from c to a and another from c to b. There exists a function
  //          from c to a and b, seems like definition of Product as explained by wadler
  //
  // a^(b+c) = (a^b)(a^c)
  // (b+c) -> a = (b -> a, c -> a)
  // English: Given a function from b+c to a. There exist two function one from b to a another from
  //         c to a
  //
  //
  // ─── LOGIC IDENTITIES ───────────────────────────────────────────────────────────
  //
  // Logic identities do not always give rise to equivelence of types. They reveal "equal implementability"
  // of types ie. a -> b "implies" if a is implementable then b is implementable. They don't reveal
  // much about the types themselves, like how many maps are there from a to b - while arithematic identities
  // reveal that.
  //
  //
  // ─── SUMMARY ────────────────────────────────────────────────────────────────────
  //
  // Arithematic formulas are related to type equivalence
  // Logic formulas are related to implementability (being able to implement types)
  //    - Interesting for functions.
  //
  // Equivalence of types leads to us choosing right types for the job. If two types are equivalent we can
  // use either one of them as going from one type to other is possible.
  // To find equivalent types one need to use arithematic reason. Translate types to arithematic forumala
  // and reason with them using high-school algebra with polynomials and powers. Polynomial types are
  // usually called algebraic types.
  //
  // Exponential polynomial expressions:  constants, sums, products, exponentials.
  // Exponential polynomial types: primitive types, disjunction, tuples, functions.
  //
  // Arithematic = Type
  // a x b       = (a, b)       product/conjunction
  // a + b       = Either[a, b] sum/disjunction
  // a ^ b       =  b => a      exponential/function(implication)
  // a           = a            constant/primitive type

  //
  // ─── WHY IS THERE CORRESPONDENCE BETWEEN ARITHEMATIC AND TYPES ──────────────────
  //
  // Maybe its because arithematic shows the quantitative aspect of a type. ie. for
  // an arithematic equation like this (a + b) x (c + d) = ac + ad + bc + bd.
  // we end up with equivalent numbers on both sides of the equation - may be
  // this is revealing the both the types have same number of elements.
  // And as you explained that the equality (=) is  biconditional (<=>).
  // ie. x number of elements <=(mapping to)=> x number of elements.
  // Hence there is bound to be a 1-to-1 mapping from both directions.
  // I imagine if these observations are correct and sufficient to establish
  // isomorphism b/w two types?

}
