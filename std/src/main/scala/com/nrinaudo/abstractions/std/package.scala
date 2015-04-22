package com.nrinaudo.abstractions

/** Type class instances for standard Scala classes.
  *
  * Note that most of these instances' implementations are sub-optimal and have been written with understandability
  * rather than performance in mind.
  *
  * Additionally, since this is more of a learning experiment than a library designed to actually be used by anyone,
  * some trivial methods have been re-implemented. Typical examples are `List` and `Option`'s `map` method: they already
  * exist, but re-implementing them in the type class instances makes what's actually happening clearer.
  */
package object std {
  /** Declares implicit instances for all standard types. */
  object all extends ListInstances      with
                     ValInstances       with
                     OptionInstances    with
                     Function1Instances with
                     Tuple2Instances    with
                     OrdInstances
}
