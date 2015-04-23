package com.nrinaudo.abstractions

/** Common abstractions, such as the {{{Reader}}} monad. */
package object common {
  type Reader[A, B] = ReaderT[Id, A, B]
}
