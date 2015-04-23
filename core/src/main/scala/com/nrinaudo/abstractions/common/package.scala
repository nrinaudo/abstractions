package com.nrinaudo.abstractions

package object common {
  type Reader[A, B] = ReaderT[Id, A, B]
}
