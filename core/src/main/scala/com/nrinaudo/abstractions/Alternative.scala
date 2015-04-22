package com.nrinaudo.abstractions

trait Alternative[F[_]] extends Applicative[F] with MonoidHK[F]