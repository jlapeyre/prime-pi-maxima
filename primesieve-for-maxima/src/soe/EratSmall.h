//
// Copyright (c) 2011 Kim Walisch, <kim.walisch@gmail.com>.
// All rights reserved.
//
// This file is part of primesieve.
// Homepage: http://primesieve.googlecode.com
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
//   * Redistributions of source code must retain the above copyright
//     notice, this list of conditions and the following disclaimer.
//   * Redistributions in binary form must reproduce the above
//     copyright notice, this list of conditions and the following
//     disclaimer in the documentation and/or other materials provided
//     with the distribution.
//   * Neither the name of the author nor the names of its
//     contributors may be used to endorse or promote products derived
//     from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
// FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
// OF THE POSSIBILITY OF SUCH DAMAGE.

#ifndef ERATSMALL_H
#define ERATSMALL_H

#include "EratBase.h"
#include "WheelFactorization.h"
#include "defs.h"

class SieveOfEratosthenes;

/**
 * EratSmall is an implementation of the segmented sieve of
 * Eratosthenes with wheel factorization optimized for small sieving
 * primes with many multiples per segment.
 * It uses a sieve array with 30 numbers per byte, 8 bytes per sieving
 * prime and a hardcoded modulo 30 wheel that skips multiples of 2, 3
 * and 5. The algorithm is a further optimized implementation of Achim
 * Flammenkamp's prime_sieve.c, see:
 * http://wwwhomes.uni-bielefeld.de/achim/prime_sieve.html
 */
class EratSmall: public EratBase<Modulo30Wheel_t> {
public:
  EratSmall(const SieveOfEratosthenes&);
  void sieve(uint8_t*, uint32_t);
};

#endif /* ERATSMALL_H */
