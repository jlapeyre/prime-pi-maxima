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

#include "PrimeNumberFinder.h"
#include "PrimeSieve.h"
#include "SieveOfEratosthenes.h"
#include "defs.h"
#include "bithacks.h"

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <sstream>

#if defined(_OPENMP)
  #include <omp.h>
#endif

extern int global_got_interrupt_flag;

/// Bit patterns corresponding to prime k-tuplets
/// within bytes of the sieve array.
const uint32_t PrimeNumberFinder::kTupletBitmasks_[6][5] =
{
  { 0x06, 0x18, 0xc0, END },       // Twin primes
  { 0x07, 0x0e, 0x1c, 0x38, END }, // Prime triplets
  { 0x1e, END },                   // Prime quadruplets
  { 0x1f, 0x3e, END },             // Prime quintuplets
  { 0x3f, END },                   // Prime sextuplets
  { 0xfe, END }                    // Prime septuplets
};

PrimeNumberFinder::PrimeNumberFinder(PrimeSieve& ps) :
  SieveOfEratosthenes(
      std::max<uint64_t>(7, ps.getStartNumber()),
      ps.getStopNumber(),
      ps.getSieveSize(),
      ps.getPreSieveLimit()),
  ps_(ps),
  kTupletByteCounts_(NULL)
{
  static_assert(PrimeSieve::COUNTS_SIZE == 7, "PrimeSieve::COUNTS_SIZE == 7");
  if (ps_.testFlags(ps_.COUNT_KTUPLETS))
    this->initLookupTables();
}

PrimeNumberFinder::~PrimeNumberFinder() {
  if (kTupletByteCounts_ != NULL) {
    for (int i = 0; i < 6; i++)
      delete[] kTupletByteCounts_[i];
    delete[] kTupletByteCounts_;
  }
}

/**
 * Check if PrimeNumberFinder requires a PrimeNumberGenerator
 * object to generate its sieving primes.
 */
bool PrimeNumberFinder::needGenerator() const {
  return (this->getSquareRoot() > this->getPreSieveLimit());
}

/**
 * Initialize the lookup tables needed to count prime k-tuplets
 * (twin primes, prime triplets, ...) per byte.
 */
void PrimeNumberFinder::initLookupTables() {
  kTupletByteCounts_ = new uint32_t*[6];
  for (uint32_t i = 0; i < 6; i++) {
    kTupletByteCounts_[i] = NULL;
    if (ps_.testFlags(ps_.COUNT_TWINS << i)) {
      kTupletByteCounts_[i] = new uint32_t[256];
      for (uint32_t j = 0; j < 256; j++) {
        uint32_t bitmaskCount = 0;
        for (const uint32_t* b = kTupletBitmasks_[i]; *b <= j; b++) {
          if ((j & *b) == *b)
            bitmaskCount++;
        }
        kTupletByteCounts_[i][j] = bitmaskCount;
      }
    }
  }
}

/**
 * Generate/count the primes and prime k-tuplets within the current
 * segment i.e. [segmentLow_+7, segmentHigh_].
 */
void PrimeNumberFinder::analyseSieve(const uint8_t* sieve, uint32_t sieveSize) {
  if (ps_.testFlags(ps_.COUNT_FLAGS))
    this->count(sieve, sieveSize);
  if (ps_.testFlags(ps_.GENERATE_FLAGS))
    this->generate(sieve, sieveSize);
  uint32_t processed = sieveSize * NUMBERS_PER_BYTE;
  ps_.parent_->doStatus(processed);
}

/**
 * Count the primes and prime k-tuplets within
 * the current segment.
 */
void PrimeNumberFinder::count(const uint8_t* sieve, uint32_t sieveSize) {
  // count prime numbers (1 bits within the sieve array)
  if (ps_.testFlags(ps_.COUNT_PRIMES)) {
    const uint64_t* sieve64 = reinterpret_cast<const uint64_t*> (sieve);
    uint32_t size64    = sieveSize / SIZEOF(uint64_t);
    uint32_t bytesLeft = sieveSize % SIZEOF(uint64_t);
    // see bithacks.h
    uint32_t primeCount = popcount_lauradoux(sieve64, size64);
    if (bytesLeft > 0)
      primeCount += popcount_kernighan(&sieve[sieveSize - bytesLeft], bytesLeft);
    // add up to total prime count
    ps_.counts_[0] += primeCount;
  }
  // count prime k-tuplets (i=0 twins, i=1 triplets, ...)
  // using lookup tables
  for (uint32_t i = 0; i < 6; i++) {
    if (ps_.testFlags(ps_.COUNT_TWINS << i)) {
      uint32_t kCount = 0;
      for (uint32_t j = 0; j < sieveSize; j++)
        kCount += kTupletByteCounts_[i][sieve[j]];
      ps_.counts_[i+1] += kCount;
    }
  }
}

/**
 * Generate the primes or prime k-tuplets (twin primes, prime
 * triplets, ...) within the current segment.
 */
void PrimeNumberFinder::generate(const uint8_t* sieve, uint32_t sieveSize) {
  if( global_got_interrupt_flag == 1) {
    std::cout << "sieve found interrupt flag" << std::endl;
    //    generator.finish();
    //    finder.finish();
    return;
  }

  if (ps_.testFlags(ps_.PRINT_KTUPLETS)) {
    uint64_t lowerBound = this->getSegmentLow();
    // i=0 twins, i=1 triplets, ...
    uint32_t i = 0;
    for (; !ps_.testFlags(ps_.PRINT_TWINS << i); i++)
      ;
    // print prime k-tuplets to std::cout
    for (uint32_t j = 0; j < sieveSize; j++, lowerBound += NUMBERS_PER_BYTE) {
      for (const uint32_t* bitmask = kTupletBitmasks_[i]; *bitmask <= sieve[j]; bitmask++) {
        if ((sieve[j] & *bitmask) == *bitmask) {
          uint32_t leastBit = bitScanForward(*bitmask);
          uint32_t limit    = leastBit + (i + 1);
          std::ostringstream kTuplet;
          kTuplet << "(";
          for (uint32_t l = leastBit; l <= limit; l++)
            kTuplet << lowerBound + bitValues_[l] << (l < limit ? ", " : ")\n");
          std::cout << kTuplet.str();
        }
      }
    }
  }
  else
#if defined(_OPENMP)
  #pragma omp critical (generate)
#endif
  {
    // the GENERATE_PRIMES() macro is defined in defs.h
         if (ps_.testFlags(ps_.CALLBACK32_PRIMES))     GENERATE_PRIMES(ps_.callback32_,      uint32_t)
    else if (ps_.testFlags(ps_.CALLBACK32_OOP_PRIMES)) GENERATE_PRIMES(this->callback32_OOP, uint32_t)
    else if (ps_.testFlags(ps_.CALLBACK64_PRIMES))     GENERATE_PRIMES(ps_.callback64_,      uint64_t)
    else if (ps_.testFlags(ps_.CALLBACK64_OOP_PRIMES)) GENERATE_PRIMES(this->callback64_OOP, uint64_t)
    else if (ps_.testFlags(ps_.PRINT_PRIMES))          GENERATE_PRIMES(this->print,          uint64_t)
  }
}

void PrimeNumberFinder::callback32_OOP(uint32_t prime) const {
  ps_.callback32_OOP_(prime, ps_.cbObj_);
}

void PrimeNumberFinder::callback64_OOP(uint64_t prime) const {
  ps_.callback64_OOP_(prime, ps_.cbObj_);
}

void PrimeNumberFinder::print(uint64_t prime) const {
  std::cout << prime << '\n';
}
