// Copyright (C) 2012 John Lapeyre
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.

#include "PrimeSieve.h"
#include "EratSmall.h"
#include "SieveOfEratosthenes.h"
#include "EratBase.h"
#include "WheelFactorization.h"
#include "defs.h"
#include "ParallelPrimeSieve.h"
#include <iostream>
#include <signal.h>


int global_got_interrupt_flag = 0;

/*
 *  These routines are meant to be called from lisp via cffi or an ffi.
 *  The extern "C" is to avoid c++ symbol munging.  gjl 2012
 *
 */


void lisp_cpp_signal_callback_handler(int signum) {
  std::cout << "prime_pi: Caught interrupt " << signum << 
    ". Cancelling sieving." << std::endl;
  global_got_interrupt_flag = 1;
}


extern "C" int check_error_status () {
  return global_got_interrupt_flag;
}

extern "C" uint64_t prime_pi (int k_tuplet, int num_threads, int progress,  uint64_t min , uint64_t max) {
  uint64_t result;
  sighandler_t old_handler;
  old_handler = signal(SIGINT, lisp_cpp_signal_callback_handler);
  global_got_interrupt_flag = 0;
  if (num_threads == 1) {
    PrimeSieve ps;
    switch (k_tuplet) {
    case 1:
      ps.setFlags(ps.COUNT_PRIMES); break;
    case 2:
      ps.setFlags(ps.COUNT_TWINS); break;
    case 3:
      ps.setFlags(ps.COUNT_TRIPLETS); break;
    case 4:
      ps.setFlags(ps.COUNT_QUADRUPLETS); break;
    case 5:
      ps.setFlags(ps.COUNT_QUINTUPLETS); break;
    case 6:
      ps.setFlags(ps.COUNT_SEXTUPLETS); break;
    case 7:
      ps.setFlags(ps.COUNT_SEPTUPLETS); break; }
    if (progress == 1) 
      ps.addFlags(ps.PRINT_STATUS);
    ps.setStartNumber(min);
    ps.setStopNumber(max);
    ps.sieve();
    result = ps.getCounts( k_tuplet - 1 );
  }
  else {
    ParallelPrimeSieve pps;
    pps.setNumThreads(num_threads);
    switch (k_tuplet) {
    case 1:
      pps.setFlags(pps.COUNT_PRIMES); break;
    case 2:
      pps.setFlags(pps.COUNT_TWINS); break;
    case 3:
      pps.setFlags(pps.COUNT_TRIPLETS); break;
    case 4:
      pps.setFlags(pps.COUNT_QUADRUPLETS); break;
    case 5:
      pps.setFlags(pps.COUNT_QUINTUPLETS); break;
    case 6:
      pps.setFlags(pps.COUNT_SEXTUPLETS); break;
    case 7:
      pps.setFlags(pps.COUNT_SEPTUPLETS); break; }
    if (progress == 1) 
      pps.addFlags(pps.PRINT_STATUS);
    pps.setStartNumber(min);
    pps.setStopNumber(max);
    pps.sieve();
    result = pps.getCounts( k_tuplet - 1 );
  }
  signal(SIGINT, old_handler);
  //  std::cout << "global interrupt flag is " << global_got_interrupt_flag << std::endl;
  if ( global_got_interrupt_flag == 1 )
    return 0;
  return result;
}


// not used now
extern "C" uint64_t p_prime_pi (int num_threads, uint64_t min , uint64_t max) {
  ParallelPrimeSieve pps;
  uint64_t result;
  sighandler_t old_handler;
  pps.setNumThreads(num_threads);
  old_handler = signal(SIGINT, lisp_cpp_signal_callback_handler);
  global_got_interrupt_flag = 0;
  result = pps.getPrimeCount(min,max);
  signal(SIGINT, old_handler);
  if ( global_got_interrupt_flag == 1 ) {
    return 0;
  }
  else {
    return result;
  }
}

/*
extern "C" uint64_t prime_pi (uint64_t min , uint64_t max) {
  PrimeSieve ps;
  return (ps.getPrimeCount(min,max));
}
*/

/*
extern "C" uint64_t prime_pi (uint64_t min , uint64_t max) {
  PrimeSieve ps;
  sighandler_t old_handler;
  old_handler = signal(SIGINT, lisp_cpp_signal_callback_handler);
  try {
    return (ps.getPrimeCount(min,max));
  }
  catch (...) {
    std::cout << "prime_pi caught error "  << std::endl;
    signal(SIGINT, old_handler);
    return(0);
  }
  return(0);
}
*/

extern "C" uint64_t prime_twins (uint64_t min , uint64_t max) {
  PrimeSieve ps;
  ps.setFlags(ps.COUNT_TWINS);
  ps.setStartNumber(min);
  ps.setStopNumber(max);
  ps.sieve();
  return (ps.getTwinCount());
}
