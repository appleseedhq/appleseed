/*****************************************************************************/
/*                                                                           */
/*  Header: timer.hpp                                                        */
/*                                                                           */
/*  Accompanies STANN Version 0.71 B                                         */
/*  Dec 07, 2009                                                             */
/*                                                                           */
/*  Copyright 2007, 2008                                                     */
/*  Michael Connor and Piyush Kumar                                          */
/*  Florida State University                                                 */
/*  Tallahassee FL, 32306-4532                                               */
/*                                                                           */
/*****************************************************************************/



#ifndef MYTIMER_ 
#define MYTIMER_ 

/*!\file timer.hpp
\brief Implements a timer object*/
#if defined (_MSC_VER) 
// IF visual C++ is being used to compile...

#include <time.h>
#include <windows.h>

const unsigned __int64 MICROSEC_BETWEEN_EPOCHS = 11644473600000000;

// From sys/time.h
struct timezone 
{
  int  tz_minuteswest; /* minutes W of Greenwich */
  int  tz_dsttime;     /* type of dst correction */
};

int gettimeofday(struct timeval *tv, struct timezone *tz)
{
  // Assumes tz == NULL
  if (NULL != tv)
  {
    FILETIME ft; // contains a 64-bit value representing the number of
    	         // 100-nanosecond intervals since Jan 1, 1601
    GetSystemTimeAsFileTime(&ft);

    unsigned __int64 tmpres = ft.dwHighDateTime;
    tmpres <<= 32;
    tmpres |= ft.dwLowDateTime;

    tmpres /= 10;  /* convert into microseconds */
    /* converting file time to unix epoch from windoes epoch */
    tmpres -= MICROSEC_BETWEEN_EPOCHS; 
    tv->tv_sec  = (long)(tmpres / 1000000UL);
    tv->tv_usec = (long)(tmpres % 1000000UL);
  }

  return 0;
}

#else
// On linux its easy...
#include <sys/time.h>
#endif

// Modified from Reviver's original source
class MyTimer
{
public:
	MyTimer()  { restart(); frozen=false;}

 float GetTimeElapsed(){
   if(!frozen)
     gettimeofday(&t, NULL);
   return (float)(t.tv_sec-mstart.tv_sec+1e-6*(t.tv_usec-mstart.tv_usec));
 };

 void  FreezeTimer() {
   gettimeofday(&t, NULL);
   frozen=true;
 };

 void   restart() { gettimeofday(&mstart, NULL); }

 ~MyTimer(){};

private:
 struct timeval mstart;
 struct timeval t;
 bool   frozen;
};


inline std::ostream& operator<<(std::ostream& o, MyTimer& mt){
	long s = static_cast<long> (mt.GetTimeElapsed ());
	long m = 0;

	if(s > 60) {
		m = s / 60;
		s = s - (m*60);
		o << m << " min  " << s << " sec ";
	}
	else {
		o << mt.GetTimeElapsed () << " sec ";
	}

	return o;
}


// Modified from boost library
class MyTimer_display {

 public:
  explicit MyTimer_display( unsigned long expected_count,
                             std::ostream & os = std::cout )
   // os is hint; implementation may ignore
   : _os(os) { restart(expected_count); }

  void           restart( unsigned long expected_count )
  //  Effects: display appropriate scale
  //  Postconditions: count()==0, expected_count()==expected_count
  {
    _count = _next_tic_count = _tic = 0;
    _expected_count = expected_count;

    _os << "\n0%   10   20   30   40   50   60   70   80   90   100%\n"
             "|----|----|----|----|----|----|----|----|----|----|" << std::endl;
    if ( !_expected_count ) _expected_count = 1;  // prevent divide by zero
  } // restart

   unsigned long  operator+=( unsigned long increment )
   //  Effects: Display appropriate progress tic if needed.
   //  Postconditions: count()== original count() + increment
   //  Returns: count().
   {
    if ( (_count += increment) >= _next_tic_count ) { display_tic(); }
    return _count;
   }

   unsigned long  operator++()           { return operator+=( 1 ); }
   unsigned long  count() const          { return _count; }
   unsigned long  expected_count() const { return _expected_count; }

 private:
  std::ostream & _os; // may not be present in all imps
  unsigned long _count, _expected_count, _next_tic_count;
  unsigned int  _tic;
  void display_tic()
  {
    // use of floating point ensures that both large and small counts
    // work correctly.  static_cast<>() is also used several places
    // to suppress spurious compiler warnings.
    unsigned int tics_needed =
      static_cast<unsigned int>(
        (static_cast<float>(_count)/_expected_count)*50.0 );
    do { _os << '*' << std::flush; } while ( ++_tic < tics_needed );
    _next_tic_count =
      static_cast<unsigned long>((_tic/50.0)*_expected_count);
    if ( _count == _expected_count ) {
      if ( _tic < 51 ) _os << '*';
      _os << std::endl;
      }
  } // display_tic

};

#endif
