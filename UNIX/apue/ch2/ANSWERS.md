Chapter 2
=========
1. Headers must be written with pre-processor directives
  #ifndef VAR  
  #define VAR 100  
  #endif  

2. Primitive system data types on Arch Linux (Kernel 4.0.5) - /usr/include/limits.h

* Number of bits in a `char'.  
CHAR_BIT	8

* Minimum and maximum values a `signed char' can hold.  
SCHAR_MIN	(-128)
SCHAR_MAX	127

* Maximum value an `unsigned char' can hold.  (Minimum is 0.)  
UCHAR_MAX	255

* Minimum and maximum values a `signed short int' can hold.  
SHRT_MIN	(-32768)
SHRT_MAX	32767

* Maximum value an `unsigned short int' can hold.  (Minimum is 0.)  
USHRT_MAX	65535

* Minimum and maximum values a `signed int' can hold.  
INT_MIN	(-INT_MAX - 1)
INT_MAX	2147483647

* Maximum value an `unsigned int' can hold.  (Minimum is 0.)  
UINT_MAX	4294967295U


* Maximum value an `unsigned long int' can hold.  (Minimum is 0.)  
ULONG_MAX	18446744073709551615UL

* Minimum and maximum values a `signed long long int' can hold.  
LLONG_MAX	9223372036854775807LL
LLONG_MIN	(-LLONG_MAX - 1LL)

* Maximum value an `unsigned long long int' can hold.  (Minimum is 0.)  
ULLONG_MAX	18446744073709551615ULL

3. term

key 
---
term - completed at terminal
