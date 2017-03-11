/* scheme/write_sob_fraction.asm
 * Take a pointer to a Scheme fraction object, and 
 * prints (to stdout) the character representation
 * of that object.
 * 
 * Programmer: Tal Berger, 2017
 */

  WRITE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(R1);
  PUSH(R2);

  MOV(R0, FPARG(0)); // the fraction

  MOV(R1, INDD(R0, 1)); 
  MOV(R1, INDD(R1,1));// the NUMERATOR
  MOV(R2, INDD(R0, 2)); 
  MOV(R2, INDD(R2,1)); // the DENOMINATOR
  PUSH(R1);
  CALL(WRITE_INTEGER);
  DROP(1);
  PUSH(IMM('/'));
  CALL(PUTCHAR);
  DROP(1);
  PUSH(R2);
  CALL(WRITE_INTEGER);
  DROP(1);
  POP(R2);
  POP(R1);
  POP(FP);
  
  RETURN;