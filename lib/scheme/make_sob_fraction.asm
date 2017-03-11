/* scheme/make_sob_fraction.asm
 * Takes 2 sob_integer objects, and place the corresponding Scheme object in R0
 * 
 * Programmer: Tal Berger, 2017
 */

 MAKE_SOB_FRACTION:
  PUSH(FP);
  MOV(FP, SP);
  PUSH(IMM(3));
  CALL(MALLOC);
  DROP(1);
  MOV(IND(R0), T_FRACTION);
  MOV(INDD(R0, 1), FPARG(2));
  MOV(INDD(R0, 2), FPARG(3));
  POP(FP);
  RETURN;
