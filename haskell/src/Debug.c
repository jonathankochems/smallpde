void perf_marker(void)
{
     unsigned int tbl, tbu0, tbu1;

     // do 
     {
      __asm__ __volatile__ ("nop");
     } //while (tbu0 != tbu1);

}

