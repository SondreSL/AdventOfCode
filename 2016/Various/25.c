int advent(int a)
{
    /* d = a; */    
    /* c = 4; */
    /* while (c != 0) { // 4 times */
    /*     b = 643; */   
    /*     while (b != 0) { // 643 times */
    /*         d++; */   
    /*         b--; */   
    /*     } */
    /*     c--; */    
    /* } */

    int b, c, d;

    d = a + 2572;

    while (true) {
        // Main loop
        a = d;
        do {
            /* b = a; */
            /* a = 0; */
            /* while (true) { */
            /*     c = 2; */
            /*     do { */
            /*         if (b == 0) { */
            /*             /1* --> goto Block C *1/ */
            /*             c = 2 - (a % 2); */
            /*         } */
            /*         b--; c--; */
            /*     } while (c != 0); */
            /*     a++; */
            /* } */
            print(a % 2);
            a = a / 2; 
        } while (a != 0);
    }
    return 0;
}
