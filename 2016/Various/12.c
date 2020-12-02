#include <stdio.h>
int main() { int a,b,c,d;a=b=c=d=0;
L1:a=1;
L2:b=1;
L3:d=26;
L4:if(c!=0) goto L6;
L5:if(1!=0) goto L10;
L6:c=7;
L7:++d;
L8:--c;
L9:if(c!=0) goto L7;
L10:c=a;
L11:++a;
L12:--b;
L13:if(b!=0) goto L11;
L14:b=c;
L15:--d;
L16:if(d!=0) goto L10;
L17:c=16;
L18:d=17;
L19:++a;
L20:--d;
L21:if(d!=0) goto L19;
L22:--c;
L23:if(c!=0) goto L18;
printf("%d\n",a); }
