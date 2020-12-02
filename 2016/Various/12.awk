BEGIN{print "#include <stdio.h>\nint main() { int a,b,c,d;a=b=c=d=0;";}
{printf("L%s:", NR);}
/cpy/{printf("%s=%s;\n",$3,$2);}
/inc/{printf("++%s;\n",$2);}
/dec/{printf("--%s;\n",$2);}
/jnz/{printf("if(%s!=0) goto L%d;\n",$2,NR+$3);}
END{print "printf(\"%d\\n\",a); }";}
