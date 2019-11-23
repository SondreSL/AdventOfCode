#include <stdio.h>

void sumFile(char *filename);
void firstRepeat(char *filename);


int main(int argc, char *argv[])
{
    char *filename = "day01.txt";

    sumFile(filename);
    firstRepeat(filename);

    return 0;
}


void sumFile(char filename[])
{

    FILE *fp;
    fp = fopen(filename, "r");

    char str[100];
    long i;
    long sum = 0;

    while (fgets(str, 20, fp) != NULL) {
        sscanf(str, "%li", &i);
        sum = sum + i;
    }
    fclose(fp);

    printf("Sum: %li\n", sum);
}


void firstRepeat(char *filename)
{
    FILE *fp;
    fp = fopen(filename, "r");

    char str[100];
    long i;
    long sum = 0;

    char index[1000000];

    for (unsigned int i = 0; i <= 1000000; i++) {
        index[i] = 0;
    }

    while (index[500000 + sum] != 1) {
        if (fgets(str, 20, fp) != NULL) {
            index[500000 + sum] = 1;
            sscanf(str, "%li", &i);
            sum = sum + i;
        } else {
            fclose(fp);
            fp = fopen(filename, "r");
        }
    }
    printf("First repeated sum: %li\n", sum);
}
