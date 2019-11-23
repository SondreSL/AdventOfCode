#include <stdio.h>


int power_level(int x, int y, int grid_serial)
{
    return ((((x + 10) * (((x + 10) * y) + grid_serial)) / 100) % 10) - 5;
}

void find_max(int ans[], int grid[300][300])
{
    int maximum = 0;
    for (int s = 1; s < 301; s++) {
        for (int i = 0; i < 300 - (s - 1); i++) {
            for (int j = 0; j < 300 - (s - 1); j++) {
                int val = 0;
                for (int y = 0; y < s; y++) {
                    for (int x = 0; x < s; x++) {
                        val = val + grid[y + i][x + j];
                    }
                }
                if (val > maximum) {
                    maximum = val;
                    ans[0] = j + 1;
                    ans[1] = i + 1;
                    ans[2] = val;
                    ans[3] = s;
                }
            }
        }
    }
}


int main(int argc, char *argv[])
{
    int puzzle_input = 18;
    int grid[300][300];

    for (int i = 0; i < 300; i++) {
        for (int j = 0; j < 300; j++) {
            grid[i][j] = power_level(j + 1, i + 1, puzzle_input);
        }
    }

    int ans[4];
    find_max(ans, grid);

    for (int i = 0; i < 4; ++i) {
        printf("%d ", ans[i]);
    }
    printf("\n");
    return 0;
}
