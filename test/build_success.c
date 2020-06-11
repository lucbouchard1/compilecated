int bar(int a, float c) {
    int b = 5;

    if (c) {
        return a + b;
    } else {
        return 2;
    }
}

int main() {
    int c = bar(0, 1);
}
