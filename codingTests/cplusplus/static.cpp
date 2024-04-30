#include <iostream>

int getValue() {
    static int value = 10; // Static variable, initialized once
    return value;
}

int main() {
    int result = getValue();
    std::cout << "Result: " << result << std::endl;

    // Modify the value
    // This won't affect the value returned by getValue()
    getValue() += 5;

    // Call again to see if the value changed
    result = getValue();
    std::cout << "Result after modification: " << result << std::endl;

    return 0;
}
