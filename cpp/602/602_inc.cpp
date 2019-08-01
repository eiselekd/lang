/*
 * Local Variables:
 * buffer-gtest-rule:"gtest2"
 * End:
 */

#include "gtest/gtest.h"

unsigned int Factorial( unsigned int number ) {
    return number <= 1 ? number : Factorial(number-1)*number;
}

TEST(FactorialTest, HandlesZeroInput) {
  EXPECT_EQ(2, Factorial(1));
  EXPECT_EQ(1, Factorial(1));
  EXPECT_EQ(2, Factorial(1));
}

TEST(FactorialTest2, HandlesZeroInput2) {
  EXPECT_EQ(1, Factorial(1));
  EXPECT_EQ(2, Factorial(1));
  EXPECT_EQ(1, Factorial(1));
}

int main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
