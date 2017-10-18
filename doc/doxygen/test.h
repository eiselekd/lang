struct b {
  int c;
};

struct a : b {
  int value;

  //! A normal member taking two arguments and returning an integer value.
  /*!
    \param a an integer argument.
    \param s a constant character pointer.
    \return The test results
    \sa QTstyle_Test(), ~QTstyle_Test(), testMeToo() and publicVar()
  */
  int testMe(int a,const char *s) { return 0; }

}
