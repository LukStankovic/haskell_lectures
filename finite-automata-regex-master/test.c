int match(const char* cs)
{
  int state = 0;
  int accept = 1;
  while (1) {
    switch (*(cs++)) {
      case 'a':
        switch (state) {
          case 0:
            state = 1;
            accept = 0;
            break;
          default: return 0;
        }
        break;
      case 'b':
        switch (state) {
          case 1:
            state = 0;
            accept = 1;
            break;
          default: return 0;
        }
        break;
      case '\0': return accept;
      default: return 0;
    }
  }
}
