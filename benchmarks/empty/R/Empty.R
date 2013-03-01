cat("OK\n")

while(TRUE) {
  str = readLines(con="stdin", 1)
  if(str == "EXIT") {
    cat("OK\n");
    break;
  }

  cat("RESULT 42.0")
  cat("\n")
}
