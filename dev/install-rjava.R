# One-time installation of rJava and RJDBC on Posit Workbench, without sudo.
#
# R's Java configuration on the server points to /usr/lib/jvm/jre-11-openjdk,
# which no longer exists, so a plain install.packages("rJava") fails.
# This script points the rJava build to a JDK that is installed.
# Not needed once an administrator has run `sudo R CMD javareconf`.

# 1. Pick a JDK
list.files("/usr/lib/jvm")
jh <- "/usr/lib/jvm/java-17-openjdk"   # replace with a directory from the list above

# All three must be TRUE; otherwise this is a JRE, not a JDK
stopifnot(file.exists(file.path(jh, c("bin/javac", "include/jni.h", "lib/server/libjvm.so"))))

# 2. Build rJava against this JDK
Sys.setenv(
  JAVA_HOME            = jh,
  JAVA                 = file.path(jh, "bin/java"),
  JAVAC                = file.path(jh, "bin/javac"),
  JAR                  = file.path(jh, "bin/jar"),
  JAVAH                = "",
  JAVA_CPPFLAGS        = paste0("-I", jh, "/include -I", jh, "/include/linux"),
  JAVA_LIBS            = paste0("-L", jh, "/lib/server -ljvm"),
  JAVA_LD_LIBRARY_PATH = file.path(jh, "lib/server")
)
install.packages(c("rJava", "RJDBC"))

# 3. Afterwards: add JAVA_HOME=<jh> to ~/.Renviron and restart R