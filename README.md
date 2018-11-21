# README

## Environment setup

1. Download Java SDK (https://www.oracle.com/technetwork/java/javase/downloads/index.html)
2. Set Path to the JDK bin folder (usually ($JAVA_INSTALL_DIR)\jdk-($VERSION)\bin)
3. (optional) set install path for Leiningen "set LEIN_HOME=.\.lein\", defaults to ($USER)\.lein\
4. In terminal run "lein self-install" to install Leiningen
5. Start nREPL by typing "lein repl" in terminal
6. (optional) Download Clojure extension for VSCode (also Rainbow Brackets extension is recommended)
7. (optional) Connect VSCode with running REPL using command palette ("Clojure: Connect to a running nREPL")
8. Run Clojure project in terminal using "lein run" (read more on Leiningen)

## Docs

1. http://jaredpetersen.github.io/codeprinter/ - used to generate code printout

## Additional resources
 * About Leiningen: https://github.com/technomancy/leiningen/blob/stable/doc/TUTORIAL.md
 * About Clojure: http://clojure-doc.org/articles/tutorials/introduction.html, https://objectcomputing.com/resources/publications/sett/march-2009-clojure-functional-programming-for-the-jvm