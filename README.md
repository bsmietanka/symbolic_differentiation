# README

## Environment setup

1. Download Java SDK (https://www.oracle.com/technetwork/java/javase/downloads/index.html)
2. Set Path to the JDK bin folder (usually ($JAVA_INSTALL_DIR)\jdk-($VERSION)\bin)
3. (optional) set install path for Leiningen "set LEIN_HOME=.\.lein\", defaults to ($USER)\.lein\
4. In terminal run "lein self-install" to install Leiningen
5. (optional) Download Clojure extension (syntax colouring) for VSCode (also Rainbow Brackets extension is recommended)
6. Run Clojure project in terminal using "lein run" (read more on Leiningen)

Running in REPL:
1. Start nREPL by typing "lein repl" in terminal
2. (optional) Connect VSCode with running REPL using command palette ("Clojure: Connect to a running nREPL"), Clojure: Eval compiles current file
3. In nREPL "(load-file "$SRC_FILE_PATH")"
4.          "($NAMESPACE/$FUNC_NAME $ARGS)" - to run function

## Additional resources
 * About Leiningen: https://github.com/technomancy/leiningen/blob/stable/doc/TUTORIAL.md
 * About Clojure: http://clojure-doc.org/articles/tutorials/introduction.html, https://objectcomputing.com/resources/publications/sett/march-2009-clojure-functional-programming-for-the-jvm