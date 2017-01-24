
java -Xms512M -Xmx3072M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:+UseCodeCacheFlushing -XX:MaxPermSize=384M -jar %~p0\jars\sbt-launch-0.13.1.jar %*
