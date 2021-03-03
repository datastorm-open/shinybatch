rsconnect::setAccountInfo(name='datastorm-demo',
                          token='629C84C6B51885829553B233EC94EAD0',
                          secret='<secret>')

rsconnect::deployApp(
  appDir = "inst/demo_app",
  appName = "shinybatch",
  appTitle = "shinybatch"
)