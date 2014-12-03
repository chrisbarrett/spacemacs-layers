#!/usr/bin/env macruby
#
# Uses the scripting bridge to capture the current page in Safari.
# Requires the Emacs server to be running.

framework 'ScriptingBridge'
require 'uri'

# Return the app bundle path for the frontmost application.
def current_app
  `osascript -e 'POSIX path of (path to frontmost application as text)'`.chomp
end

# Get the title and URL of the current browser.
def current_browser_tab(app_bundle_path)
  if app_bundle_path =~ /Safari/
    app = SBApplication.applicationWithBundleIdentifier 'com.apple.Safari'
    tab = app.windows[0].currentTab
    [tab.URL, tab.name]
  elsif app_bundle_path =~ /Chrome/
    app = SBApplication.applicationWithBundleIdentifier 'com.google.Chrome'
    tab = app.windows[0].activeTab
    [tab.URL, tab.title]
  else
    abort("unknown app: #{app_bundle_path}")
  end
end

# Shell-escape the given string
def escape(str)
  URI.escape str, /[:?\/']/
end

# Capture URL and TITLE with emacsclient.
def capture(url, title)
  template_key = 'l'
  ec = '/usr/local/bin/emacsclient'
  `#{ec} org-protocol:/capture:/#{template_key}/'#{escape url}'/'#{escape title}'/''`
end

# Perform capture.
app = current_app
url, title = current_browser_tab app
capture url, title
