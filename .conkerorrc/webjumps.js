//browser_prevent_automatic_form_focus_mode(true);
google_search_bind_number_shortcuts();
webjumps.g = webjumps.google;

// by default search on google
read_url_handler_list = [read_url_make_default_webjump_handler("google")];

// for strings that do not match this
function possibly_valid_url (str) {
    return /^\s*[^\/\s]*(\/|\s*$)/.test(str)
        && /[:\.]/.test(str);
}

// 2c webjumps
define_webjump("o", "http://octopart.com/search?q=%s");

// Webjump oneliners
define_webjump("leo", "http://dict.leo.org/?lp=ende&lang=de&searchLoc=0&cmpType=relaxed&relink=on&sectHdr=off&spellToler=std&search=%s");
define_webjump("wolframalpha", "http://www.wolframalpha.com/input/?i=%s");
define_webjump("youtube", "http://www.youtube.com/results?search_query=%s&search=Search");
define_webjump("gh", "http://github.com/search?q=%s&type=Everything");

// JS Webjumps
define_webjump("longurl", "javascript:void(function(){if(typeof%20jQuery%20==%20'undefined'){var%20s=document.createElement('script');s.src='http://ajax.googleapis.com/ajax/libs/jquery/1.2.6/jquery.min.js';document.getElementsByTagName('head')[0].appendChild(s);}var%20l=document.createElement('script');l.src='http://www.longurlplease.com/js/longurlplease.js';document.getElementsByTagName('head')[0].appendChild(l);function%20runIfReady(){try{if($.longurlplease){%20clearInterval(interval);%20$.longurlplease();}}catch(e){}};%20var%20interval%20=%20window.setInterval(runIfReady,100);}())");

wikipedia_enable_didyoumean = true;
define_wikipedia_webjumps("en", "de");

define_webjump("wayback",
  function (url) {
    if (url) {
      return "http://web.archive.org/web/*/" + url;
    } else {
      return "javascript:window.location.href='http://web.archive.org/web/*/'+window.location.href;";
    }
  },
  $argument = "optional",
  $completer = history_completer($use_history = false, $use_bookmarks = true));
