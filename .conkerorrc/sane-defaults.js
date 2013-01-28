require("clicks-in-new-buffer.js");
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND;
clicks_in_new_buffer_button = 1;

var local_module_path = "file://" + get_pref("conkeror.rcfile") + "/modules";
if (load_paths.indexOf(local_module_path) == -1) {
    load_paths.unshift(local_module_path);
}
require("navigate-on-paste.js");

hints_minibuffer_annotation_mode(true);
url_completion_use_history = true;

session_pref('browser.history_expire_days', 10000);

user_pref("extensions.checkCompatibility", false);
user_pref("extensions.checkUpdateSecurity", false);
session_pref("xpinstall.whitelist.required", false);

// the default page for new buffers.
homepage = "about:blank";

// load urls from the command line in new buffers instead
// of new windows.
url_remoting_fn = load_url_in_new_buffer;

// load download buffers in the background in the current
// window, instead of in new windows.
download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;

// save a keystroke when selecting a dom node by number.
hints_auto_exit_delay = 500;
hints_ambiguous_auto_exit_delay = 0;
hints_display_url_panel = true;

// default directory for downloads and shell commands.
cwd = get_home_directory();
cwd.append("Downloads");

// use emacs as external editor.
editor_shell_command = "e -c";

// view source in your editor.
view_source_use_external_editor = true;

// no silly mouse scroll
user_pref("general.autoScroll", false);

require("session.js");
session_auto_save_auto_load = true;
