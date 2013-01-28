if (load_paths.indexOf("chrome://conkeror-contrib/content/") == -1) {
    load_paths.unshift("chrome://conkeror-contrib/content/");
}

require("new-tabs.js");
tab_bar_button_close = 1;

require("favicon");
read_buffer_show_icons = true;

require("mode-line-buttons.js");
var back_widget;
if (!back_widget) {
    back_widget = make_button_widget("back", "go-back");
}

remove_hook("mode_line_hook", mode_line_adder(clock_widget));

add_hook("mode_line_hook", mode_line_adder(buffer_icon_widget), true);
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(buffer_count_widget), true);
add_hook("mode_line_hook", mode_line_adder(back_widget), true);
