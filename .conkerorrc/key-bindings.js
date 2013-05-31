undefine_key(default_global_keymap, "q"); // just too easy to press
define_key(content_buffer_normal_keymap, "C-t", "find-url-new-buffer");
define_key(content_buffer_normal_keymap, "C-l", "find-url");
//define_key(content_buffer_normal_keymap, "C-t", "make-buffer");
define_key(content_buffer_normal_keymap, "C-w", "kill-current-buffer");
define_key(content_buffer_normal_keymap, "M-left", "back");
define_key(content_buffer_normal_keymap, "M-right", "forward");
